use portaudio;

use rnes_core::sinks::AudioFrame;
use rnes_core::sinks::SinkRef;
use time_source::TimeSource;

use std::borrow::Cow;
use std::cmp::Ordering;
use std::iter::Iterator;
use std::option::Option;
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};

pub struct RingBuffer {
    inner: Box<[i16]>,

    write_pos: usize,
    read_pos: usize,

    samples_read: u64,
}

impl RingBuffer {
    fn push(&mut self, value: i16) {
        self.inner[self.write_pos] = value;

        self.write_pos += 1;
        if self.write_pos >= self.inner.len() {
            self.write_pos = 0;
        }
    }
}

impl Iterator for RingBuffer {
    type Item = i16;

    fn next(&mut self) -> Option<i16> {
        let ret = self.inner[self.read_pos];

        self.read_pos += 1;
        if self.read_pos >= self.inner.len() {
            self.read_pos = 0;
        }

        self.samples_read += 1;

        Some(ret)
    }
}

struct PortAudioBufferSink {
    ring_buffer: Arc<Mutex<RingBuffer>>,
}

impl SinkRef<[AudioFrame]> for PortAudioBufferSink {
    fn append(&mut self, buffer: &[AudioFrame]) {
        let mut ring_buffer = self.ring_buffer.lock().unwrap();
        for &(left, right) in buffer {
            ring_buffer.push(left);
            ring_buffer.push(right);
        }
    }
}

struct PortAudioDriverTimeSource {
    ring_buffer: Arc<Mutex<RingBuffer>>,
    sample_rate: u32,
}

impl TimeSource for PortAudioDriverTimeSource {
    fn time_ns(&self) -> u64 {
        let ring_buffer = self.ring_buffer.lock().unwrap();
        1_000_000_000 * (ring_buffer.samples_read / 2) / (self.sample_rate as u64)
    }
}

pub struct PortAudioDriver {
    ring_buffer: Arc<Mutex<RingBuffer>>,
    sample_rate: u32,

    _portaudio: portaudio::PortAudio,
    _stream: portaudio::Stream<portaudio::NonBlocking, portaudio::Output<i16>>,
    // _voice: VoiceId,
    // _join_handle: JoinHandle<()>,
}

impl PortAudioDriver {
    pub fn new(sample_rate: u32, desired_latency_ms: u32) -> PortAudioDriver {
        if desired_latency_ms == 0 {
            panic!("desired_latency_ms must be greater than 0");
        }

        let buffer_frames = (sample_rate * desired_latency_ms / 1000 * 2) as usize;
        let ring_buffer = Arc::new(Mutex::new(RingBuffer {
            inner: vec![0; buffer_frames].into_boxed_slice(),

            write_pos: 0,
            read_pos: 0,

            samples_read: 0,
        }));

        let output_sample_rate = 41000;
        let pa = portaudio::PortAudio::new().unwrap();
        for device in pa.devices().unwrap() {
            println!("{:?}", device);
        }
        for host in pa.host_apis() {
            println!("{:?}", host);
        }
        let mut settings = pa
            .default_output_stream_settings(2, output_sample_rate as f64, output_sample_rate / 500)
            .unwrap();
        settings.flags = portaudio::stream_flags::CLIP_OFF;

        let mut resampler = LinearResampler::new(sample_rate as _, output_sample_rate as _);
        let read_ring_buffer = ring_buffer.clone();
        let callback = move |portaudio::OutputStreamCallbackArgs { buffer, frames, .. }| {
            let mut read_ring_buffer = read_ring_buffer.lock().unwrap();
            for f in 0..frames {
                buffer[f * 2 + 0] = resampler.next(&mut *read_ring_buffer);
                buffer[f * 2 + 1] = resampler.next(&mut *read_ring_buffer);
            }
            portaudio::Continue
        };

        let mut stream = pa.open_non_blocking_stream(settings, callback).unwrap();
        stream.start().unwrap();

        PortAudioDriver {
            ring_buffer: ring_buffer,
            sample_rate: sample_rate,

            _portaudio: pa,
            _stream: stream,
        }
    }

    pub fn time_source(&self) -> Box<TimeSource> {
        Box::new(PortAudioDriverTimeSource {
            ring_buffer: self.ring_buffer.clone(),
            sample_rate: self.sample_rate,
        })
    }

    pub fn sink(&self) -> Box<SinkRef<[AudioFrame]>> {
        Box::new(PortAudioBufferSink {
            ring_buffer: self.ring_buffer.clone(),
        })
    }
}

struct LinearResampler {
    from_sample_rate: u32,
    to_sample_rate: u32,

    current_from_frame: AudioFrame,
    next_from_frame: AudioFrame,
    from_fract_pos: u32,

    current_frame_channel_offset: u32,
}

impl LinearResampler {
    fn new(from_sample_rate: u32, to_sample_rate: u32) -> LinearResampler {
        let sample_rate_gcd = {
            fn gcd(a: u32, b: u32) -> u32 {
                if b == 0 {
                    a
                } else {
                    gcd(b, a % b)
                }
            }

            gcd(from_sample_rate, to_sample_rate)
        };

        LinearResampler {
            from_sample_rate: from_sample_rate / sample_rate_gcd,
            to_sample_rate: to_sample_rate / sample_rate_gcd,

            current_from_frame: (0, 0),
            next_from_frame: (0, 0),
            from_fract_pos: 0,

            current_frame_channel_offset: 0,
        }
    }

    fn next(&mut self, input: &mut Iterator<Item = i16>) -> i16 {
        fn interpolate(a: i16, b: i16, num: u32, denom: u32) -> i16 {
            (((a as i32) * ((denom - num) as i32) + (b as i32) * (num as i32)) / (denom as i32))
                as _
        }

        let ret = match self.current_frame_channel_offset {
            0 => interpolate(
                self.current_from_frame.0,
                self.next_from_frame.0,
                self.from_fract_pos,
                self.to_sample_rate,
            ),
            _ => interpolate(
                self.current_from_frame.1,
                self.next_from_frame.1,
                self.from_fract_pos,
                self.to_sample_rate,
            ),
        };

        self.current_frame_channel_offset += 1;
        if self.current_frame_channel_offset >= 2 {
            self.current_frame_channel_offset = 0;

            self.from_fract_pos += self.from_sample_rate;
            while self.from_fract_pos > self.to_sample_rate {
                self.from_fract_pos -= self.to_sample_rate;

                self.current_from_frame = self.next_from_frame;

                let left = input.next().unwrap_or(0);
                let right = input.next().unwrap_or(0);
                self.next_from_frame = (left, right);
            }
        }

        ret
    }
}
