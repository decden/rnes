use frame_sink::MostRecentFrameSink;

use rnes_core::game_pad::Button;
use rnes_core::sinks::{AudioFrame, Sink, SinkRef};
use rnes_core::Mapper;
use rnes_core::Nes;
use time_source::TimeSource;

use minifb::{Key, Scale, Window, WindowOptions};

use std::{thread, time};

struct SimpleAudioFrameSink {
    inner: Vec<AudioFrame>,
}

impl Sink<AudioFrame> for SimpleAudioFrameSink {
    fn append(&mut self, frame: AudioFrame) {
        self.inner.push(frame);
    }
}

pub struct Emulator {
    window: Window,

    pub nes: Nes,

    audio_sink: Box<SinkRef<[AudioFrame]>>,
    time_source: Box<TimeSource>,
    time_source_start_time_ns: u64,

    emulated_cycles: u64,
}

impl Emulator {
    pub fn new(
        rom: Mapper,
        audio_sink: Box<SinkRef<[AudioFrame]>>,
        time_source: Box<TimeSource>,
    ) -> Emulator {
        Emulator {
            window: Window::new(
                "rnes",
                256,
                240,
                WindowOptions {
                    borderless: true,
                    title: true,
                    resize: false,
                    scale: Scale::X4,
                },
            )
            .unwrap(),

            nes: Nes::new(rom),

            audio_sink: audio_sink,
            time_source: time_source,
            time_source_start_time_ns: 0,

            emulated_cycles: 0,
        }
    }

    pub fn run(&mut self) {
        self.time_source_start_time_ns = self.time_source.time_ns();

        while self.window.is_open() && !self.window.is_key_down(Key::Escape) {
            let mut video_sink = MostRecentFrameSink::new();
            let mut audio_sink = SimpleAudioFrameSink { inner: Vec::new() };

            let target_emulated_time_ns =
                self.time_source.time_ns() - self.time_source_start_time_ns;
            let target_emulated_cycles = target_emulated_time_ns / 559;

            let mut cycles = 0;
            while self.emulated_cycles < target_emulated_cycles && cycles < 100000 {
                cycles += 1;
                self.emulated_cycles += self.nes.step(&mut video_sink, &mut audio_sink);
            }

            self.audio_sink.append(audio_sink.inner.as_slice());
            audio_sink.inner.clear();

            if let Some(frame) = video_sink.into_frame() {
                self.window.update_with_buffer(&frame).unwrap();

                // Read input
                self.read_input();
            }

            thread::sleep(time::Duration::from_millis(1));
        }
    }

    fn read_input(&mut self) {
        self.nes
            .interconnect
            .game_pad
            .set_button_pressed(Button::A, self.window.is_key_down(Key::A));
        self.nes
            .interconnect
            .game_pad
            .set_button_pressed(Button::B, self.window.is_key_down(Key::S));
        self.nes
            .interconnect
            .game_pad
            .set_button_pressed(Button::Select, self.window.is_key_down(Key::Space));
        self.nes
            .interconnect
            .game_pad
            .set_button_pressed(Button::Start, self.window.is_key_down(Key::Enter));

        self.nes
            .interconnect
            .game_pad
            .set_button_pressed(Button::Up, self.window.is_key_down(Key::Up));
        self.nes
            .interconnect
            .game_pad
            .set_button_pressed(Button::Down, self.window.is_key_down(Key::Down));
        self.nes
            .interconnect
            .game_pad
            .set_button_pressed(Button::Left, self.window.is_key_down(Key::Left));
        self.nes
            .interconnect
            .game_pad
            .set_button_pressed(Button::Right, self.window.is_key_down(Key::Right));
    }
}
