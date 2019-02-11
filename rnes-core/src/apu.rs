use crate::log;
use crate::sinks::*;

use APU_SAMPLING_FREQUENCY;
use CPU_FREQUENCY;

const APU_SAMPLING_RATE: f64 = APU_SAMPLING_FREQUENCY / CPU_FREQUENCY;
const APU_FRAME_COUNTER_RATE: f64 = 240.0 / CPU_FREQUENCY;

#[derive(Debug, Eq, PartialEq)]
pub enum ApuChannel {
    Pulse1,
    Pulse2,
    Triangle,
    Noise,
}

#[derive(Debug)]
enum FrameCounterMode {
    FourSteps,
    FiveSteps,
}

#[derive(Debug)]
pub struct Apu {
    cpu_cycles_since_start: u64,
    dmc_counter: u8,

    frame_counter: u32,
    frame_counter_mode: FrameCounterMode,
    frame_counter_irq_inhibit: bool,

    pulse1: Pulse,
    pulse2: Pulse,

    enable_dmc: bool,
    enable_noise: bool,
    enable_triangle: bool,

    triangle_timer: u16,
    noise_timer: u16,

    a: u16,
}

const PULSE_LENGTH_TABLE: [u8; 32] = [
    10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96, 22,
    192, 24, 72, 26, 16, 28, 32, 30,
];
const PULSE_DUTY_TABLE: [[i16; 8]; 4] = [
    [0, 1, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 1, 1, 1, 1, 1],
];

impl Apu {
    pub fn new() -> Apu {
        Apu {
            cpu_cycles_since_start: 0,

            dmc_counter: 0,

            frame_counter: 0,
            frame_counter_mode: FrameCounterMode::FourSteps,
            frame_counter_irq_inhibit: false,

            pulse1: Pulse::new(1),
            pulse2: Pulse::new(2),

            enable_dmc: false,
            enable_noise: false,
            enable_triangle: false,

            triangle_timer: 0,
            noise_timer: 0,

            a: 0,
        }
    }

    pub fn cycles(&mut self, cycles: u64, audio_sink: &mut Sink<AudioFrame>) -> bool {
        let executed_apu_cycles = self.cpu_cycles_since_start / 2;
        self.cpu_cycles_since_start += cycles;
        let target_apu_cycles = self.cpu_cycles_since_start / 2;
        let mut fire_irq = false;

        for i in executed_apu_cycles..target_apu_cycles {
            self.step_timer();

            let f1 = ((i * 2 + 0) as f64 * APU_FRAME_COUNTER_RATE) as u64;
            let f2 = ((i * 2 + 2) as f64 * APU_FRAME_COUNTER_RATE) as u64;
            if f1 != f2 {
                fire_irq |= self.step_frame_counter();
            }

            let s1 = ((i * 2 + 0) as f64 * APU_SAMPLING_RATE) as u64;
            let s2 = ((i * 2 + 2) as f64 * APU_SAMPLING_RATE) as u64;
            if s1 != s2 {
                let val = self.pulse1.sample() as u16 + self.pulse2.sample() as u16;
                let val = (val as i16 - 8) * 128 * 16;
                audio_sink.append((val, val));
            }
        }

        fire_irq
    }

    pub fn step_timer(&mut self) {
        self.pulse1.step_timer();
        self.pulse2.step_timer();
    }

    pub fn step_frame_counter(&mut self) -> bool {
        let mut fire_irq = false;
        match self.frame_counter_mode {
            FrameCounterMode::FourSteps => {
                self.frame_counter = (self.frame_counter + 1) % 4;
                match self.frame_counter {
                    0 | 2 => {
                        self.step_envelope();
                    }
                    1 => {
                        self.step_envelope();
                        self.step_sweep();
                        self.step_length();
                    }
                    3 => {
                        self.step_envelope();
                        self.step_sweep();
                        self.step_length();
                        fire_irq = true;
                    }
                    _ => {}
                }
            }
            FrameCounterMode::FiveSteps => {
                // println!("[APU] WARN Five step counting is not yet implemented!");
            }
        }

        fire_irq
    }

    pub fn step_envelope(&mut self) {
        self.pulse1.step_envelope();
        self.pulse2.step_envelope()
    }
    pub fn step_sweep(&mut self) {
        self.pulse1.step_sweep();
        self.pulse2.step_sweep();
    }
    pub fn step_length(&mut self) {
        self.pulse1.step_length();
        self.pulse2.step_length();
    }

    pub fn write_channel_control_reg(&mut self, channel: ApuChannel, value: u8) {
        match channel {
            ApuChannel::Pulse1 => self.pulse1.write_control(value),
            ApuChannel::Pulse2 => self.pulse2.write_control(value),
            ApuChannel::Triangle => {
                let _control = (value & 0b1000_0000) >> 7;
            }
            ApuChannel::Noise => {
                let _halt = (value & 0b0010_0000) >> 5;
                let _const_volume = (value & 0b0001_0000) >> 4;
                let _volume = value & 0b0000_1111;
            }
        }
    }

    pub fn write_sweep_unit_reg(&mut self, channel: ApuChannel, value: u8) {
        match channel {
            ApuChannel::Pulse1 => self.pulse1.write_sweep(value),
            ApuChannel::Pulse2 => self.pulse2.write_sweep(value),
            _ => panic!(
                "Writing {:02X} to APU_SWEEPUNIT register for channel {:?}",
                value, channel
            ),
        }
    }

    pub fn write_timer_low_reg(&mut self, channel: ApuChannel, value: u8) {
        match channel {
            ApuChannel::Pulse1 => self.pulse1.write_timer_lo(value),
            ApuChannel::Pulse2 => self.pulse2.write_timer_lo(value),
            ApuChannel::Triangle => {
                self.triangle_timer = (self.triangle_timer & 0xff00) | (value as u16);
            }
            ApuChannel::Noise => {
                self.noise_timer = (self.noise_timer & 0xff00) | (value as u16);
            }
        }
    }

    pub fn write_length_counter_timer_high_reg(&mut self, channel: ApuChannel, value: u8) {
        match channel {
            ApuChannel::Pulse1 => self.pulse1.write_timer_hi(value),
            ApuChannel::Pulse2 => self.pulse2.write_timer_hi(value),
            ApuChannel::Triangle => {
                self.triangle_timer =
                    (self.triangle_timer & 0x00ff) | (((value as u16) & 0x07ff) << 8);
            }
            ApuChannel::Noise => {
                self.noise_timer = (self.noise_timer & 0x00ff) | (((value as u16) & 0x07ff) << 8);
            }
        }
    }
    pub fn write_dmc_control_reg(&mut self, value: u8) {
        // TODO: Implement register
        let irq_enable = (value & 0b1000_0000) != 0;
        let do_loop = (value & 0b0100_0000) != 0;
        let frequency = value & 0b0000_1111;
        log_stub!(
            "APU",
            "Writing DMC_CONTROL_REG irq_enable={} do_loop={} frequency={}",
            irq_enable,
            do_loop,
            frequency
        )
    }

    pub fn write_dmc_load_counter_reg(&mut self, value: u8) {
        self.dmc_counter = value & 0b0111_1111;
    }

    pub fn write_dmc_sample_address_reg(&mut self, value: u8) {
        // TODO: Implement register
        let sample_address = value;
        log_stub!(
            "APU",
            "Writing DMC_SAMPLE_ADDRESS_REG value=0x{:02X}",
            sample_address
        );
    }

    pub fn write_dmc_sample_length_reg(&mut self, value: u8) {
        // TODO: Implement register
        let sample_length = value;
        log_stub!(
            "APU",
            "Writing DMC_SAMPLE_LENGTH_REG value=0x{:02X}",
            sample_length
        );
    }

    pub fn write_status_reg(&mut self, value: u8) {
        self.enable_dmc = (value & 0b0001_0000) != 0;
        self.enable_noise = (value & 0b0000_1000) != 0;
        self.enable_triangle = (value & 0b0000_0100) != 0;
        self.pulse2.set_enabled((value & 0b0000_0010) != 0);
        self.pulse1.set_enabled((value & 0b0000_0001) != 0);
        // TODO: Clear DMC interrupt if dmc is disabled
        // TODO: Set outputs to 0 when flag is cleared
        log_stub!(
            "APU",
            "Incomplete implementation of write to STATUS_REG value={}",
            value
        );
    }

    pub fn write_frame_counter_reg(&mut self, value: u8) {
        self.frame_counter = 0;
        self.frame_counter_mode = if (value & 0b1000_0000) != 0 {
            FrameCounterMode::FourSteps
        } else {
            FrameCounterMode::FiveSteps
        };
        self.frame_counter_irq_inhibit = (value & 0b0100_0000) != 0;
    }
}

trait NesVoice {
    fn set_enabled(&mut self, enabled: bool);

    fn write_control(&mut self, value: u8);
    fn write_sweep(&mut self, value: u8);
    fn write_timer_lo(&mut self, value: u8);
    fn write_timer_hi(&mut self, value: u8);

    fn step_timer(&mut self);
    fn step_envelope(&mut self);
    fn step_sweep(&mut self);
    fn step_length(&mut self);

    fn sample(&self) -> u8;
}

#[derive(Debug)]
struct Pulse {
    enabled: bool,
    length_enabled: bool,

    channel: u8,

    envelope_enabled: bool,
    envelope_loop: bool,
    envelope_start: bool,
    envelope_period: u8,
    envelope_value: u8,

    sweep_enabled: bool,
    sweep_negate: bool,
    sweep_reload: bool,
    sweep_shift: u8,
    sweep_period: u8,
    sweep_value: u8,

    constant_volume: u8,
    envelope_volume: u8,

    timer_value: u16,
    timer_period: u16,

    length_value: u8,

    duty_mode: u8,
    duty_value: u8,
}

impl Pulse {
    fn new(channel: u8) -> Pulse {
        Pulse {
            enabled: false,
            length_enabled: false,
            channel: channel,

            envelope_enabled: false,
            envelope_loop: false,
            envelope_start: false,
            envelope_period: 0,
            envelope_value: 0,

            sweep_enabled: false,
            sweep_negate: false,
            sweep_reload: false,
            sweep_shift: 0,
            sweep_period: 0,
            sweep_value: 0,

            constant_volume: 0,
            envelope_volume: 0,

            timer_value: 0,
            timer_period: 0,

            length_value: 0,

            duty_mode: 0,
            duty_value: 0,
        }
    }
}

impl NesVoice for Pulse {
    fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    fn write_control(&mut self, value: u8) {
        self.duty_mode = (value >> 6) & 0b11;
        self.length_enabled = ((value >> 5) & 0b1) == 0;
        self.envelope_loop = ((value >> 5) & 0b1) == 1;
        self.envelope_enabled = ((value >> 4) & 0b1) == 0;
        self.envelope_period = value & 0b1111;
        self.constant_volume = value & 0b1111;
        self.envelope_start = true;
    }
    fn write_sweep(&mut self, value: u8) {
        self.sweep_enabled = ((value >> 7) & 0b1) == 1;
        self.sweep_period = ((value >> 4) & 0b111) + 1;
        self.sweep_negate = ((value >> 3) & 1) == 1;
        self.sweep_shift = value & 0b111;
        self.sweep_reload = true;
    }
    fn write_timer_lo(&mut self, value: u8) {
        self.timer_period = (self.timer_period & 0xff00) | (value as u16);
    }
    fn write_timer_hi(&mut self, value: u8) {
        self.length_value = PULSE_LENGTH_TABLE[(value >> 3) as usize];
        self.timer_period = (self.timer_period & 0x00ff) | (((value & 0b111) as u16) << 8);
        self.envelope_start = true;
        self.duty_value = 0;
    }

    fn step_timer(&mut self) {
        if self.timer_value == 0 {
            self.timer_value = self.timer_period;
            self.duty_value = (self.duty_value + 1) % 8;
        } else {
            self.timer_value -= 1;
        }
    }

    fn step_envelope(&mut self) {
        if self.envelope_start {
            self.envelope_volume = 15;
            self.envelope_value = self.envelope_period;
            self.envelope_start = false;
        } else if self.envelope_value > 0 {
            self.envelope_value -= 1;
        } else {
            if self.envelope_volume > 0 {
                self.envelope_volume -= 1;
            } else if self.envelope_loop {
                self.envelope_volume = 15
            }
            self.envelope_value = self.envelope_period;
        }
    }
    fn step_sweep(&mut self) {
        if self.sweep_reload {
            if self.sweep_enabled && self.sweep_value == 0 {
                self.sweep()
            }
            self.sweep_value = self.sweep_period;
            self.sweep_reload = false;
        } else if self.sweep_value > 0 {
            self.sweep_value -= 1;
        } else {
            if self.sweep_enabled {
                self.sweep();
            }
            self.sweep_value = self.sweep_period;
        }
    }
    fn step_length(&mut self) {
        if self.length_enabled && self.length_value > 0 {
            self.length_value -= 1;
        }
    }

    fn sample(&self) -> u8 {
        if !self.enabled {
            return 0;
        }

        if self.length_value == 0 {
            return 0;
        }

        if PULSE_DUTY_TABLE[self.duty_mode as usize][self.duty_value as usize] == 0 {
            return 0;
        }

        if self.timer_period < 8 || self.timer_period > 0x7FF {
            return 0;
        }

        if self.envelope_enabled {
            return self.envelope_volume;
        } else {
            return self.constant_volume;
        }
    }
}

impl Pulse {
    fn sweep(&mut self) {
        let delta = self.timer_period >> self.sweep_shift;
        if self.sweep_negate {
            self.timer_period -= delta;
            if self.channel == 1 {
                self.timer_period -= 1;
            }
        } else {
            self.timer_period += delta;
        }
    }
}
