extern crate cpal;
extern crate minifb;
extern crate rnes_core;

mod cpal_driver;
mod emulator;
mod frame_sink;
mod time_source;

use cpal_driver::CpalDriver;
use emulator::Emulator;
use rnes_core::Mapper;
use rnes_core::APU_SAMPLING_FREQUENCY;

use std::env;

fn main() {
    let audio_driver = CpalDriver::new(APU_SAMPLING_FREQUENCY as u32, 100).unwrap();

    let audio_buffer_sink = audio_driver.sink();
    let time_source = audio_driver.time_source();

    let args: Vec<String> = env::args().collect();
    let rom = Mapper::from_ines_file(&args[1]);
    let mut emulator = Emulator::new(rom, audio_buffer_sink, time_source);

    emulator.run();
}
