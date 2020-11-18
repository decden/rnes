use cartridge::Cartridge;
use cpu::Cpu;
use interconnect::Interconnect;
use sinks::*;

pub const CPU_FREQUENCY: f64 = 1_789_773.0;
pub const APU_SAMPLING_FREQUENCY: f64 = 41_000.0;

pub struct Nes {
    pub cpu: Cpu,
    pub interconnect: Interconnect,
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Nes {
        let mut nes = Nes {
            cpu: Cpu::new(),
            interconnect: Interconnect::new(cartridge),
        };
        nes.cpu.reset(&mut nes.interconnect);
        nes
    }

    pub fn step(
        &mut self,
        video_sink: &mut dyn Sink<VideoFrame>,
        audio_sink: &mut dyn Sink<AudioFrame>,
    ) -> u64 {
        let ret = self.cpu.step(&mut self.interconnect);

        let (nmi, irq) = self.interconnect.cycles(ret, video_sink, audio_sink);
        if nmi {
            self.cpu.request_nmi();
        }
        if irq {
            self.cpu.request_irq();
        }

        ret
    }
}
