use apu::Apu;
use game_pad::GamePad;
use mapper::Mapper;
use mem_map;
use mem_map::Addr;
use ppu::Ppu;
use sinks::*;

/// Interface for reading and writing to memory and memory mapped IO
pub trait MemoryBus {
    /// Writes a byte to the given address
    fn write_byte(&mut self, addr: u16, value: u8);
    /// Reades a byte from the given address
    fn read_byte(&mut self, addr: u16) -> u8;

    /// Reades addr and addr+1
    fn read_word(&mut self, addr: u16) -> u16 {
        let b1 = self.read_byte(addr);
        let b2 = self.read_byte((addr & 0xff00) + ((addr + 1) & 0x00ff));
        (b1 as u16) | ((b2 as u16) << 8)
    }

    fn read_zeropage_word(&mut self, addr: u8) -> u16 {
        let b1 = self.read_byte(addr as u16);
        let b2 = self.read_byte(addr.wrapping_add(1) as u16);
        (b1 as u16) | ((b2 as u16) << 8)
    }
}

pub struct Interconnect {
    ram: Box<[u8]>,
    rom: Mapper,
    ppu: Ppu,
    apu: Apu,
    pub game_pad: GamePad,
}

impl Interconnect {
    pub fn new(rom: Mapper) -> Interconnect {
        Interconnect {
            ram: vec![0; 0x0800].into_boxed_slice(),
            rom: rom,
            ppu: Ppu::new(),
            apu: Apu::new(),
            game_pad: GamePad::new(),
        }
    }

    // Runs `cycles` cycles of the devices connected to the interconnect
    pub fn cycles(
        &mut self,
        cycles: u64,
        frame_sink: &mut Sink<VideoFrame>,
        audio_sink: &mut Sink<AudioFrame>,
    ) -> (bool, bool) {
        // Execute n cycles on different hardware
        let (nmi, dma) = self.ppu.cycles(cycles, &self.rom, frame_sink);
        let irq = self.apu.cycles(cycles, audio_sink);

        // TODO: The DMA operation, as it is implemented here is instantaneous....
        // TODO: What happens if the oam_addr_reg is initially != 0?
        if let Some(dma) = dma {
            self.ppu.write_oam_addr_reg(0);
            for i in 0..256 {
                let val = self.read_byte(dma + i as u16);
                self.ppu.write_oam_data_reg(val);
            }
        }

        (nmi, irq)
    }
}

impl MemoryBus for Interconnect {
    fn write_byte(&mut self, addr: u16, value: u8) {
        let addr = mem_map::map_addr(addr);
        match addr {
            Addr::Ram(offset) => self.ram[offset as usize] = value,
            Addr::RegPpuCtrl => self.ppu.write_ctrl_reg(value),
            Addr::RegPpuMask => self.ppu.write_mask_reg(value),
            Addr::RegOamAddr => self.ppu.write_oam_addr_reg(value),
            Addr::RegOamData => self.ppu.write_oam_data_reg(value),
            Addr::RegPpuScroll => self.ppu.write_scroll_reg(value),
            Addr::RegPpuAddr => self.ppu.write_addr_reg(value),
            Addr::RegPpuData => self.ppu.write_data_reg(&mut self.rom, value),
            Addr::RegApuChannelControl(channel) => {
                self.apu.write_channel_control_reg(channel, value)
            }
            Addr::RegApuSweepUnit(channel) => self.apu.write_sweep_unit_reg(channel, value),
            Addr::RegApuTimerLow(channel) => self.apu.write_timer_low_reg(channel, value),
            Addr::RegApuLengthCouterTimerHigh(channel) => {
                self.apu.write_length_counter_timer_high_reg(channel, value)
            }
            Addr::RegApuDmcControl => self.apu.write_dmc_control_reg(value),
            Addr::RegApuDmcLoadCounter => self.apu.write_dmc_load_counter_reg(value),
            Addr::RegApuDmcSampleAddress => self.apu.write_dmc_sample_address_reg(value),
            Addr::RegApuDmcSampleLength => self.apu.write_dmc_sample_length_reg(value),
            Addr::RegOamDma => self.ppu.write_oam_dma_reg(value),
            Addr::RegApuStatus => self.apu.write_status_reg(value),
            Addr::RegJoy1 => self.game_pad.write_joy1_reg(value),
            Addr::RegJoy2 => self.apu.write_frame_counter_reg(value),
            Addr::Cartridge(offset) => self.rom.write_byte(offset, value),
            _ => panic!("Unknown address {:?} (write byte)", addr),
        }
    }

    fn read_byte(&mut self, addr: u16) -> u8 {
        let addr = mem_map::map_addr(addr);
        match addr {
            Addr::Ram(offset) => self.ram[offset as usize],
            Addr::RegPpuStatus => self.ppu.read_status_reg(),
            Addr::RegOamData => self.ppu.read_oam_data_reg(),
            Addr::RegApuStatus => 0, // TODO
            Addr::RegPpuData => self.ppu.read_data_reg(&mut self.rom),
            Addr::RegJoy1 => self.game_pad.read_joy1_reg(),
            Addr::RegJoy2 => 0, // TODO
            Addr::Cartridge(offset) => self.rom.read_byte(offset),
            _ => panic!("Unknown address {:?} (read byte)", addr),
        }
    }
}
