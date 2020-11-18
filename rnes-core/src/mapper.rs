use cartridge::NametableMirroring;

#[derive(Debug)]
pub enum MapperAddr {
    PrgRom(usize),   // PRG rom (offset into prg rom, as if contigugous banks)
    PrgRam(usize),   // PRG ram (offset into prg ram)
    InternalReg(u8), // Value
}
#[derive(Debug)]
pub enum MapperPpuAddr {
    ChrRom(usize),
    ChrRam(usize),
    Vram(u16),
}
pub trait Mapper {
    fn map_cpu_read(&self, addr: u16) -> MapperAddr;
    fn map_cpu_write(&mut self, addr: u16, value: u8) -> MapperAddr;

    fn map_ppu_addr(&self, addr: u16) -> MapperPpuAddr;
}

// Mapper 000 [NROM] implementation
pub struct Mapper000 {
    nametable_mirroring: NametableMirroring,
    prg_bank_count: usize,
    chr_rom_banks: usize,
}
impl Mapper000 {
    pub fn new(
        nametable_mirroring: NametableMirroring,
        prg_bank_count: usize,
        chr_rom_banks: usize,
    ) -> Self {
        Self {
            nametable_mirroring,
            prg_bank_count,
            chr_rom_banks,
        }
    }

    fn map_nametable_addr(&self, addr: u16) -> u16 {
        let chunk = (addr & 0b1100_0000_0000) >> 10; // 1K chunk index
        let addr = addr & 0b0011_1111_1111; // 1K addresses

        let mirror_config = match self.nametable_mirroring {
            NametableMirroring::Horizontal => [0, 0, 0x400, 0x400],
            NametableMirroring::Vertical => [0, 0x400, 0, 0x400],
        };

        mirror_config[chunk as usize] + addr
    }
}
impl Mapper for Mapper000 {
    fn map_cpu_read(&self, addr: u16) -> MapperAddr {
        match addr {
            0x6000..=0x7FFF => MapperAddr::PrgRam(addr as usize - 0x6000),
            0x8000..=0xFFFF => {
                MapperAddr::PrgRom((addr as usize - 0x8000) & (self.prg_bank_count * 0x4000 - 1))
            }
            _ => panic!("Invalid mapper 0 address 0x{:04X}", addr),
        }
    }

    fn map_cpu_write(&mut self, addr: u16, _value: u8) -> MapperAddr {
        match addr {
            0x6000..=0x7FFF => MapperAddr::PrgRam(addr as usize - 0x6000),
            0x8000..=0xFFFF => MapperAddr::PrgRom(addr as usize - 0x8000),
            _ => panic!("Invalid mapper 0 address 0x{:04X}", addr),
        }
    }

    fn map_ppu_addr(&self, addr: u16) -> MapperPpuAddr {
        match (addr, self.chr_rom_banks > 0) {
            (0x0000..=0x1FFF, true) => MapperPpuAddr::ChrRom(addr as usize),
            (0x0000..=0x1FFF, false) => MapperPpuAddr::ChrRam(addr as usize),
            (0x2000..=0x2FFF, _) => MapperPpuAddr::Vram(self.map_nametable_addr(addr - 0x2000)),
            _ => panic!("Invalid mapper 0 ppu address 0x{:04X}", addr),
        }
    }
}

// Mapper 001 [MMC1] implementation
pub struct Mapper001 {
    nametable_mirroring: NametableMirroring,
    prg_bank_count: usize,

    prg_bank_idx: usize,
    prg_bank_mode: u8,

    chr_bank_0: usize,
    chr_bank_1: usize,

    mmc1_ctrl_shift_reg: u8,
    mmc1_ctrl_shift_reg_writes: u8,
}
impl Mapper001 {
    pub fn new(nametable_mirroring: NametableMirroring, prg_bank_count: usize) -> Self {
        Self {
            nametable_mirroring,
            prg_bank_count,

            // TODO: Verify initial values
            prg_bank_idx: 0,
            prg_bank_mode: 3,
            chr_bank_0: 0,
            chr_bank_1: 0x1000,

            mmc1_ctrl_shift_reg: 0,
            mmc1_ctrl_shift_reg_writes: 0,
        }
    }

    fn map_nametable_addr(&self, addr: u16) -> u16 {
        let chunk = (addr & 0b1100_0000_0000) >> 10; // 1K chunk index
        let addr = addr & 0b0011_1111_1111; // 1K addresses

        let mirror_config = match self.nametable_mirroring {
            NametableMirroring::Horizontal => [0, 0, 0x400, 0x400],
            NametableMirroring::Vertical => [0, 0x400, 0, 0x400],
        };

        mirror_config[chunk as usize] + addr
    }

    fn prg_bank_0(&self) -> usize {
        match self.prg_bank_mode {
            0 | 1 => (self.prg_bank_idx & 0xfffe) * 0x4000,
            2 => 0,
            3 => self.prg_bank_idx * 0x4000,
            _ => unreachable!(),
        }
    }
    fn prg_bank_1(&self) -> usize {
        match self.prg_bank_mode {
            0 | 1 => (self.prg_bank_idx | 0x0001) * 0x4000,
            2 => self.prg_bank_idx * 0x4000,
            3 => (self.prg_bank_count - 1) * 0x4000,
            _ => unreachable!(),
        }
    }

    fn write_internal_ctrl_reg(&mut self, reg: u16, value: u8) {
        match reg {
            0b00 => {
                let mirror_flag = (value >> 0) & 0b11;
                self.nametable_mirroring = match mirror_flag {
                    0 | 1 => panic!("One screen"),
                    2 => NametableMirroring::Vertical,
                    3 => NametableMirroring::Horizontal,
                    _ => unreachable!(),
                };
                self.prg_bank_mode = (value >> 2) & 0b11;
                let chr_rom_mode = (value >> 3) & 0b1;

                log_stub!(
                    "mapper",
                    "MMC1 Control reg is set to mirroring={}, prg_mode={}, chr_mode={}",
                    mirror_flag,
                    self.prg_bank_mode,
                    chr_rom_mode
                );
            }
            0b01 => {
                self.chr_bank_0 = value as usize * 0x1000;
                log_info!(
                    "mapper",
                    "Switching CHR bank 0 to 0x{:04X}",
                    self.chr_bank_0
                );
            }
            0b10 => {
                self.chr_bank_1 = value as usize * 0x1000;
                log_info!(
                    "mapper",
                    "Switching CHR bank 1 to 0x{:04X}",
                    self.chr_bank_1
                );
            }
            0b11 => {
                self.prg_bank_idx = (value as usize) & 0b01111;
                let prg_ram_enabled = (value & 0b10000) == 0;
                log_info!(
                    "mapper",
                    "Switching PRG bank index={}, prg_ram_enabled={}",
                    self.prg_bank_idx,
                    prg_ram_enabled
                );
            }
            _ => unreachable!(),
        }
    }
}
impl Mapper for Mapper001 {
    fn map_cpu_read(&self, addr: u16) -> MapperAddr {
        match addr {
            0x6000..=0x7FFF => MapperAddr::PrgRam(addr as usize - 0x6000),
            0x8000..=0xBFFF => MapperAddr::PrgRom(addr as usize - 0x8000 + self.prg_bank_0()),
            0xC000..=0xFFFF => MapperAddr::PrgRom(addr as usize - 0xC000 + self.prg_bank_1()),
            _ => panic!("Invalid mapper 1 address 0x{:04X}", addr),
        }
    }

    fn map_cpu_write(&mut self, addr: u16, value: u8) -> MapperAddr {
        match addr {
            0x6000..=0x7FFF => MapperAddr::PrgRam(addr as usize - 0x6000),
            0x8000..=0xFFFF => {
                if value & 0xf0 != 0 {
                    self.mmc1_ctrl_shift_reg_writes = 0;
                } else {
                    self.mmc1_ctrl_shift_reg_writes += 1;
                    self.mmc1_ctrl_shift_reg >>= 1;
                    self.mmc1_ctrl_shift_reg |= if value & 0x01 != 0 { 0b10000 } else { 0 };
                    if self.mmc1_ctrl_shift_reg_writes == 5 {
                        // The last write address (bit 13 and 14) selects the internal register which is written to
                        let value = self.mmc1_ctrl_shift_reg;
                        self.write_internal_ctrl_reg((addr >> 13) & 0b11, value);
                        self.mmc1_ctrl_shift_reg_writes = 0;
                        self.mmc1_ctrl_shift_reg = 0;
                    }
                }
                MapperAddr::InternalReg(value)
            }
            _ => panic!("Invalid mapper 1 address 0x{:04X}", addr),
        }
    }

    fn map_ppu_addr(&self, addr: u16) -> MapperPpuAddr {
        match addr {
            0x0000..=0x0FFF => MapperPpuAddr::ChrRam(addr as usize - 0x0000 + self.chr_bank_0),
            0x1000..=0x1FFF => MapperPpuAddr::ChrRam(addr as usize - 0x1000 + self.chr_bank_1),
            0x2000..=0x3FFF => MapperPpuAddr::Vram(self.map_nametable_addr(addr & 0x0FFF)),
            _ => panic!("Invalid mapper 1 ppu address 0x{:04X}", addr),
        }
    }
}
