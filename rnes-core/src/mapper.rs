use crate::log;

use std::fs::File;
use std::io::Read;
use std::str;

pub struct Mapper {
    prg_ram: Box<[u8]>,
    prg_rom: Box<[u8]>,
    chr_rom: Box<[u8]>,
    prg_banks: usize,
    chr_banks: usize,
    chr_ram: Box<[u8]>,

    prg_bank: usize,
    chr_bank_1: usize,
    chr_bank_2: usize,

    mmc1_control_writes: u8,
    mmc1_control_reg: u8,

    nametable_mirror_addr: [usize; 4],
}

impl Mapper {
    pub fn from_ines_file(filename: &str) -> Mapper {
        let mut file = File::open(filename).unwrap();
        let mut header = [0u8; 16];
        file.read(&mut header).unwrap();

        let magic = str::from_utf8(&header[0..4]).unwrap();
        if magic != "NES\x1a" {
            panic!("Invalid iNES file");
        }

        let prg_rom_banks = header[4] as usize; // *16KB
        let chr_rom_banks = header[5] as usize; // * 8KB
        let mapper = header[6] >> 4;
        let nametable_mirror_addr = match header[6] & 1 {
            0 => [0, 0, 0x400, 0x400],
            _ => [0, 0x400, 0, 0x400],
        };

        log_info!("mapper", "PRG mapper size:  {} x 16KB", prg_rom_banks);
        log_info!("mapper", "CHR mapper size:  {} x  8KB", chr_rom_banks);
        log_info!("mapper", "Mapper:           {}", mapper);
        log_info!("mapper", "Nametable config: {:?}", nametable_mirror_addr);

        // TODO: There could be a trainer (consuming 512 bytes) before prg_rom
        let mut prg_rom = vec![0; prg_rom_banks * 0x4000].into_boxed_slice();
        file.read(&mut prg_rom).unwrap();
        let mut chr_rom = vec![0; chr_rom_banks * 0x2000].into_boxed_slice();
        file.read(&mut chr_rom).unwrap();

        Mapper {
            prg_ram: vec![0; 0x2000].into_boxed_slice(),
            prg_rom: prg_rom,
            chr_rom: chr_rom,
            chr_ram: vec![0; 0x2000].into_boxed_slice(),

            prg_banks: prg_rom_banks,
            chr_banks: chr_rom_banks,

            prg_bank: 0,
            chr_bank_1: 0,
            chr_bank_2: 0,

            mmc1_control_writes: 0,
            mmc1_control_reg: 0,

            nametable_mirror_addr,
        }
    }

    pub fn get_physical_nametable_addr(&self, address: usize) -> usize {
        let chunk = (address & 0b1100_0000_0000) >> 10; // 1K chunk index
        let addr = address & 0b0011_1111_1111; // 1K addresses

        self.nametable_mirror_addr[chunk as usize] + addr
    }

    pub fn read_byte(&self, offset: u16) -> u8 {
        match offset {
            0x6000...0x7fff => self.prg_ram[offset as usize - 0x6000],
            0x8000...0xbfff => {
                let bank_offset = self.prg_bank * 0x4000;
                let offset = (offset - 0x8000) as usize + bank_offset;
                self.prg_rom[offset]
            }
            0xc000...0xffff => {
                let bank_offset = (self.prg_banks - 1) * 0x4000;
                let offset = (offset - 0xc000) as usize + bank_offset;
                self.prg_rom[offset]
            }
            _ => panic!("Unknown cartridge offset {:04X}", offset),
        }
    }

    pub fn write_byte(&mut self, offset: u16, value: u8) {
        match offset {
            0x6000...0x7fff => self.prg_ram[offset as usize - 0x6000] = value,
            0x8000...0xffff => {
                if value & 0xf0 != 0 {
                    self.mmc1_control_writes = 0;
                } else {
                    self.mmc1_control_writes += 1;
                    self.mmc1_control_reg >>= 1;
                    self.mmc1_control_reg |= if value & 0x01 != 0 { 0b10000 } else { 0 };
                    if self.mmc1_control_writes == 5 {
                        self.mmc1_control_writes = 0;
                        let bank_reg = (offset >> 13) & 0b11;
                        let bank_flags = self.mmc1_control_reg & 0b11111;
                        self.write_bank_reg(bank_reg as u8, bank_flags as u8);
                    }
                }
            }
            _ => panic!("Unknown cartrige address for write {:04X}", offset),
        }
    }

    fn write_bank_reg(&mut self, bank_reg: u8, bank_flags: u8) {
        // println!("Writing to bank register {:02X} {:02X}", bank_reg, bank_flags);

        // Mirroring
        match bank_reg {
            // Mirroring
            0 => {
                let mirror_flag = (bank_flags >> 0) & 0b11;
                let prg_rom_mode = (bank_flags >> 2) & 0b11;
                let chr_rom_mode = (bank_flags >> 3) & 0b1;
                log_stub!(
                    "mapper",
                    "Mirroring is set to {} prg_mode={}, chr_mode={}",
                    mirror_flag,
                    prg_rom_mode,
                    chr_rom_mode
                );
            }
            1 => {
                self.chr_bank_1 = (bank_flags & 0b0001_1111) as usize;
                log_info!(
                    "mapper",
                    "Switching CHR0 bank to 0x{:04X}",
                    self.chr_bank_1 * 0x1000,
                );
            }
            2 => {
                self.chr_bank_2 = (bank_flags & 0b0001_1111) as usize;
                log_info!(
                    "mapper",
                    "Switching CHR1 bank to 0x{:04X}",
                    self.chr_bank_2 * 0x1000,
                );
            }
            // Prg switching
            3 => {
                self.prg_bank = (bank_flags & 0b1111) as usize;
            }
            _ => panic!("Unknown bank register: {}", bank_reg),
        }
    }

    pub fn read_word(&self, offset: u16) -> u16 {
        (self.read_byte(offset) as u16) | ((self.read_byte(offset + 1) as u16) << 8)
    }

    pub fn read_chr_byte(&self, offset: u16) -> u8 {
        if self.chr_banks == 0 {
            return self.chr_ram[offset as usize];
        }

        match offset {
            0x0000...0x0fff => self.chr_rom[self.chr_bank_1 * 0x1000 + offset as usize],
            0x1000...0x1fff => self.chr_rom[self.chr_bank_2 * 0x1000 + offset as usize],
            _ => panic!("Invalid CHR address {:04X}", offset),
        }
    }

    pub fn write_chr_byte(&mut self, offset: u16, value: u8) {
        if self.chr_banks == 0 {
            self.chr_ram[offset as usize] = value;
        } else {
            panic!("Writing to readonly CHR memory!");
        }
    }
}
