use crate::log;
use mapper::{Mapper, Mapper000, Mapper001, MapperAddr, MapperPpuAddr};

use std::fs::File;
use std::io::Read;
use std::str;

#[derive(Debug)]
pub enum NametableMirroring {
    Horizontal,
    Vertical,
}

pub struct Cartridge {
    prg_rom: Box<[u8]>,
    chr_rom: Box<[u8]>,

    prg_ram: Box<[u8]>,
    chr_ram: Box<[u8]>,

    mapper_impl: Box<Mapper>,
}

impl Cartridge {
    pub fn from_ines_file(filename: &str) -> Cartridge {
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
        let nametable_mirroring = match header[6] & 1 {
            0 => NametableMirroring::Horizontal,
            _ => NametableMirroring::Vertical,
        };

        log_info!("mapper", "PRG mapper size:     {} x 16KB", prg_rom_banks);
        log_info!("mapper", "CHR mapper size:     {} x  8KB", chr_rom_banks);
        log_info!("mapper", "Mapper:              {}", mapper);
        log_info!("mapper", "Nametable mirroring: {:?}", nametable_mirroring);

        // TODO: There could be a trainer (consuming 512 bytes) before prg_rom
        let mut prg_rom = vec![0; prg_rom_banks * 0x4000].into_boxed_slice();
        file.read(&mut prg_rom).unwrap();
        let mut chr_rom = vec![0; chr_rom_banks * 0x2000].into_boxed_slice();
        file.read(&mut chr_rom).unwrap();

        let mapper: Box<Mapper> = match mapper {
            0 => Box::new(Mapper000::new(nametable_mirroring, prg_rom_banks, chr_rom_banks)),
            1 => Box::new(Mapper001::new(nametable_mirroring, prg_rom_banks)),
            _ => panic!("Unsupported mapper type"),
        };

        Cartridge {
            prg_ram: vec![0; 0x2000].into_boxed_slice(),
            prg_rom: prg_rom,
            chr_rom: chr_rom,
            chr_ram: vec![0; 0x2000].into_boxed_slice(),
            mapper_impl: mapper,
        }
    }

    pub fn get_physical_nametable_addr(&self, address: u16) -> u16 {
        match self.mapper_impl.map_ppu_addr(address) {
            MapperPpuAddr::Vram(vram_addr) => vram_addr,
            _ => panic!("Nametable not in vram?"),
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match self.mapper_impl.map_cpu_read(addr) {
            MapperAddr::PrgRom(offset) => self.prg_rom[offset as usize],
            MapperAddr::PrgRam(offset) => self.prg_ram[offset as usize],
            MapperAddr::InternalReg(value) => value,
        }
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        match self.mapper_impl.map_cpu_write(addr, value) {
            MapperAddr::PrgRom(offset) => self.prg_rom[offset as usize] = value,
            MapperAddr::PrgRam(offset) => self.prg_ram[offset as usize] = value,
            MapperAddr::InternalReg(_) => (),
        };
    }

    pub fn read_chr_byte(&self, offset: u16) -> u8 {
        match self.mapper_impl.map_ppu_addr(offset) {
            MapperPpuAddr::ChrRom(chr_addr) => self.chr_rom[chr_addr as usize],
            MapperPpuAddr::ChrRam(chr_addr) => self.chr_ram[chr_addr as usize],
            _ => panic!("Chr in vram?"),
        }
    }

    pub fn write_chr_byte(&mut self, offset: u16, value: u8) {
        match self.mapper_impl.map_ppu_addr(offset) {
            MapperPpuAddr::ChrRom(_) => panic!("Writing to chr rom?"),
            MapperPpuAddr::ChrRam(chr_addr) => self.chr_ram[chr_addr as usize] = value,
            _ => panic!("Chr in vram?"),
        };
    }
}
