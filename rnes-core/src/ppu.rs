use crate::cartridge::Cartridge;
use crate::sinks::*;

use std::cmp::min;

#[derive(PartialEq, Debug)]
enum SpriteSize {
    S8x8,
    S8x16,
}
#[derive(Debug)]
enum MasterSlave {
    Masater,
    Slave,
}
#[derive(Debug)]
struct PpuCtrlReg {
    base_nametable_address: u16,
    vram_address_increment: u16,
    sprite_pattern_table_address: u16,
    background_pattern_table_address: u16,
    sprite_size: SpriteSize,
    master_slave: MasterSlave,
    vblank_nmi_enabled: bool,
}

#[derive(Copy, Clone)]
struct OamEntry {
    oam_entry_index: u8,
    tile_index: u8,
    tile_x: u8,
    tile_y: u8,
    palette: u8,
    flip_x: bool,
    flip_y: bool,
}

impl From<u8> for PpuCtrlReg {
    fn from(value: u8) -> PpuCtrlReg {
        PpuCtrlReg {
            base_nametable_address: ((value as u16) & 0b0000_0011) * 0x400 + 0x2000,
            vram_address_increment: if value & 0b0000_0100 != 0 { 32 } else { 1 },
            sprite_pattern_table_address: (((value as u16) >> 3) & 0b1) * 0x1000, // ignored in 8x16 mode
            background_pattern_table_address: (((value as u16) >> 4) & 0b1) * 0x1000,
            sprite_size: if value & 0b0010_0000 != 0 {
                SpriteSize::S8x16
            } else {
                SpriteSize::S8x8
            },
            master_slave: if value & 0b0100_0000 != 0 {
                MasterSlave::Masater
            } else {
                MasterSlave::Slave
            }, // TODO: Is this correct?
            vblank_nmi_enabled: value & 0b1000_0000 != 0,
        }
    }
}

const NES_COLORS_LUT: [u32; 64] = [
    0xFF545454, 0xFF001E74, 0xFF081090, 0xFF300088, 0xFF440064, 0xFF5C0030, 0xFF540400, 0xFF3C1800,
    0xFF202A00, 0xFF083A00, 0xFF004000, 0xFF003C00, 0xFF00323C, 0xFF000000, 0xFF000000, 0xFF000000,
    0xFF989698, 0xFF084CC4, 0xFF3032EC, 0xFF5C1EE4, 0xFF8814B0, 0xFFA01464, 0xFF982220, 0xFF783C00,
    0xFF545A00, 0xFF287200, 0xFF087C00, 0xFF007628, 0xFF006678, 0xFF000000, 0xFF000000, 0xFF000000,
    0xFFECEEEC, 0xFF4C9AEC, 0xFF787CEC, 0xFFB062EC, 0xFFE454EC, 0xFFEC58B4, 0xFFEC6A64, 0xFFD48820,
    0xFFA0AA00, 0xFF74C400, 0xFF4CD020, 0xFF38CC6C, 0xFF38B4CC, 0xFF3C3C3C, 0xFF000000, 0xFF000000,
    0xFFECEEEC, 0xFFA8CCEC, 0xFFBCBCEC, 0xFFD4B2EC, 0xFFECAEEC, 0xFFECAED4, 0xFFECB4B0, 0xFFE4C490,
    0xFFCCD278, 0xFFB4DE78, 0xFFA8E290, 0xFF98E2B4, 0xFFA0D6E4, 0xFFA0A2A0, 0xFF000000, 0xFF000000,
];

const SCANLINES: u64 = 262;
const FIRST_VBLANK_SCANLINE: u64 = 242;
const CYCLES_PER_SCANLINE: u64 = 341;
const CYCLES_PER_FRAME: u64 = SCANLINES * CYCLES_PER_SCANLINE;

pub struct Ppu {
    vram: Box<[u8]>,
    oam: [u8; 256],
    oam_buffer: [Option<OamEntry>; 8],
    palette: [u8; 32],

    reg_ctrl: PpuCtrlReg,
    reg_oam_addr: u8,
    reg_mask: u8,
    reg_scroll_x: u64,
    reg_scroll_y: u64,
    reg_address: u16,
    reg_address_latch: bool,

    ppudata_read_buffer: u8,

    flag_vblank: bool,
    flag_sprite_zero_hit: bool,
    dma_write: Option<u16>,

    next_frame: Option<VideoFrame>,
    cycles_since_frame_start: u64,
}

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            vram: vec![0; 0x1000].into_boxed_slice(),
            oam: [0; 256],
            oam_buffer: [None; 8],
            palette: [0; 32],
            reg_ctrl: 0u8.into(),

            reg_mask: 0,
            reg_oam_addr: 0,
            reg_scroll_x: 0,
            reg_scroll_y: 0,
            reg_address: 0,
            reg_address_latch: false,

            ppudata_read_buffer: 0,

            flag_vblank: false,
            flag_sprite_zero_hit: false,
            dma_write: None,

            next_frame: None,
            cycles_since_frame_start: 0,
        }
    }

    pub fn write_ctrl_reg(&mut self, value: u8) {
        self.reg_ctrl = value.into();
    }

    pub fn write_mask_reg(&mut self, value: u8) {
        self.reg_mask = value;
    }

    pub fn read_status_reg(&mut self) -> u8 {
        let status = if self.flag_vblank { 1 << 7 } else { 0 }
            | if self.flag_sprite_zero_hit { 1 << 6 } else { 0 };
        self.flag_vblank = false;
        status
    }

    pub fn write_oam_addr_reg(&mut self, value: u8) {
        self.reg_oam_addr = value;
    }

    pub fn write_oam_data_reg(&mut self, value: u8) {
        self.oam[self.reg_oam_addr as usize] = value;
        self.reg_oam_addr = self.reg_oam_addr.wrapping_add(1);
    }

    pub fn write_oam_dma_reg(&mut self, value: u8) {
        self.dma_write = Some((value as u16) << 8);
    }

    pub fn write_scroll_reg(&mut self, value: u8) {
        self.reg_scroll_x = self.reg_scroll_y;
        self.reg_scroll_y = value as u64;
    }

    pub fn write_addr_reg(&mut self, value: u8) {
        self.reg_address <<= 8;
        self.reg_address |= value as u16;
        self.reg_address_latch = !self.reg_address_latch;
    }

    pub fn write_data_reg(&mut self, cartridge: &mut Cartridge, value: u8) {
        if self.reg_address_latch {
            panic!("Writing to incomplete address!!");
        }
        // 0x2000 to 0x2fff is normally mapped to vram with a mirroring configuration provided by the cartridge..=
        match self.reg_address {
            0x0000..=0x1fff => cartridge.write_chr_byte(self.reg_address, value),
            0x2000..=0x3eff => {
                let address = cartridge.get_physical_nametable_addr(self.reg_address as u16);
                self.vram[address as usize] = value;
            }
            0x3f00..=0x3fff => {
                self.palette[(self.reg_address & 0x1f) as usize] = value & 0b0011_1111;
                if self.reg_address == 0x3f00
                    || self.reg_address == 0x3f04
                    || self.reg_address == 0x3f08
                    || self.reg_address == 0x3f0C
                {
                    self.palette[0x00] = value & 0b0011_1111;
                    self.palette[0x04] = value & 0b0011_1111;
                    self.palette[0x08] = value & 0b0011_1111;
                    self.palette[0x0C] = value & 0b0011_1111;
                }
            }
            _ => panic!("Unknown PPU memory location {:04X}", self.reg_address),
        }
        self.reg_address = self
            .reg_address
            .wrapping_add(self.reg_ctrl.vram_address_increment)
            & 0x3fff;
    }

    pub fn read_oam_data_reg(&mut self) -> u8 {
        self.oam[self.reg_oam_addr as usize]
    }

    pub fn read_addr_reg(&self) -> u8 {
        // TODO: Is this the correct implementation?
        self.reg_address as u8
    }

    pub fn read_data_reg(&mut self, cartridge: &mut Cartridge) -> u8 {
        match self.reg_address {
            0x0000..=0x1FFF => {
                let buffered_value = self.ppudata_read_buffer;
                self.ppudata_read_buffer = cartridge.read_chr_byte(self.reg_address);
                buffered_value
            }
            0x2000..=0x3eff => {
                let address = cartridge.get_physical_nametable_addr(self.reg_address as u16);
                self.vram[address as usize]
            }
            _ => panic!("Unknown PPU memory location {:04X}", self.reg_address),
        }
    }

    pub fn cycles(
        &mut self,
        cycles: u64,
        cartridge: &Cartridge,
        frame_sink: &mut dyn Sink<VideoFrame>,
    ) -> (bool, Option<u16>) {
        let ppu_cycles = cycles * 3;

        let old_scanline = self.cycles_since_frame_start / CYCLES_PER_SCANLINE;
        self.cycles_since_frame_start += ppu_cycles;
        let new_scanline = self.cycles_since_frame_start / CYCLES_PER_SCANLINE;

        // Process the scanlines
        for scanline in old_scanline..new_scanline {
            match scanline {
                // Pre-render
                0 => self.process_prerender_scanline(cartridge),
                1..=240 => self.process_scanline(cartridge, scanline - 1),
                241 => {
                    frame_sink.append(self.next_frame.take().unwrap());
                }
                _ => (), // vblank: nothing to do
            }
        }

        self.cycles_since_frame_start %= CYCLES_PER_FRAME;

        let mut trigger_nmi = false;

        // Set the vblank status and (optionally) trigger a NMI
        // TODO: This logic is not entirely accurate since setting vblank_nmi_enabled to true during vblnk should
        //       also trigger an NMI
        let is_vblank = new_scanline >= FIRST_VBLANK_SCANLINE;
        let was_vblank = old_scanline >= FIRST_VBLANK_SCANLINE;
        if is_vblank && !was_vblank {
            self.flag_vblank = is_vblank;
            if self.reg_ctrl.vblank_nmi_enabled {
                trigger_nmi = true;
            }
        }

        (trigger_nmi, self.dma_write.take())
    }

    pub fn fill_oam_buffer(&mut self, _cartridge: &Cartridge, scanline: u64) {
        // Search the first 8 sprites which fit into the
        let mut sprite = 0;
        for i in 0..64 {
            let sprite_y = self.oam[i * 4 + 0] as u64;
            let begin_y = sprite_y;
            let end_y = match self.reg_ctrl.sprite_size {
                SpriteSize::S8x8 => begin_y + 7,
                SpriteSize::S8x16 => begin_y + 15,
            };
            if scanline >= begin_y && scanline <= end_y {
                self.oam_buffer[sprite] = Some(OamEntry {
                    oam_entry_index: i as u8,
                    tile_index: self.oam[i * 4 + 1],
                    tile_x: self.oam[i * 4 + 3],
                    tile_y: self.oam[i * 4 + 0],
                    palette: (self.oam[i * 4 + 2] & 0b11) + 4,
                    flip_x: self.oam[i * 4 + 2] & 0b0100_0000 != 0,
                    flip_y: self.oam[i * 4 + 2] & 0b1000_0000 != 0,
                });
                sprite += 1;
                if sprite == 8 {
                    break;
                }
            }
        }

        // Mark all of the unused sprites as out of bounds!
        for i in sprite..8 {
            self.oam_buffer[i] = None;
        }
    }

    pub fn process_prerender_scanline(&mut self, _cartridge: &Cartridge) {
        self.flag_sprite_zero_hit = false;
        self.next_frame = Some(vec![0xff000000; 256 * 240].into_boxed_slice());
        self.oam_buffer = [None; 8];
    }

    pub fn process_scanline(&mut self, cartridge: &Cartridge, scanline: u64) {
        for x in 0..256 as u64 {
            let virtual_y = scanline + self.reg_scroll_y;
            let virtual_x = x + self.reg_scroll_x;

            let (bg_pixel, bg_transparent) =
                self.get_background_pixel_at(cartridge, virtual_x, virtual_y);
            if let Some(frame) = self.next_frame.as_mut() {
                frame[(scanline * 256 + x) as usize] = bg_pixel;
            }

            // Fill sprite scanline
            let oam_buffer = self.oam_buffer;
            for oam_option in oam_buffer.iter() {
                if let Some(oam_entry) = oam_option.as_ref() {
                    let y = oam_entry.tile_y as u64;
                    let x = oam_entry.tile_x as u64;
                    let tile_index = match self.reg_ctrl.sprite_size {
                        SpriteSize::S8x8 => oam_entry.tile_index,
                        SpriteSize::S8x16 => oam_entry.tile_index & 0b1111_1110,
                    };
                    let pattern_table = match self.reg_ctrl.sprite_size {
                        SpriteSize::S8x8 => self.reg_ctrl.sprite_pattern_table_address,
                        SpriteSize::S8x16 => {
                            if oam_entry.tile_index & 0b1 != 0 {
                                0x1000
                            } else {
                                0x0000
                            }
                        }
                    };

                    if y != 0xFF {
                        for pos_x in x..min(x + 8, 256) {
                            // Dual tile index
                            let tile_y = (scanline - 1 - y) as u8;
                            debug_assert!(tile_y < 16);
                            let mut tile_x = pos_x - x;
                            let mut tile_y = tile_y;
                            let mut index = tile_index;

                            if oam_entry.flip_x {
                                tile_x = 7 - tile_x;
                            }
                            if oam_entry.flip_y {
                                if tile_y >= 8 {
                                    tile_y -= 8;
                                    tile_y = 7 - tile_y;
                                } else {
                                    tile_y = 7 - tile_y;
                                    index += 1;
                                }
                            } else {
                                if tile_y >= 8 {
                                    tile_y -= 8;
                                    index += 1;
                                }
                            }

                            let color = self.get_tile_pixel(
                                cartridge,
                                pattern_table,
                                index,
                                tile_x as u8,
                                tile_y,
                            );
                            if color != 0 {
                                if oam_entry.oam_entry_index == 0 && !bg_transparent {
                                    self.flag_sprite_zero_hit = true;
                                }
                                let palette = oam_entry.palette as usize;
                                let color = NES_COLORS_LUT
                                    [self.palette[palette * 4 + color as usize] as usize];
                                if let Some(frame) = self.next_frame.as_mut() {
                                    frame[(scanline * 256 + pos_x) as usize] = color;
                                }
                            }
                        }
                    }
                }
            }
        }
        self.fill_oam_buffer(cartridge, scanline);
    }

    pub fn get_tile_pixel(
        &mut self,
        cartridge: &Cartridge,
        pattern_table_addr: u16,
        tile_index: u8,
        tile_x: u8,
        tile_y: u8,
    ) -> u8 {
        assert!(tile_x < 8);
        assert!(tile_y < 8);
        let byte1 = cartridge
            .read_chr_byte(pattern_table_addr + (tile_index as u16 * 16 + tile_y as u16) as u16);
        let byte2 = cartridge.read_chr_byte(
            pattern_table_addr + (tile_index as u16 * 16 + tile_y as u16 + 8) as u16,
        );

        ((byte1 >> (7 - tile_x)) & 0b1) | ((byte2 >> (7 - tile_x)) & 0b1) << 1
    }

    pub fn get_background_pixel_at(
        &mut self,
        cartridge: &Cartridge,
        x: u64,
        y: u64,
    ) -> (u32, bool) {
        let nametable_offset = ((x / 256) * 0x400 + (y / 240) * 0x800) as usize;
        let nametable_offset =
            (nametable_offset + self.reg_ctrl.base_nametable_address as usize - 0x2000) & 0x0FFF;
        let x = x % 256;
        let y = y % 240;
        let tile_x = (x % 8) as u8;
        let tile_y = (y % 8) as u8;

        let tile_row = y / 8;
        let tile_col = x / 8;
        let nametable_addr =
            0x2000 + nametable_offset as usize + (tile_row * 32 + tile_col) as usize;
        let nametable_addr = cartridge.get_physical_nametable_addr(nametable_addr as u16);
        let nametable_entry = self.vram[nametable_addr as usize];
        let pattern_table_addr = self.reg_ctrl.background_pattern_table_address;
        let color_index = self.get_tile_pixel(
            cartridge,
            pattern_table_addr,
            nametable_entry,
            tile_x,
            tile_y,
        );

        let attrib_col = tile_col / 4;
        let attrib_row = tile_row / 4;
        let attrib_byte = self.vram[(0x3C0 + attrib_row * 8 + attrib_col) as usize];
        let attrib_bits =
            if tile_row % 4 < 2 { 0 } else { 4 } + if tile_col % 4 < 2 { 0 } else { 2 };
        let palette = ((attrib_byte >> attrib_bits) & 0b11) as usize;

        (
            NES_COLORS_LUT[self.palette[palette * 4 + color_index as usize] as usize],
            color_index == 0,
        )
    }
}
