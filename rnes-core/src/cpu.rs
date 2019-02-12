use interconnect::MemoryBus;

pub struct Cpu {
    pub reg_a: u8,
    pub reg_x: u8,
    pub reg_y: u8,
    pub reg_s: u8,
    pub reg_pc: u16,

    pub flag_negative: bool,
    pub flag_overflow: bool,
    pub flag_breakpoint: bool,
    pub flag_decimal: bool,
    pub flag_interrupt: bool,
    pub flag_zero: bool,
    pub flag_carry: bool,

    pub flag_nmi_pending: bool,
    pub flag_irq_pending: bool,
}

#[derive(Clone, Copy, Debug)]
enum AddrMode {
    None,
    A,
    X,
    Y,
    S,
    Zp,
    ZpX,
    ZpY,
    Imm,
    Abs,
    AbsX,
    AbsY,
    Ind,
    IndX,
    IndY,
}
#[derive(Clone, Copy, Debug)]
pub enum Register {
    A,
    X,
    Y,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            reg_s: 0xff,
            reg_pc: 0,

            flag_negative: false,
            flag_overflow: false,
            flag_breakpoint: false,
            flag_decimal: false,
            flag_interrupt: false,
            flag_zero: false,
            flag_carry: false,

            flag_nmi_pending: false,
            flag_irq_pending: false,
        }
    }

    pub fn reset(&mut self, interconnect: &mut impl MemoryBus) {
        // Read address from reset vector
        self.reg_pc = interconnect.read_word(0xfffc);
        self.flag_negative = false;
        self.flag_overflow = false;
        self.flag_breakpoint = false;
        self.flag_decimal = false;
        self.flag_interrupt = true;
        self.flag_zero = false;
        self.flag_carry = false;
        self.flag_nmi_pending = false;
    }

    fn decode<'a>(instr: u8) -> (&'a str, u8, u8, AddrMode) {
        // (opcode, cycles, bytes, addr_mode)
        match instr {
            0x01 => ("ORA", 6, 2, AddrMode::IndX),
            0x05 => ("ORA", 3, 2, AddrMode::Zp),
            0x06 => ("ASL", 5, 2, AddrMode::Zp),
            0x08 => ("PHP", 3, 1, AddrMode::None),
            0x09 => ("ORA", 2, 2, AddrMode::Imm),
            0x0A => ("ASL", 2, 1, AddrMode::A),
            0x0D => ("ORA", 4, 3, AddrMode::Abs),
            0x0E => ("ASL", 6, 3, AddrMode::Abs),
            0x10 => ("BPL", 2 /*4*/, 2, AddrMode::Imm), // Actually, relative addressing mode?
            0x11 => ("ORA", 5 /*6*/, 2, AddrMode::IndY),
            0x15 => ("ORA", 4, 2, AddrMode::ZpX),
            0x16 => ("ASL", 6, 2, AddrMode::ZpX),
            0x18 => ("CLC", 2, 1, AddrMode::None),
            0x19 => ("ORA", 4 /*5*/, 3, AddrMode::AbsY),
            0x1D => ("ORA", 3 /*4*/, 3, AddrMode::AbsX),
            0x1E => ("ASL", 7, 3, AddrMode::AbsX),
            0x20 => ("JSR", 6, 3, AddrMode::Abs),
            0x21 => ("AND", 6, 2, AddrMode::IndX),
            0x24 => ("BIT", 3, 2, AddrMode::Zp),
            0x25 => ("AND", 3, 2, AddrMode::Zp),
            0x26 => ("ROL", 5, 2, AddrMode::Zp),
            0x28 => ("PLP", 4, 1, AddrMode::None),
            0x29 => ("AND", 2, 2, AddrMode::Imm),
            0x2A => ("ROL", 2, 1, AddrMode::A),
            0x2C => ("BIT", 4, 3, AddrMode::Abs),
            0x2D => ("AND", 4, 3, AddrMode::Abs),
            0x2E => ("ROL", 6, 3, AddrMode::Abs),
            0x30 => ("BMI", 2 /*4*/, 2, AddrMode::Imm), // Actually, relative addressing mode?
            0x31 => ("AND", 5 /*6*/, 2, AddrMode::IndY),
            0x35 => ("AND", 4, 2, AddrMode::ZpX),
            0x36 => ("ROL", 6, 2, AddrMode::ZpX),
            0x38 => ("SEC", 2, 1, AddrMode::None),
            0x39 => ("AND", 4 /*5*/, 3, AddrMode::AbsY),
            0x3D => ("AND", 4 /*5*/, 3, AddrMode::AbsX),
            0x3E => ("ROL", 7, 3, AddrMode::AbsX),
            0x40 => ("RTI", 6, 1, AddrMode::None),
            0x41 => ("EOR", 6, 2, AddrMode::IndX),
            0x45 => ("EOR", 3, 2, AddrMode::Zp),
            0x46 => ("LSR", 5, 2, AddrMode::Zp),
            0x48 => ("PHA", 3, 1, AddrMode::None),
            0x49 => ("EOR", 2, 2, AddrMode::Imm),
            0x4A => ("LSR", 1, 2, AddrMode::A),
            0x4C => ("JMP", 4, 3, AddrMode::Abs),
            0x4D => ("EOR", 4, 3, AddrMode::Abs),
            0x4E => ("LSR", 6, 3, AddrMode::Abs),
            0x50 => ("BVC", 2 /*4*/, 2, AddrMode::Imm), // Actually, relative addressing mode?
            0x51 => ("EOR", 5 /*6*/, 2, AddrMode::IndY),
            0x55 => ("EOR", 4, 2, AddrMode::ZpX),
            0x56 => ("LSR", 6, 2, AddrMode::ZpX),
            0x59 => ("EOR", 4 /*5*/, 3, AddrMode::AbsY),
            0x5D => ("EOR", 4 /*5*/, 3, AddrMode::AbsX),
            0x5E => ("LSR", 7, 3, AddrMode::AbsX),
            0x60 => ("RTS", 6, 1, AddrMode::None),
            0x61 => ("ADC", 6, 2, AddrMode::IndX),
            0x65 => ("ADC", 3, 2, AddrMode::Zp),
            0x66 => ("ROR", 5, 2, AddrMode::Zp),
            0x68 => ("PLA", 4, 1, AddrMode::None),
            0x69 => ("ADC", 2, 2, AddrMode::Imm),
            0x6A => ("ROR", 2, 1, AddrMode::A),
            0x6C => ("JMP", 5, 3, AddrMode::Ind),
            0x6D => ("ADC", 4, 3, AddrMode::Abs),
            0x6E => ("ROR", 6, 3, AddrMode::Abs),
            0x70 => ("BVS", 2 /*4*/, 2, AddrMode::Imm), // Actually, relative addressing mode?
            0x71 => ("ADC", 5 /*6*/, 2, AddrMode::IndY),
            0x75 => ("ADC", 4, 2, AddrMode::ZpX),
            0x76 => ("ROR", 6, 2, AddrMode::ZpX),
            0x78 => ("SEI", 2, 1, AddrMode::None),
            0x79 => ("ADC", 4 /*5*/, 3, AddrMode::AbsY),
            0x7D => ("ADC", 4 /*5*/, 3, AddrMode::AbsX),
            0x7E => ("ROR", 7, 3, AddrMode::AbsX),
            0x81 => ("STA", 6, 2, AddrMode::IndX),
            0x84 => ("STY", 3, 2, AddrMode::Zp),
            0x85 => ("STA", 3, 2, AddrMode::Zp),
            0x86 => ("STX", 3, 2, AddrMode::Zp),
            0x88 => ("DEY", 2, 1, AddrMode::None),
            0x8A => ("TXA", 2, 1, AddrMode::None),
            0x8C => ("STY", 4, 3, AddrMode::Abs),
            0x8D => ("STA", 4, 3, AddrMode::Abs),
            0x8E => ("STX", 4, 3, AddrMode::Abs),
            0x90 => ("BCC", 2 /*4*/, 2, AddrMode::Imm), // Actually, relative addressing mode?
            0x91 => ("STA", 6, 2, AddrMode::IndY),
            0x94 => ("STY", 4, 2, AddrMode::ZpX),
            0x95 => ("STA", 4, 2, AddrMode::ZpX),
            0x96 => ("STX", 4, 2, AddrMode::ZpY),
            0x98 => ("TYA", 2, 1, AddrMode::None),
            0x99 => ("STA", 5, 3, AddrMode::AbsY),
            0x9A => ("TXS", 2, 1, AddrMode::None),
            0x9D => ("STA", 5, 3, AddrMode::AbsX),
            0xA0 => ("LDY", 2, 2, AddrMode::Imm),
            0xA1 => ("LDA", 6, 2, AddrMode::IndX),
            0xA2 => ("LDX", 2, 2, AddrMode::Imm),
            0xA4 => ("LDY", 3, 2, AddrMode::Zp),
            0xA5 => ("LDA", 3, 2, AddrMode::Zp),
            0xA6 => ("LDX", 3, 2, AddrMode::Zp),
            0xA8 => ("TAY", 2, 1, AddrMode::None),
            0xA9 => ("LDA", 2, 2, AddrMode::Imm),
            0xAA => ("TAX", 2, 1, AddrMode::None),
            0xAC => ("LDY", 4, 3, AddrMode::Abs),
            0xAD => ("LDA", 4, 3, AddrMode::Abs),
            0xAE => ("LDX", 4, 3, AddrMode::Abs),
            0xB0 => ("BCS", 2 /*4*/, 2, AddrMode::Imm), // Actually, relative addressing mode?
            0xB1 => ("LDA", 5 /*6*/, 2, AddrMode::IndY),
            0xB4 => ("LDY", 4, 2, AddrMode::ZpX),
            0xB5 => ("LDA", 4, 2, AddrMode::ZpX),
            0xB6 => ("LDX", 4, 2, AddrMode::ZpY),
            0xB8 => ("CLV", 2, 1, AddrMode::None),
            0xB9 => ("LDA", 4 /*5*/, 3, AddrMode::AbsY),
            0xBA => ("TSX", 2, 1, AddrMode::S),
            0xBC => ("LDY", 4 /*5*/, 3, AddrMode::AbsX),
            0xBD => ("LDA", 4 /*5*/, 3, AddrMode::AbsX),
            0xBE => ("LDX", 4 /*5*/, 3, AddrMode::AbsY),
            0xC0 => ("CPY", 2, 2, AddrMode::Imm),
            0xC1 => ("CMP", 6, 2, AddrMode::IndX),
            0xC4 => ("CPY", 3, 2, AddrMode::Zp),
            0xC5 => ("CMP", 3, 2, AddrMode::Zp),
            0xC6 => ("DEC", 5, 2, AddrMode::Zp),
            0xC8 => ("INY", 2, 1, AddrMode::None),
            0xC9 => ("CMP", 2, 2, AddrMode::Imm),
            0xCC => ("CPY", 4, 3, AddrMode::Abs),
            0xCD => ("CMP", 4, 3, AddrMode::Abs),
            0xCA => ("DEX", 2, 1, AddrMode::None),
            0xCE => ("DEC", 6, 3, AddrMode::Abs),
            0xD0 => ("BNE", 2 /*4*/, 2, AddrMode::Imm), // Actually, relative addressing mode?
            0xD1 => ("CMP", 5 /*6*/, 2, AddrMode::IndY),
            0xD5 => ("CMP", 4, 2, AddrMode::ZpX),
            0xD6 => ("DEC", 6, 2, AddrMode::ZpX),
            0xD8 => ("CLD", 2, 1, AddrMode::None),
            0xD9 => ("CMP", 4 /*5*/, 3, AddrMode::AbsY),
            0xDD => ("CMP", 4 /*5*/, 3, AddrMode::AbsX),
            0xDE => ("DEC", 7, 3, AddrMode::AbsX),
            0xE0 => ("CPX", 2, 2, AddrMode::Imm),
            0xE1 => ("SBC", 6, 2, AddrMode::IndX),
            0xE4 => ("CPX", 2, 3, AddrMode::Zp),
            0xE5 => ("SBC", 3, 2, AddrMode::Zp),
            0xE6 => ("INC", 5, 2, AddrMode::Zp),
            0xE8 => ("INX", 2, 1, AddrMode::None),
            0xE9 => ("SBX", 2, 2, AddrMode::Imm),
            0xEA => ("NOP", 2, 1, AddrMode::None),
            0xEC => ("CPX", 4, 3, AddrMode::Abs),
            0xED => ("SBC", 4, 3, AddrMode::Abs),
            0xEE => ("INC", 6, 3, AddrMode::Abs),
            0xF0 => ("BEQ", 2 /*4*/, 2, AddrMode::Imm), // Actually, relative addressing mode?
            0xF1 => ("SBC", 5 /*6*/, 2, AddrMode::IndY),
            0xF5 => ("SBC", 4, 2, AddrMode::ZpX),
            0xF6 => ("INC", 6, 2, AddrMode::ZpX),
            0xF8 => ("SED", 2, 1, AddrMode::None),
            0xF9 => ("SBC", 4 /*5*/, 3, AddrMode::AbsY),
            0xFD => ("SBC", 4 /*5*/, 3, AddrMode::AbsX),
            0xFE => ("INC", 7, 3, AddrMode::AbsX),
            _ => panic!("Unknown opcode {:02X}", instr),
        }
    }

    #[allow(dead_code)]
    fn print_opcode(&mut self, interconnect: &mut impl MemoryBus, opcode: u8) {
        let decoded = Cpu::decode(opcode);
        let pc = self.reg_pc - 1;

        print!(
            "A:{:02X} X:{:02X} Y:{:02X} P:{}{}{}{}{}{}{}  ",
            self.reg_a,
            self.reg_x,
            self.reg_y,
            if self.flag_negative { "N" } else { "-" },
            if self.flag_overflow { "V" } else { "-" },
            if self.flag_breakpoint { "B" } else { "-" },
            if self.flag_decimal { "D" } else { "-" },
            if self.flag_interrupt { "I" } else { "-" },
            if self.flag_zero { "Z" } else { "-" },
            if self.flag_carry { "C" } else { "-" }
        );

        print!("{:04X} {:3} ", pc, decoded.0);
        match decoded.3 {
            AddrMode::A => print!("A"),
            AddrMode::X => print!("X"),
            AddrMode::Y => print!("Y"),
            AddrMode::S => print!("S"),
            AddrMode::Imm => print!("#${:02X}", interconnect.read_byte(pc)),
            AddrMode::Zp => print!("${:02X}", interconnect.read_byte(pc)),
            AddrMode::ZpX => print!("${:02X},X", interconnect.read_byte(pc)),
            AddrMode::ZpY => print!("${:02X},Y", interconnect.read_byte(pc)),
            AddrMode::Abs => print!("${:04X}", interconnect.read_word(pc)),
            AddrMode::AbsX => print!("${:04X},X", interconnect.read_word(pc)),
            AddrMode::AbsY => print!("${:04X},Y", interconnect.read_word(pc)),
            AddrMode::IndX => print!("(${:02X},X)", interconnect.read_byte(pc)),
            AddrMode::IndY => print!("(${:02X}),Y", interconnect.read_byte(pc)),
            AddrMode::Ind => print!("(${:04X})", interconnect.read_word_same_page(pc)),
            AddrMode::None => {}
        };
        println!("");
    }

    // Executes one step of the processor and returns the number of cycles it took
    pub fn step(&mut self, interconnect: &mut impl MemoryBus) -> u64 {
        // TODO: How are multiple interrupts handled?
        if self.flag_nmi_pending {
            let pc = self.reg_pc;
            self.push_word(interconnect, pc);
            self.push_status_reg(interconnect);
            self.flag_nmi_pending = false;
            self.flag_interrupt = true;
            self.flag_breakpoint = false;
            self.reg_pc = interconnect.read_word(0xfffa);
        } else if self.flag_irq_pending {
            let pc = self.reg_pc;
            self.push_word(interconnect, pc);
            self.push_status_reg(interconnect);
            self.flag_nmi_pending = false;
            self.flag_interrupt = true;
            self.flag_breakpoint = false;
            self.reg_pc = interconnect.read_word(0xfffe);
        }

        // get instruction
        let instr = self.load_immediate_byte(interconnect);
        // self.print_opcode(interconnect, instr);
        match instr {
            0x01 => self.read_instr_sz(interconnect, AddrMode::IndX, Register::A, |r, m| r | m), // ORA zeropage
            0x05 => self.read_instr_sz(interconnect, AddrMode::Zp, Register::A, |r, m| r | m), // ORA zeropage
            0x06 => {
                self.writeback_instr_szc(interconnect, AddrMode::Zp, |m| (m << 1, m & 0x80 != 0))
            }
            // ASL zeropage
            0x08 => self.push_status_reg(interconnect), // PHP
            0x09 => self.read_instr_sz(interconnect, AddrMode::Imm, Register::A, |r, m| r | m), // ORA imm
            0x0A => {
                self.writeback_instr_szc(interconnect, AddrMode::A, |m| (m << 1, m & 0x80 != 0))
            } // ASL A
            0x0D => self.read_instr_sz(interconnect, AddrMode::Abs, Register::A, |r, m| r | m), // ORA #abs
            0x0E => {
                self.writeback_instr_szc(interconnect, AddrMode::Abs, |m| (m << 1, m & 0x80 != 0))
            } // ASL #abs
            0x10 => self.branch(interconnect, |n, _v, _d, _i, _z, _c| !n), // BPL
            0x11 => self.read_instr_sz(interconnect, AddrMode::IndY, Register::A, |r, m| r | m), // ORA indirect,y
            0x15 => self.read_instr_sz(interconnect, AddrMode::ZpX, Register::A, |r, m| r | m), // ORA zeropage,x
            0x16 => {
                self.writeback_instr_szc(interconnect, AddrMode::ZpX, |m| (m << 1, m & 0x80 != 0))
            } // ASL #abs
            0x18 => self.flag_carry = false, // CLC
            0x19 => self.read_instr_sz(interconnect, AddrMode::AbsY, Register::A, |r, m| r | m), // ORA #abs,y
            0x1D => self.read_instr_sz(interconnect, AddrMode::AbsX, Register::A, |r, m| r | m), // ORA #abs,x
            0x1E => {
                self.writeback_instr_szc(interconnect, AddrMode::AbsX, |m| (m << 1, m & 0x80 != 0))
            } // ASL #abs
            0x20 => {
                // JSR #abs
                let subroutine = self.load_immediate_word(interconnect);
                let pc_to_push = self.reg_pc - 1;
                self.push_word(interconnect, pc_to_push);
                self.reg_pc = subroutine;
            }
            0x21 => self.read_instr_sz(interconnect, AddrMode::IndX, Register::A, |r, m| r & m), // AND (indirect,x)
            0x24 => {
                // BIT zeropage
                let mem = self.read_mem(interconnect, AddrMode::Zp);
                self.flag_negative = mem & 0b1000_0000 != 0;
                self.flag_overflow = mem & 0b0100_0000 != 0;
                self.flag_zero = (mem & self.reg_a) == 0;
            }
            0x25 => self.read_instr_sz(interconnect, AddrMode::Zp, Register::A, |r, m| r & m), // AND zeropage
            0x26 => self.instr_rol(interconnect, AddrMode::Zp), // ROL zeropage
            0x28 => self.pop_status_reg(interconnect),
            0x29 => self.read_instr_sz(interconnect, AddrMode::Imm, Register::A, |r, m| r & m), // AND imm
            0x2A => self.instr_rol(interconnect, AddrMode::A), // ROL A
            0x2C => {
                // BIT #abs
                let mem = self.read_mem(interconnect, AddrMode::Abs);
                self.flag_negative = mem & 0b1000_0000 != 0;
                self.flag_overflow = mem & 0b0100_0000 != 0;
                self.flag_zero = (mem & self.reg_a) == 0;
            }
            0x2D => self.read_instr_sz(interconnect, AddrMode::Abs, Register::A, |r, m| r & m), // AND #abs
            0x2E => self.instr_rol(interconnect, AddrMode::Abs), // ROL #abs
            0x30 => self.branch(interconnect, |n, _v, _d, _i, _z, _c| n), // BMI
            0x31 => self.read_instr_sz(interconnect, AddrMode::IndY, Register::A, |r, m| r & m), // AND (indirect),y
            0x35 => self.read_instr_sz(interconnect, AddrMode::ZpX, Register::A, |r, m| r & m), // AND zeropage,x
            0x36 => self.instr_rol(interconnect, AddrMode::ZpX), // ROL zeropage,x
            0x38 => self.flag_carry = true,                      // SEC
            0x39 => self.read_instr_sz(interconnect, AddrMode::AbsY, Register::A, |r, m| r & m), // AND #abs,y
            0x3D => self.read_instr_sz(interconnect, AddrMode::AbsX, Register::A, |r, m| r & m), // AND #abs,x
            0x3E => self.instr_rol(interconnect, AddrMode::AbsX), // ROL #abs,x
            0x40 => {
                // RTI
                self.pop_status_reg(interconnect);
                self.reg_pc = self.pop_word(interconnect);
            }
            0x41 => self.read_instr_sz(interconnect, AddrMode::IndX, Register::A, |r, m| r ^ m), // EOR (indirect,x)
            0x45 => self.read_instr_sz(interconnect, AddrMode::Zp, Register::A, |r, m| r ^ m), // EOR zeropage
            0x46 => self.instr_lsr(interconnect, AddrMode::Zp), // LSR zeropage
            0x49 => self.read_instr_sz(interconnect, AddrMode::Imm, Register::A, |r, m| r ^ m), // EOR imm
            0x48 => {
                let a = self.reg_a;
                self.push_byte(interconnect, a);
            } // PHA
            0x4A => self.instr_lsr(interconnect, AddrMode::A), // LSR A
            0x4C => self.reg_pc = self.load_immediate_word(interconnect), // JMP imm
            0x4D => self.read_instr_sz(interconnect, AddrMode::Abs, Register::A, |r, m| r ^ m), // EOR #abs
            0x4E => self.instr_lsr(interconnect, AddrMode::Abs), // LSR #abs
            0x50 => self.branch(interconnect, |_n, v, _d, _i, _z, _c| !v), // BVC
            0x51 => self.read_instr_sz(interconnect, AddrMode::IndY, Register::A, |r, m| r ^ m), // EOR (indirect),y
            0x55 => self.read_instr_sz(interconnect, AddrMode::ZpX, Register::A, |r, m| r ^ m), // EOR zeropage,x
            0x56 => self.instr_lsr(interconnect, AddrMode::ZpX), // LSR zeropage,x
            0x59 => self.read_instr_sz(interconnect, AddrMode::AbsY, Register::A, |r, m| r ^ m), // EOR #abs,y
            0x5D => self.read_instr_sz(interconnect, AddrMode::AbsX, Register::A, |r, m| r ^ m), // EOR #abs,x
            0x5E => self.instr_lsr(interconnect, AddrMode::AbsX), // LSR #abs,x
            0x60 => {
                // RTS
                let return_addr = self.pop_word(interconnect);
                self.reg_pc = return_addr + 1;
            }
            0x61 => self.instr_adc(interconnect, AddrMode::IndX), // ADC (indirect,x)
            0x65 => self.instr_adc(interconnect, AddrMode::Zp),   // ADC zeropage
            0x66 => self.instr_ror(interconnect, AddrMode::Zp),   // ROR zeropage
            0x68 => self.reg_a = self.pop_byte(interconnect),     // PLA
            0x69 => self.instr_adc(interconnect, AddrMode::Imm),  // ADC imm
            0x6A => self.instr_ror(interconnect, AddrMode::A),    // ROR A
            0x6C => {
                // JMP ind
                let addr = self.load_immediate_word(interconnect);
                self.reg_pc = interconnect.read_word_same_page(addr);
            }
            0x6D => self.instr_adc(interconnect, AddrMode::Abs), // ADC #abs
            0x6E => self.instr_ror(interconnect, AddrMode::Abs), // ROR #abs
            0x70 => self.branch(interconnect, |_n, v, _d, _i, _z, _c| v), // BVS
            0x71 => self.instr_adc(interconnect, AddrMode::IndY), // ADC (indirect),y
            0x75 => self.instr_adc(interconnect, AddrMode::ZpX), // ADC zeropage,x
            0x76 => self.instr_ror(interconnect, AddrMode::ZpX), // ROR zeropage,x
            0x78 => self.flag_interrupt = true,
            0x79 => self.instr_adc(interconnect, AddrMode::AbsY), // ADC #abs,y
            0x7D => self.instr_adc(interconnect, AddrMode::AbsX), // ADC #abs,x
            0x7E => self.instr_ror(interconnect, AddrMode::AbsX), // ROR #abs,x
            0x81 => self.write_instr(interconnect, AddrMode::IndX, Register::A), // STA (indirect,x)
            0x84 => self.write_instr(interconnect, AddrMode::Zp, Register::Y), // STY zeropage
            0x85 => self.write_instr(interconnect, AddrMode::Zp, Register::A), // STA zeropage
            0x86 => self.write_instr(interconnect, AddrMode::Zp, Register::X), // STX zeropage
            0x88 => self.reg_instr_sz(interconnect, Register::Y, |r| r.wrapping_sub(1)), // DEY
            0x8c => self.write_instr(interconnect, AddrMode::Abs, Register::Y), // STY #abs
            0x8d => self.write_instr(interconnect, AddrMode::Abs, Register::A), // STA #abs
            0x8e => self.write_instr(interconnect, AddrMode::Abs, Register::X), // STX #abs
            0x8A => self.read_instr_sz(interconnect, AddrMode::X, Register::A, |_r, m| m), // TXA
            0x90 => self.branch(interconnect, |_n, _v, _d, _i, _z, c| !c), // BCC
            0x91 => self.write_instr(interconnect, AddrMode::IndY, Register::A), // STA indirect,y
            0x94 => self.write_instr(interconnect, AddrMode::ZpX, Register::Y), // STY zeropage,x
            0x95 => self.write_instr(interconnect, AddrMode::ZpX, Register::A), // STA zeropage,x
            0x96 => self.write_instr(interconnect, AddrMode::ZpY, Register::X), // STX zeropage,y
            0x98 => self.read_instr_sz(interconnect, AddrMode::Y, Register::A, |_r, m| m), // TYA
            0x99 => self.write_instr(interconnect, AddrMode::AbsY, Register::A), // STA absolute,y
            0x9A => self.reg_s = self.reg_x,                      // TXS
            0x9D => self.write_instr(interconnect, AddrMode::AbsX, Register::A), // STA absolute,x
            0xA0 => self.read_instr_sz(interconnect, AddrMode::Imm, Register::Y, |_r, m| m), // LDY imm
            0xA1 => self.read_instr_sz(interconnect, AddrMode::IndX, Register::A, |_r, m| m), // LDA (indirect,x)
            0xA2 => self.read_instr_sz(interconnect, AddrMode::Imm, Register::X, |_r, m| m), // LDX imm
            0xA4 => self.read_instr_sz(interconnect, AddrMode::Zp, Register::Y, |_r, m| m), // LDY zeropage
            0xA5 => self.read_instr_sz(interconnect, AddrMode::Zp, Register::A, |_r, m| m), // LDA zeropage
            0xA6 => self.read_instr_sz(interconnect, AddrMode::Zp, Register::X, |_r, m| m), // LDX zeropage
            0xA8 => self.read_instr_sz(interconnect, AddrMode::A, Register::Y, |_r, m| m),  // TAY
            0xA9 => self.read_instr_sz(interconnect, AddrMode::Imm, Register::A, |_r, m| m), // LDA imm
            0xAA => self.read_instr_sz(interconnect, AddrMode::A, Register::X, |_r, m| m),   // TAX
            0xAC => self.read_instr_sz(interconnect, AddrMode::Abs, Register::Y, |_r, m| m), // LDY #abs
            0xAD => self.read_instr_sz(interconnect, AddrMode::Abs, Register::A, |_r, m| m), // LDA #abs
            0xAE => self.read_instr_sz(interconnect, AddrMode::Abs, Register::X, |_r, m| m), // LDX #abs
            0xB0 => self.branch(interconnect, |_n, _v, _d, _i, _z, c| c),                    // BCS
            0xB1 => self.read_instr_sz(interconnect, AddrMode::IndY, Register::A, |_r, m| m), // LDA indirect,y
            0xB4 => self.read_instr_sz(interconnect, AddrMode::ZpX, Register::Y, |_r, m| m), // LDY zeropage,y
            0xB5 => self.read_instr_sz(interconnect, AddrMode::ZpX, Register::A, |_r, m| m), // LDA zeropage,y
            0xB6 => self.read_instr_sz(interconnect, AddrMode::ZpY, Register::X, |_r, m| m), // LDX zeropage,y
            0xB8 => self.flag_overflow = false,                                              // CLV
            0xB9 => self.read_instr_sz(interconnect, AddrMode::AbsY, Register::A, |_r, m| m), // LDA #abs,y
            0xBA => self.read_instr_sz(interconnect, AddrMode::S, Register::X, |_r, m| m),    // TSX
            0xBC => self.read_instr_sz(interconnect, AddrMode::AbsX, Register::Y, |_r, m| m), // LDY #abs,x
            0xBD => self.read_instr_sz(interconnect, AddrMode::AbsX, Register::A, |_r, m| m), // LDA #abs,x
            0xBE => self.read_instr_sz(interconnect, AddrMode::AbsY, Register::X, |_r, m| m), // LDX #abs,y
            0xC0 => self.instr_cmp(interconnect, AddrMode::Imm, Register::Y), // CPY imm
            0xC1 => self.instr_cmp(interconnect, AddrMode::IndX, Register::A), // CMP (indirect,x)
            0xC4 => self.instr_cmp(interconnect, AddrMode::Zp, Register::Y),  // CPY zeropage
            0xC5 => self.instr_cmp(interconnect, AddrMode::Zp, Register::A),  // CMP zeropage
            0xC6 => self.writeback_instr_sz(interconnect, AddrMode::Zp, |m| m.wrapping_sub(1)), // DEC zeropage
            0xC8 => self.reg_instr_sz(interconnect, Register::Y, |r| r.wrapping_add(1)), // INY
            0xC9 => self.instr_cmp(interconnect, AddrMode::Imm, Register::A),            // CMP imm
            0xCA => self.reg_instr_sz(interconnect, Register::X, |r| r.wrapping_sub(1)), // DEX
            0xCC => self.instr_cmp(interconnect, AddrMode::Abs, Register::Y),            // CPY #abs
            0xCD => self.instr_cmp(interconnect, AddrMode::Abs, Register::A),            // CMP #abs
            0xCE => self.writeback_instr_sz(interconnect, AddrMode::Abs, |m| m.wrapping_sub(1)), // DEC absolute
            0xD0 => self.branch(interconnect, |_n, _v, _d, _i, z, _c| !z), // BNE
            0xD1 => self.instr_cmp(interconnect, AddrMode::IndY, Register::A), // CMP (indirect),y
            0xD5 => self.instr_cmp(interconnect, AddrMode::ZpX, Register::A), // CMP zeropage,x
            0xD6 => self.writeback_instr_sz(interconnect, AddrMode::ZpX, |m| m.wrapping_sub(1)), // DEC zeropage,x
            0xD8 => self.flag_decimal = false, // CLD
            0xD9 => self.instr_cmp(interconnect, AddrMode::AbsY, Register::A), // CMP #abs,y
            0xDE => self.writeback_instr_sz(interconnect, AddrMode::AbsX, |m| m.wrapping_sub(1)), // DEC #abs,x
            0xDD => self.instr_cmp(interconnect, AddrMode::AbsX, Register::A), // CMP #abs,x
            0xE0 => self.instr_cmp(interconnect, AddrMode::Imm, Register::X),  // CPX imm
            0xE1 => self.instr_sbc(interconnect, AddrMode::IndX),              // SBC (indirect,x)
            0xE4 => self.instr_cmp(interconnect, AddrMode::Zp, Register::X),   // CPX zeropage
            0xE5 => self.instr_sbc(interconnect, AddrMode::Zp),                // SBC zeropage
            0xE8 => self.reg_instr_sz(interconnect, Register::X, |r| r.wrapping_add(1)), // INX
            0xE9 => self.instr_sbc(interconnect, AddrMode::Imm),               // SBC imm
            0xE6 => self.writeback_instr_sz(interconnect, AddrMode::Zp, |m| m.wrapping_add(1)), // INC zeropage
            0xEA => {}                                                        // NOP
            0xEC => self.instr_cmp(interconnect, AddrMode::Abs, Register::X), // CPX #abs
            0xED => self.instr_sbc(interconnect, AddrMode::Abs),              // SBC #abs
            0xEE => self.writeback_instr_sz(interconnect, AddrMode::Abs, |m| m.wrapping_add(1)), // INC abs
            0xF0 => self.branch(interconnect, |_n, _v, _d, _i, z, _c| z), // BEQ
            0xF1 => self.instr_sbc(interconnect, AddrMode::IndY),         // SBC (indirect),y
            0xF5 => self.instr_sbc(interconnect, AddrMode::ZpX),          // SBC zeropage,x
            0xF6 => self.writeback_instr_sz(interconnect, AddrMode::ZpX, |m| m.wrapping_add(1)), // INC zeropage,x
            0xF8 => self.flag_decimal = true, // SED
            0xF9 => self.instr_sbc(interconnect, AddrMode::AbsY), // SBC #abs,y
            0xFD => self.instr_sbc(interconnect, AddrMode::AbsX), // SBC #abs,x
            0xFE => self.writeback_instr_sz(interconnect, AddrMode::AbsX, |m| m.wrapping_add(1)), // INC #abs,x
            _ => panic!("Unknown instruction {:02X}", instr),
        }

        Cpu::decode(instr).2 as u64
    }

    pub fn request_nmi(&mut self) {
        self.flag_nmi_pending = true;
    }

    pub fn request_irq(&mut self) {
        if self.flag_interrupt == false {
            self.flag_irq_pending = true;
        }
    }

    pub fn read_reg(&self, reg: Register) -> u8 {
        match reg {
            Register::A => self.reg_a,
            Register::X => self.reg_x,
            Register::Y => self.reg_y,
        }
    }

    pub fn write_reg(&mut self, reg: Register, value: u8) {
        match reg {
            Register::A => self.reg_a = value,
            Register::X => self.reg_x = value,
            Register::Y => self.reg_y = value,
        };
    }

    fn resolve_mem_addr(&mut self, interconnect: &mut impl MemoryBus, addr_mode: AddrMode) -> u16 {
        match addr_mode {
            AddrMode::None => panic!("Not an addressing mode!"),
            AddrMode::A => panic!("A register does not have an address!"),
            AddrMode::X => panic!("A register does not have an address!"),
            AddrMode::Y => panic!("A register does not have an address!"),
            AddrMode::S => panic!("A register does not have an address!"),
            AddrMode::Zp => self.load_immediate_byte(interconnect) as u16,
            AddrMode::ZpX => self
                .load_immediate_byte(interconnect)
                .wrapping_add(self.reg_x) as u16,
            AddrMode::ZpY => self
                .load_immediate_byte(interconnect)
                .wrapping_add(self.reg_y) as u16,
            AddrMode::Imm => {
                let addr = self.reg_pc;
                self.reg_pc += 1;
                addr
            }
            AddrMode::Abs => self.load_immediate_word(interconnect),
            AddrMode::AbsX => self.load_immediate_word(interconnect) + self.reg_x as u16,
            AddrMode::AbsY => self.load_immediate_word(interconnect) + self.reg_y as u16,
            AddrMode::Ind => panic!("Indirect addressing mode is only used by JMP!"),
            AddrMode::IndX => {
                let addr = self.load_immediate_byte(interconnect);
                let addr = interconnect.read_zeropage_word(addr.wrapping_add(self.reg_x));
                addr
            }
            AddrMode::IndY => {
                let addr = self.load_immediate_byte(interconnect);
                let addr = interconnect
                    .read_zeropage_word(addr)
                    .wrapping_add(self.reg_y as u16);
                addr
            }
        }
    }

    fn read_mem(&mut self, interconnect: &mut impl MemoryBus, addr_mode: AddrMode) -> u8 {
        match addr_mode {
            AddrMode::A => self.reg_a,
            AddrMode::X => self.reg_x,
            AddrMode::Y => self.reg_y,
            AddrMode::S => self.reg_s,
            memory_mode => {
                let addr = self.resolve_mem_addr(interconnect, memory_mode);
                interconnect.read_byte(addr)
            }
        }
    }

    fn write_mem(&mut self, interconnect: &mut impl MemoryBus, addr_mode: AddrMode, value: u8) {
        match addr_mode {
            AddrMode::A => self.reg_a = value,
            memory_mode => {
                let addr = self.resolve_mem_addr(interconnect, memory_mode);
                interconnect.write_byte(addr, value);
            }
        };
    }

    fn write_instr(
        &mut self,
        interconnect: &mut impl MemoryBus,
        addr_mode: AddrMode,
        reg: Register,
    ) {
        let reg_value = self.read_reg(reg);
        self.write_mem(interconnect, addr_mode, reg_value);
    }

    pub fn set_flags_sz(&mut self, val: u8) {
        self.flag_negative = val & 0x80 != 0;
        self.flag_zero = val == 0;
    }

    fn read_instr_sz<F>(
        &mut self,
        interconnect: &mut impl MemoryBus,
        addr_mode: AddrMode,
        reg: Register,
        f: F,
    ) where
        F: FnOnce(u8, u8) -> u8,
    {
        let reg_value = self.read_reg(reg);
        let mem_value = self.read_mem(interconnect, addr_mode);
        let val = f(reg_value, mem_value);
        self.write_reg(reg, val);
        self.set_flags_sz(val);
    }

    fn reg_instr_sz<F>(&mut self, _interconnect: &mut impl MemoryBus, reg: Register, f: F)
    where
        F: FnOnce(u8) -> u8,
    {
        let reg_value = self.read_reg(reg);
        let val = f(reg_value);
        self.write_reg(reg, val);
        self.set_flags_sz(val);
    }

    fn writeback_instr_sz<F>(
        &mut self,
        interconnect: &mut impl MemoryBus,
        addr_mode: AddrMode,
        f: F,
    ) where
        F: FnOnce(u8) -> u8,
    {
        let pc = self.reg_pc;
        let mem = self.read_mem(interconnect, addr_mode);
        self.reg_pc = pc;

        let mem = f(mem);
        self.write_mem(interconnect, addr_mode, mem);

        self.flag_zero = mem == 0;
        self.flag_negative = (mem & 0x80) != 0;
    }

    fn writeback_instr_szc<F>(
        &mut self,
        interconnect: &mut impl MemoryBus,
        addr_mode: AddrMode,
        f: F,
    ) where
        F: FnOnce(u8) -> (u8, bool),
    {
        let pc = self.reg_pc;
        let mem = self.read_mem(interconnect, addr_mode);
        self.reg_pc = pc;

        let (mem, carry) = f(mem);
        self.write_mem(interconnect, addr_mode, mem);

        self.flag_negative = (mem & 0x80) != 0;
        self.flag_zero = mem == 0;
        self.flag_carry = carry;
    }

    fn read_instr_szc<F>(
        &mut self,
        interconnect: &mut impl MemoryBus,
        addr_mode: AddrMode,
        reg: Register,
        f: F,
    ) where
        F: FnOnce(u8, u8) -> (u8, bool),
    {
        let reg_value = self.read_reg(reg);
        let mem_value = self.read_mem(interconnect, addr_mode);
        let (val, carry) = f(reg_value, mem_value);
        self.write_reg(reg, val);

        self.flag_negative = val & 0x80 != 0;
        self.flag_zero = val == 0;
        self.flag_carry = carry;
    }

    fn read_instr_szvc<F>(
        &mut self,
        interconnect: &mut impl MemoryBus,
        addr_mode: AddrMode,
        reg: Register,
        f: F,
    ) where
        F: FnOnce(u8, u8) -> (u8, bool, bool),
    {
        let reg_value = self.read_reg(reg);
        let mem_value = self.read_mem(interconnect, addr_mode);
        let (val, carry, overflow) = f(reg_value, mem_value);
        self.write_reg(reg, val);

        self.flag_negative = val & 0x80 != 0;
        self.flag_zero = val == 0;
        self.flag_carry = carry;
        self.flag_overflow = overflow;
    }

    pub fn branch<F>(&mut self, interconnect: &mut impl MemoryBus, do_branch: F)
    where
        F: FnOnce(bool, bool, bool, bool, bool, bool) -> bool,
    {
        let offset = self.load_immediate_byte(interconnect) as i8 as u16;
        let do_branch = do_branch(
            self.flag_negative,
            self.flag_overflow,
            self.flag_decimal,
            self.flag_interrupt,
            self.flag_zero,
            self.flag_carry,
        );
        if do_branch {
            self.reg_pc = self.reg_pc.wrapping_add(offset);
        }
    }

    pub fn load_immediate_byte(&mut self, interconnect: &mut impl MemoryBus) -> u8 {
        let byte = interconnect.read_byte(self.reg_pc);
        self.reg_pc += 1;
        byte
    }
    pub fn load_immediate_word(&mut self, interconnect: &mut impl MemoryBus) -> u16 {
        let b1 = self.load_immediate_byte(interconnect);
        let b2 = self.load_immediate_byte(interconnect);
        (b1 as u16) | ((b2 as u16) << 8)
    }

    pub fn push_status_reg(&mut self, interconnect: &mut impl MemoryBus) {
        let status_reg = if self.flag_negative { 0b1000_0000 } else { 0 }
            | if self.flag_overflow { 0b0100_0000 } else { 0 }
            | if self.flag_breakpoint { 0b0001_0000 } else { 0 }
            | if self.flag_decimal { 0b0000_1000 } else { 0 }
            | if self.flag_interrupt { 0b0000_0100 } else { 0 }
            | if self.flag_zero { 0b0000_0010 } else { 0 }
            | if self.flag_carry { 0b0000_0001 } else { 0 };
        self.push_byte(interconnect, status_reg);
    }

    pub fn pop_status_reg(&mut self, interconnect: &mut impl MemoryBus) {
        let status = self.pop_byte(interconnect);
        self.flag_negative = status & 0b1000_0000 != 0;
        self.flag_overflow = status & 0b0100_0000 != 0;
        self.flag_breakpoint = status & 0b0001_0000 != 0;
        self.flag_decimal = status & 0b0000_1000 != 0;
        self.flag_interrupt = status & 0b0000_0100 != 0;
        self.flag_zero = status & 0b0000_0010 != 0;
        self.flag_carry = status & 0b0000_0001 != 0;
    }

    pub fn push_byte(&mut self, interconnect: &mut impl MemoryBus, value: u8) {
        interconnect.write_byte(0x0100 | self.reg_s as u16, value);
        self.reg_s = self.reg_s.wrapping_sub(1);
    }

    pub fn pop_byte(&mut self, interconnect: &mut impl MemoryBus) -> u8 {
        self.reg_s = self.reg_s.wrapping_add(1);
        interconnect.read_byte(0x0100 | self.reg_s as u16)
    }

    pub fn push_word(&mut self, interconnect: &mut impl MemoryBus, value: u16) {
        self.push_byte(interconnect, (value >> 8) as u8);
        self.push_byte(interconnect, (value & 0xff) as u8);
    }

    pub fn pop_word(&mut self, interconnect: &mut impl MemoryBus) -> u16 {
        let b1 = self.pop_byte(interconnect);
        let b2 = self.pop_byte(interconnect);

        (b1 as u16) | ((b2 as u16) << 8)
    }

    fn instr_adc(&mut self, interconnect: &mut impl MemoryBus, addr_mode: AddrMode) {
        debug_assert!(!self.flag_decimal);
        let reg_a = self.reg_a as u16;
        let acc = reg_a.wrapping_add(if self.flag_carry { 1 } else { 0 });
        self.read_instr_szvc(interconnect, addr_mode, Register::A, |_r, m| {
            let temp = acc + m as u16;
            (
                temp as u8,
                temp > 0xff,
                ((reg_a ^ m as u16) & 0x80 == 0) && ((reg_a ^ temp) & 0x80 != 0),
            )
        });
    }

    fn instr_lsr(&mut self, interconnect: &mut impl MemoryBus, addr_mode: AddrMode) {
        let pc = self.reg_pc;
        let mem = self.read_mem(interconnect, addr_mode);
        self.reg_pc = pc;

        let new_mem = mem >> 1;
        self.flag_negative = new_mem & 0x80 != 0;
        self.flag_zero = new_mem == 0;
        self.flag_carry = mem & 0b1 != 0;
        self.write_mem(interconnect, addr_mode, new_mem);
    }

    fn instr_sbc(&mut self, interconnect: &mut impl MemoryBus, addr_mode: AddrMode) {
        debug_assert!(!self.flag_decimal);
        let reg_a = self.reg_a as u16;
        let acc = reg_a.wrapping_add(if self.flag_carry { 1 } else { 0 });
        self.read_instr_szvc(interconnect, addr_mode, Register::A, |_r, m| {
            let m = !m;
            let temp = acc + m as u16;
            (
                temp as u8,
                temp > 0xff,
                ((reg_a ^ m as u16) & 0x80 == 0) && ((reg_a ^ temp) & 0x80 != 0),
            )
        });
    }

    fn instr_rol(&mut self, interconnect: &mut impl MemoryBus, addr_mode: AddrMode) {
        let carry_in = if self.flag_carry { 1 } else { 0 } as u8;
        let mut carry_out = false;

        self.writeback_instr_sz(interconnect, addr_mode, |m| {
            carry_out = m & 0x80 != 0;
            (m << 1) | carry_in
        });
        self.flag_carry = carry_out;
    }

    fn instr_ror(&mut self, interconnect: &mut impl MemoryBus, addr_mode: AddrMode) {
        let carry_in = if self.flag_carry { 0x80 } else { 0 } as u8;
        let mut carry_out = false;

        self.writeback_instr_sz(interconnect, addr_mode, |m| {
            carry_out = m & 0b1 != 0;
            (m >> 1) | carry_in
        });
        self.flag_carry = carry_out;
    }

    fn instr_cmp(
        &mut self,
        interconnect: &mut impl MemoryBus,
        addr_mode: AddrMode,
        register: Register,
    ) {
        let reg_backup = self.read_reg(register);
        self.read_instr_szc(interconnect, addr_mode, register, |r, m| {
            (r.wrapping_sub(m), r >= m)
        });
        self.write_reg(register, reg_backup);
    }
}
