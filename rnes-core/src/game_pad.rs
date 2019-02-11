pub enum Button {
    A,
    B,
    Select,
    Start,
    Up,
    Down,
    Left,
    Right,
}

pub struct GamePad {
    joy1_buttons: u8,
    joy1_strobing: bool,
    joy1_shiftreg: u8,
}

impl GamePad {
    pub fn new() -> GamePad {
        GamePad {
            joy1_buttons: 0,
            joy1_strobing: false,
            joy1_shiftreg: 0,
        }
    }

    pub fn set_button_pressed(&mut self, btn: Button, pressed: bool) {
        let mask = match btn {
            Button::A => 0b1000_0000,
            Button::B => 0b0100_0000,
            Button::Select => 0b0010_0000,
            Button::Start => 0b0001_0000,
            Button::Up => 0b0000_1000,
            Button::Down => 0b0000_0100,
            Button::Left => 0b0000_0010,
            Button::Right => 0b0000_0001,
        };
        let value = if pressed { 0xff } else { 0 };
        self.joy1_buttons = (self.joy1_buttons & !mask) | (value & mask);
    }

    pub fn write_joy1_reg(&mut self, value: u8) {
        self.strobe();
        self.joy1_strobing = value & 0b1 != 0;
        self.strobe();
    }

    pub fn read_joy1_reg(&mut self) -> u8 {
        // TODO: This is not accurate!
        self.strobe();
        let val = self.joy1_shiftreg;
        self.joy1_shiftreg <<= 1;
        val >> 7
    }

    fn strobe(&mut self) {
        if self.joy1_strobing {
            self.joy1_shiftreg = self.joy1_buttons;
        }
    }
}
