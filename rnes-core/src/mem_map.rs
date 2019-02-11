use apu::ApuChannel;

const REG_PPUCTRL: u16 = 0x2000;
const REG_PPUMASK: u16 = 0x2001;
const REG_PPUSTATUS: u16 = 0x2002;
const REG_OAMADDR: u16 = 0x2003;
const REG_OAMDATA: u16 = 0x2004;
const REG_PPUSCROLL: u16 = 0x2005;
const REG_PPUADDR: u16 = 0x2006;
const REG_PPUDATA: u16 = 0x2007;

const REG_APUPULSE1CONTROL: u16 = 0x4000;
const REG_APUPULSE1SWEEPUNIT: u16 = 0x4001;
const REG_APUPULSE1TIMERLOW: u16 = 0x4002;
const REG_APUPULSE1LCTH: u16 = 0x4003;
const REG_APUPULSE2CONTROL: u16 = 0x4004;
const REG_APUPULSE2SWEEPUNIT: u16 = 0x4005;
const REG_APUPULSE2TIMERLOW: u16 = 0x4006;
const REG_APUPULSE2LCTH: u16 = 0x4007;
const REG_APUTRIGCONTROL: u16 = 0x4008;
const REG_APUTRIGTIMERLOW: u16 = 0x400A;
const REG_APUTRIGLCTH: u16 = 0x400B;
const REG_APUNOISECONTROL: u16 = 0x400C;
const REG_APUNOISEPERIOD: u16 = 0x400E;
const REG_APUNOISELC: u16 = 0x400F;
const REG_APUDMCCONTROL: u16 = 0x4010;
const REG_APUDMCLOADCOUNTER: u16 = 0x4011;
const REG_APUDMCSAMPLEADDRESS: u16 = 0x4012;
const REG_APUDMCSAMPLELENGTH: u16 = 0x4013;

const REG_OAMDMA: u16 = 0x4014;
const REG_APUSTATUS: u16 = 0x4015;
const REG_JOY1: u16 = 0x4016;
const REG_JOY2: u16 = 0x4017;

const CARTRIDGE_BEGIN: u16 = 0x6000;
const CARTRIDGE_SIZE: u16 = 0xA000;
const CARTRIDGE_END: u16 = CARTRIDGE_BEGIN + (CARTRIDGE_SIZE - 1);

#[derive(Debug, Eq, PartialEq)]
pub enum Addr {
    Ram(u16),
    RegPpuCtrl,
    RegPpuMask,
    RegPpuStatus,
    RegOamAddr,
    RegOamData,
    RegPpuScroll,
    RegPpuAddr,
    RegPpuData,
    RegApuChannelControl(ApuChannel),
    RegApuSweepUnit(ApuChannel),
    RegApuTimerLow(ApuChannel),
    RegApuLengthCouterTimerHigh(ApuChannel),
    RegApuDmcControl,
    RegApuDmcLoadCounter,
    RegApuDmcSampleAddress,
    RegApuDmcSampleLength,
    RegOamDma,
    RegApuStatus,
    RegJoy1,
    RegJoy2,
    Cartridge(u16),
}
/// Maps a physical memory address to the relative hardware
///
///     # use rnes_core::mem_map::{map_addr, Addr};
///     assert_eq!(map_addr(0x0123), Addr::Ram(0x0123));
///     assert_eq!(map_addr(0x2000), Addr::RegPpuCtrl);
///
pub fn map_addr(addr: u16) -> Addr {
    match addr {
        // Ram is mirrored 4 times
        0x0000...0x1fff => Addr::Ram(addr & 0x07ff),

        0x2000...0x3fff => match (addr & 7) + 0x2000 {
            REG_PPUCTRL => Addr::RegPpuCtrl,
            REG_PPUMASK => Addr::RegPpuMask,
            REG_PPUSTATUS => Addr::RegPpuStatus,
            REG_OAMADDR => Addr::RegOamAddr,
            REG_OAMDATA => Addr::RegOamData,
            REG_PPUSCROLL => Addr::RegPpuScroll,
            REG_PPUADDR => Addr::RegPpuAddr,
            REG_PPUDATA => Addr::RegPpuData,
            _ => panic!("{:X}", (addr & 7) + 0x2000),
        },

        REG_APUPULSE1CONTROL => Addr::RegApuChannelControl(ApuChannel::Pulse1),
        REG_APUPULSE1SWEEPUNIT => Addr::RegApuSweepUnit(ApuChannel::Pulse1),
        REG_APUPULSE1TIMERLOW => Addr::RegApuTimerLow(ApuChannel::Pulse1),
        REG_APUPULSE1LCTH => Addr::RegApuLengthCouterTimerHigh(ApuChannel::Pulse1),
        REG_APUPULSE2CONTROL => Addr::RegApuChannelControl(ApuChannel::Pulse2),
        REG_APUPULSE2SWEEPUNIT => Addr::RegApuSweepUnit(ApuChannel::Pulse2),
        REG_APUPULSE2TIMERLOW => Addr::RegApuTimerLow(ApuChannel::Pulse2),
        REG_APUPULSE2LCTH => Addr::RegApuLengthCouterTimerHigh(ApuChannel::Pulse2),
        REG_APUTRIGCONTROL => Addr::RegApuChannelControl(ApuChannel::Triangle),
        REG_APUTRIGTIMERLOW => Addr::RegApuTimerLow(ApuChannel::Triangle),
        REG_APUTRIGLCTH => Addr::RegApuLengthCouterTimerHigh(ApuChannel::Triangle),
        REG_APUNOISECONTROL => Addr::RegApuChannelControl(ApuChannel::Noise),
        REG_APUNOISEPERIOD => Addr::RegApuTimerLow(ApuChannel::Noise),
        REG_APUNOISELC => Addr::RegApuLengthCouterTimerHigh(ApuChannel::Noise),
        REG_APUDMCCONTROL => Addr::RegApuDmcControl,
        REG_APUDMCLOADCOUNTER => Addr::RegApuDmcLoadCounter,
        REG_APUDMCSAMPLEADDRESS => Addr::RegApuDmcSampleAddress,
        REG_APUDMCSAMPLELENGTH => Addr::RegApuDmcSampleLength,

        REG_OAMDMA => Addr::RegOamDma,
        REG_APUSTATUS => Addr::RegApuStatus,

        REG_JOY1 => Addr::RegJoy1,
        REG_JOY2 => Addr::RegJoy2,

        CARTRIDGE_BEGIN...CARTRIDGE_END => Addr::Cartridge(addr),
        _ => panic!("Unknown physical address {:04X}", addr),
    }
}
