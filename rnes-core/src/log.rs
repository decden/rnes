use std::fmt;

pub enum LogLevel {
    Stub,
    Info,
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LogLevel::Stub => "STUB",
                LogLevel::Info => "INFO",
            }
        )
    }
}

#[macro_export]
macro_rules! log {
	($level:expr, $domain:expr, $fmt:expr, $($arg:tt)*) => (
		println!("[{:4}][{}] {}", $level, $domain, format!($fmt, $($arg)*));
	);
}

#[macro_export]
macro_rules! log_stub {
	($($arg:tt)*) => (
		log!($crate::log::LogLevel::Stub, $($arg)*);
	);
}

#[macro_export]
macro_rules! log_info {
	($($arg:tt)*) => (
		log!($crate::log::LogLevel::Info, $($arg)*);
	);
}
