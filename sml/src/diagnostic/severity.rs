use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum Severity {
    Error(u32),
    Warning(u32),
}

#[derive(Debug)]
pub enum LowSeverity {
    Error(u16),
    Warning(u16),
}

impl From<(LowSeverity, u8)> for Severity {
    #[rustfmt::skip]
    fn from((severity, high_id): (LowSeverity, u8)) -> Self {
        match severity {
            LowSeverity::Error(low_id) => Self::Error((high_id as u32) << 16 | low_id as u32),
            LowSeverity::Warning(low_id) => Self::Warning((high_id as u32) << 16 | low_id as u32),
        }
    }
}

impl Display for Severity {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error(id) => write!(f, "Ex{:010X}", id),
            Self::Warning(id) => write!(f, "Wx{:010X}", id),
        }
    }
}
