use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum Severity {
    Error(u32),
    Warning(u32),
}

#[derive(Debug)]
pub enum KindSeverity {
    Error(u16),
    Warning(u16),
}

impl From<(KindSeverity, u8)> for Severity {
    #[rustfmt::skip]
    fn from((severity, high_id): (KindSeverity, u8)) -> Self {
        match severity {
            KindSeverity::Error(low_id) => Self::Error((high_id as u32) << 16 | low_id as u32),
            KindSeverity::Warning(low_id) => Self::Warning((high_id as u32) << 16 | low_id as u32),
        }
    }
}

impl Display for Severity {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error(id) => write!(f, "Ex{:06X}", id),
            Self::Warning(id) => write!(f, "Wx{:06X}", id),
        }
    }
}
