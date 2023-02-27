#[derive(Debug)]
pub enum Fakable<T> {
    Real(T),
    Fake,
}

impl<T> Fakable<T> {
    pub fn is_real(&self) -> bool {
        matches!(self, Self::Real(_))
    }

    pub fn is_fake(&self) -> bool {
        matches!(self, Self::Fake)
    }

    pub fn box_inner(self) -> Fakable<Box<T>> {
        match self {
            Self::Real(value) => Fakable::Real(Box::new(value)),
            Self::Fake => Fakable::Fake,
        }
    }
}
