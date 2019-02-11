use rnes_core::sinks::*;

pub struct MostRecentFrameSink<T> {
    inner: Option<T>,
}

impl<T> MostRecentFrameSink<T> {
    pub fn new() -> MostRecentFrameSink<T> {
        MostRecentFrameSink { inner: None }
    }

    pub fn into_frame(self) -> Option<T> {
        self.inner
    }
}

impl<T> Sink<T> for MostRecentFrameSink<T> {
    fn append(&mut self, frame: T) {
        self.inner = Some(frame);
    }
}
