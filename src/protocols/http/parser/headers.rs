use bytes::BytesMut;
use http::HeaderMap;

pub struct HeaderParser {
    max_size: usize,
    state: ParserState,
    buffer: BytesMut,
}

impl HeaderParser {
    pub fn new(max_size: usize) -> Self {
        Self {
            max_size,
            state: ParserState::Start,
            buffer: BytesMut::with_capacity(8192),
        }
    }
} 