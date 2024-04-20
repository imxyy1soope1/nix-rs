use std::ops::{AddAssign, SubAssign};

pub struct ByteCode {
    pub(crate) inner: Box<[u8]>,
    pub(crate) pos: usize,
}

/* impl Debug for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut result = String::new();
        let header = "         00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F".cyan();
        result.push_str(&header);
        result.push('\n');

        let current_pos = self.pos;
        let mut replica = ByteCode {
            inner: self.inner,
            pos: self.pos,
        };
        replica.reset();

        // If the amount of data is large, it may be better to use a lookup table.
        let mut content: Vec<String> = replica
            .inner
            .iter()
            .map(|byte| format!("{:02X}", byte))
            .collect();
        if let Some(byte) = content.get_mut(current_pos) {
            *byte = byte.green();
        }
        for (i, line) in content.chunks(16).map(|line| line.join(" ")).enumerate() {
            let line_number = {
                let mut line_number = "0".repeat(8);
                let hex = format!("{:02X}", i);
                line_number.replace_range((8 - hex.len() - 1)..7, &hex);
                line_number
            };
            result.push_str(&line_number);
            result.push(' ');
            result.push_str(&line);
            result.push('\n');
        }
        write!(f, "\n{}", result)
    }
} */

impl ByteCode {
    pub fn new(slice: Box<[u8]>) -> Self {
        ByteCode {
            inner: slice,
            pos: 0,
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.inner[self.pos..self.len()]
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn reset(&mut self) {
        self.pos = 0;
    }

    pub fn is_end(&self) -> bool {
        self.pos == self.len()
    }

    pub fn peek(&self, num: usize) -> &[u8] {
        if num > self.len() {
            panic!(
                "range end index {} out of range for slice of length {}",
                num,
                self.len()
            );
        }
        &self.inner[self.pos..self.pos+num]
    }

    pub fn starts_with(&self, v: &[u8]) -> bool {
        self.peek(v.len()) == v
    }

    pub fn next(&mut self) {
        *self += 1;
    }

    pub fn prev(&mut self) {
        *self -= 1;
    }

    pub fn skip(&mut self, num: usize) {
        *self += num;
    }

    pub fn take(&mut self, num: usize) -> Vec<u8> {
        let result = self.peek(num).to_owned();
        self.skip(num);
        result
    }

    pub fn take_into_u8(&mut self) -> u8 {
        self.take(1)[0]
    }

    pub fn take_into_u16(&mut self) -> u16 {
        let bytes: [u8; 2] = self.take(2).try_into().unwrap();
        u16::from_le_bytes(bytes)
    }

    pub fn take_into_u32(&mut self) -> u32 {
        let bytes: [u8; 4] = self.take(4).try_into().unwrap();
        u32::from_le_bytes(bytes)
    }

    pub fn take_into_string(&mut self, num: usize) -> String {
        let bytes = self.take(num);
        String::from_utf8(bytes).unwrap()
    }
}

impl AddAssign<usize> for ByteCode {
    fn add_assign(&mut self, rhs: usize) {
        if rhs > self.inner.len() {
            panic!(
                "index out of bounds: the slice can only move forward {}, but tried to move {}",
                self.inner.len(),
                rhs
            );
        }
        self.pos += rhs;
    }
}

impl SubAssign<usize> for ByteCode {
    fn sub_assign(&mut self, rhs: usize) {
        if rhs > self.pos {
            panic!(
                "index out of bounds: the slice can only move back {}, but tried to move {}",
                self.pos, rhs
            );
        }
        self.pos -= rhs;
    }
}

