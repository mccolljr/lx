use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::ops::Deref;
use std::rc::Rc;

pub struct Code {
    pub raw: Rc<[char]>,
    offset: usize,
    length: usize,
}

impl Clone for Code {
    fn clone(&self) -> Self {
        Code {
            raw: Rc::clone(&self.raw),
            offset: self.offset,
            length: self.length,
        }
    }
}

impl Debug for Code {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(
            f,
            "Source(raw: [char; {}], offset: {}, length: {})",
            self.raw.len(),
            self.offset,
            self.length
        )
    }
}

impl Display for Code {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.iter().collect::<String>())
    }
}

impl<T> From<T> for Code
where
    T: AsRef<str>,
{
    fn from(src: T) -> Self {
        let chars = src.as_ref().chars().collect::<Vec<char>>();
        let char_count = chars.len();
        Code {
            raw: Rc::from(chars),
            offset: 0,
            length: char_count,
        }
    }
}

impl Deref for Code {
    type Target = [char];

    fn deref(&self) -> &Self::Target {
        &(self.raw[self.offset..self.offset + self.length])
    }
}

#[cfg(test)]
mod tests {
    use super::Code;

    #[test]
    fn test_code() {
        let s = Code::from("abcdef");
        assert_eq!(
            format!("{:?}", s),
            "Source(raw: [char; 6], offset: 0, length: 6)"
        );
        assert_eq!(format!("{}", s), "abcdef");
        assert_eq!(s.len(), 6);
        assert_eq!(s[0], 'a');
        assert_eq!(s[1], 'b');
        assert_eq!(s[2], 'c');
        assert_eq!(s[3], 'd');
        assert_eq!(s[4], 'e');
        assert_eq!(s[5], 'f');
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    pub offset: usize,
    pub length: usize,
}

impl Pos {
    pub fn span(offset: usize, length: usize) -> Self {
        Pos { offset, length }
    }

    pub fn one(offset: usize) -> Self {
        Pos { offset, length: 1 }
    }

    pub fn mark(offset: usize) -> Self {
        Pos { offset, length: 0 }
    }
}
