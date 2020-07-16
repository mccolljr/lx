use std::fmt::{
    Debug,
    Display,
    Formatter,
    Result as FmtResult,
};
use std::ops::{
    Deref,
    Index,
};
use std::rc::Rc;

#[derive(Clone)]
pub struct Code(Rc<[char]>);

impl Debug for Code {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "Code(Rc<[char; {}]>)", self.0.len(),)
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
        Self(Rc::from(src.as_ref().chars().collect::<Vec<char>>()))
    }
}

impl Deref for Code {
    type Target = [char];

    fn deref(&self) -> &Self::Target { self.0.as_ref() }
}

impl Index<Pos> for Code {
    type Output = [char];

    fn index(&self, index: Pos) -> &Self::Output {
        &self.0.as_ref()[index.offset..(index.offset + index.length)]
    }
}

impl Index<usize> for Code {
    type Output = char;

    fn index(&self, index: usize) -> &Self::Output { &self.0.as_ref()[index] }
}

#[cfg(test)]
mod tests {
    use super::Code;

    #[test]
    fn test_code() {
        let s = Code::from("abcdef");
        assert_eq!(format!("{:?}", s), "Code(Rc<[char; 6]>)");
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
    pub fn span(offset: usize, length: usize) -> Self { Pos { offset, length } }

    pub fn one(offset: usize) -> Self { Pos { offset, length: 1 } }

    pub fn mark(offset: usize) -> Self { Pos { offset, length: 0 } }
}
