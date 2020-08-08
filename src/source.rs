use pathdiff;
use serde::{
    Deserialize,
    Serialize,
};

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
pub struct Code {
    path: Rc<str>,
    data: Rc<[char]>,
}

impl Debug for Code {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "Code(Rc<[char; {}]>)", self.data.len(),)
    }
}

impl Display for Code {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "//{}\n{}", self.path, self.iter().collect::<String>())
    }
}

impl Deref for Code {
    type Target = [char];

    fn deref(&self) -> &Self::Target { self.data.as_ref() }
}

impl Index<Pos> for Code {
    type Output = [char];

    fn index(&self, index: Pos) -> &Self::Output {
        &self.data.as_ref()[index.offset..(index.offset + index.length)]
    }
}

impl Index<usize> for Code {
    type Output = char;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data.as_ref()[index]
    }
}

impl Code {
    pub fn new(path: &str, data: impl Into<String>) -> Self {
        Code {
            path: Rc::from(path),
            data: Rc::from(
                Into::<String>::into(data).chars().collect::<Vec<char>>(),
            ),
        }
    }

    pub fn describe(&self, pos: Pos) -> String {
        let Pos { offset, .. } = pos;
        let mut line_offset = 0i64;
        let mut line = 0i64;
        let mut next_char_change_line = true;
        for i in 0..=offset {
            if next_char_change_line {
                line += 1;
                line_offset = 1;
                next_char_change_line = false;
            } else {
                line_offset += 1;
            }
            if i < self.len() && self[i] == '\n' {
                next_char_change_line = true;
            }
        }
        format!("{}:{}:{}", self.path.as_ref(), line, line_offset)
    }
}

#[cfg(test)]
mod tests {
    use super::Code;
    use super::Pos;

    #[test]
    fn test_code() {
        let s = Code::new("-", "abcdef");
        assert_eq!(format!("{:?}", s), "Code(Rc<[char; 6]>)");
        assert_eq!(format!("{}", s), "//-\nabcdef");
        assert_eq!(s.len(), 6);
        assert_eq!(s[0], 'a');
        assert_eq!(s[1], 'b');
        assert_eq!(s[2], 'c');
        assert_eq!(s[3], 'd');
        assert_eq!(s[4], 'e');
        assert_eq!(s[5], 'f');
    }

    #[test]
    fn test_describe() {
        let s = Code::new("-", "abc\n def\nghi\n jkl");
        assert_eq!(s.describe(Pos::span(0, 17)), "-:1:1");
        assert_eq!(s.describe(Pos::span(1, 16)), "-:1:2");
        assert_eq!(s.describe(Pos::span(2, 15)), "-:1:3");
        assert_eq!(s.describe(Pos::span(3, 14)), "-:1:4");
        assert_eq!(s.describe(Pos::span(4, 13)), "-:2:1");
        assert_eq!(s.describe(Pos::span(5, 12)), "-:2:2");
        assert_eq!(s.describe(Pos::span(6, 11)), "-:2:3");
        assert_eq!(s.describe(Pos::span(7, 10)), "-:2:4");
        assert_eq!(s.describe(Pos::span(8, 9)), "-:2:5");
        assert_eq!(s.describe(Pos::span(9, 8)), "-:3:1");
        assert_eq!(s.describe(Pos::span(10, 7)), "-:3:2");
        assert_eq!(s.describe(Pos::span(11, 6)), "-:3:3");
        assert_eq!(s.describe(Pos::span(12, 5)), "-:3:4");
        assert_eq!(s.describe(Pos::span(13, 4)), "-:4:1");
        assert_eq!(s.describe(Pos::span(14, 3)), "-:4:2");
        assert_eq!(s.describe(Pos::span(15, 2)), "-:4:3");
        assert_eq!(s.describe(Pos::span(16, 1)), "-:4:4");
        assert_eq!(s.describe(Pos::span(17, 0)), "-:4:5");
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Ord,
    PartialOrd,
    Hash,
    Serialize,
    Deserialize,
)]
pub struct Pos {
    pub offset: usize,
    pub length: usize,
}

impl Pos {
    pub fn span(offset: usize, length: usize) -> Self { Pos { offset, length } }

    pub fn one(offset: usize) -> Self { Pos { offset, length: 1 } }

    pub fn mark(offset: usize) -> Self { Pos { offset, length: 0 } }
}
