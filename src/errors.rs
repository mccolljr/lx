use super::source::Pos;

#[derive(Debug)]
pub enum Error {
    InvalidCharacter {
        at: Pos,
        ch: char,
    },

    UnexpectedCharacter {
        at:      Pos,
        ch:      char,
        context: &'static str,
    },

    UnterminatedStringLiteral {
        at: Pos,
    },

    InvalidEscapeSequence {
        at: Pos,
        ch: char,
    },

    Expected {
        at:     Pos,
        wanted: String,
        found:  String,
    },

    InvalidAssignment {
        at:    Pos,
        found: String,
    },

    Redeclaration {
        at:       Pos,
        original: Pos,
        name:     String,
    },

    Undeclared {
        at:   Pos,
        name: String,
    },
}
