use std::rc::Rc;
use super::lexem::*;

pub struct Lexer {
    src: Rc<String>,
    cursor: usize,
}

impl Lexer {
    pub fn new(src: Rc<String>) -> Self {
        Self {
            src,
            cursor: 0
        }
    }

    pub fn next(&mut self) -> Option<Lexem> {
        while let Some(char) = self.peek() {
            if char.is_whitespace() {
                self.take();
            } else {
                break;
            }
        }

        let char = match self.take() {
            Some(char) => char,
            None => return None
        };

        if let Some(second_char) = self.peek() {
            if let Some(lexem) = is_double_char_lexem(char, second_char) {
                return Some(Lexem::new(lexem, self.src.clone(), self.cursor - 1, self.cursor + 1))
            }
        }

        if let Some(lexem) = is_single_char_lexem(char) {
            return Some(Lexem::new(lexem, self.src.clone(), self.cursor - 1, self.cursor));
        }

        if char.is_alphabetic() {
            self.revert();
            return self.consume_identifier_or_keyword()
        }

        if char.is_numeric() {
            self.revert();
            return self.consume_number()
        }

        None
    }

    pub fn eof(&self) -> bool {
        self.cursor >= self.src.chars().count()
    }

    fn peek(&self) -> Option<char> {
        self.src.chars().nth(self.cursor)
    }

    fn take(&mut self) -> Option<char> {
        let result = self.peek();
        if let Some(_) = result {
            self.cursor += 1;
        }
        result
    }

    fn revert(&mut self) {
        if self.cursor > 0 {
            self.cursor -= 1;
        } else {
            panic!("Cannot revert past first character");
        }
    }

    fn consume_while<P: Fn(char) -> bool>(&mut self, pred: P) -> Option<(usize, usize)> {
        let start = self.cursor;
        while !self.eof() {
            let char = self.peek().unwrap();
            if !pred(char) {
                break;
            }
            self.take();
        }
        let end = self.cursor;
        if end > start {
            Some((start, end))
        } else {
            None
        }
    }

    fn consume_identifier_or_keyword(&mut self) -> Option<Lexem> {
        if let Some((start, end)) = self.consume_while(|c| c.is_alphanumeric() || c == '_') {
            let text = &self.src[start..end];
            if let Some(keyword) = is_keyword(text) {
                Some(Lexem::new(keyword, self.src.clone(), start, end))
            } else {
                Some(Lexem::new(LexemKind::Identifier, self.src.clone(), start, end))
            }
        } else {
            None
        }
    }

    fn consume_number(&mut self) -> Option<Lexem> {
        if let Some((start, end)) = self.consume_while(char::is_numeric) {
            Some(Lexem::new(LexemKind::IntegerLiteral, self.src.clone(), start, end))
        } else {
            None
        }
    }
}
