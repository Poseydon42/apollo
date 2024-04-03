use std::fmt::*;
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq)]
pub struct Span {
    src: Option<Rc<String>>,
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(src: Rc<String>, start: usize, end: usize) -> Self {
        Self {
            src: Some(src),
            start,
            end
        }
    }

    pub fn merge_with(self, other: &Span) -> Self {
        assert_eq!(self.src, other.src);
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            ..self
        }
    }

    pub fn empty_eof() -> Self {
        Self {
            src: None,
            start: 0,
            end: 0
        }
    }

    pub fn text(&self) -> &str {
        if let Some(src) = &self.src {
            &src[self.start..self.end]
        } else {
            ""
        }
    }

    pub fn line(&self) -> usize {
        if let Some(src) = &self.src {
            src.chars().take(self.start).filter(|c| *c == '\n').count() + 1
        } else {
            0
        }
    }

    pub fn column(&self) -> usize {
        if let Some(src) = &self.src {
            let last_newline = src
                .chars()
                .take(self.start)
                .enumerate()
                .filter(|(_, c)| *c == '\n')
                .last()
                .and_then(|(i, _)| Some(i))
                .unwrap_or(0)
                + 1;
            src.chars().take(self.start).skip(last_newline).count() + 1
        } else {
            0
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "\"{}\" @({}, {})", self.text(), self.line(), self.column())
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(self, f)
    }
}