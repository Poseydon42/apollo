use crate::span::Span;
use std::ops::Deref;

#[derive(Debug)]
pub struct Node<T> {
    pub data: T,
    pub span: Span,
}

impl<T> Node<T> {
    pub fn new(data: T, span: Span) -> Self {
        Self {
            data,
            span
        }
    }
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
