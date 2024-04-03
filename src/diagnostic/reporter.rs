use super::*;

pub trait Reporter {
    fn report(&mut self, diagnostic: impl Diagnostic);
}

pub struct ConsoleReporter {
}

impl ConsoleReporter {
    pub fn new() -> Self {
        Self {
        }
    }
}

impl Reporter for ConsoleReporter {
    fn report(&mut self, diagnostic: impl Diagnostic) {
        println!("{} @ ({},{}): {}", diagnostic.severity(), diagnostic.location().line(), diagnostic.location().column(), diagnostic.message());
    }
}
