#[cfg(test)]
mod tests {
    use crate::lexer::*;

    macro_rules! test_single_lexem {
        ($test_name:ident, $src:literal, $expected:expr) => {
            #[test]
            fn $test_name() {
                use std::rc::Rc;
                let src = Rc::new($src.to_owned());
                let mut lexer = Lexer::new(src.clone());
                let lexem = lexer.next();
                assert!(lexem.is_some(), "No lexem was produced");
                let lexem = lexem.unwrap();
                assert!(lexem.kind == $expected, "Lexem {} is not the same as expected lexem {}", lexem, $expected);
                assert!(lexem.text() == *src, "Lexem {} did not fully match source string {}", lexem, src);
            }
        };
    }

    macro_rules! test_multiple_lexems {
        ($test_name:ident, $src:literal, $($expected:expr, $text:literal),+) => {
            #[test]
            fn $test_name() {
                use std::rc::Rc;
                let src = Rc::new($src.to_owned());
                let mut lexer = Lexer::new(src.clone());
                $({
                    let lexem = lexer.next();
                    assert!(lexem.is_some(), "No lexem was produced; expected lexem {} with text {}", $expected, $text);
                    let lexem = lexem.unwrap();
                    assert!(lexem.kind == $expected, "Lexem {} is not the same as expected lexem {}", lexem, $expected);
                    assert!(lexem.text() == $text, "Text of lexem {} does not match expected text {:?}", lexem, $text);
                };)+
            }
        };
    }

    test_single_lexem!(identifier_alphabetical_only, "foo", LexemKind::Identifier);
    test_single_lexem!(identifier_alphanumeric, "foo123", LexemKind::Identifier);
    test_single_lexem!(identifier_with_underscore, "foo_bar", LexemKind::Identifier);

    test_single_lexem!(positive_integer, "11", LexemKind::IntegerLiteral);
    test_single_lexem!(zero_integer, "0", LexemKind::IntegerLiteral);

    test_single_lexem!(keyword_fn, "fn", LexemKind::Fn);
    test_single_lexem!(keyword_return, "return", LexemKind::Return);

    test_single_lexem!(left_paren, "(", LexemKind::LeftParen);
    test_single_lexem!(right_paren, ")", LexemKind::RightParen);
    test_single_lexem!(left_brace, "{", LexemKind::LeftBrace);
    test_single_lexem!(right_brace, "}", LexemKind::RightBrace);
    test_single_lexem!(semicolon, ";", LexemKind::Semicolon);
    test_single_lexem!(arrow, "->", LexemKind::Arrow);

    test_multiple_lexems!(punctuation_no_space, "(}->",
        LexemKind::LeftParen, "(",
        LexemKind::RightBrace, "}",
        LexemKind::Arrow, "->"
    );

    test_multiple_lexems!(punctuation_before_identifier, ";foo",
        LexemKind::Semicolon, ";",
        LexemKind::Identifier, "foo"
    );
    test_multiple_lexems!(punctuation_after_identifier, "foo;",
        LexemKind::Identifier, "foo",
        LexemKind::Semicolon, ";"
    );

    test_multiple_lexems!(identifiers_with_newlines, "foo\n\nbar\r\nbaz",
        LexemKind::Identifier, "foo",
        LexemKind::Identifier, "bar",
        LexemKind::Identifier, "baz"
    );
}
