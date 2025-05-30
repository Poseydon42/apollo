use apollo::ast::*;
use apollo::codegen::*;
use apollo::diagnostic::ConsoleReporter;
use apollo::lexer::Lexer;
use apollo::parser::Parser;
use apollo::typechecker::TypeChecker;
use std::rc::Rc;

fn main() -> Result<(), i32> {
    let src = Rc::new(
        r"fn main() -> i32 {\
            let a: i32 = 30; \
            return 12 + a; \
        }".to_owned());

    let mut lexer = Lexer::new(src);
    let mut lexems = vec![];
    while !lexer.eof() {
        if let Some(lexem) = lexer.next() {
            lexems.push(lexem);
        }
    }

    let mut reporter = ConsoleReporter::new();
    let mut parser = Parser::new(lexems, &mut reporter);
    let mut module = parser.parse();

    println!("===== AST =====");
    let stdout = &mut std::io::stdout();
    let mut printer = ASTPrinter::new(stdout);
    printer.visit_module(&module);

    println!("===== TYPECHECK =====");
    let type_ok = TypeChecker::new().check(&mut module);
    println!("Type checking {}", if type_ok { "succeeded" } else { "failed" });
    if type_ok {
        printer.visit_module(&module);
    }

    println!("===== IR =====");
    let ir_generator = IRGenerator::new();
    let ir_module = ir_generator.generate(&module);
    let function = &ir_module.functions()[0];
    for (instruction_ref, instruction) in function.code.instructions() {
        if instruction.produces_value() {
            println!("{} = {}", function.code.get_value(instruction_ref).unwrap(), instruction);
        } else {
            println!("{}", instruction);
        }
    }

    println!("===== CODEGEN =====");
    let isa = x86_64::ISA::new();
    let native_function = codegen_function(function, isa, true);

    if let Some(native_function) = native_function {
        println!();
        native_function.lines.iter().for_each(|line| println!("{}", line));
        Ok(())
    } else {
        println!("Codegen failed");
        Err(1)
    }
}
