use apollo::ast::*;
use apollo::codegen::*;
use apollo::diagnostic::ConsoleReporter;
use apollo::ir::Function;
use apollo::lexer::Lexer;
use apollo::opt::run_opt_pipeline;
use apollo::parser::Parser;
use apollo::typechecker::TypeChecker;
use std::rc::Rc;

fn main() -> Result<(), i32> {
    let src = Rc::new(
        r"fn main() -> i32 {\
            let cond: bool = true;\
            if cond 3 + 3 else 6 + 6\
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
    let type_ok = TypeChecker::new(&mut reporter).check(&mut module);
    println!("Type checking {}", if type_ok { "succeeded" } else { "failed" });
    if type_ok {
        printer.visit_module(&module);
    } else {
        return Ok(());
    }

    println!("===== IR =====");
    let ir_generator = IRGenerator::new();
    let mut ir_module = ir_generator.generate(&module);
    let function = ir_module.functions_mut().get_mut(0).unwrap();
    print_function(function);

    println!("===== OPTIMIZED IR =====");
    run_opt_pipeline(function);
    print_function(function);

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

fn print_function(function: &Function) {
    for bb in function.get_basic_blocks() {
        println!("{}:", bb.name());
        for (instruction_ref, instruction) in function.get_instructions_in_basic_block(bb.name()) {
            if instruction.produces_value() {
                println!("  {} = {}", function.get_value(instruction_ref).unwrap(), instruction);
            } else {
                println!("  {}", instruction);
            }
        }
    }
}
