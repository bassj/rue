use std::io::Write;

use {
    std::io,
    rue,
    rue::ast::*,
};

fn evaulate_expression(expr: Expression) -> i64 {
    match expr {
        Expression::IntegerLiteral(i) => i,
        Expression::NoOp => 0,
        Expression::BinaryOperation(op, lhs, rhs) => {
            let lhs = evaulate_expression(*lhs);
            let rhs = evaulate_expression(*rhs);
            match op {
                Operator::Add => { lhs + rhs },
                Operator::Subtract => { lhs - rhs },
                Operator::Multiply => { lhs * rhs },
                Operator::Divide => { lhs / rhs },
            }
        }
    }
}

fn main() -> io::Result<()> {
    println!("Welcome to the thunderdome!");
    loop {
        print!(">");
        io::stdout().flush()?;
        let mut line = String::new();
        io::stdin().read_line(&mut line)?;

        if line == "q" {
            break
        }

        let ast = rue::parse::parse_source(line);
        println!("{}", evaulate_expression(ast));
    }

    Ok(())
}