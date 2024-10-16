use rustyline::DefaultEditor;
use std::collections::HashMap;

const VERSION: &str = "0.1.0";

fn main() {
    println!("Exsym {VERSION}");
    let mut scope = HashMap::new();

    let mut rl = DefaultEditor::new().unwrap();
    loop {
        if let Ok(code) = rl.readline("> ") {
            if code.trim() == ":q" {
                break;
            }

            rl.add_history_entry(&code).unwrap_or_default();
            if let Some(ast) = run_program(code, &mut scope) {
                println!("{}", ast.display(&mut scope));
            }
        }
    }
}

fn run_program(source: String, scope: &mut HashMap<String, Expr>) -> Option<Type> {
    let source = tokenize_program(source);
    let mut result = Type::Null;

    for line in source {
        if line.len() == 2 {
            let mut define = line[0].trim().to_string();
            let mut is_recalc = true;
            if define.trim().ends_with(':') {
                define.remove(define.len() - 1);
                define = define.trim().to_string();
                is_recalc = false;
            }

            if define.contains('[') && define.trim().ends_with(']') {
                define.remove(define.len() - 1);
                let (target, index) = define.split_once("[").unwrap();

                let mut array = scope.clone().get(target)?.eval(scope)?.get_array();

                array[parse_expr(index.to_string())?.eval(scope)?.get_number() as usize] =
                    if is_recalc {
                        parse_expr(line[1].to_string())?
                    } else {
                        Expr::Value(parse_expr(line[1].to_string())?.eval(scope)?)
                    };
                result = Type::Array(array);
                scope.insert(target.to_string(), Expr::Value(result.clone()));
            } else {
                result = parse_expr(line[1].clone())?.eval(scope)?;
                if is_recalc {
                    scope.insert(define, parse_expr(line[1].clone())?);
                } else {
                    scope.insert(define, Expr::Value(result.clone()));
                }
            }
        } else {
            result = parse_expr(line[0].clone())?.eval(scope)?;
        }
    }
    Some(result)
}

fn parse_expr(soruce: String) -> Option<Expr> {
    let tokens: Vec<String> = tokenize_expr(soruce)?;

    let left = tokens.last()?.trim().to_string();
    let left = if let Ok(n) = left.parse::<f64>() {
        Expr::Value(Type::Number(n))
    } else if left.starts_with('"') && left.ends_with('"') {
        let left = {
            let mut left = left.clone();
            left.remove(0);
            left.remove(left.len() - 1);
            left
        };
        Expr::Value(Type::String(left.to_string()))
    } else if left.starts_with('(') && left.ends_with(')') {
        let left = {
            let mut left = left.clone();
            left.remove(0);
            left.remove(left.len() - 1);
            left
        };
        parse_expr(left)?
    } else if left.starts_with('[') && left.ends_with(']') {
        let left = {
            let mut left = left.clone();
            left.remove(0);
            left.remove(left.len() - 1);
            left
        };
        Expr::Value(Type::Array(
            tokenize_args(left)?
                .iter()
                .map(|x| parse_expr(x.trim().to_string()).unwrap())
                .collect(),
        ))
    } else if left.contains('(') && left.ends_with(')') {
        let mut left = left.clone();
        left.remove(left.len() - 1);
        let (func, args) = left.split_once("(").unwrap();
        Expr::Function(
            match func {
                "sum" => |args, scope| {
                    args.get(0)?
                        .eval(scope)?
                        .get_array()
                        .iter()
                        .cloned()
                        .reduce(|a, c| {
                            let binding = a.eval(scope).unwrap().get_number()
                                + c.eval(scope).unwrap().get_number();
                            Expr::Value(Type::Number(binding))
                        })?
                        .eval(scope)
                },
                "count" => |args, scope| {
                    Some(Type::Number(
                        args.get(0)?.eval(scope)?.get_array().len() as f64
                    ))
                },
                "range" => |args, scope| {
                    Some(Type::Array(
                        (args.get(0)?.eval(scope)?.get_number() as usize
                            ..args.get(1)?.eval(scope)?.get_number() as usize)
                            .step_by(args.get(2)?.eval(scope)?.get_number() as usize)
                            .map(|x| Expr::Value(Type::Number(x as f64)))
                            .collect(),
                    ))
                },
                "if" => |args, scope| {
                    if args.get(0)?.eval(scope)?.get_bool() {
                        args.get(1)?.eval(scope)
                    } else {
                        args.get(2)?.eval(scope)
                    }
                },
                "lookup" => |args, scope| {
                    let index = args.get(1)?.eval(scope)?.get_array().iter().position(|x| {
                        x.eval(scope).unwrap().display(scope)
                            == args.get(0).unwrap().eval(scope).unwrap().display(scope)
                    })?;
                    args.get(2)?
                        .eval(scope)?
                        .get_array()
                        .get(index)?
                        .eval(scope)
                },
                _ => |args, scope| args.get(0)?.eval(scope),
            },
            tokenize_args(args.to_string())?
                .iter()
                .map(|x| parse_expr(x.to_owned()).unwrap())
                .collect::<Vec<Expr>>(),
        )
    } else if left.contains('[') && left.ends_with(']') {
        let mut left = left.clone();
        left.remove(left.len() - 1);
        let (target, index) = left.split_once("[").unwrap();
        Expr::Access(
            Box::new(parse_expr(target.to_string())?),
            Box::new(parse_expr(index.to_string())?),
        )
    } else {
        Expr::Value(Type::Symbol(left))
    };

    if let Some(operator) = {
        let mut tokens = tokens.clone();
        tokens.reverse();
        tokens
    }
    .get(1)
    {
        let operator = match operator.as_str() {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "%" => Operator::Mod,
            "=" => Operator::Equal,
            "<" => Operator::LessThan,
            ">" => Operator::GreaterThan,
            "++" => Operator::Concat,
            "&" => Operator::And,
            "|" => Operator::Or,
            _ => return None,
        };
        Some(Expr::Infix(Box::new(Infix {
            operator,
            values: (
                parse_expr(tokens.get(..tokens.len() - 2)?.to_vec().join(" "))?,
                left,
            ),
        })))
    } else {
        return Some(left);
    }
}

fn tokenize_program(input: String) -> Vec<Vec<String>> {
    let mut tokens: Vec<Vec<String>> = Vec::new();
    let mut current_token = String::new();
    let mut after_equal = String::new();
    let mut is_equal = false;
    let mut in_parentheses: usize = 0;
    let mut in_quote = false;

    for c in input.chars() {
        match c {
            '(' | '[' if !in_quote => {
                if is_equal {
                    after_equal.push(c);
                } else {
                    current_token.push(c);
                }
                in_parentheses += 1;
            }
            ')' | ']' if !in_quote => {
                if is_equal {
                    after_equal.push(c);
                } else {
                    current_token.push(c);
                }
                in_parentheses -= 1;
            }
            ';' if !in_quote => {
                if in_parentheses != 0 {
                    if is_equal {
                        after_equal.push(c);
                    } else {
                        current_token.push(c);
                    }
                } else if !current_token.is_empty() {
                    if is_equal {
                        is_equal = false;
                        tokens.push(vec![current_token.clone(), after_equal.clone()]);
                        current_token.clear();
                        after_equal.clear();
                    } else {
                        tokens.push(vec![current_token.clone()]);
                        current_token.clear();
                    }
                }
            }
            '=' if !in_quote => {
                if in_parentheses != 0 {
                    if is_equal {
                        after_equal.push(c);
                    } else {
                        current_token.push(c);
                    }
                } else {
                    is_equal = true;
                }
            }
            '"' => {
                in_quote = !in_quote;
                if is_equal {
                    after_equal.push(c);
                } else {
                    current_token.push(c);
                }
            }
            _ => {
                if is_equal {
                    after_equal.push(c);
                } else {
                    current_token.push(c);
                }
            }
        }
    }

    if in_parentheses == 0 && !current_token.is_empty() {
        if is_equal {
            tokens.push(vec![current_token.clone(), after_equal]);
            current_token.clear();
        } else {
            tokens.push(vec![current_token.clone()]);
            current_token.clear();
        }
    }
    tokens
}

fn tokenize_args(input: String) -> Option<Vec<String>> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current_token = String::new();
    let mut in_parentheses: usize = 0;
    let mut in_quote = false;

    for c in input.chars() {
        match c {
            '(' | '[' if !in_quote => {
                current_token.push(c);
                in_parentheses += 1;
            }
            ')' | ']' if !in_quote => {
                current_token.push(c);
                if in_parentheses > 0 {
                    in_parentheses -= 1;
                } else {
                    eprintln!("Error! there's duplicate end of the parentheses");
                    return None;
                }
            }
            ',' if !in_quote => {
                if in_parentheses != 0 {
                    current_token.push(c);
                } else if !current_token.is_empty() {
                    tokens.push(current_token.clone());
                    current_token.clear();
                }
            }
            '"' => {
                in_quote = !in_quote;
                current_token.push(c);
            }
            _ => {
                current_token.push(c);
            }
        }
    }

    // Syntax error check
    if in_quote {
        eprintln!("Error! there's not end of the quote");
        return None;
    }
    if in_parentheses != 0 {
        eprintln!("Error! there's not end of the parentheses");
        return None;
    }

    if in_parentheses == 0 && !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }
    Some(tokens)
}

fn tokenize_expr(input: String) -> Option<Vec<String>> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current_token = String::new();
    let mut in_parentheses: usize = 0;
    let mut in_quote = false;

    for c in input.chars() {
        match c {
            '(' | '[' if !in_quote => {
                current_token.push(c);
                in_parentheses += 1;
            }
            ')' | ']' if !in_quote => {
                current_token.push(c);
                if in_parentheses > 0 {
                    in_parentheses -= 1;
                } else {
                    eprintln!("Error! there's duplicate end of the parentheses");
                    return None;
                }
            }
            ' ' | '　' | '\t' if !in_quote => {
                if in_parentheses != 0 {
                    current_token.push(c);
                } else if !current_token.is_empty() {
                    tokens.push(current_token.clone());
                    current_token.clear();
                }
            }
            '"' => {
                in_quote = !in_quote;
                current_token.push(c);
            }
            _ => {
                current_token.push(c);
            }
        }
    }

    // Syntax error check
    if in_quote {
        eprintln!("Error! there's not end of the quote");
        return None;
    }
    if in_parentheses != 0 {
        eprintln!("Error! there's not end of the parentheses");
        return None;
    }

    if in_parentheses == 0 && !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }
    Some(tokens)
}

#[derive(Debug, Clone)]
enum Type {
    Number(f64),
    Bool(bool),
    String(String),
    Array(Vec<Expr>),
    Symbol(String),
    Null,
}

impl Type {
    fn get_number(&self) -> f64 {
        match self {
            Type::Number(n) => n.to_owned(),
            Type::String(s) | Type::Symbol(s) => s.trim().parse().unwrap_or(0.0),
            Type::Bool(b) => {
                if *b {
                    1.0
                } else {
                    0.0
                }
            }
            _ => 0.0,
        }
    }

    fn get_bool(&self) -> bool {
        match self {
            Type::Number(n) => *n != 0.0,
            Type::String(s) | Type::Symbol(s) => s.trim().parse().unwrap_or(false),
            Type::Bool(b) => *b,
            _ => false,
        }
    }

    fn get_string(&self) -> String {
        match self {
            Type::String(s) | Type::Symbol(s) => s.to_string(),
            Type::Number(n) => n.to_string(),
            Type::Bool(b) => b.to_string(),
            _ => String::new(),
        }
    }

    fn display(&self, scope: &mut HashMap<String, Expr>) -> String {
        match self {
            Type::String(s) => format!("\"{}\"", s),
            Type::Symbol(s) => s.to_string(),
            Type::Number(n) => n.to_string(),
            Type::Bool(b) => b.to_string(),
            Type::Array(a) => format!(
                "[{}]",
                a.iter()
                    .map(|x| x.eval(scope).unwrap().display(scope))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::Null => "null".to_string(),
        }
    }

    fn get_array(&self) -> Vec<Expr> {
        match self {
            Type::Array(s) => s.to_owned(),
            Type::String(s) => s
                .chars()
                .map(|x| Expr::Value(Type::String(x.to_string())))
                .collect(),
            other => vec![Expr::Value(other.to_owned())],
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Infix(Box<Infix>),
    Function(
        fn(Vec<Expr>, &mut HashMap<String, Expr>) -> Option<Type>,
        Vec<Expr>,
    ),
    Access(Box<Expr>, Box<Expr>),
    Value(Type),
}

impl Expr {
    fn eval(&self, scope: &mut HashMap<String, Expr>) -> Option<Type> {
        Some(match self {
            Expr::Infix(infix) => (*infix).eval(scope)?,
            Expr::Value(value) => {
                if let Type::Symbol(name) = value {
                    if let Some(refer) = scope.get(name.as_str()).cloned() {
                        refer.eval(scope)?
                    } else {
                        value.clone()
                    }
                } else {
                    value.clone()
                }
            }
            Expr::Function(func, args) => func(args.to_owned(), scope)?,
            Expr::Access(target, index) => target
                .eval(scope)?
                .get_array()
                .iter()
                .map(|x| x.eval(scope).unwrap())
                .collect::<Vec<Type>>()
                .get(index.eval(scope)?.get_number() as usize)?
                .clone(),
        })
    }
}

#[derive(Debug, Clone)]
struct Infix {
    operator: Operator,
    values: (Expr, Expr),
}

#[derive(Debug, Clone)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    LessThan,
    GreaterThan,
    And,
    Or,
    Concat,
}

impl Infix {
    fn eval(&self, scope: &mut HashMap<String, Expr>) -> Option<Type> {
        let left = self.values.0.eval(scope)?;
        let right = self.values.1.eval(scope)?;
        Some(match self.operator {
            Operator::Add => Type::Number(left.get_number() + right.get_number()),
            Operator::Sub => Type::Number(left.get_number() - right.get_number()),
            Operator::Mul => Type::Number(left.get_number() * right.get_number()),
            Operator::Div => Type::Number(left.get_number() / right.get_number()),
            Operator::Mod => Type::Number(left.get_number() % right.get_number()),
            Operator::Equal => Type::Bool(left.get_string() == right.get_string()),
            Operator::LessThan => Type::Bool(left.get_number() < right.get_number()),
            Operator::GreaterThan => Type::Bool(left.get_number() > right.get_number()),
            Operator::And => Type::Bool(left.get_bool() && right.get_bool()),
            Operator::Or => Type::Bool(left.get_bool() || right.get_bool()),
            Operator::Concat => Type::String(left.get_string() + &right.get_string()),
        })
    }
}
