use rustyline::DefaultEditor;
use std::collections::{HashMap, HashSet};

const VERSION: &str = "0.1.0";

fn main() {
    println!("Exsym {VERSION}");
    let mut scope: HashMap<String, Expr> = HashMap::from([
        (
            "sum".to_string(),
            Expr::Value(Type::Function(Function::BuiltIn(|args, scope| {
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
            }))),
        ),
        (
            "max".to_string(),
            Expr::Value(Type::Function(Function::BuiltIn(|args, scope| {
                args.get(0)?
                    .eval(scope)?
                    .get_array()
                    .iter()
                    .cloned()
                    .reduce(|a, c| {
                        let a = a.eval(scope).unwrap().get_number();
                        let c = c.eval(scope).unwrap().get_number();
                        if a > c {
                            Expr::Value(Type::Number(a))
                        } else {
                            Expr::Value(Type::Number(c))
                        }
                    })?
                    .eval(scope)
            }))),
        ),
        (
            "min".to_string(),
            Expr::Value(Type::Function(Function::BuiltIn(|args, scope| {
                args.get(0)?
                    .eval(scope)?
                    .get_array()
                    .iter()
                    .cloned()
                    .reduce(|a, c| {
                        let a = a.eval(scope).unwrap().get_number();
                        let c = c.eval(scope).unwrap().get_number();
                        if a < c {
                            Expr::Value(Type::Number(a))
                        } else {
                            Expr::Value(Type::Number(c))
                        }
                    })?
                    .eval(scope)
            }))),
        ),
        (
            "average".to_string(),
            Expr::Value(Type::Function(Function::BuiltIn(|args, scope| {
                Some(Type::Number(
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
                        .eval(scope)?
                        .get_number()
                        / args.get(0)?.eval(scope)?.get_array().len() as f64,
                ))
            }))),
        ),
        (
            "count".to_string(),
            Expr::Value(Type::Function(Function::BuiltIn(|args, scope| {
                Some(Type::Number(
                    args.get(0)?.eval(scope)?.get_array().len() as f64
                ))
            }))),
        ),
        (
            "filter".to_string(),
            Expr::Value(Type::Function(Function::BuiltIn(|args, scope| {
                let mut result = vec![];
                let func = args.get(1)?.eval(scope)?.get_function();
                for target in args.get(0)?.eval(scope)?.get_array() {
                    if Expr::Function(func.clone(), vec![target.clone()])
                        .eval(scope)?
                        .get_bool()
                    {
                        result.push(target);
                    }
                }
                Some(Type::Array(result))
            }))),
        ),
        (
            "map".to_string(),
            Expr::Value(Type::Function(Function::BuiltIn(|args, scope| {
                let mut result = vec![];
                let func = args.get(1)?.eval(scope)?.get_function();
                for target in args.get(0)?.eval(scope)?.get_array() {
                    result.push(Expr::Value(
                        Expr::Function(func.clone(), vec![target]).eval(scope)?,
                    ));
                }
                Some(Type::Array(result))
            }))),
        ),
        (
            "range".to_string(),
            Expr::Value(Type::Function(Function::BuiltIn(|args, scope| {
                Some(Type::Array(
                    (args.get(0)?.eval(scope)?.get_number() as usize
                        ..args.get(1)?.eval(scope)?.get_number() as usize)
                        .step_by(args.get(2)?.eval(scope)?.get_number() as usize)
                        .map(|x| Expr::Value(Type::Number(x as f64)))
                        .collect(),
                ))
            }))),
        ),
        (
            "if".to_string(),
            Expr::Value(Type::Function(Function::BuiltIn(|args, scope| {
                if args.get(0)?.eval(scope)?.get_bool() {
                    let func = args.get(1)?.eval(scope)?.get_function();
                    Expr::Function(func, vec![]).eval(scope)
                } else {
                    let func = args.get(2)?.eval(scope)?.get_function();
                    Expr::Function(func, vec![]).eval(scope)
                }
            }))),
        ),
        (
            "split".to_string(),
            Expr::Value(Type::Function(Function::BuiltIn(|args, scope| {
                Some(Type::Array(
                    args.get(0)?
                        .eval(scope)?
                        .get_string()
                        .split(&args.get(1)?.eval(scope)?.get_string())
                        .map(|x| Expr::Value(Type::String(x.to_string())))
                        .collect(),
                ))
            }))),
        ),
        (
            "join".to_string(),
            Expr::Value(Type::Function(Function::BuiltIn(|args, scope| {
                Some(Type::String(
                    args.get(0)?
                        .eval(scope)?
                        .get_array()
                        .iter()
                        .map(|x| x.eval(scope).unwrap().get_string())
                        .collect::<Vec<String>>()
                        .join(&args.get(1)?.eval(scope)?.get_string()),
                ))
            }))),
        ),
    ]);

    let mut rl = DefaultEditor::new().unwrap();
    loop {
        if let Ok(code) = rl.readline("> ") {
            let code = code.trim().to_string();
            if code == ":q" {
                break;
            } else if code == ":m" {
                if scope.len() == 0 {
                    println!("Symbols {{ }}");
                } else {
                    println!("Symbols {{");
                    let width = scope.keys().map(|s| s.len()).max().unwrap_or(0);
                    let mut names = scope
                        .keys()
                        .cloned()
                        .collect::<HashSet<String>>()
                        .iter()
                        .cloned()
                        .collect::<Vec<String>>();
                    names.sort();
                    for name in names {
                        let value = scope[&name].clone();
                        if let Some(result) = value.eval(&mut scope) {
                            println!(" {:>width$}: {}", name, result.display(&mut scope));
                        }
                    }
                    println!("}}");
                }
            } else if !code.is_empty() {
                rl.add_history_entry(&code).unwrap_or_default();
                if let Some(ast) = run_program(code, &mut scope) {
                    println!("{}", ast.display(&mut scope));
                }
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
                let index = parse_expr(index.to_string(), scope)?
                    .eval(scope)?
                    .get_number() as usize;
                let value = if is_recalc {
                    parse_expr(line[1].to_string(), scope)?
                } else {
                    Expr::Value(parse_expr(line[1].to_string(), scope)?.eval(scope)?)
                };

                if array.len() <= index {
                    array.push(value);
                } else {
                    array[index] = value;
                }

                result = Type::Array(array);
                scope.insert(target.to_string(), Expr::Value(result.clone()));
            } else {
                result = parse_expr(line[1].clone(), scope)?.eval(scope)?;
                if is_recalc {
                    let value = parse_expr(line[1].clone(), scope)?;
                    scope.insert(define, value);
                } else {
                    scope.insert(define, Expr::Value(result.clone()));
                }
            }
        } else {
            result = parse_expr(line[0].clone(), scope)?.eval(scope)?;
        }
    }
    Some(result)
}

fn parse_expr(soruce: String, scope: &mut HashMap<String, Expr>) -> Option<Expr> {
    let tokens: Vec<String> = tokenize_expr(soruce)?;

    let left = tokens.last()?.trim().to_string();
    let left = if let Ok(n) = left.parse::<f64>() {
        Expr::Value(Type::Number(n))
    } else if left.starts_with("!") {
        let mut left = left.clone();
        left.remove(0);
        Expr::Prefix(Box::new(Prefix {
            operator: Operator::Not,
            values: parse_expr(left.to_string(), scope)?,
        }))
    } else if left.starts_with("-") {
        let mut left = left.clone();
        left.remove(0);
        Expr::Prefix(Box::new(Prefix {
            operator: Operator::Sub,
            values: parse_expr(left.to_string(), scope)?,
        }))
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
        parse_expr(left, scope)?
    } else if left.starts_with("function(") && left.ends_with('}') {
        let left = {
            let mut left = left.clone();
            left = left.replacen("function(", "", 1);
            left.remove(left.len() - 1);
            left
        };
        let splited = left.split_once("){").unwrap();
        let args = tokenize_args(splited.0.to_string())?
            .iter()
            .map(|x| x.trim().to_string())
            .collect();
        Expr::Value(Type::Function(Function::UserDefined(
            args,
            splited.1.to_string(),
        )))
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
                .map(|x| parse_expr(x.trim().to_string(), scope).unwrap())
                .collect(),
        ))
    } else if left.starts_with('{') && left.ends_with('}') {
        let left = {
            let mut left = left.clone();
            left.remove(0);
            left.remove(left.len() - 1);
            left
        };
        Expr::Block(left)
    } else if left.contains('(') && left.ends_with(')') {
        let mut left = left.clone();
        left.remove(left.len() - 1);
        let (func, args) = left.split_once("(").unwrap();
        Expr::Function(
            if let Type::Function(f) = parse_expr(func.to_string(), scope)?.eval(scope)? {
                f
            } else {
                return None;
            },
            tokenize_args(args.to_string())?
                .iter()
                .map(|x| parse_expr(x.to_owned(), scope).unwrap())
                .collect::<Vec<Expr>>(),
        )
    } else if left.contains('[') && left.ends_with(']') {
        let mut left = left.clone();
        left.remove(left.len() - 1);
        let (target, index) = left.split_once("[").unwrap();
        Expr::Access(
            Box::new(parse_expr(target.to_string(), scope)?),
            Box::new(parse_expr(index.to_string(), scope)?),
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
            "^" => Operator::Pow,
            "==" => Operator::Equal,
            "<" => Operator::LessThan,
            ">" => Operator::GreaterThan,
            "&" => Operator::And,
            "|" => Operator::Or,
            _ => return None,
        };
        Some(Expr::Infix(Box::new(Infix {
            operator,
            values: (
                parse_expr(tokens.get(..tokens.len() - 2)?.to_vec().join(" "), scope)?,
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
            '(' | '[' | '{' if !in_quote => {
                if is_equal {
                    after_equal.push(c);
                } else {
                    current_token.push(c);
                }
                in_parentheses += 1;
            }
            ')' | ']' | '}' if !in_quote => {
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
            '(' | '[' | '{' if !in_quote => {
                current_token.push(c);
                in_parentheses += 1;
            }
            ')' | ']' | '}' if !in_quote => {
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
            '(' | '[' | '{' if !in_quote => {
                current_token.push(c);
                in_parentheses += 1;
            }
            ')' | ']' | '}' if !in_quote => {
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
    Function(Function),
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
            Type::Function(Function::BuiltIn(f)) => format!("function({f:?})"),
            Type::Function(Function::UserDefined(args, code)) => {
                format!("function({}){{{code}}}", args.join(", "))
            }
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
    fn get_function(&self) -> Function {
        match self {
            Type::Function(func) => func.clone(),
            _ => Function::UserDefined(vec![], "".to_string()),
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Infix(Box<Infix>),
    Prefix(Box<Prefix>),
    Function(Function, Vec<Expr>),
    Block(String),
    Access(Box<Expr>, Box<Expr>),
    Value(Type),
}

#[derive(Clone, Debug)]
enum Function {
    BuiltIn(fn(Vec<Expr>, &mut HashMap<String, Expr>) -> Option<Type>),
    UserDefined(Vec<String>, String),
}

impl Expr {
    fn eval(&self, scope: &mut HashMap<String, Expr>) -> Option<Type> {
        Some(match self {
            Expr::Prefix(prefix) => (*prefix).eval(scope)?,
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
            Expr::Function(Function::BuiltIn(func), args) => func(args.to_owned(), scope)?,
            Expr::Function(Function::UserDefined(params, code), args) => {
                let mut scope = scope.clone();
                for i in params.iter().zip(args) {
                    scope.insert(i.0.to_string(), i.1.clone());
                }
                run_program(code.to_string(), &mut scope)?
            }
            Expr::Block(code) => run_program(code.to_string(), &mut scope.clone())?,
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
struct Prefix {
    operator: Operator,
    values: Expr,
}

#[derive(Debug, Clone)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Equal,
    LessThan,
    GreaterThan,
    And,
    Or,
    Not,
}

impl Infix {
    fn eval(&self, scope: &mut HashMap<String, Expr>) -> Option<Type> {
        let left = self.values.0.eval(scope)?;
        let right = self.values.1.eval(scope)?;
        Some(match self.operator {
            Operator::Add => {
                if let Type::Number(left) = left {
                    Type::Number(left + right.get_number())
                } else if let Type::String(left) = left {
                    Type::String(left + &right.get_string())
                } else if let Type::Array(left) = left {
                    Type::Array([left, right.get_array()].concat())
                } else {
                    Type::Null
                }
            }
            Operator::Sub => {
                if let Type::Number(left) = left {
                    Type::Number(left - right.get_number())
                } else if let Type::String(left) = left {
                    Type::String(left.replace(&right.get_string(), ""))
                } else {
                    Type::Null
                }
            }
            Operator::Mul => {
                if let Type::Number(left) = left {
                    Type::Number(left * right.get_number())
                } else if let Type::String(left) = left {
                    Type::String(left.repeat(right.get_number() as usize))
                } else {
                    Type::Null
                }
            }
            Operator::Div => Type::Number(left.get_number() / right.get_number()),
            Operator::Mod => Type::Number(left.get_number() % right.get_number()),
            Operator::Pow => Type::Number(left.get_number().powf(right.get_number())),
            Operator::Equal => Type::Bool(left.get_string() == right.get_string()),
            Operator::LessThan => Type::Bool(left.get_number() < right.get_number()),
            Operator::GreaterThan => Type::Bool(left.get_number() > right.get_number()),
            Operator::And => Type::Bool(left.get_bool() && right.get_bool()),
            Operator::Or => Type::Bool(left.get_bool() || right.get_bool()),
            _ => todo!(),
        })
    }
}

impl Prefix {
    fn eval(&self, scope: &mut HashMap<String, Expr>) -> Option<Type> {
        let value = self.values.eval(scope)?;
        Some(match self.operator {
            Operator::Sub => Type::Number(-value.get_number()),
            Operator::Not => Type::Bool(!value.get_bool()),
            _ => todo!(),
        })
    }
}
