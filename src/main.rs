use std::{collections::HashMap, os::linux::raw::stat};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    Put,
    Integer(u8),
    Identifier(u32),
    Assign,
    Semicolon,
    While,
    LeftBraces,
    RightBraces,
    Dec,
    AddAssign,
    SubAssign,
    None,
}

fn match_put(word: &str) -> bool {
    word == "put"
}

fn match_integer(word: &str) -> (bool, u8) {
    for c in word.chars() {
        match c {
            '0'..='9' => {},
            _ => return (false, 0),
        }
    }

    (true, word.parse().unwrap())
}

fn match_identifier(word: &str, id_map: &mut HashMap<String, u32>) -> (bool, u32) {
    for c in word.chars() {
        match c {
            '0'..='9' => {},
            'a'..='z' => {},
            'A'..='Z' => {},
            _ => return (false, 0),
        }
    }
    if !id_map.contains_key(word) {
        id_map.insert(word.to_string(), id_map.len() as u32);
    }

    (true, id_map[word])
}

fn lex(input: &str) -> (Vec<Token>, usize) {
    let mut tokens = Vec::new();

    let mut id_map = HashMap::<String, u32>::new();

    for word in input.split(&[' ', '\n']) {
        if word.is_empty() {
            continue;
        }

        let sub_words = word.split(';').collect::<Vec<_>>();

        for (i, &sub_word) in sub_words.iter().enumerate() {
            //println!("{sub_word}");
            if i > 0 {
                tokens.push(Token::Semicolon);
            }

            if sub_word.is_empty() {
                continue;
            }
            if match_put(sub_word) {
                tokens.push(Token::Put);
            } else if sub_word == "while" {
                tokens.push(Token::While);
            } else if sub_word == "+=" {
                tokens.push(Token::AddAssign);
            } else if sub_word == "-=" {
                tokens.push(Token::SubAssign);
            } else if sub_word == "dec" {
                tokens.push(Token::Dec);
            } else if let (true, v) = match_integer(sub_word) {
                tokens.push(Token::Integer(v));
            } else if let (true, v) = match_identifier(sub_word, &mut id_map) {
                tokens.push(Token::Identifier(v));
            } else if sub_word == "=" {
                tokens.push(Token::Assign);
            } else if sub_word == ";" {
                tokens.push(Token::Semicolon);
            } else if sub_word == "{" {
                tokens.push(Token::LeftBraces);
            } else if sub_word == "}" {
                tokens.push(Token::RightBraces);
            }
        }
    }

    (tokens, id_map.len())
}

fn swallow_tokens(tokens: &mut Vec<Token>, token: &[Token]) -> (bool, Vec<Token>) {
    let mut output = Vec::<Token>::new();
    let mut copied = tokens.clone();
    for &to_swallow in token {
        if let Some(&t) = copied.first() {
            if std::mem::discriminant(&t) != std::mem::discriminant(&to_swallow) {
                return (false, output);
            } else {
                copied.remove(0);
                output.push(t);
            }
        } else {
            return (false, output);
        }
    }

    *tokens = copied;

    (true, output)
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Block {
    Statements(u32, Vec<Statement>),
    Failed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Statement {
    Expr(Expr),
    While(u32, Box<Vec<Statement>>),
    Dec(u32),
    AddAssign(u32, Expr),
    SubAssign(u32, Expr),
    Failed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Assignment(u32, Box<Expr>),
    Integer(u8),
    Identifier(u32),
    Put(Box<Expr>),
    Failed,
}

fn parse_statement(tokens: &mut Vec<Token>) -> (bool, Statement) {
    if let (true, b) = parse_block(tokens) {
        match b {
            Block::Statements(id, s) => {
                return (true, Statement::While(id, Box::new(s)));
            }
            Block::Failed => {},
        }
    }
    else if let (true, toks) = swallow_tokens(tokens, &[Token::Dec, Token::Identifier(0)]) {
        if let (true, _) = swallow_tokens(tokens, &[Token::Semicolon]) {
            let id = match toks[1] {
                Token::Identifier(x) => x,
                _ => unreachable!(),
            };

            return (true, Statement::Dec(id));
        } else {
            println!("expected semicolon");
            return (false, Statement::Failed);
        }
    } else if let (true, toks) = swallow_tokens(tokens, &[Token::Identifier(0), Token::AddAssign]) {
        if let (true, e) = parse_expr(tokens) {
            if let (true, _) = swallow_tokens(tokens, &[Token::Semicolon]) {
                let id = match toks[0] {
                    Token::Identifier(x) => x,
                    _ => unreachable!(),
                };

                return (true, Statement::AddAssign(id, e));
            }
        }
        else {
            println!("invalid add assign");
            return (false, Statement::Failed);
        }
    } else if let (true, toks) = swallow_tokens(tokens, &[Token::Identifier(0), Token::SubAssign]) {
        if let (true, e) = parse_expr(tokens) {
            if let (true, _) = swallow_tokens(tokens, &[Token::Semicolon]) {
                let id = match toks[0] {
                    Token::Identifier(x) => x,
                    _ => unreachable!(),
                };

                return (true, Statement::SubAssign(id, e));
            }
        }
        else {
            println!("invalid sub assign");
            return (false, Statement::Failed);
        }
    } else if let (true, expr) = parse_expr(tokens) {
        if let (true, _) = swallow_tokens(tokens, &[Token::Semicolon]) {
            return (true, Statement::Expr(expr));
        }
        else {
            println!("expected semicolon");
            return (false, Statement::Failed);
        }
    }

    (false, Statement::Failed)
}

fn parse_block(tokens: &mut Vec<Token>) -> (bool, Block) {
    if let (true, toks) = swallow_tokens(tokens, &[Token::While, Token::Identifier(0)]) {
        if let (true, _toks2) = swallow_tokens(tokens, &[Token::LeftBraces]) {
            let id = match toks[1] {
                Token::Identifier(x) => x,
                _ => unreachable!(),
            };

            let mut statements = Vec::<Statement>::new();

            while let (false, _) = swallow_tokens(tokens, &[Token::RightBraces]) {
                let (p, s) = parse_statement(tokens);
                statements.push(s);
            }

            return (true, Block::Statements(id, statements));
        }
    }

    (false, Block::Failed)
}

fn parse_expr(tokens: &mut Vec<Token>) -> (bool, Expr) {
    if let (true, toks) = swallow_tokens(tokens, &[Token::Identifier(0), Token::Assign]) {
        if let (true, expr) = parse_expr(tokens) {
            let id = match toks[0] {
                Token::Identifier(x) => x,
                _ => unreachable!()
            };
            return (true, Expr::Assignment(id, Box::new(expr)));
        }
    } else if let (true, toks) = swallow_tokens(tokens, &[Token::Identifier(0)]) {
        let id = match toks[0] {
            Token::Identifier(x) => x,
            _ => unreachable!(),
        };
        return (true, Expr::Identifier(id));
    } else if let (true, toks) = swallow_tokens(tokens, &[Token::Integer(0)]) {
        let v = match toks[0] {
            Token::Integer(x) => x,
            _ => unreachable!(),
        };
        return (true, Expr::Integer(v));
    } else if let (true, toks) = swallow_tokens(tokens, &[Token::Put]) {
        if let (true, expr) = parse_expr(tokens) {
            return (true, Expr::Put(Box::new(expr)));
        }
    }

    (false, Expr::Failed)
}

fn compile_statement(statement: &Statement, var_count: usize) -> String {
    let mut out = String::new();

    match statement {
        Statement::Expr(e) => {
            out += &compile_expr(e, var_count);
        },
        Statement::While(i, b) => {
            out += &">".repeat(*i as usize);
            out += "[";
            out += &"<".repeat(*i as usize);
            for s in *b.clone() {
                out += &compile_statement(&s, var_count);
            }
            out += &">".repeat(*i as usize);
            out += "]";
            out += &"<".repeat(*i as usize);
        },
        Statement::Dec(i) => {
            out += &">".repeat(*i as usize);
            out += "-";
            out += &"<".repeat(*i as usize);
        },

        Statement::AddAssign(i, e) => {
            match e {
                Expr::Integer(x) => {
                    out += &">".repeat(*i as usize);
                    out += &"+".repeat(*x as usize);
                    out += &"<".repeat(*i as usize);
                },
                _ => todo!(),
            }
        },

        Statement::SubAssign(i, e) => {
            match e {
                Expr::Integer(x) => {
                    out += &">".repeat(*i as usize);
                    out += &"-".repeat(*x as usize);
                    out += &"<".repeat(*i as usize);
                },
                _ => todo!(),
            }
        },

        Statement::Failed => {},
    };

    out
}

fn compile_expr(expr: &Expr, var_count: usize) -> String {
    let mut out = String::new();
    match expr {
        Expr::Assignment(i, v) => {
            out += &">".repeat(*i as usize);
            out += "[-]";
            match **v {
                Expr::Integer(x) => {
                    out += &"+".repeat(x as usize);
                },

                Expr::Identifier(i2) => {
                    let (forward, backward) = if i2 > *i {
                        ('>', '<')
                    } else {
                        ('<', '>')
                    };

                    let delta = (i2 as i32 - *i as i32).abs() as usize;
                    let last_delta = var_count as u32 - *i;
                    out += &forward.to_string().repeat(delta);
                    out += "[";
                    out += "-";
                    out += &backward.to_string().repeat(delta);
                    out += "+";
                    out += &">".repeat(last_delta as usize);
                    out += "+";
                    out += &"<".repeat(last_delta as usize);
                    out += &forward.to_string().repeat(delta);
                    out += "]";
                    out += &backward.to_string().repeat(delta);
                    out += &">".repeat(last_delta as usize);
                    out += "[";
                    out += "-";
                    out += &"<".repeat(last_delta as usize);
                    out += &forward.to_string().repeat(delta);
                    out += "+";
                    out += &backward.to_string().repeat(delta);
                    out += &">".repeat(last_delta as usize);
                    out += "]";
                    out += &"<".repeat(last_delta as usize);
                },

                _ => todo!(),
            }
            out += &"<".repeat(*i as usize);
        },
        Expr::Put(v) => {
            match **v {
                Expr::Integer(x) => {
                    out += &">".repeat(var_count);
                    out += &"+".repeat(x as usize);
                    out += ".";
                    out += "[-]";
                    out += &"<".repeat(var_count);
                },
                Expr::Identifier(i) => {
                    out += &">".repeat(i as usize);
                    out += ".";
                    out += &"<".repeat(i as usize);
                }
                _ => todo!(),
            }
        }
        Expr::Integer(_) => {},
        Expr::Failed => {},
        _ => todo!(),
    }
    out
}

fn compile_program(tokens: &mut Vec<Token>, var_count: usize) -> String {
    let mut out = String::new();
    let mut statements = Vec::new();
    while let (true, s) = parse_statement(tokens) {
        println!("{s:#?}");
        statements.push(s);
    }

    for statement in statements {
        out += &compile_statement(&statement, var_count);
    }

    out
}

fn even_pair(program: &str, pair: (char, char)) -> String {
    let mut result_chars: Vec<char> = Vec::with_capacity(program.len()); // Pre-allocate capacity
    for c in program.chars() {
        if let Some(&last_char) = result_chars.last() {
            if (c == pair.0 && last_char == pair.1) || (c == pair.1 && last_char == pair.0) {
                result_chars.pop();
            } else {
                result_chars.push(c);
            }
        } else {
            result_chars.push(c);
        }
    }

    result_chars.into_iter().collect()
}

fn optimize(program: &str) -> String {
    let mut optimized = program.to_string();
    optimized = even_pair(&optimized, ('>', '<'));
    optimized = even_pair(&optimized, ('+', '-'));
    optimized
}

fn main() {
    let input = std::fs::read_to_string("main.galaxy").unwrap();
    println!("{input}");

    let (mut tokens, var_count) = lex(&input).clone();

    for token in &tokens {
        println!("{token:#?}");
    }

    let program = compile_program(&mut tokens, var_count);
    // println!("{program}");
    println!("{}", &optimize(&program));
}
