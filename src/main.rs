use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    Put,
    Integer(u8),
    Identifier(u32),
    Assign,
    Semicolon,
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
            println!("{sub_word}");
            if i > 0 {
                tokens.push(Token::Semicolon);
            }

            if sub_word.is_empty() {
                continue;
            }
            if match_put(sub_word) {
                tokens.push(Token::Put);
            } else if let (true, v) = match_integer(sub_word) {
                tokens.push(Token::Integer(v));
            } else if let (true, v) = match_identifier(sub_word, &mut id_map) {
                tokens.push(Token::Identifier(v));
            } else if sub_word == "=" {
                tokens.push(Token::Assign);
            } else if sub_word == ";" {
                tokens.push(Token::Semicolon);
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
enum Statement {
    Expr(Expr),
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
    if let (true, expr) = parse_expr(tokens) {
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

fn compile_prgram(tokens: &mut Vec<Token>, var_count: usize) -> String {
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

fn main() {
    let input = std::fs::read_to_string("main.galaxy").unwrap();

    let (mut tokens, var_count) = lex(&input).clone();

    for token in &tokens {
        println!("{token:#?}");
    }

    println!("{}", compile_prgram(&mut tokens, var_count));
}
