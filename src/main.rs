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

fn lex(input: &str) -> Vec<Token> {
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

    tokens
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
    }

    (false, Expr::Failed)
}

fn main() {
    let input = std::fs::read_to_string("main.galaxy").unwrap();

    let mut tokens = lex(&input).clone();

    for token in &tokens {
        println!("{token:#?}");
    }

    let (p, s) = parse_statement(&mut tokens);

    println!("{s:#?}");
}
