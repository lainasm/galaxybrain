use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    Put,
    Integer(u8),
    Identifier(u32),
    Assign,
    Semicolon,
    While,
    If,
    LeftBraces,
    RightBraces,
    Dec,
    AddAssign,
    SubAssign,
    Function(u32),
    DeclArray(u32),
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

fn match_identifier(word: &str, id_map: &mut HashMap<String, u32>, next_id: &mut u32) -> (bool, u32) {
    for c in word.chars() {
        match c {
            '0'..='9' => {},
            'a'..='z' => {},
            'A'..='Z' => {},
            _ => return (false, 0),
        }
    }
    if !id_map.contains_key(word) {
        id_map.insert(word.to_string(), *next_id);
        *next_id += 1;
    }

    (true, id_map[word])
}

fn match_function(word: &str, function_map: &mut HashMap<String, u32>) -> (bool, u32) {
    let mut function = String::new();
    let mut has_parenthesis = false;
    for c in word.chars() {
        match c {
            '0'..='9' => function.push(c),
            'a'..='z' => function.push(c),
            'A'..='Z' => function.push(c),
            '(' => {has_parenthesis = true; break;},
            _ => return (false, 0),
        }
    }

    if !has_parenthesis {
        return (false, 0);
    }

    if !function_map.contains_key(&function) {
        function_map.insert(function.clone(), function_map.len() as u32);
    }

    (true, function_map[&function])
}

fn match_array(word: &str, identifier_map: &mut HashMap<String, u32>, next_id: &mut u32) -> (bool, u32) {
    let mut array = String::new();
    let mut has_brackets = false;
    for c in word.chars() {
        match c {
            '0'..='9' => array.push(c),
            'a'..='z' => array.push(c),
            'A'..='Z' => array.push(c),
            '[' => {has_brackets = true; break;},
            _ => return (false, 0),
        }
    }

    if !has_brackets {
        return (false, 0);
    }

    let beg = word.find('[').unwrap() + 1;
    let end = word.find(']').unwrap();
    let arr_len: u32 = word[beg..end].parse().unwrap();
    println!("creating array of length {arr_len}");

    if !identifier_map.contains_key(&array) {
        identifier_map.insert(array.clone(), *next_id);
        *next_id += arr_len + 2;
    }

    (true, identifier_map[&array])
}

fn lex(input: &str) -> (Vec<Token>, usize) {
    let mut tokens = Vec::new();

    let mut id_map = HashMap::<String, u32>::new();
    let mut function_map = HashMap::<String, u32>::new();
    let mut next_id = 0;

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
            } else if let (true, v) = match_array(sub_word, &mut id_map, &mut next_id) {
                tokens.push(Token::DeclArray(v));
            } else if sub_word == "if" {
                tokens.push(Token::If);
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
            } else if let (true, v) = match_function(sub_word, &mut function_map) {
                tokens.push(Token::Function(v));
            } else if let (true, v) = match_identifier(sub_word, &mut id_map, &mut next_id) {
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

    (tokens, next_id as usize)
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
enum WhileBlock {
    Statements(u32, Vec<Statement>),
    Failed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum IfBlock {
    Statements(u32, Vec<Statement>),
    Failed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum FunctionBlock {
    Statements(u32, Vec<Statement>),
    Failed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Statement {
    Expr(Expr),
    While(u32, Box<Vec<Statement>>),
    If(u32, Box<Vec<Statement>>),
    FunctionDef(u32, Box<Vec<Statement>>),
    FunctionCall(u32),
    Dec(u32),
    AddAssign(u32, Expr),
    SubAssign(u32, Expr),
    ArrayDecl(u32),
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
    if let (true, b) = parse_while_block(tokens) {
        match b {
            WhileBlock::Statements(id, s) => {
                return (true, Statement::While(id, Box::new(s)));
            }
            WhileBlock::Failed => {},
        }
    } else if let (true, b) = parse_if_block(tokens) {
        match b {
            IfBlock::Statements(id, s) => {
                return (true, Statement::If(id, Box::new(s)));
            }
            IfBlock::Failed => {},
        }
    } else if let (true, b) = parse_function_block(tokens) {
        match b {
            FunctionBlock::Statements(id, s) => {
                return (true, Statement::FunctionDef(id, Box::new(s)));
            },
            FunctionBlock::Failed => {},
        }
    } else if let (true, toks) = swallow_tokens(tokens, &[Token::DeclArray(0)]) {
        if let (true, _) = swallow_tokens(tokens, &[Token::Semicolon]) {
            let id = match toks[0] {
                Token::DeclArray(x) => x,
                _ => unreachable!(),
            };

            return (true, Statement::ArrayDecl(id));
        } else {
            println!("expected semicolon");
        }
    } else if let (true, toks) = swallow_tokens(tokens, &[Token::Function(0)]) {
        if let (true, _) = swallow_tokens(tokens, &[Token::Semicolon]) {
            let id = match toks[0] {
                Token::Function(x) => x,
                _ => unreachable!(),
            };

            return (true, Statement::FunctionCall(id));
        } else {
            println!("expected semicolon");
        }
    } else if let (true, toks) = swallow_tokens(tokens, &[Token::Dec, Token::Identifier(0)]) {
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

fn parse_while_block(tokens: &mut Vec<Token>) -> (bool, WhileBlock) {
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

            return (true, WhileBlock::Statements(id, statements));
        }
    }

    (false, WhileBlock::Failed)
}

fn parse_if_block(tokens: &mut Vec<Token>) -> (bool, IfBlock) {
    if let (true, toks) = swallow_tokens(tokens, &[Token::If, Token::Identifier(0)]) {
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

            return (true, IfBlock::Statements(id, statements));
        }
    }

    (false, IfBlock::Failed)
}

fn parse_function_block(tokens: &mut Vec<Token>) -> (bool, FunctionBlock) {
    if let (true, toks) = swallow_tokens(tokens, &[Token::Function(0), Token::LeftBraces]) {
        let id = match toks[0] {
            Token::Function(x) => x,
            _ => unreachable!(),
        };

        let mut statements = Vec::<Statement>::new();

        while let (false, _) = swallow_tokens(tokens, &[Token::RightBraces]) {
            let (p, s) = parse_statement(tokens);
            statements.push(s);
        }

        return (true, FunctionBlock::Statements(id, statements));
    }

    (false, FunctionBlock::Failed)
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

fn compile_statement(statement: &Statement, var_count: usize, function_definitions: &HashMap<u32, Vec<Statement>>) -> String {
    let mut out = String::new();

    match statement {
        Statement::ArrayDecl(i) => {

        },

        Statement::FunctionDef(i, s) => {

        },
        Statement::FunctionCall(i) => {
            for s in &function_definitions[i] {
                out += &compile_statement(s, var_count, function_definitions);
            }
        },
        Statement::Expr(e) => {
            out += &compile_expr(e, var_count);
        },
        Statement::While(i, b) => {
            out += &">".repeat(*i as usize);
            out += "[";
            out += &"<".repeat(*i as usize);
            for s in *b.clone() {
                out += &compile_statement(&s, var_count, function_definitions);
            }
            out += &">".repeat(*i as usize);
            out += "]";
            out += &"<".repeat(*i as usize);
        },
        Statement::If(i, b) => {
            let delta = var_count as u32 - *i;
            out += &">".repeat(*i as usize);
            out += "[";
            out += "-";
            out += &">".repeat(delta as usize);
            out += "+";
            out += ">";
            out += "+";
            out += "<";
            out += &"<".repeat(delta as usize);
            out += "]";
            out += &">".repeat(delta as usize);
            out += ">";
            out += "[";
            out += "-";
            out += "<";
            out += &"<".repeat(delta as usize);
            out += "+";
            out += &">".repeat(delta as usize);
            out += ">";
            out += "]";
            out += "<";
            out += "[";
            out += &"<".repeat(delta as usize);
            out += &"<".repeat(*i as usize);
            for s in *b.clone() {
                out += &compile_statement(&s, var_count + 1, function_definitions);
            }
            out += &">".repeat(var_count as usize);
            out += "[";
            out += "-";
            out += "]";
            out += "]";
            out += &"<".repeat(var_count as usize);
        }
        Statement::Dec(i) => {
            out += &">".repeat(*i as usize);
            out += "-";
            out += &"<".repeat(*i as usize);
        },

        Statement::AddAssign(i, e) => {
            out += &">".repeat(*i as usize);

            match e {
                Expr::Integer(x) => {
                    out += &"+".repeat(*x as usize);
                },

                Expr::Identifier(i2) => {
                    let (forward, backward) = if *i2 > *i {
                        ('>', '<')
                    } else {
                        ('<', '>')
                    };

                    let delta = (*i2 as i32 - *i as i32).abs() as usize;
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

        Statement::SubAssign(i, e) => {
            out += &">".repeat(*i as usize);

            match e {
                Expr::Integer(x) => {
                    out += &"-".repeat(*x as usize);
                },

                Expr::Identifier(i2) => {
                    let (forward, backward) = if *i2 > *i {
                        ('>', '<')
                    } else {
                        ('<', '>')
                    };

                    let delta = (*i2 as i32 - *i as i32).abs() as usize;
                    let last_delta = var_count as u32 - *i;
                    out += &forward.to_string().repeat(delta);
                    out += "[";
                    out += "-";
                    out += &backward.to_string().repeat(delta);
                    out += "-";
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

fn compile_program(tokens: &mut Vec<Token>, var_count: usize, function_definitions: &mut HashMap<u32, Vec<Statement>>) -> String {
    let mut out = String::new();
    let mut statements = Vec::new();
    while let (true, s) = parse_statement(tokens) {
        println!("{s:#?}");
        if let Statement::FunctionDef(i, b) = s.clone() {
            function_definitions.insert(i, *b.clone());
        }
        statements.push(s);
    }

    for statement in statements {
        out += &compile_statement(&statement, var_count, function_definitions);
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
    let mut function_definitions = HashMap::<u32, Vec<Statement>>::new();

    for token in &tokens {
        println!("{token:#?}");
    }

    let program = compile_program(&mut tokens, var_count, &mut function_definitions);
    // println!("{program}");
    println!("{}", &optimize(&program));
}
