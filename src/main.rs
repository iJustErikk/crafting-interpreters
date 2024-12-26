use std::{env, fmt, fs, io::{self, stdin, BufRead, BufReader}, process::exit, str::Chars};

use itertools::{multipeek, peek_nth, Itertools, MultiPeek, PeekNth};

use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(Debug)]
#[derive(Clone)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
    Comment,
    Whitespace
}

#[derive(Debug)]
pub enum ErrorType {
    InvalidToken,
    UnterminatedString,
    UnterminatedMultilineComment
}

struct Token {
    token_type: TokenType,
    lexeme: String,
    // literal: SomeEnumLol
    // line: u32,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {}", self.token_type, self.lexeme)
    }
}

struct Scanner<'a> {
    chars:  PeekNth<MultiPeek<Chars<'a>>>,
    concluded: bool,
    line_num: u32,
    current: String,
}

enum RLoxError {
    LexicalError(u32, String, ErrorType),
}

type RLoxResult<T> = Result<T, RLoxError>;

static KEYWORDS: Lazy<HashMap<&'static str, TokenType>> = Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert("and", TokenType::And);
    map.insert("class", TokenType::Class);
    map.insert("else", TokenType::Else);
    map.insert("false", TokenType::False);
    map.insert("for", TokenType::For);
    map.insert("fun", TokenType::Fun);
    map.insert("if", TokenType::If);
    map.insert("nil", TokenType::Nil);
    map.insert("or", TokenType::Or);
    map.insert("print", TokenType::Print);
    map.insert("return", TokenType::Return);
    map.insert("super", TokenType::Super);
    map.insert("this", TokenType::This);
    map.insert("true", TokenType::True);
    map.insert("var", TokenType::Var);
    map.insert("while", TokenType::While);
    map
});


impl<'a> Scanner<'a> {

    // REFLECT: we only really lookahead 2 characters but this was just so clean to do
    // peeks len(expected) chars
    // consumes them if they fully match
    fn match_str(&mut self, expected: &str) -> bool {
        // apparently reduce doesn't let you return a different type than the iter type
        // this is a truly beautiful loop
        let matched_len = expected.chars().enumerate().fold(0, |acc, (ind, expected)| {
            acc + if self.chars.peek_nth(ind).map(|got| {got == &expected}).unwrap_or(false) {
                1
            } else {
                0
            }
        });
        if matched_len != expected.len() {
            return false
        }
        // i am in love with by_ref
        self.current.extend(self.chars.by_ref().take(expected.len()));
        return true
    }
    fn next_token(&mut self, token_type: TokenType) -> Token {
        let mut lexeme = String::new();
        std::mem::swap(&mut self.current, &mut lexeme);
        Token { token_type: token_type, lexeme: lexeme}
    }
    fn swap_current(&mut self) -> String {
        let mut to_swap = String::new();
        std::mem::swap(&mut self.current, &mut to_swap);
        to_swap
    }
    fn keyword_or_identifier(&mut self) -> Token {
        let lexeme = self.swap_current();
        // why does as_str work but not &lexeme? bug?
        let token_type: TokenType = match KEYWORDS.get(lexeme.as_str()) {
            // cloning an enum
            Some(token_type) => token_type.clone(),
            None => TokenType::Identifier
        };
        Token { token_type: token_type, lexeme: lexeme }
    }
    // gobble into current until predicate is not true
    // includes last (negative)
    fn gobble_until<P>(&mut self, p: P) where P: FnMut(&char) -> bool, {
        // sadly no take_while_ref_inclusive
        self.current.extend(self.chars.by_ref().take_while_inclusive(p));
    }
    // same as above, but does not consume last (negative)
    fn gobble_until_peek<P>(&mut self, p: P) where P: FnMut(&char) -> bool, {
        self.current.extend(self.chars.peeking_take_while(p));
    }
    fn next(&mut self) -> Option<char> {
        self.chars.next().map(|c| {
            self.current.push(c);
            c
        })
    }
    // used for multiline tokens (strings, multiline comments)
    fn gobble_until_inc_newline(&mut self, to_match: &str) {
        let mut lines_encountered = 0;
        let mut window = String::new();
        self.gobble_until(|x| {
            if *x == '\n' {
                lines_encountered += 1;
            }
            window.push(*x);
            if window.len() > to_match.len() {
                window.remove(0);
            }
            &window != to_match
        });
        self.line_num += lines_encountered;
    }
    fn get_next(&mut self) -> RLoxResult<Option<Token>> {
        let next = self.next();
        match next {
            Some(c) => {
                match c {
                '(' => Ok(Some(self.next_token(TokenType::LeftParen))),
                ')' => Ok(Some(self.next_token(TokenType::RightParen))),
                '{' => Ok(Some(self.next_token(TokenType::LeftBrace))),
                '}' => Ok(Some(self.next_token(TokenType::RightBrace))),
                ',' => Ok(Some(self.next_token(TokenType::Comma))),
                '.' => Ok(Some(self.next_token(TokenType::Dot))),
                '-' => Ok(Some(self.next_token(TokenType::Minus))),
                '+' => Ok(Some(self.next_token(TokenType::Plus))),
                ';' => Ok(Some(self.next_token(TokenType::Semicolon))),
                '*' => Ok(Some(self.next_token(TokenType::Star))),
                '!' => Ok(Some(if self.match_str("=") {self.next_token(TokenType::BangEqual)} else {self.next_token(TokenType::Bang)})),
                '=' => Ok(Some(if self.match_str("=") {self.next_token(TokenType::EqualEqual)} else {self.next_token(TokenType::Equal)})),
                '<' => Ok(Some(if self.match_str("=") {self.next_token(TokenType::LessEqual)} else {self.next_token(TokenType::Less)})),
                '>' => Ok(Some(if self.match_str("=") {self.next_token(TokenType::GreaterEqual)} else {self.next_token(TokenType::Greater)})),
                '/' => {
                    if self.match_str("/") {
                        // gobble until new line
                        self.gobble_until(|x| *x != '\n');
                        Ok(Some(self.next_token(TokenType::Comment)))
                    } else if self.match_str("*") {
                        self.gobble_until_inc_newline("*/");
                        // handle case where multiline comment never terminated
                        if !self.current.ends_with("*/") {
                            return Err(RLoxError::LexicalError(self.line_num, self.swap_current(), ErrorType::UnterminatedMultilineComment))
                        }
                        Ok(Some(self.next_token(TokenType::Comment)))
                    } else {
                        Ok(Some(self.next_token(TokenType::Slash)))
                    }
                }
                ' ' | '\r' | '\t' => {
                    Ok(Some(self.next_token(TokenType::Whitespace)))
                }
                '\n' => {
                    self.line_num += 1;
                    Ok(Some(self.next_token(TokenType::Whitespace)))
                }
                '"' => {
                    // strings can span lines!
                    // string lexeme will contain the quotes
                    self.gobble_until_inc_newline("\"");
                    // handle case where string never terminated
                    if !self.current.ends_with("\"") {
                        return Err(RLoxError::LexicalError(self.line_num, self.swap_current(), ErrorType::UnterminatedString))
                    }
                    Ok(Some(self.next_token(TokenType::String)))
                }
                other => {
                    if other.is_alphabetic() {
                        self.gobble_until_peek(|x| {x.is_alphanumeric()});
                        Ok(Some(self.keyword_or_identifier()))
                    } else if other.is_digit(10) {
                        // handle number literals
                        self.gobble_until_peek(|x| x.is_digit(10));
                        if self.chars.peek().map(|c| *c == '.').unwrap_or(false) {
                            self.next();
                            self.gobble_until_peek(|x| x.is_digit(10));
                        }
                        Ok(Some(self.next_token(TokenType::Number)))
                    } else {
                        Err(RLoxError::LexicalError(self.line_num, self.swap_current(), ErrorType::InvalidToken))
                    }
                    
                }
                }
            },
            None => Ok(None)
        }
    }
    fn new(src: &'a str) -> Scanner<'a>  {
        let chars = src.chars();
        Scanner { chars: peek_nth(multipeek(chars)), concluded: false, line_num: 1, current: String::new() }
    }
}

impl Iterator for Scanner<'_> {
    type Item = RLoxResult<Token>;
    fn next(&mut self) -> Option<RLoxResult<Token>> {
        let next_token = self.get_next();
        match next_token {
            Err(e) => Some(Err(e)),
            Ok(Some(token)) => Some(Ok(token)),
            Ok(None) => {
                if self.concluded {
                    None
                } else {
                    self.concluded = true;
                    Some(Ok(Token{token_type: TokenType::Eof, lexeme: String::new()}))
                }
            }
        }
    }
}

fn main() -> io::Result<()>{
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        println!("Usage rlox [script]");
        exit(64)
    } 
    // handle cargo run
    else if args.len() == 1 && args[0] != "target/debug/crafting-interpreters" {
        run_file(&args[0])?;
    } else {
        run_prompt()?;
    }
    Ok(())
}

fn run_prompt() -> io::Result<()> {
    let buf = BufReader::new(stdin());
    for line in buf.split(b'\n') {
        match line {
            Ok(l) => {
                let l = String::from_utf8_lossy(&l);
                run(l.to_string());
            }
            Err(e) => {
                return Err(e)
            }
        }
    }
    Ok(())
}

fn run_file(name: &str) -> io::Result<()> {
    run(fs::read_to_string(name)?);
    Ok(())
}

fn run(src: String) {
    for line in src.split("\n") {
        for token in Scanner::new(line).into_iter() {
            match token {
                Err(err) => report(err),
                Ok(token) => match token.token_type {
                    TokenType::Whitespace => (),
                    _ => println!("{}", token)
                },
            }
        }
    }
}

fn report(err: RLoxError) {
    match err {
        RLoxError::LexicalError(line, context, error) => {
            println!("[line {}] Error, Context: {:#?}, {}", line, error, context);
        }
    }
}