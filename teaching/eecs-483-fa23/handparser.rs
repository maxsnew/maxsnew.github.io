use std::iter::Peekable;

#[derive(Debug)]
enum Token {
    Num(i64),
    Plus,
    LParen,
    RParen,
}

#[derive(Debug)]
enum Ast {
    Num(i64),
    Plus(Box<Ast>, Box<Ast>),
}

/* Our LL(1) grammar
 * T  ⟼ S$
 *
 * S  ⟼ ES'
 * 
 * S’ ⟼ e
 * S’ ⟼ + S
 * 
 * E  ⟼ $
 * */

struct ParserState<'t, I>
where
    I: Iterator<Item = &'t Token>,
{
    toks: Peekable<I>,
}

impl<'t, I> ParserState<'t, I>
where
    I: Iterator<Item = &'t Token>,
{
    /* T
     * on number | (
     * transition to S $
     * */
    fn parse_t(&mut self) -> Result<Ast, String> {
        println!("State: T \t\tLookahead: {:?}", self.toks.peek());
        match self.toks.peek() {
            Some(Token::Num(_)) | Some(Token::LParen) => {
                let ans = self.parse_s()?;
                match self.toks.next() {
                    None => {
                        Ok(ans)
                    },
                    Some(c) => Err(format!("expected EOF, found {:?}", c)),
                }
            }
            _ => Err(String::from("expected '(' or number")),
        }
    }

    /* S
     * on number | (
     * transition to E S'
     */
    fn parse_s(&mut self) -> Result<Ast, String> {
        println!("State: S \t\tLookahead: {:?}", self.toks.peek());
        match self.toks.peek() {
            Some(Token::Num(_)) | Some(Token::LParen) => {
                let prefix = self.parse_e()?;
                self.parse_s_prime(prefix)
            }
            _ => Err(String::from("expected '(' or number")),
        }
    }

    /* S'
     * on + transition to + S
     * on ) or EOF transition to ε
     * */
    fn parse_s_prime(&mut self, prefix: Ast) -> Result<Ast, String> {
        println!("State: S'\t\tLookahead: {:?}", self.toks.peek());
        match self.toks.peek() {
            Some(Token::Plus) => {
                let _ = self.toks.next();
                Ok(Ast::Plus(Box::new(prefix), Box::new(self.parse_s()?)))
            }
            Some(Token::RParen) | None => Ok(prefix),
            _ => Err(String::from("expected '+', ')' or EOF")),
        }
    }

    /* E
     * on number transition to number
     * on ( transition to ( S )
     * */
    fn parse_e(&mut self) -> Result<Ast, String> {
        println!("State: E \t\tLookahead: {:?}", self.toks.peek());
        match self.toks.next() {
            Some(Token::Num(n)) => Ok(Ast::Num(*n)),
            Some(Token::LParen) => {
                let ans = self.parse_s()?;
                match self.toks.next() {
                    Some(Token::RParen) => Ok(ans),
                    _ => Err(String::from("expected ')'")),
                }
            }
            _ => Err(String::from("expected '(' or number")),
        }
    }
}

fn parse<'t, T: Iterator<Item = &'t Token>>(toks: &mut T) -> Result<Ast, String> {
    let mut ps = ParserState {
        toks: toks.peekable(),
    };
    ps.parse_t()
}

fn main() {
    use Token::*;
    // (1 + 2 + (3 + 4)) + 5
    let test_input = vec![
        LParen,
        Num(1),
        Plus,
        Num(2),
        Plus,
        LParen,
        Num(3),
        Plus,
        Num(4),
        RParen,
        RParen,
        Plus,
        Num(5),
    ];
    println!("{:?}", test_input);
    println!(
        "Parsing (1 + 2 + (3 + 4)) + 5\n{:?}",
        parse(&mut test_input.iter())
    );
}
