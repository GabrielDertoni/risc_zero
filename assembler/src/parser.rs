use pest_derive::Parser;
use pest::Parser;
use pest::Span;
use pest::iterators::Pair;

use crate::ast::*;

use crate::utils::IterExt;

#[derive(Parser)]
#[grammar = "zasm.pest"]
pub struct ZASMParser;

pub type Error = pest::error::Error<Rule>;
type Result<T> = std::result::Result<T, Error>;

macro_rules! error {
    ($msg:expr, $span:expr) => {
        Err(pest::error::Error::new_from_span(pest::error::ErrorVariant::CustomError { message: $msg.into() }, $span))
    };
}

macro_rules! first {
    ($pair:expr) => {
        $pair.into_inner().next().unwrap()
    };
}

fn parse_label(pair: Pair<Rule>) -> Result<Label> {
    let ident = first!(pair);
    Ok(mk_lbl(ident.as_str(), ident.as_span()))
}

fn extract_str<'a>(s: &'a str) -> Result<&'a str> {
    let inner = &s[1..s.len()-1];
    Ok(inner)
}

fn extract_chr(s: &str) -> char {
    match &s[1..s.len()-1] {
        "\\n"  => '\n',
        "\\r"  => '\r',
        "\\t"  => '\t',
        "\\\\" => '\\',
        "\\'"  => '\'',
        "\\0"  => '\0',
        s      => s.chars().nth(0).unwrap(),
    }
}

fn parse_lbl(pair: Pair<Rule>) -> Result<Label> {
    let ident = first!(pair);
    Ok(mk_lbl(ident.as_str(), ident.as_span()))
}

fn parse_lit(pair: Pair<Rule>) -> Result<Lit> {
    let lit = first!(pair);
    let span = lit.as_span();
    let parsed = match lit.as_rule() {
        Rule::lbl       => Lit::Lbl(parse_lbl(lit)?),
        Rule::num       => match lit.as_str().parse() {
                            Ok(n)  => Lit::Num(Spanned::new(n, span)),
                            Err(e) => return error!(e.to_string(), span),
                          },
        Rule::str       => Lit::Str(Spanned::new(extract_str(lit.as_str())?, span)),
        Rule::chr       => Lit::Chr(Spanned::new(extract_chr(lit.as_str()), span)),
        Rule::lit_ref   => Lit::Ref(Box::new(parse_lit(lit.into_inner().next().unwrap())?)),
        Rule::lit_deref => parse_deref(lit)?,
        _               => unreachable!(),
    };

    Ok(parsed)
}

fn extract_arg_lbl<'a>(s: &'a str, span: Span<'a>) -> Result<Label<'a>> {
    let len = s.len();
    let inner = &s[1..len-1];
    if inner.chars().nth(0).unwrap() == '.' {
        Ok(mk_lbl(inner, span))
    } else {
        error!("only local labels can be argument labels", span)
    }
}

fn parse_arg(pair: Pair<Rule>) -> Result<Arg> {
    let arg = first!(pair);
    let span = arg.as_span();
    let mut it = arg.into_inner();
    let reg = it.next().unwrap();

    if let Some(imm) = it.next() {
        Ok(Arg::RegImm())
    }

    let parsed = match arg.as_rule() {
        Rule::lit     => Arg::Lit(parse_lit(arg)?),
        _             => unreachable!(),
    };
    Ok(parsed)
}

fn parse_inst(pair: Pair<Rule>) -> Result<Inst> {
    let mut inst_iter = pair.into_inner();
    let ident = inst_iter.next().unwrap();

    let op = match ident.as_str() {
        "hlt" => Op::Hlt,
        "add" => Op::Add,
        "mul" => Op::Mul,
        "cle" => Op::Cle,
        "ceq" => Op::Ceq,
        "jmp" => Op::Jmp,
        "beq" => Op::Beq,
        "cpy" => Op::Cpy,
        "put" => Op::Put,
        "ptn" => Op::Ptn,

        "psh" => Op::Psh,
        "pop" => Op::Pop,
        "cal" => Op::Cal,
        "ret" => Op::Ret,
        _     => return error!("not a valid instruction", ident.as_span()),
    };

    let span = ident.as_span();
    let arg_lst: Vec<_> = inst_iter.collect();

    if arg_lst.len() == op.nargs() {
        let args: Result<Vec<_>, _> = arg_lst.into_iter().map(parse_arg).collect();
        let args = args?;
        Ok(Inst { op, args, span })
    } else {
        error!(format!("expected {} argument(s) but got {}", op.nargs(), arg_lst.len()), span)
    }
}

fn parse_str(pair: Pair<Rule>) -> Result<Str> {
    let str = pair
        .into_inner()
        .next()
        .unwrap();

    if str.as_rule() == Rule::str {
        Ok(Spanned::new(extract_str(str.as_str())?, str.as_span()))
    } else {
        error!("expected a string", str.as_span())
    }
}

fn parse_ident(pair: Pair<Rule>) -> Result<Ident> {
    let ident = pair
        .into_inner()
        .next()
        .unwrap();

    if ident.as_rule() == Rule::ident {
        Ok(Ident {
            content: ident.as_str(),
            span: ident.as_span(),
        })
    } else {
        error!("expected an identifier", ident.as_span())
    }
}

fn parse_macro_args

fn parse_stmt(pair: Pair<Rule>) -> Result<Stmt> {
    let stmt = pair
        .into_inner()
        .next()
        .unwrap();

    match stmt.as_rule() {
        Rule::marker => {
            let marker = stmt
                .into_inner()
                .next()
                .unwrap();

            let span = marker.as_span();

            match marker.as_rule() {
                Rule::include => Ok(Stmt::Include(parse_str(marker)?)),
                Rule::define  => {
                    let [ident, lit] = marker.into_inner().take_collect_n().unwrap();
                    Ok(Stmt::Define(parse_ident(ident)?, parse_lit(lit)?))
                },
                Rule::macro_rule => {
                    let [ident, args, stmts] = marker.into_inner().take_collect_n().unwrap();
                    Ok(Stmt::Macro(Macro {
                        name: parse_ident(ident)?,
                        args: parse_macro_args(args)?,
                        stmts: parse_stmts(stmts)?,
                        span,
                    }))
                },
                _  => unreachable!(),
            }
        },
        Rule::label  => Ok(Stmt::Label(parse_label(stmt)?)),
        Rule::inst   => Ok(Stmt::Inst(parse_inst(stmt)?)),
        Rule::lit    => Ok(Stmt::Lit(parse_lit(stmt)?)),
        _            => unreachable!(),
    }
}

pub fn parse_zasm(program: &str) -> Result<Prog> {
    let stmts = ZASMParser::parse(Rule::zasm, program)?.next().unwrap();
    let span = stmts.as_span();
    let stmts = stmts.into_inner()
        .map(parse_stmt)
        .collect::<Result<Vec<_>>>()?;

    Ok(Prog {
        stmts,
        span,
    })
}
