use std::collections::HashMap;

use pest_derive::Parser;
use pest::Parser;
use pest::Span;
use pest::iterators::{ Pair, Pairs };

use crate::ast::*;

use crate::utils::IterExt;

#[derive(Parser)]
#[grammar = "zasm.pest"]
pub struct ZASMParser;

// TODO: Find a better way of exporting this rule type.
pub type ParserRule = Rule;

pub type Error = pest::error::Error<Rule>;
type Result<T> = std::result::Result<T, Error>;

macro_rules! error {
    ($msg:expr, $span:expr) => {
        Err(pest::error::Error::new_from_span(pest::error::ErrorVariant::CustomError { message: $msg.into() }, $span))
    };
}

macro_rules! fst {
    ($iter:expr) => {
        $iter.next().unwrap()
    }
}

macro_rules! take_n {
    ($iter:expr) => {
        $iter.to_array().unwrap()
    }
}

macro_rules! first {
    ($pair:expr) => {
        $pair.into_inner().next().unwrap()
    };
}

impl<'a> From<Pair<'a, Rule>> for Ident<'a> {
    #[inline]
    fn from(pair: Pair<'a, Rule>) -> Ident<'a> {
        Ident::new(pair.as_str(), pair.as_span())
    }
}

#[derive(Debug, Clone)]
struct Context<'a> {
    macro_args: HashMap<&'a str, Arg<'a>>,
    macro_depth: usize,
}

#[inline]
fn parse_lbl_name(lbl_name: Pair<Rule>) -> Result<Label> {
    Ok(Label::new(lbl_name.into(), 0))
}

#[inline]
fn parse_label(pair: Pair<Rule>) -> Result<Label> {
    let lbl_name = first!(pair);
    parse_lbl_name(lbl_name)
}

fn parse_str(str: Pair<Rule>) -> Result<Str> {
    if str.as_rule() == Rule::str {
        let s = str.as_str();

        // Trim " from beginning and end.
        let inner = &s[1..s.len()-1];

        Ok(Str::new(inner, str.as_span()))
    } else {
        error!("expected a string", str.as_span())
    }
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

fn parse_num(pair: Pair<Rule>) -> Result<Num> {
    let span = pair.as_span();
    let s = pair.as_str();

    let res = match s.get(0..2) {
        Some("0b") => i16::from_str_radix(&s[2..], 2),
        Some("0o") => i16::from_str_radix(&s[2..], 8),
        Some("0d") => i16::from_str_radix(&s[2..], 10),
        Some("0x") => i16::from_str_radix(&s[2..], 16),
        _          => i16::from_str_radix(s, 10),
    };

    match res {
        Ok(n)  => Ok(Num::new(n, span)),
        Err(e) => return error!(e.to_string(), span),
    }
}

fn parse_lit(pair: Pair<Rule>) -> Result<Lit> {
    let lit = first!(pair);
    let span = lit.as_span();

    let parsed = match lit.as_rule() {
        Rule::lbl_name  => Lit::Lbl(parse_lbl_name(lit)?),
        Rule::chr       => Lit::Chr(Chr::new(extract_chr(lit.as_str()), span)),
        Rule::expr      => Lit::Num(parse_expr(lit)?),
        _               => unreachable!(),
    };

    Ok(parsed)
}

// TODO: Maybe it would be better to add a Reg type to the AST.
fn parse_reg(pair: Pair<Rule>) -> Result<Reg> {
    use RegAddr::*;

    let span = pair.as_span();

    let reg = match pair.as_str() {
        "$tmp" => Tmp,
        "$hi"  => Hi,
        "$lo"  => Lo,
        "$sp"  => Sp,
        "$adr" => Adr,
        "$acc" => Acc,
        "$flg" => Flags,
        "$r1"  => R1,
        "$r2"  => R2,
        "$r3"  => R3,
        "$r4"  => R4,
        "$r5"  => R5,
        "$r6"  => R6,
        "$r7"  => R7,
        "$r8"  => R8,
        "$r9"  => R9,
        s      => return error!(format!("{} is not a valid register", s), span),
    };

    Ok(Reg::new(reg, span))
}

fn parse_arg<'a>(pair: Pair<'a, Rule>, cx: Option<&mut Context<'a>>) -> Result<Arg<'a>> {
    let arg = fst!(pair.into_inner());

    let parsed = match arg.as_rule() {
        Rule::arg_reg     => Arg::Reg(parse_reg(fst!(arg.into_inner()))?),
        Rule::arg_imm     => Arg::Imm(parse_lit(fst!(arg.into_inner()))?),
        Rule::arg_reg_imm => {
            let [arg_reg, arg_imm] = arg.into_inner().to_array().unwrap();
            let reg = fst!(arg_reg.into_inner());
            let imm = fst!(arg_imm.into_inner());
            Arg::RegImm(parse_reg(reg)?, parse_lit(imm)?)
        },
        Rule::macro_arg   => {
            if let Some(cx) = cx {
                if let Some(val) = cx.macro_args.get(&arg.as_str()) {
                    val.clone()
                } else {
                    return error!("macro argument used but not defined", arg.as_span());
                }
            } else {
                return error!("cannot use macro argument outside a macro", arg.as_span());
            }
        },
        _ => unreachable!(),
    };

    Ok(parsed)
}

fn parse_inst<'a>(pair: Pair<'a, Rule>, cx: Option<&mut Context<'a>>) -> Result<Inst<'a>> {
    let span = pair.as_span();
    let mut inst_iter = pair.into_inner();
    let ident = inst_iter.next().unwrap().into();

    let arg_list: Vec<_> = inst_iter
        .next()
        .unwrap()       // Rule::arg_list
        .into_inner()
        .collect();

    let args = arg_list
        .into_iter()
        .map(|el| parse_arg(el, cx))
        .collect::<Result<_>>()?;

    Ok(Inst { ident, args, span })
}

#[inline]
fn parse_stmts<'a>(pairs: Pairs<'a, Rule>, cx: Option<&mut Context<'a>>) -> Result<Vec<Stmt<'a>>> {
    // pairs.map(|el| parse_stmt(el, cx)).collect()
    let mut res = Vec::new();
    for el in pairs {
        res.push(parse_stmt(el, cx)?);
    }
    Ok(res)
}

fn parse_stmt<'a>(pair: Pair<'a, Rule>, cx: Option<&mut Context<'a>>) -> Result<Stmt<'a>> {
    let stmt = first!(pair);
    let span = stmt.as_span();

    let parsed = match stmt.as_rule() {
        Rule::include => {
            let s = fst!(stmt.into_inner());
            Stmt::Include(parse_str(s)?)
        },
        Rule::define  => {
            let [ident, lit] = take_n!(stmt.into_inner());
            Stmt::Define(ident.into(), parse_lit(lit)?)
        },
        Rule::macro_rule => {
            let [ident, args, stmts] = take_n!(stmt.into_inner());

            Stmt::Macro(Macro {
                name: ident.into(),
                args: args
                    .into_inner()
                    .map(parse_macro_arg)
                    .collect::<Result<_>>()?,

                contents: stmts.into_inner(),
                span,
            })
        },
        Rule::label  => Stmt::Label(parse_label(stmt)?),
        Rule::inst   => Stmt::Inst(parse_inst(stmt, cx)?),
        Rule::lit    => Stmt::Lit(parse_lit(stmt)?),
        Rule::str    => Stmt::Str(parse_str(stmt)?),
        r            => unreachable!("unexpected rule {:?}", r),
    };

    Ok(parsed)
}

fn parse_macro_arg(pair: Pair<Rule>) -> Result<Ident> {
    let arg = fst!(pair.into_inner());
    Ok(arg.into())
}

/*
fn parse_macro_arg(pair: Pair<Rule>) -> Result<MacroArg> {
    let arg = fst!(pair.into_inner());

    let macro_arg = match arg.as_rule() {
        Rule::macro_arg_reg => MacroArg::Reg(fst!(arg.into_inner()).into()),
        Rule::macro_arg_imm => MacroArg::Imm(fst!(arg.into_inner()).into()),
        _                   => unimplemented!(),
    };

    Ok(macro_arg)
}
*/

fn parse_expr(pair: Pair<Rule>) -> Result<Num> {

    // Expects pair.as_rule() == Rule::op
    fn precedence_of(op: &str) -> i32 {
        match op {
            "|"         => 1,
            "&"         => 2,
            ">>" | "<<" => 3,
            "+"  | "-"  => 4,
            "*"  | "/"  => 5,
            s           => unreachable!("unexpected operator '{}'", s),
        }
    }

    fn compute_op<'a>(op: &'a str, a: i16, b: i16) -> i16 {
        match op {
            "+"  => a + b,
            "-"  => a - b,
            "*"  => a * b,
            "/"  => a / b,
            "|"  => a | b,
            "&"  => a & b,
            ">>" => a >> b,
            "<<" => a << b,
            s    => unreachable!("unexpected operator '{}'", s),
        }
    }

    fn parse_atom<'a>(cur_tok: Pair<'a, Rule>) -> Result<Num<'a>> {
        let atom = fst!(cur_tok.into_inner());

        match atom.as_rule() {
            Rule::num   => parse_num(atom),
            Rule::paren => {
                let expr = fst!(atom.into_inner());
                parse_precedence(&mut expr.into_inner(), 0)
            },
            _           => unreachable!(),
        }
    }

    fn parse_precedence<'a>(tokens: &mut Pairs<'a, Rule>, min_prec: i32) -> Result<Num<'a>> {
        let mut lhs = parse_atom(tokens.next().unwrap())?;

        while let Some(op) = tokens.peek() {
            let op = op.as_str();

            if precedence_of(op) < min_prec {
                break;
            }

            tokens.next();

            let rhs = parse_precedence(tokens, precedence_of(op) + 1)?;
            let res = compute_op(op, lhs.val, rhs.val);
            let op_span = lhs.span.start_pos().span(&rhs.span.end_pos());
            lhs = Num::new(res, op_span);
        }

        Ok(lhs)
    }


    let mut tokens = pair.into_inner();
    parse_precedence(&mut tokens, 0)
}

pub fn parse_zasm(program: &str) -> Result<Prog> {
    let stmts = fst!(ZASMParser::parse(Rule::zasm, program)?);
    let span = stmts.as_span();
    let stmts = parse_stmts(stmts.into_inner(), None)?;

    Ok(Prog {
        stmts,
        span,
    })
}


#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_match_extract {
        ($expr:expr, $pat:pat => $extract:expr) => {
            match $expr {
                $pat => $extract,
                _ => panic!("assert failed {:?}", $expr),
            }
        };
    }

    macro_rules! parse {
        ($input:expr => $rule:path) => {
            match ZASMParser::parse($rule, $input) {
                Ok(mut val) => fst!(val),
                Err(e)  => {
                    panic!("{}", e);
                }
            }
        };
    }

    #[test]
    fn test_parse_label() {
        let label = parse!("main:\n" => Rule::label);
        let label = parse_label(label).unwrap();

        assert_matches!(
            label,
            Label {
                ident: Ident {
                    content: "main",
                    ..
                },
                id: 0
            }
        );
    }

    #[test]
    fn test_parse_str() {
        let s = parse!(r#""hello, world!""# => Rule::str);
        let s = parse_str(s).unwrap();

        assert_eq!(s.content, "hello, world!");
    }

    #[test]
    fn test_parse_expr() {
        let expr = parse!("2 - 3" => Rule::expr);
        let expr = parse_expr(expr).unwrap();
        dbg!(&expr);

        assert_eq!(expr.val, 2 - 3);

        let expr = parse!("2 + 2 * 3" => Rule::expr);
        let expr = parse_expr(expr).unwrap();

        assert_eq!(expr.val, 2 + 2 * 3);

        let expr = parse!("(2 + 2) * 3" => Rule::expr);
        let expr = parse_expr(expr).unwrap();

        assert_eq!(expr.val, (2 + 2) * 3);

        let expr = parse!("0b1100 | 0b0011" => Rule::expr);
        let expr = parse_expr(expr).unwrap();

        assert_eq!(expr.val, 0b1100 | 0b0011);

        let expr = parse!("0b1100 & 0b0011" => Rule::expr);
        let expr = parse_expr(expr).unwrap();

        assert_eq!(expr.val, 0b1100 & 0b0011);

        let expr = parse!("0b1100 | 0xff & 0b0011" => Rule::expr);
        let expr = parse_expr(expr).unwrap();

        assert_eq!(expr.val, 0b1100 | 0xff & 0b0011);
    }

    #[test]
    fn failing() {

        let expr = parse!("0b1100 >> 2" => Rule::expr);
        let expr = parse_expr(expr).unwrap();

        assert_eq!(expr.val, 0b1100 >> 2);
    }

    #[test]
    fn test_parse_lit() {
        let lit_label = parse!("loop" => Rule::lit);
        let lit_label = parse_lit(lit_label).unwrap();

        assert_matches!(
            lit_label,
            Lit::Lbl(Label {
                ident: Ident {
                    content: "loop",
                    ..
                },
                id: 0
            })
        );

        let lit_chr = parse!("'a'" => Rule::lit);
        let lit_chr = parse_lit(lit_chr).unwrap();

        assert_matches!(
            lit_chr,
            Lit::Chr(Chr {
                chr: 'a',
                ..
            })
        );

        let lit_dec = parse!("1234" => Rule::lit);
        let lit_dec = parse_lit(lit_dec).unwrap();

        assert_matches!(
            lit_dec,
            Lit::Num(Num {
                val: 1234,
                ..
            })
        );

        let lit_hex = parse!("0x1234" => Rule::lit);
        let lit_hex = parse_lit(lit_hex).unwrap();

        assert_matches!(
            lit_hex,
            Lit::Num(Num {
                val: 0x1234,
                ..
            })
        );

        let lit_bin = parse!("0b01010101" => Rule::lit);
        let lit_bin = parse_lit(lit_bin).unwrap();

        assert_matches!(
            lit_bin,
            Lit::Num(Num {
                val: 0b01010101,
                ..
            })
        );
    }

    #[test]
    fn test_parse_inst() {
        let input = "addi $acc, 20";
        let inst = parse!(input => Rule::inst);
        let inst = parse_inst(inst, None).unwrap();

        let args = assert_match_extract!(
            inst,
            Inst {
                ident: Ident {
                    content: "addi",
                    ..
                },
                args,
                ..
            } => args
        );

        assert_eq!(args.len(), 2);

        assert_matches!(
            args[0],
            Arg::Reg(Reg {
                addr: RegAddr::Acc,
                ..
            })
        );

        assert_matches!(
            args[1],
            Arg::Imm(Lit::Num(Num {
                val: 20,
                ..
            }))
        );
    }

    #[test]
    fn test_parse_stmt() {
        let input = r#"@include "utils.zasm""#;
        let include = parse!(input => Rule::stmt);
        let include = parse_stmt(include, None).unwrap();

        assert_matches!(
            include,
            Stmt::Include(Str {
                content: "utils.zasm",
                ..
            })
        );

        let input = "@define NUM 20120";
        let define = parse!(input => Rule::stmt);
        let define = parse_stmt(define, None).unwrap();

        assert_matches!(
            define,
            Stmt::Define(
                Ident {
                    content: "NUM",
                    ..
                },
                Lit::Num(Num {
                    val: 20120,
                    ..
                })
            )
        );

        let input = r"@macro push %reg {
                        stw %reg, $sp(0)
                        addi $sp, -1    
                       }";

        let mac = parse!(input => Rule::stmt);
        let mac = parse_stmt(mac, None).unwrap();

        let (args, _contents) = assert_match_extract!(
            mac,
            Stmt::Macro(Macro {
                name: Ident {
                    content: "push",
                    ..
                },
                args,
                contents,
                ..
            }) => (args, contents)
        );

        assert_matches!(
            args[..],
            [
                Ident {
                    content: "reg",
                    ..
                }
            ]
        );

        let input = "main:";
        let label = parse!(input => Rule::stmt);
        let label = parse_stmt(label, None).unwrap();

        assert_matches!(
            label,
            Stmt::Label(Label {
                ident: Ident {
                    content: "main",
                    ..
                },
                id: 0,
            })
        );

        let input = "la $r1, msg";
        let inst = parse!(input => Rule::stmt);
        let inst = parse_stmt(inst, None).unwrap();

        assert_matches!(
            inst,
            Stmt::Inst(Inst {
                ident: Ident {
                    content: "la",
                    ..
                },
                ..
            })
        );

    }

    #[test]
    fn test_parse_arg() {
        let registers = &[
            ("$tmp", RegAddr::Tmp),
            ("$hi" , RegAddr::Hi),
            ("$lo" , RegAddr::Lo),
            ("$sp" , RegAddr::Sp),
            ("$adr", RegAddr::Adr),
            ("$acc", RegAddr::Acc),
            ("$flg", RegAddr::Flags),
            ("$r1" , RegAddr::R1),
            ("$r2" , RegAddr::R2),
            ("$r3" , RegAddr::R3),
            ("$r4" , RegAddr::R4),
            ("$r5" , RegAddr::R5),
            ("$r6" , RegAddr::R6),
            ("$r7" , RegAddr::R7),
            ("$r8" , RegAddr::R8),
            ("$r9" , RegAddr::R9),
        ];

        for (input, addr) in registers {
            let arg_reg = parse!(input => Rule::arg);
            let arg_reg = parse_arg(arg_reg, None).unwrap();

            let reg = assert_match_extract!(arg_reg, Arg::Reg(r) => r);

            assert_eq!(reg.addr, *addr);
        }

        let immediates = &[
            ("1234"     , 1234),
            ("0x1234"   , 0x1234),
            ("0o1234"   , 0o1234),
            ("0d1234"   , 1234),
            ("0b101010" , 0b101010),
        ];

        for (input, val) in immediates {
            let arg_imm = parse!(input => Rule::arg);
            let arg_imm = parse_arg(arg_imm, None).unwrap();

            let lit = assert_match_extract!(arg_imm, Arg::Imm(Lit::Num(n)) => n);
            
            assert_eq!(lit.val, *val);
        }

        let input = "loop";
        let arg_imm = parse!(input => Rule::arg);
        let arg_imm = parse_arg(arg_imm, None).unwrap();

        assert_matches!(
            arg_imm,
            Arg::Imm(Lit::Lbl(Label {
                ident: Ident {
                    content: "loop",
                    ..
                },
                id: 0,
            }))
        );

        let input = "$sp(2)";
        let arg_reg_imm = parse!(input => Rule::arg);
        let arg_reg_imm = parse_arg(arg_reg_imm, None).unwrap();

        assert_matches!(
            arg_reg_imm,
            Arg::RegImm(
                Reg {
                    addr: RegAddr::Sp,
                    ..
                },
                Lit::Num(Num {
                    val: 2,
                    ..
                })
            )
        );
    }
}
