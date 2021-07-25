use std::collections::BTreeMap;

use pest_derive::Parser;
use pest::Parser;
use pest::iterators::{ Pair, Pairs };

use crate::ast::*;
use crate::error::Error;
use crate::error;

use crate::utils::IterExt;

#[derive(Parser)]
#[grammar = "zasm.pest"]
pub struct ZASMParser;

// TODO: Find a better way of exporting this rule type.
pub type ParserRule = Rule;

type Result<T> = std::result::Result<T, Error>;

#[macro_export]
macro_rules! next {
    ($iter:expr) => {
        $iter.next().unwrap()
    }
}

#[macro_export]
macro_rules! take_n {
    ($iter:expr) => {
        $iter.to_array().unwrap()
    }
}

impl<'a> From<Pair<'a, Rule>> for Ident<'a> {
    #[inline]
    fn from(pair: Pair<'a, Rule>) -> Ident<'a> {
        Ident::new(pair.as_str(), pair.as_span())
    }
}

#[derive(Debug, Clone)]
pub struct Context<'a> {
    pub macro_args: BTreeMap<(&'a str, usize), Arg<'a>>,
    pub macro_depth: usize,
}

impl<'a> Context<'a> {
    pub fn new() -> Context<'a> {
        Context {
            macro_args: BTreeMap::new(),
            macro_depth: 0,
        }
    }

    pub fn insert_macro_arg(&mut self, name: &'a str, arg: Arg<'a>) -> Option<Arg<'a>> {
        let depth = self.macro_depth;
        self.macro_args.insert((name, depth), arg)
    }

    pub fn get_macro_arg(&mut self, name: &'a str) -> Option<&Arg<'a>> {
        let depth = self.macro_depth;
        self.macro_args
            .range((name, 0)..=(name, depth))
            .rev()
            .next() // The match to `name` with the highest possible depth.
            .map(|(_, v)| v)
    }

    pub fn remove_macro_args_in_depth(&mut self) {
        let this_depth = self.macro_depth;
        self.macro_args
            .drain_filter(|&(_, depth), _| depth == this_depth)
            .for_each(drop)
    }
}

#[inline]
fn parse_lbl_name<'a>(lbl_name: Pair<'a, Rule>, cx: Option<&mut Context<'a>>) -> Result<Label<'a>> {
    let ident = Ident::from(lbl_name);

    if let Some(cx) = cx {
        Ok(Label::Macro(ident, cx.macro_depth))
    } else if ident.content.starts_with(".") {
        Ok(Label::local(ident))
    } else {
        Ok(Label::global(ident))
    }
}

#[inline]
fn parse_label<'a>(pair: Pair<'a, Rule>, cx: Option<&mut Context<'a>>) -> Result<Label<'a>> {
    let lbl_name = next!(pair.into_inner());
    parse_lbl_name(lbl_name, cx)
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
        Some("0b") => i32::from_str_radix(&s[2..], 2),
        Some("0o") => i32::from_str_radix(&s[2..], 8),
        Some("0d") => i32::from_str_radix(&s[2..], 10),
        Some("0x") => i32::from_str_radix(&s[2..], 16),
        _          => i32::from_str_radix(s, 10),
    };

    match res {
        Ok(n)  => Ok(Num::new(n, span)),
        Err(e) => return error!(e.to_string(), span),
    }
}

fn parse_reg(pair: Pair<Rule>) -> Result<Reg> {
    use architecture_utils::Reg::*;

    let span = pair.as_span();

    let reg = match pair.as_str() {
        "$tmp" => TMP,
        "$hi"  => HI,
        "$lo"  => LO,
        "$sp"  => SP,
        "$adr" => ADR,
        "$acc" => ACC,
        "$flg" => FL,
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

fn get_macro_arg<'a>(pair: Pair<'a, Rule>, cx: Option<&mut Context<'a>>) -> Result<Arg<'a>> {
    if let Some(cx) = cx {
        if let Some(val) = cx.get_macro_arg(&pair.as_str()) {
            Ok(val.clone())
        } else {
            return error!("macro argument used but not defined", pair.as_span());
        }
    } else {
        return error!("cannot use macro argument outside a macro", pair.as_span());
    }
}

fn parse_arg<'a>(pair: Pair<'a, Rule>, cx: Option<&mut Context<'a>>) -> Result<Arg<'a>> {
    let arg = next!(pair.into_inner());

    let parsed = match arg.as_rule() {
        Rule::arg_reg     => {
            let arg_reg = next!(arg.into_inner());

            match arg_reg.as_rule() {
                Rule::reg       => Arg::Reg(parse_reg(arg_reg)?),
                Rule::macro_arg => get_macro_arg(arg_reg, cx)?,
                _               => unreachable!(),
            }
        },
        Rule::arg_imm     => {
            let arg_imm = next!(arg.into_inner());
            Arg::Imm(parse_expr(arg_imm, cx)?)
        },
        Rule::arg_reg_imm => {
            let [arg_reg, arg_imm] = arg.into_inner().to_array().unwrap();
            let reg = next!(arg_reg.into_inner());
            let imm = next!(arg_imm.into_inner());
            Arg::RegImm(parse_reg(reg)?, parse_expr(imm, cx)?)
        },
        _ => unreachable!(),
    };

    Ok(parsed)
}

fn parse_inst<'a>(pair: Pair<'a, Rule>, mut cx: Option<&mut Context<'a>>) -> Result<Inst<'a>> {
    let mut inst_iter = pair.into_inner();
    let ident: Ident = inst_iter.next().unwrap().into();
    let mut span = ident.span();

    let arg_list = match inst_iter.next() {
        Some(arg_list) => {
            // Fix the span to range from the ident to the end of the argument list. This is
            // necessary because we don't want the `eol` rule to be included in the span.
            span = span.start_pos().span(&arg_list.as_span().end_pos());
            arg_list.into_inner().collect()
        },
        None           => vec![],
    };

    let args = arg_list
        .into_iter()
        .map(|el| parse_arg(el, cx.as_deref_mut()))
        .collect::<Result<_>>()?;


    Ok(Inst { ident, args, span })
}

fn parse_marker<'a>(pair: Pair<'a, Rule>, cx: Option<&mut Context<'a>>) -> Result<Marker<'a>> {
    let marker = next!(pair.into_inner());
    let span = marker.as_span();

    match marker.as_rule() {
        Rule::marker_data  => Ok(Marker::DotData(span)),
        Rule::marker_text  => Ok(Marker::DotText(span)),
        Rule::marker_entry => {
            let lbl_name = next!(marker.into_inner());
            Ok(Marker::Entry(parse_lbl_name(lbl_name, cx)?, span))
        },
        _                  => unreachable!(),
    }
}

#[inline]
pub fn parse_stmts<'a>(pairs: Pairs<'a, Rule>, mut cx: Option<&mut Context<'a>>) -> Result<Vec<Stmt<'a>>> {
    pairs
        .map(|el| parse_stmt(el, cx.as_deref_mut()))
        .collect()
}

fn parse_stmt<'a>(pair: Pair<'a, Rule>, cx: Option<&mut Context<'a>>) -> Result<Stmt<'a>> {
    let stmt = next!(pair.into_inner());
    let span = stmt.as_span();

    let parsed = match stmt.as_rule() {
        Rule::include => {
            let s = next!(stmt.into_inner());
            Stmt::Include(parse_str(s)?)
        }

        Rule::define  => {
            let [ident, lit] = take_n!(stmt.into_inner());
            Stmt::Define(ident.into(), parse_expr(lit, cx)?)
        }

        Rule::marker => Stmt::Marker(parse_marker(stmt, cx)?),

        Rule::macro_rule => {
            let mut rule_iter = stmt.into_inner();
            let ident = next!(rule_iter);
            let args_or_body = next!(rule_iter);

            let args: Vec<Ident>;
            let body: Pairs<Rule>;

            match args_or_body.as_rule() {
                Rule::macro_args => {
                    args = args_or_body
                        .into_inner()
                        .map(parse_macro_arg)
                        .collect::<Result<_>>()?;

                    body = next!(rule_iter).into_inner();
                }

                Rule::macro_body => {
                    args = Vec::new();
                    body = args_or_body.into_inner();
                }

                _ => unreachable!(),
            }

            Stmt::Macro(Macro {
                name: ident.into(),
                args,
                body,
                span,
            })
        }

        Rule::label  => Stmt::Label(parse_label(stmt, cx)?),
        Rule::inst   => Stmt::Inst(parse_inst(stmt, cx)?),
        Rule::expr   => Stmt::Expr(parse_expr(stmt, cx)?),
        Rule::str    => Stmt::Str(parse_str(stmt)?),
        r            => unreachable!("unexpected rule {:?}", r),
    };

    Ok(parsed)
}

#[inline]
fn parse_macro_arg(pair: Pair<Rule>) -> Result<Ident> {
    Ok(pair.into())
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

fn parse_expr<'a>(pair: Pair<'a, Rule>, cx: Option<&mut Context<'a>>) -> Result<Expr<'a>> {

    fn try_to_num(expr: &Expr) -> Option<i32> {
        match expr {
            Expr::Lbl(..) |
            Expr::Bin(..)  => None,
            Expr::Chr(chr) => Some(chr.chr as i32),
            Expr::Num(num) => Some(num.val),
        }
    }

    fn compute_op(a: i32, op: &str, b: i32) -> i32 {
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

    fn parse_atom<'a>(cur_tok: Pair<'a, Rule>, mut cx: Option<&mut Context<'a>>) -> Result<Expr<'a>> {
        let atom = next!(cur_tok.into_inner());
        let span = atom.as_span();

        match atom.as_rule() {
            Rule::num      => Ok(Expr::Num(parse_num(atom)?)),
            Rule::chr      => Ok(Expr::Chr(Chr::new(extract_chr(atom.as_str()), span))),
            Rule::lbl_name => Ok(Expr::Lbl(parse_lbl_name(atom, None)?)),
            Rule::paren    => {
                let expr = next!(atom.into_inner());
                parse_precedence(&mut expr.into_inner(), 0, cx.as_deref_mut())
            },
            Rule::macro_arg => {
                let arg = get_macro_arg(atom, cx)?;

                if let Arg::Imm(expr) = arg {
                    Ok(expr)
                } else {
                    error!("expected an immediate value", span)
                }
            }
            rule => unreachable!("unexpected rule {:?}", rule),
        }
    }

    fn parse_precedence<'a>(tokens: &mut Pairs<'a, Rule>, min_prec: i32, mut cx: Option<&mut Context<'a>>) -> Result<Expr<'a>> {
        let mut lhs = parse_atom(tokens.next().unwrap(), cx.as_deref_mut())?;

        while let Some(op) = tokens.peek() {
            let prec = precedence_of(op.as_str());
            if prec < min_prec {
                break;
            }

            tokens.next();

            let rhs = parse_precedence(tokens, prec + 1, cx.as_deref_mut())?;

            // TODO: find out a better way to use the span.
            let span = op.as_span().clone();

            // If the expression can be reduced immediately, it will be.
            if let (Some(a), Some(b)) = (try_to_num(&lhs), try_to_num(&rhs)) {
                lhs = Expr::Num(Num::new(compute_op(a, op.as_str(), b), span));
            } else {
                lhs = Expr::Bin(BinExpr {
                    lhs: Box::new(lhs),
                    operator: op.into(),
                    rhs: Box::new(rhs),
                    span,
                });
            }
        }

        Ok(lhs)
    }

    let mut tokens = pair.into_inner();
    parse_precedence(&mut tokens, 0, cx)
}

pub fn parse_zasm<'a>(zasm: Pair<'a, Rule>, cx: Option<&mut Context<'a>>) -> Result<Prog<'a>> {
    let span = zasm.as_span();
    let stmts = next!(zasm.into_inner());
    let stmts = parse_stmts(stmts.into_inner(), cx)?;

    Ok(Prog {
        stmts,
        span,
    })
}

pub fn parse_src(program: &str) -> Result<Prog> {
    let zasm = next!(ZASMParser::parse(Rule::zasm, program)?);
    parse_zasm(zasm, None)
}


#[cfg(test)]
mod test {
    use super::*;
    use architecture_utils as risc0;

    #[macro_export]
    macro_rules! assert_match_extract {
        ($expr:expr, $pat:pat => $extract:expr) => {
            match $expr {
                $pat => $extract,
                _ => panic!("assert failed {:?}", $expr),
            }
        };
    }

    #[macro_export]
    macro_rules! parse {
        ($input:expr => $rule:path) => {
            match crate::parser::ZASMParser::parse($rule, $input) {
                Ok(mut val) => $crate::next!(val),
                Err(e)  => {
                    panic!("{}", e);
                }
            }
        };
    }

    #[test]
    fn test_parse_label() {
        let label = parse!("main:\n" => Rule::label);
        let label = parse_label(label, None).unwrap();

        assert_matches!(
            label,
            Label::Global(
                Ident {
                    content: "main",
                    ..
                }
            )
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
        let expr = parse_expr(expr, None).unwrap();

        let expr = assert_match_extract!(expr, Expr::Num(num) => num);

        assert_eq!(expr.val, 2 - 3);

        let expr = parse!("2 + 2 * 3" => Rule::expr);
        let expr = parse_expr(expr, None).unwrap();

        let expr = assert_match_extract!(expr, Expr::Num(num) => num);

        assert_eq!(expr.val, 2 + 2 * 3);

        let expr = parse!("(2 + 2) * 3" => Rule::expr);
        let expr = parse_expr(expr, None).unwrap();

        let expr = assert_match_extract!(expr, Expr::Num(num) => num);

        assert_eq!(expr.val, (2 + 2) * 3);

        let expr = parse!("0b1100 | 0b0011" => Rule::expr);
        let expr = parse_expr(expr, None).unwrap();

        let expr = assert_match_extract!(expr, Expr::Num(num) => num);

        assert_eq!(expr.val, 0b1100 | 0b0011);

        let expr = parse!("0b1100 & 0b0011" => Rule::expr);
        let expr = parse_expr(expr, None).unwrap();

        let expr = assert_match_extract!(expr, Expr::Num(num) => num);

        assert_eq!(expr.val, 0b1100 & 0b0011);

        let expr = parse!("0b1100 | 0xff & 0b0011" => Rule::expr);
        let expr = parse_expr(expr, None).unwrap();

        let expr = assert_match_extract!(expr, Expr::Num(num) => num);

        assert_eq!(expr.val, 0b1100 | 0xff & 0b0011);

        let expr = parse!("0b1100 >> 2" => Rule::expr);
        let expr = parse_expr(expr, None).unwrap();

        let expr = assert_match_extract!(expr, Expr::Num(num) => num);

        assert_eq!(expr.val, 0b1100 >> 2);
    }

    #[test]
    fn test_parse_expr_lit() {
        let lit_label = parse!("loop" => Rule::expr);
        let lit_label = parse_expr(lit_label, None).unwrap();

        assert_matches!(
            lit_label,
            Expr::Lbl(Label::Global(
                    Ident {
                        content: "loop",
                        ..
                    }
            ))
        );

        let lit_chr = parse!("'a'" => Rule::expr);
        let lit_chr = parse_expr(lit_chr, None).unwrap();

        assert_matches!(
            lit_chr,
            Expr::Chr(Chr {
                chr: 'a',
                ..
            })
        );

        let lit_dec = parse!("1234" => Rule::expr);
        let lit_dec = parse_expr(lit_dec, None).unwrap();

        assert_matches!(
            lit_dec,
            Expr::Num(Num {
                val: 1234,
                ..
            })
        );

        let lit_hex = parse!("0x1234" => Rule::expr);
        let lit_hex = parse_expr(lit_hex, None).unwrap();

        assert_matches!(
            lit_hex,
            Expr::Num(Num {
                val: 0x1234,
                ..
            })
        );

        let lit_bin = parse!("0b01010101" => Rule::expr);
        let lit_bin = parse_expr(lit_bin, None).unwrap();

        assert_matches!(
            lit_bin,
            Expr::Num(Num {
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
                addr: risc0::Reg::ACC,
                ..
            })
        );

        assert_matches!(
            args[1],
            Arg::Imm(Expr::Num(Num {
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
                Expr::Num(Num {
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

        let args = assert_match_extract!(
            mac,
            Stmt::Macro(Macro {
                name: Ident {
                    content: "push",
                    ..
                },
                args,
                ..
            }) => args
        );

        assert_matches!(
            args[..],
            [
                Ident {
                    content: "%reg",
                    ..
                }
            ]
        );

        let input = "main:";
        let label = parse!(input => Rule::stmt);
        let label = parse_stmt(label, None).unwrap();

        assert_matches!(
            label,
            Stmt::Label(Label::Global(Ident {
                content: "main",
                ..
            }))
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
            ("$tmp", risc0::Reg::TMP),
            ("$hi" , risc0::Reg::HI),
            ("$lo" , risc0::Reg::LO),
            ("$sp" , risc0::Reg::SP),
            ("$adr", risc0::Reg::ADR),
            ("$acc", risc0::Reg::ACC),
            ("$flg", risc0::Reg::FL),
            ("$r1" , risc0::Reg::R1),
            ("$r2" , risc0::Reg::R2),
            ("$r3" , risc0::Reg::R3),
            ("$r4" , risc0::Reg::R4),
            ("$r5" , risc0::Reg::R5),
            ("$r6" , risc0::Reg::R6),
            ("$r7" , risc0::Reg::R7),
            ("$r8" , risc0::Reg::R8),
            ("$r9" , risc0::Reg::R9),
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

            let lit = assert_match_extract!(arg_imm, Arg::Imm(Expr::Num(n)) => n);
            
            assert_eq!(lit.val, *val);
        }

        let input = "loop";
        let arg_imm = parse!(input => Rule::arg);
        let arg_imm = parse_arg(arg_imm, None).unwrap();

        assert_matches!(
            arg_imm,
            Arg::Imm(Expr::Lbl(Label::Global(Ident {
                content: "loop",
                ..
            })))
        );

        let input = "$sp(2)";
        let arg_reg_imm = parse!(input => Rule::arg);
        let arg_reg_imm = parse_arg(arg_reg_imm, None).unwrap();

        assert_matches!(
            arg_reg_imm,
            Arg::RegImm(
                Reg {
                    addr: risc0::Reg::SP,
                    ..
                },
                Expr::Num(Num {
                    val: 2,
                    ..
                })
            )
        );
    }
}
