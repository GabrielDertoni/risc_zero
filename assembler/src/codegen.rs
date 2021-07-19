#![allow(unused_macros)]

use std::collections::HashMap;
use std::io::{ Seek, SeekFrom, Write };

use pest::Span;

use crate::ast;
use crate::parser::{ Context, parse_stmts };
use crate::error::Error;
use crate::error;

/*
macro_rules! inst_args {
    ($span:expr => (@lit $($toks:tt)*)) => {
        ast::Arg::Lit(lit!($span => $($toks)*))
    };

    ($span:expr => (% $($toks:tt)*)) => {
        ast::Arg::Lit(lit!($span => $($toks)*))
    };

    ($span:expr => (@lbl $lbl:expr)) => {
        ast::Arg::Lbl($lbl.into())
    };

    ($span:expr => <$lbl:ident>) => {
        ast::Arg::Lbl($lbl.into())
    };

    ($span:expr => $lit:expr) => {
        $lit.into()
    };

    ($span:expr =>) => { };
}

macro_rules! inst {
    ($span:expr => $inst:ident $($tail:tt)*) => {
        ast::Inst::new(ast::Op::$inst, vec![$(inst_args!($span => $tail)),*], $span)
    };
}

macro_rules! lit {
    ($span:expr => (# $expr:expr)) => {
        ast::Lit::Num(ast::Spanned::new($expr, $span.clone()))
    };

    ($span:expr => (@num $expr:expr)) => {
        ast::Lit::Num(ast::Spanned::new($expr, $span.clone()))
    };

    ($span:expr => (@chr $expr:expr)) => {
        ast::Lit::Chr(ast::Spanned::new($expr, $span.clone()))
    };

    ($span:expr => (@str $expr:expr)) => {
        ast::Lit::Str(ast::Spanned::new($expr, $span.clone()))
    };

    ($span:expr => (@lbl $expr:expr)) => {
        ast::Lit::Lbl(ast::Spanned::new($expr, $span.clone()))
    };

    ($span:expr => [$expr:expr]) => {
        ast::Lit::Lbl(ast::Spanned::new($expr, $span.clone()))
    };

    ($span:expr => @& $($toks:tt)*) => {
        ast::Lit::Ref(Box::new(lit!($span => $($toks)*)))
    };

    ($span:expr => @* $($toks:tt)*) => {
        ast::Lit::Deref(Box::new(lit!($span => $($toks)*)))
    };

    ($span:expr => $expr:expr) => {
        $expr.into()
    };
}

macro_rules! stmt {
    ($span:expr => label $lbl:expr) => {
        ast::Stmt::Label($lbl)
    };

    ($span:expr => lit $($toks:tt)*) => {
        ast::Stmt::Lit(lit!($span => $($toks)*))
    };

    ($span:expr => $($toks:tt)*) => {
        ast::Stmt::Inst(inst!($span => $($toks)*))
    };
}

macro_rules! stmts {
    ($span:expr => $([$($toks:tt)*])+) => {
        &[$(stmt!($span => $($toks)*)),+]
    };
}
*/

const EMPTY_DEFAULT: i32 = -1;

type Result<T> = std::result::Result<T, Error>;

type Addr = usize;
type Word = u16;

/*
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
struct Ident<'a>(&'a str, usize);

impl<'a> Ident<'a> {
    #[inline]
    fn new(name: &'a str, id: usize) -> Ident<'a> {
        Ident(name, id)
    }

    #[inline]
    fn is_local(&self) -> bool {
        self.0.starts_with(".")
    }
}

impl<'a> From<&ast::Ident<'a>> for Ident<'a> {
    #[inline]
    fn from(ident: &ast::Ident<'a>) -> Ident<'a> {
        Ident::new(ident.content, 0)
    }
}

impl<'a> From<(&'a str, usize)> for Ident<'a> {
    #[inline]
    fn from((name, id): (&'a str, usize)) -> Ident<'a> {
        Ident::new(name, id)
    }
}

impl<'a> std::fmt::Display for Ident<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        if self.1 > 0 {
            write!(f, "_{:02x}", self.1)?;
        }
        Ok(())
    }
}
*/

#[derive(Debug, PartialEq, Eq, Clone)]
struct LabelDef<'a> {
    addr: Addr,
    span: Option<Span<'a>>,
}

impl<'a> LabelDef<'a> {
    fn new(pos: Addr, span: Span<'a>) -> LabelDef<'a> {
        LabelDef { addr: pos, span: Some(span) }
    }

    fn auto(pos: Addr) -> LabelDef<'a> {
        LabelDef { addr: pos, span: None }
    }
}

struct LabelRef<'a> {
    // Where the reference was on the tape.
    pos: Addr,
    parent: Option<ast::Label<'a>>,
    expr: ast::Expr<'a>,
}

impl<'a> LabelRef<'a> {
    #[inline]
    fn new(pos: Addr, parent: Option<ast::Label<'a>>, expr: ast::Expr<'a>) -> LabelRef<'a> {
        LabelRef { pos, parent, expr }
    }

    #[inline]
    fn span(&self) -> Span<'a> {
        self.expr.span()
    }
}

impl<'a> std::fmt::Display for LabelRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (line, col) = self.span().start_pos().line_col();
        write!(f, "{} at {}:{} -> {}", self.span().as_str(), line, col, self.pos)
    }
}

impl<'a> std::fmt::Debug for LabelRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

/*
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Auto<'a> {
    Lbl(Ident<'a>, u32),
    Num(i32, u32),
    Str(&'a str, u32),
}

impl<'a> Auto<'a> {
    fn unwrap_lbl(self) -> (Ident<'a>, u32) {
        if let Auto::Lbl(s, lvl) = self {
            (s, lvl)
        } else {
            panic!("tried to `unwrap_lbl` on Auto that is not Auto::Lbl")
        }
    }

    fn unwrap_num(self) -> (i32, u32) {
        if let Auto::Num(num, lvl) = self {
            (num, lvl)
        } else {
            panic!("tried to `unwrap_num` on Auto that is not Auto::Num")
        }
    }

    fn unwrap_str(self) -> (&'a str, u32) {
        if let Auto::Str(s, lvl) = self {
            (s, lvl)
        } else {
            panic!("tried to `unwrap_str` on Auto that is not Auto::Str")
        }
    }

    fn ref_lvl(&self) -> u32 {
        match self {
            &Auto::Lbl(_, lvl) |
            &Auto::Str(_, lvl) |
            &Auto::Num(_, lvl) => lvl
        }
    }

    fn is_lbl(&self) -> bool { matches!(self, Auto::Lbl(..)) }
    fn is_num(&self) -> bool { matches!(self, Auto::Num(..)) }
    fn is_str(&self) -> bool { matches!(self, Auto::Str(..)) }

    fn is_val_eq(&self, other: &Auto<'a>) -> bool {
        match self {
            Auto::Lbl(a, _) => match other {
                Auto::Lbl(b, _) => a == b,
                _               => false,
            },
            Auto::Num(a, _) => match other {
                Auto::Num(b, _) => a == b,
                _               => false,
            },
            Auto::Str(a, _) => match other {
                Auto::Str(b, _) => a == b,
                _                => false,
            },
        }
    }
}

impl<'a> std::fmt::Display for Auto<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Auto::*;

        match self {
            Lbl(ident, lvl) => {
                if *lvl > 0 {
                    write!(f, "'__lbl_")?;
                    for _ in 0..*lvl {
                        write!(f, "ref_")?;
                    }
                }
                write!(f, "{}", ident.0)?;
                if ident.1 > 0 {
                    write!(f, "_{:02x}", ident.1)?;
                }
                Ok(())
            },
            Num(num, lvl)  => {
                write!(f, "'__num_")?;
                for _ in 0..*lvl {
                    write!(f, "ref_")?;
                }
                write!(f, "{}", num)
            },
            Str(s, lvl)    => {
                write!(f, "'__str_")?;
                for _ in 0..*lvl {
                    write!(f, "ref_")?;
                }
                write!(f, "#[{}]", s)
            },
        }
    }
}
*/

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
struct LabelName<'a> {
    parent: Option<&'a str>,
    name: &'a str,
}

impl<'a> LabelName<'a> {
    fn new(parent: Option<&'a str>, name: &'a str) -> LabelName<'a> {
        LabelName { parent, name }
    }
}

pub struct Assembler<'a, W> {
    writer: W,
    expand: bool,
    parent: Option<ast::Label<'a>>,
    labels: HashMap<LabelName<'a>, LabelDef<'a>>,
    defines: HashMap<&'a str, ast::Lit<'a>>,
    macros: HashMap<&'a str, ast::Macro<'a>>,
    macro_count: usize,
    macro_ctx: Context<'a>,
}

impl<'a, W> Assembler<'a, W>
where
    W: Write + Seek,
{
    pub fn new(writer: W, expand: bool) -> Assembler<'a, W> {
        Assembler {
            writer,
            expand,
            parent: None,
            labels: HashMap::new(),
            defines: HashMap::new(),
            macros: HashMap::new(),
            macro_count: 0,
            macro_ctx: Context::new(),
        }
    }

    #[inline]
    fn get_addr(&mut self) -> usize {
        self.writer.seek(SeekFrom::End(0))
            .expect("seek to the end to always be successfull") as usize
    }

    #[inline]
    fn get_cur_bit(&mut self) -> usize {
        self.get_addr() * 8
    }

    fn emit_buf(&mut self, buf: &[u8]) -> Result<usize> {
        self.writer.seek(SeekFrom::End(0))?;
        let n_written = self.writer.write(buf)?;
        Ok(n_written)
    }

    #[inline]
    fn emit_byte(&mut self, byte: u8) -> Result<usize> {
        self.emit_buf(&[byte])
    }

    #[inline]
    fn emit_word(&mut self, word: Word) -> Result<usize> {
        self.emit_buf(&word.to_be_bytes())
    }

    fn emit_string(&mut self, s: &str, span: Span<'a>) -> Result<usize> {
        let bytes = s.as_bytes();
        let mut i = 0;

        while i < bytes.len() {
            let byte = if bytes[i] == b'\\' {
                i += 1;
                match bytes[i] {
                    b'n'  => b'\n',
                    b'r'  => b'\r',
                    b't'  => b'\t',
                    b'\\' => b'\\',
                    b'\'' => b'\'',
                    b'0'  => b'\0',
                    c     =>
                        return error!(
                            format!("invalid escape sequence '\\{}'", c as char),
                            span
                        ),
                }
            } else {
                bytes[i]
            };

            self.emit_byte(byte)?;
            i += 1
        }

        Ok(i)
    }

    pub fn assemble(&mut self, stmts: &[ast::Stmt<'a>]) -> Result<()> {
        self.find_labels(stmts)?;
        self.assemble_stmts(stmts)?;

        Ok(())
    }

    fn find_labels(&mut self, stmts: &[ast::Stmt<'a>]) -> Result<()> {
        stmts
            .iter()
            .filter_map(|stmt| stmt.as_label())
            .map(|lbl| self.add_label(lbl))
            .collect()
    }

    #[inline]
    fn assemble_stmts(&mut self, stmts: &[ast::Stmt<'a>]) -> Result<()> {
        stmts.into_iter()
            .map(|stmt| self.assemble_stmt(stmt))
            .collect()
    }

    fn assemble_stmt(&mut self, stmt: &ast::Stmt<'a>) -> Result<()> {
        use ast::Stmt::*;

        match stmt {
            // Assumes the label has already been accounted for in the first pass.
            Label(..) => (),

            Inst(inst) => {
                self.assemble_inst(inst)?;
            }

            Lit(lit) => {
                if self.expand {
                    if self.labels.len() > 0 {
                        print!("\t");
                    }
                    println!("{}", lit);
                }
                self.assemble_lit_stmt(lit)?;
            }

            Str(s) => {
                self.emit_string(s.content, s.span())?;
            }

            Define(name, lit) => {
                let old = self.defines.insert(name.content, lit.clone());

                if old.is_some() {
                    return error!("macro defined twice", name.span());
                }
            }

            Include(ast::Str { content: _fname, .. }) => {
                unimplemented!()
                /*
                let source = std::fs::read_to_string(fname)?;
                let prog = parse_zasm(&source)?;
                self.assemble(&prog.stmts)?;
                */
            }

            Macro(mac)  => {
                self.macros.insert(mac.name.content, mac.clone());
            }

            /*
            Org(val)  => {
                if val.inner < 0 {
                    self.goto(self.tape.len() + val.inner as usize);
                } else {
                    self.goto(val.inner as usize);
                }
                Ok(0)
            },
            */
        }

        Ok(())
    }

    fn assemble_lit_stmt(&mut self, lit: &ast::Lit<'a>) -> Result<()> {
        use ast::Lit;

        match lit {
            Lit::Expr(expr)   => {
                let val = self.assemble_expr(expr)?;

                if val <= u16::MAX as i32 && val >= i8::MIN as i32 {
                    self.emit_word(val as u16)?;
                } else {
                    return error!("literal must fit in one byte", expr.span());
                }
            }

            Lit::Str(_)       => {
                return error!("string not allowed here", lit.span());
            },
        }

        Ok(())
    }

    fn assemble_inst(&mut self, inst: &ast::Inst<'a>) -> Result<()> {
        use ast::Arg;

        let span = inst.span();
        let inst_name = inst.ident.content;

        match inst_name {
            // R - type instructions
            "add"  |
            "mult" |
            "div"  |
            "mov"  |
            "and"  |
            "or"   |
            "not"  |
            "shl"  |
            "shr"  |
            "ceq"  |
            "clt"  => {
                let (dest, src) = match inst.args.as_slice() {
                    [Arg::Reg(dest), Arg::Reg(src)] => (dest, src),
                    [_, _] => return error!(
                        "expected register arguments",
                        span
                    ),
                    _ => return error!(
                       format!("expected 2 arguments, but got {}", inst.args.len()),
                       span
                    ),
                };

                let opt = match inst_name {
                    "add"  =>  0,
                    "mult" =>  1,
                    "mov"  =>  2,
                    "div"  =>  3,
                    "and"  =>  4,
                    "or"   =>  5,
                    "not"  =>  6,
                    "shl"  =>  7,
                    "shr"  =>  8,
                    "ceq"  =>  9,
                    "clt"  => 10,
                    _      => unreachable!(),
                };

                let opcode = 1;

                let mut encoded: u16 = 0;
                encoded |= opcode << 12;
                encoded |= (dest.addr as u16) << 8;
                encoded |= ( src.addr as u16) << 4;
                encoded |= opt;
                self.emit_word(encoded)?;
            }

            // J - type instructions
            "beq"  |
            "bne"  |
            "jmp"  => {
                let target = match inst.args.as_slice() {
                    [Arg::Reg(target)] => target,
                    [_] => return error!(
                        "expected argument to be a register",
                        span
                    ),
                    _ => return error!(
                        format!("expected 1 argument, but got {}", inst.args.len()),
                        span
                    ),
                };

                let opt = match inst_name {
                    "jmp"  => 0,
                    "beq"  => 1,
                    "bne"  => 2,
                    _      => unreachable!(),
                };
                
                let opcode = 2;

                let mut encoded: u16 = 0;
                encoded |= opcode << 12;
                encoded |= (target.addr as u16) << 8;
                encoded |= opt;
                self.emit_word(encoded)?;
            }

            // I - type instructions
            "addi" |
            "andi" |
            "lui"  => {
                let (dest, imm) = match inst.args.as_slice() {
                    [Arg::Reg(dest), Arg::Imm(imm)] => {
                        let imm = self.assemble_immediate(imm)?;
                        (dest, imm)
                    }

                    [_, _] => return error!(
                        "expected a register and an immediate value as arguments",
                        span
                    ),

                    _ => return error!(
                        format!("expected 2 arguments, but got {}", inst.args.len()),
                        span
                    ),
                };

                let opcode = match inst_name {
                    "addi" => 3,
                    "lui"  => 4,
                    "andi" => 5,
                    _      => unreachable!(),
                };

                let mut encoded: u16 = 0;
                encoded |= opcode << 12;
                encoded |= (dest.addr as u16) << 8;
                encoded |= imm as u16;
                self.emit_word(encoded)?;
            }

            "ldb"  |
            "stb"  |
            "ldw"  |
            "stw"  => {
                let (dest, reg, imm) = match inst.args.as_slice() {
                    [Arg::Reg(dest), Arg::RegImm(reg, imm)] => {
                        let imm = self.assemble_immediate(imm)?;
                        (dest, reg, imm)
                    }

                    [_, _] => return error!(
                        "expected first register and register immediate arguments",
                        span
                    ),

                    _ => return error!(
                        format!("expected 2 arguments, but got {}", inst.args.len()),
                        span
                    ),
                };

                let opcode = match inst_name {
                    "ldb"  => 6,
                    "stb"  => 7,
                    "ldw"  => 8,
                    "stw"  => 9,
                    _      => unreachable!(),
                };

                let mut encoded: u16 = 0;
                encoded |= opcode << 12;
                encoded |= (dest.addr as u16) << 8;
                encoded |= ( reg.addr as u16) << 4;
                encoded |= imm as u16;
                self.emit_word(encoded)?;
            }

            "int"  => {
                let opcode = 10;

                let mut encoded: u16 = 0;
                encoded |= opcode << 12;
                self.emit_word(encoded)?;
            }

            "hlt" => {
                let opcode = 11;

                let mut encoded: u16 = 0;
                encoded |= opcode << 12;
                self.emit_word(encoded)?;
            }

            mac if self.macros.contains_key(mac) => {
                let mac = self.macros.get(mac).unwrap();
                
                if mac.args.len() != inst.args.len() {
                    return error!(
                        format!("expected {} arguments, but got {}", mac.args.len(), inst.args.len()),
                        span
                    );
                }

                for (macro_arg, inst_arg) in mac.args.iter().zip(&inst.args) {
                    self.macro_ctx.macro_args.insert(macro_arg.content, inst_arg.clone());
                }

                let stmts = parse_stmts(mac.contents.clone(), Some(&mut self.macro_ctx))?;
                self.assemble_stmts(&stmts)?;
            }

            _      => return error!("instruction not supported", span),
            /*
            "get",
            "put",
            */
        }
        Ok(())
    }

    fn assemble_immediate(&mut self, lit: &ast::Lit<'a>) -> Result<u8> {
        use ast::Lit;

        match lit {
            Lit::Expr(expr) => {
                let val = self.assemble_expr(expr)?;
                if val <= u8::MAX as i32 && val >= i8::MIN as i32 {
                    Ok(val as u8)
                } else {
                    error!("literal must fit in one byte", expr.span())
                }
            },
            Lit::Str(_) =>
                return error!("literal string not allowed in argument position", lit.span()),
        }
    }

    fn assemble_expr(&mut self, expr: &ast::Expr<'a>) -> Result<i32> {
        use ast::{ Expr, Num, Chr, BinExpr };

        let span = expr.span();

        match expr {
            Expr::Num(Num { val, .. }) => Ok(*val as i32),

            Expr::Chr(Chr { chr, .. }) => {
                if !chr.is_ascii() {
                    return error!("only ASCII characters are allowed", span);
                } else {
                    Ok(*chr as i32)
                }
            }

            Expr::Lbl(lbl) => {
                self.get_label(lbl).map(|val| val as i32)
            }

            Expr::Bin(BinExpr { lhs, operator, rhs, .. }) => {
                let a = self.assemble_expr(lhs.as_ref())?;
                let b = self.assemble_expr(rhs.as_ref())?;

                let res = match operator.content {
                    "+"  => a + b,
                    "-"  => a - b,
                    "*"  => a * b,
                    "/"  => a / b,
                    "|"  => a | b,
                    "&"  => a & b,
                    ">>" => a >> b,
                    "<<" => a << b,
                    s    => unreachable!("unexpected operator '{}'", s),
                };

                Ok(res)
            }
        }
    }

    fn get_label(&mut self, lbl: &ast::Label<'a>) -> Result<Addr> {
        use ast::Label;

        let span = lbl.span();

        let lbl_name = match lbl {
            Label::Local(local) => {
                if self.parent.is_none() {
                    return error!("local labels are only allowed inside parent labels", span);
                }

                let parent = self.parent.as_ref().map(|lbl| lbl.ident().content);
                LabelName::new(parent, local.content)
            }

            Label::Global(global) => {
                self.parent.replace(lbl.clone());
                LabelName::new(None, global.content)
            }

            Label::Macro(..) => {
                unreachable!()
            }
        };

        if let Some(def) = self.labels.get(&lbl_name).cloned() {
            Ok(def.addr)
        } else {
            error!("label not defined", lbl.span())
        }
    }

    fn add_label(&mut self, lbl: &ast::Label<'a>) -> Result<()> {
        use ast::Label;
        use std::collections::hash_map::Entry::*;

        let cur_pos = self.get_addr();
        let span = lbl.span();

        let lbl_name = match lbl {
            Label::Local(local) => {
                if self.parent.is_none() {
                    return error!("local labels are only allowed inside parent labels", span);
                }

                let parent = self.parent.as_ref().map(|lbl| lbl.ident().content);
                LabelName::new(parent, local.content)
            }

            Label::Global(global) => {
                self.parent.replace(lbl.clone());
                LabelName::new(None, global.content)
            }

            Label::Macro(..) => {
                unreachable!()
            }
        };

        match self.labels.entry(lbl_name) {
            Occupied(_)   => error!("label defined twice", lbl.span()),
            Vacant(entry) => {
                entry.insert(LabelDef::new(cur_pos, lbl.span()));
                Ok(())
            },
        }
    }
}

#[cfg(test)]
mod test {

    use std::io::Cursor;

    use super::*;
    use crate::parser::{ ParserRule, parse_zasm };
    use crate::parse;

    use pest::Parser;

    #[test]
    fn test_simple_loop() {
        let prog = parse! {
            r"
            loop:
                addi $r1, 10
                lui $r2, (loop >> 8)
                addi $r2, (loop & 0xff)
                jmp $r2
             " => ParserRule::zasm
        };

        let prog = parse_zasm(prog, None).unwrap();

        let mut assembled: Vec<u8> = Vec::new();
        let mut assembler = Assembler::new(Cursor::new(&mut assembled), false);

        assembler.assemble(prog.stmts.as_slice())
            .unwrap_or_else(|err| panic!("{}", err));

        dbg!(assembled);
    }

    #[test]
    fn test_simple_macro() {
        let prog = parse! {
            r"
            @macro li %reg, #imm {
                lui %reg, (#imm >> 8)
                addi %reg, (#imm & 0xff)
            }

            main:
                li $r2, 0x0fff
             " => ParserRule::zasm
        };

        let prog = parse_zasm(prog, None)
            .unwrap_or_else(|err| panic!("{}", err));

        let mut assembled: Vec<u8> = Vec::new();
        let mut assembler = Assembler::new(Cursor::new(&mut assembled), false);

        assembler.assemble(prog.stmts.as_slice())
            .unwrap_or_else(|err| panic!("{}", err));

        dbg!(assembled);
    }

}
