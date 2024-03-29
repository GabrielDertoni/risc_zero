#![allow(unused_macros)]

use std::collections::HashMap;
use std::io::{ Seek, SeekFrom, Write };

use pest::Span;
use architecture_utils as risc0;

use crate::ast;
use crate::parser::{ Context, parse_src, parse_stmts };
use crate::error::Result;
use crate::error;

type Addr = usize;
type Word = u16;

#[derive(Debug, PartialEq, Eq, Clone)]
struct LabelDef<'a> {
    addr: Addr,
    span: Option<Span<'a>>,
}

impl<'a> LabelDef<'a> {
    fn new(pos: Addr, span: Span<'a>) -> LabelDef<'a> {
        LabelDef { addr: pos, span: Some(span) }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
struct LabelName<'a> {
    parent: Option<&'a str>,
    name: &'a str,
    id: usize,
}

impl<'a> LabelName<'a> {
    fn new(parent: Option<&'a str>, name: &'a str) -> LabelName<'a> {
        LabelName { parent, name, id: 0 }
    }

    fn new_macro(name: &'a str, id: usize) -> LabelName<'a> {
        LabelName { parent: None, name, id }
    }
}

pub struct Assembler<'a, W> {
    writer: W,
    offset: u16,
    curr_source: Option<String>,
    sources: HashMap<String, Box<str>>,
    parent: Option<ast::Ident<'a>>,
    labels: HashMap<LabelName<'a>, LabelDef<'a>>,
    defines: HashMap<&'a str, ast::Expr<'a>>,
    macros: HashMap<&'a str, ast::Macro<'a>>,
    macro_ctx: Context<'a>,
    in_text: bool,
    inst_count: u16,
    data_count: u16,
    entry_point: Option<Addr>,
}

impl<'a, W> Assembler<'a, W>
where
    W: Write + Seek,
{
    pub fn new(writer: W, offset: u16) -> Assembler<'a, W> {
        Assembler {
            writer,
            offset,
            curr_source: None,
            sources: HashMap::new(),
            parent: None,
            labels: HashMap::new(),
            defines: HashMap::new(),
            macros: HashMap::new(),
            macro_ctx: Context::new(),
            in_text: true,
            inst_count: 0,
            data_count: 0,
            entry_point: None,
        }
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
        let mut n_bytes = 0;

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
                            span,
                            self.get_source_name()
                        ),
                }
            } else {
                bytes[i]
            };

            self.emit_byte(byte)?;
            i += 1;
            n_bytes += 1;
        }

        Ok(n_bytes)
    }

    // TODO: remove this or find a better way of doing it
    fn count_string_bytes(&self, s: &str) -> usize {
        let bytes = s.as_bytes();
        let mut i = 0;
        let mut n_bytes = 0;

        while i < bytes.len() {
            if bytes[i] == b'\\' {
                i += 1;
            }
            i += 1;
            n_bytes += 1;
        }

        n_bytes
    }

    fn add_src(&mut self, src_file: &str) -> Result<Option<&'a str>> {
        use std::collections::hash_map::Entry;

        let source = match self.sources.entry(src_file.to_string()) {
            Entry::Occupied(..)  => return Ok(None),
            Entry::Vacant(entry) =>
                entry.insert(std::fs::read_to_string(src_file)?.into_boxed_str()),
        };

        // This is hopefully ok because we are only ever getting a reference to the buffer of the
        // string, not the string struct itself, if the string never changes, we should be fine
        // even if the map itself changes.
        unsafe {
            Ok(Some(std::mem::transmute(source.as_ref())))
        }
    }

    pub fn assemble_program(&mut self, src_file: &str) -> Result<()> {
        self.curr_source.replace(src_file.to_string());
        if let Some(src) = self.add_src(src_file)? {
            let prog = parse_src(src, self.get_source_name())?;
            self.assemble_main(&prog)?;
        }
        Ok(())
    }


    fn assemble_main(&mut self, prog: &ast::Prog<'a>) -> Result<()> {
        // Write a bunch of zeros so that we can go past the header position
        self.writer.write(&[0; risc0::FileHeader::SIZE as usize])?;

        let mut offset = self.offset as usize;
        self.assemble(&prog.stmts, &mut offset)?;

        self.writer.seek(SeekFrom::Start(0))?;

        let data_start = (risc0::FileHeader::SIZE as u16) + self.inst_count * 2;
        let data_end = data_start + self.data_count;

        if let Some(entry_point) = self.entry_point {
            let header = risc0::FileHeader::new(data_start, data_end, entry_point as u16);
            header.write_to(&mut self.writer)?;
        } else {
            return error!(
                "could not find an entry point",
                prog.span.clone(),
                self.get_source_name()
            );
        }


        Ok(())
    }

    fn assemble_src(&mut self, src_file: &str, offset: &mut usize) -> Result<()> {
        let old = self.curr_source.replace(src_file.to_string());

        if let Some(src) = self.add_src(src_file)? {
            let prog = parse_src(src, self.get_source_name())?;
            self.assemble(&prog.stmts, offset)?;
        }

        self.curr_source = old;
        Ok(())
    }


    fn assemble(&mut self, stmts: &[ast::Stmt<'a>], offset: &mut usize) -> Result<()> {
        self.find_labels_and_macros(stmts, offset)?;
        self.parent = None;
        self.assemble_stmts(stmts)?;
        self.parent = None;

        Ok(())
    }

    fn find_labels_and_macros(&mut self, stmts: &[ast::Stmt<'a>], offset: &mut usize) -> Result<()> {
        use ast::Stmt::*;

        for stmt in stmts {
            match stmt {
                Marker(..)   => (),

                Include(s)   => {
                    self.assemble_src(s.content, offset)?;
                }

                Define(name, lit) => {
                    let old = self.defines.insert(name.content, lit.clone());

                    if old.is_some() {
                        return error!("macro defined twice", name.span(), self.get_source_name());
                    }
                }

                Macro(mac)   => {
                    self.macros.insert(mac.name.content, mac.clone());
                }

                Label(label) => self.add_label_to(label, *offset)?,

                Inst(inst)   => {
                    if self.is_valid_instruction(inst.ident.content) {
                        *offset += 2;
                    } else if self.macros.contains_key(&inst.ident.content) {
                        // TODO: this is not very efficient, fix it!

                        self.macro_ctx.macro_call_stack.push((inst.span(), self.get_source_name().to_string()));
                        let mac = self.macros.get(&inst.ident.content).unwrap();

                        if mac.args.len() != inst.args.len() {
                            return error!(
                                format!(
                                    "expected {} arguments, but got {}\nmacro traceback:\n{}",
                                    mac.args.len(),
                                    inst.args.len(),
                                    self.macro_ctx.traceback()
                                ),
                                inst.span(),
                                self.get_source_name()
                            );
                        }

                        for (macro_arg, inst_arg) in mac.args.iter().zip(&inst.args) {
                            self.macro_ctx.insert_macro_arg(macro_arg.content, inst_arg.clone());
                        }

                        let stmts = parse_stmts(mac.body.clone(), Some(&mut self.macro_ctx))?;
                        self.find_labels_and_macros(&stmts, offset)?;
                        self.macro_ctx.remove_macro_args_in_depth();
                        self.macro_ctx.macro_call_stack.pop();
                    }
                }

                Expr(..)     => *offset += 2,
                Str(s)       => *offset += self.count_string_bytes(s.content),
            }
        }

        Ok(())
    }

    fn is_valid_instruction(&self, name: &str) -> bool {
        match name {
            "noop" | "not"  | "add"  | "sub"  | "mult" | "mov"  | "div"  |
            "and"  | "or"   | "shl"  | "shr"  | "cmp"  | "addi" | "andi" |
            "lui"  | "jmp"  | "beq"  | "bne"  | "blt"  | "ble"  | "bgt"  |
            "bge"  | "ldb"  | "stb"  | "ldw"  | "stw"  | "int"  | "hlt"
                  => true,
            _     => false,
        }
    }

    #[inline]
    fn assemble_stmts(&mut self, stmts: &[ast::Stmt<'a>]) -> Result<()> {
        stmts.into_iter()
            .map(|stmt| self.assemble_stmt(stmt))
            .collect()
    }

    fn assemble_stmt(&mut self, stmt: &ast::Stmt<'a>) -> Result<()> {
        use ast::{ Stmt::*, Label };

        match stmt {
            Marker(ast::Marker::DotText(span)) => {
                if !self.in_text {
                    return error!(
                        "unexpected .text in data segment",
                        span.clone(),
                        self.get_source_name()
                    );
                }
            }

            Marker(ast::Marker::DotData(..)) => {
                self.in_text = false;
            }

            Marker(ast::Marker::Entry(entry_lbl, span)) => {
                let entry_point = self.get_label(entry_lbl)?;
                if self.entry_point.is_some() {
                    return error!(
                        "entry point already defined",
                        span.clone(),
                        self.get_source_name()
                    );
                }
                self.entry_point.replace(entry_point as usize);
            }

            // Assumes the label has already been accounted for in the first pass.
            Label(Label::Global(global)) => {
                self.parent.replace(global.clone());
            }

            Label(..) => (),

            Inst(inst) => {
                if !self.in_text {
                    return error!(
                        "unexpected instruction in .data segment",
                        inst.span(),
                        self.get_source_name()
                    );
                }
                self.assemble_inst(inst)?;
            }

            Expr(expr) => {
                if self.in_text {
                    return error!(
                        "unexpected expression in .text segment",
                        expr.span(),
                        self.get_source_name()
                    );
                }
                // Emits a word
                self.data_count += 2;
                self.assemble_expr_stmt(expr)?;
            }

            Str(s) => {
                if self.in_text {
                    return error!(
                        "unexpected string in .text segment",
                        s.span(),
                        self.get_source_name()
                    );
                }
                self.data_count += self.emit_string(s.content, s.span())? as u16;
            }

            // Assumes macros have already been resolved
            Macro(..)  |
            Define(..) |
            Include(..) => (),
        }

        Ok(())
    }

    fn assemble_expr_stmt(&mut self, expr: &ast::Expr<'a>) -> Result<()> {
        let val = self.assemble_expr(expr)?;

        if val <= u16::MAX as i32 && val >= i16::MIN as i32 {
            self.emit_word(val as u16)?;
        } else {
            return error!("literal must fit in one word", expr.span(), self.get_source_name());
        }

        Ok(())
    }

    fn assemble_inst(&mut self, inst: &ast::Inst<'a>) -> Result<()> {
        use ast::Arg;
        use architecture_utils::Instruction::*;

        let span = inst.span();
        let inst_name = inst.ident.content;

        self.inst_count += 1;

        match inst_name {
            // R - type instructions
            "not" => {
                let dest = match inst.args.as_slice() {
                    [Arg::Reg(dest)] => dest.addr,
                    [_] => return error!(
                        format!(
                            "expected a register argument\nmacro traceback:\n{}",
                            self.macro_ctx.traceback()
                        ),
                        span,
                        self.get_source_name()
                    ),
                    _ => return error!(
                        format!(
                            "expected 1 argument, but got {}\nmacro traceback:\n{}",
                            inst.args.len(),
                            self.macro_ctx.traceback()
                        ),
                        span,
                        self.get_source_name()
                    ),
                };

                self.emit_word(Not(dest).encode())?;
            }

            "add" | "sub" | "mult" | "div" | "mov" | "and" |
            "or"  | "shl" | "shr"  | "cmp"  => {
                let (dest, src) = match inst.args.as_slice() {
                    [Arg::Reg(dest), Arg::Reg(src)] => (dest.addr, src.addr),
                    [_, _] => return error!(
                        format!(
                            "expected register arguments\nmacro traceback:\n{}",
                            self.macro_ctx.traceback()
                        ),
                        span,
                        self.get_source_name()
                    ),
                    _ => return error!(
                       format!(
                           "expected 2 arguments, but got {}\nmacro traceback:\n{}",
                           inst.args.len(),
                           self.macro_ctx.traceback()
                       ),
                       span,
                       self.get_source_name()
                    ),
                };

                let inst = match inst_name {
                    "add"  => Add(dest, src),
                    "sub"  => Sub(dest, src),
                    "mult" => Mult(dest, src),
                    "mov"  => Mov(dest, src),
                    "div"  => Div(dest, src),
                    "and"  => And(dest, src),
                    "or"   => Or(dest, src),
                    "shl"  => Shl(dest, src),
                    "shr"  => Shr(dest, src),
                    "cmp"  => Cmp(dest, src),
                    _      => unreachable!(),
                };

                self.emit_word(inst.encode())?;
            }

            // J - type instructions
            "jmp" | "beq" | "bne" | "blt" | "ble" | "bgt" | "bge" => {
                let target = match inst.args.as_slice() {
                    [Arg::Reg(target)] => target.addr,
                    [_] => return error!(
                        format!(
                            "expected argument to be a register\nmacro traceback:\n{}",
                            self.macro_ctx.traceback()
                        ),
                        span,
                        self.get_source_name()
                    ),
                    _ => return error!(
                        format!(
                            "expected 1 argument, but got {}\nmacro traceback:\n{}",
                            inst.args.len(),
                            self.macro_ctx.traceback()
                        ),
                        span,
                        self.get_source_name()
                    ),
                };

                let inst = match inst_name {
                    "jmp" => Jmp(target),
                    "beq" => Beq(target),
                    "bne" => Bne(target),
                    "blt" => Blt(target),
                    "ble" => Ble(target),
                    "bgt" => Bgt(target),
                    "bge" => Bge(target),
                    _     => unreachable!(),
                };

                self.emit_word(inst.encode())?;
            }

            // I - type instructions
            "addi" | "andi" | "lui" => {
                let (dest, imm) = match inst.args.as_slice() {
                    [Arg::Reg(dest), Arg::Imm(imm)] => {
                        let imm = self.assemble_immediate(imm)?;
                        (dest.addr, imm)
                    }

                    [_, _] => return error!(
                        format!(
                            "expected a register and an immediate value as arguments\nmacro traceback:\n{}",
                            self.macro_ctx.traceback()
                        ),
                        span,
                        self.get_source_name()
                    ),

                    _ => return error!(
                        format!(
                            "expected 2 arguments, but got {}\nmacro traceback:\n{}",
                            inst.args.len(),
                            self.macro_ctx.traceback()
                        ),
                        span,
                        self.get_source_name()
                    ),
                };

                let inst = match inst_name {
                    "addi" => Addi(dest, imm),
                    "andi" => Andi(dest, imm),
                    "lui"  => Lui(dest, imm),
                    _      => unreachable!(),
                };

                self.emit_word(inst.encode())?;
            }

            "ldb" | "stb" | "ldw" | "stw"  => {
                let (dest, reg, imm) = match inst.args.as_slice() {
                    [Arg::Reg(dest), Arg::RegImm(reg, imm)] => {
                        let imm = self.assemble_immediate(imm)?;
                        (dest.addr, reg.addr, imm)
                    }

                    [_, _] => return error!(
                        format!(
                            "expected first register and register immediate arguments\nmacro traceback:\n{}",
                            self.macro_ctx.traceback()
                        ),
                        span,
                        self.get_source_name()
                    ),

                    _ => return error!(
                        format!(
                            "expected 2 arguments, but got {}\nmacro traceback:\n{}",
                            inst.args.len(),
                            self.macro_ctx.traceback()
                        ),
                        span,
                        self.get_source_name()
                    ),
                };

                let inst = match inst_name {
                    "ldb"  => Ldb(dest, reg, imm),
                    "stb"  => Stb(dest, reg, imm),
                    "ldw"  => Ldw(dest, reg, imm),
                    "stw"  => Stw(dest, reg, imm),
                    _      => unreachable!(),
                };

                self.emit_word(inst.encode())?;
            }

            "int" => {
                self.emit_word(Int.encode())?;
            }

            "hlt" => {
                self.emit_word(Hlt.encode())?;
            }

            mac if self.macros.contains_key(mac) => {
                // We have overcounted the number of instructions because one of them was actually
                // a macro.
                self.inst_count -= 1;

                let mac = self.macros.get(mac).unwrap();
                
                if mac.args.len() != inst.args.len() {
                    return error!(
                        format!(
                            "expected {} arguments, but got {}\nmacro traceback:\n{}",
                            mac.args.len(),
                            inst.args.len(),
                            self.macro_ctx.traceback()
                        ),
                        span,
                        self.get_source_name()
                    );
                }

                for (macro_arg, inst_arg) in mac.args.iter().zip(&inst.args) {
                    let argument = if let ast::Arg::Imm(expr) = inst_arg {
                        ast::Arg::Imm(
                            ast::Expr::Num(
                                ast::Num::new(
                                    self.assemble_expr(expr)?,
                                    span.clone(),
                                )
                            )
                        )
                    } else {
                        inst_arg.clone()
                    };

                    self.macro_ctx.insert_macro_arg(macro_arg.content, argument);
                }

                self.macro_ctx.macro_call_stack.push((span, self.get_source_name().to_string()));
                let stmts = parse_stmts(mac.body.clone(), Some(&mut self.macro_ctx))?;
                self.assemble_stmts(&stmts)?;
                self.macro_ctx.remove_macro_args_in_depth();
                self.macro_ctx.macro_call_stack.pop();
            }

            _ => return error!("instruction not supported", span, self.get_source_name()),
        }
        Ok(())
    }

    fn assemble_immediate(&mut self, expr: &ast::Expr<'a>) -> Result<u8> {
        let val = self.assemble_expr(expr)?;

        if val <= u8::MAX as i32 && val >= i8::MIN as i32 {
            Ok(val as u8)
        } else {
            error!("literal must fit in one byte", expr.span(), self.get_source_name())
        }
    }

    fn assemble_expr(&self, expr: &ast::Expr<'a>) -> Result<i32> {
        use ast::{ Expr, Num, Chr, BinExpr };

        let span = expr.span();

        match expr {
            Expr::Num(Num { val, .. }) => Ok(*val as i32),

            Expr::Chr(Chr { chr, .. }) => {
                if !chr.is_ascii() {
                    return error!("only ASCII characters are allowed", span, self.get_source_name());
                } else {
                    Ok(*chr as i32)
                }
            }

            Expr::Lbl(lbl) => {
                self.get_label(lbl)
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

    fn get_label(&self, lbl: &ast::Label<'a>) -> Result<i32> {
        use ast::Label;

        let span = lbl.span();

        let lbl_name = match lbl {
            Label::Local(local) => {
                if self.parent.is_none() {
                    return error!("local labels are only allowed inside parent labels", span, self.get_source_name());
                }

                let parent = self.parent.as_ref().map(|lbl| lbl.content);
                LabelName::new(parent, local.content)
            }

            Label::Global(global) => {
                let global_name = LabelName::new(None, global.content);

                let mac = LabelName::new_macro(global.content, self.macro_ctx.macro_depth());

                if self.labels.contains_key(&global_name) {
                    global_name
                } else {
                    mac
                }
            },
            Label::Macro(..)      => unreachable!(),

        };

        if let Some(def) = self.labels.get(&lbl_name).cloned() {
            Ok(def.addr as i32)
        } else {
            // If it is a label, maybe it's actually a define. There is no way to diferentiate when
            // parsing between a label and a define with the same name.
            if let Label::Global(global) = lbl {
                if let Some(def) = self.defines.get(&global.content) {
                    return Ok(self.assemble_expr(def)? as i32)
                }
            }
            error!("label not defined", lbl.span(), self.get_source_name())
        }
    }

    fn add_label_to(&mut self, lbl: &ast::Label<'a>, addr: usize) -> Result<()> {
        use ast::Label;
        use std::collections::hash_map::Entry::*;

        let span = lbl.span();

        let lbl_name = match lbl {
            Label::Local(local) => {
                if self.parent.is_none() {
                    return error!("local labels are only allowed inside parent labels", span, self.get_source_name());
                }

                let parent = self.parent.as_ref().map(|lbl| lbl.content);
                LabelName::new(parent, local.content)
            }

            Label::Global(global) => {
                self.parent.replace(global.clone());
                LabelName::new(None, global.content)
            }

            Label::Macro(mac, id) => {
                // unreachable!()
                LabelName::new_macro(mac.content, *id)
            }
        };

        match self.labels.entry(lbl_name) {
            Occupied(_)   => error!("label defined twice", lbl.span(), self.get_source_name()),
            Vacant(entry) => {
                entry.insert(LabelDef::new(addr, lbl.span()));
                Ok(())
            },
        }
    }

    fn get_source_name(&self) -> &str {
        self.curr_source
            .as_ref()
            .map(String::as_str)
            .unwrap_or("???")
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
        let mut assembler = Assembler::new(Cursor::new(&mut assembled));

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
        let mut assembler = Assembler::new(Cursor::new(&mut assembled));

        assembler.assemble(prog.stmts.as_slice())
            .unwrap_or_else(|err| panic!("{}", err));

        dbg!(assembled);
    }

}
