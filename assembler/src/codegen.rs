#![allow(unused_macros)]

use std::collections::HashMap;
use std::collections::BTreeMap;

use pest::Span;

use crate::ast;
use crate::parser::Error;

macro_rules! error {
    ($msg:expr, $span:expr) => {
        Err(pest::error::Error::new_from_span(pest::error::ErrorVariant::CustomError { message: $msg.into() }, $span))
    };
}

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

const EMPTY_DEFAULT: i32 = -1;

type Result<T> = std::result::Result<T, Error>;

type Position = usize;

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

impl<'a> From<(&'a str, usize)> for Ident<'a> {
    #[inline]
    fn from((name, id): (&'a str, usize)) -> Ident<'a> {
        Ident::new(name, id)
    }
}

impl<'a> From<ast::Spanned<'a, (&'a str, usize)>> for Ident<'a> {
    #[inline]
    fn from(spanned: ast::Spanned<'a, (&'a str, usize)>) -> Ident<'a> {
        spanned.inner.into()
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

#[derive(PartialEq, Eq, Clone)]
struct LabelDef<'a> {
    pos: Position,
    span: Option<Span<'a>>,
}

impl<'a> LabelDef<'a> {
    fn new(pos: Position, span: Span<'a>) -> LabelDef<'a> {
        LabelDef { pos, span: Some(span) }
    }

    fn auto(pos: Position) -> LabelDef<'a> {
        LabelDef { pos, span: None }
    }
}

struct LabelRef<'a> {
    // Where the reference was on the tape.
    pos: Position,
    // The token that used the reference.
    span: Span<'a>,
}

impl<'a> LabelRef<'a> {
    #[inline]
    fn new(pos: Position, span: Span<'a>) -> LabelRef<'a> {
        LabelRef { pos, span }
    }
}

impl<'a> std::fmt::Display for LabelRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (line, col) = self.span.start_pos().line_col();
        write!(f, "{} at {}:{} -> {}", self.span.as_str(), line, col, self.pos)
    }
}

impl<'a> std::fmt::Debug for LabelRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

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

pub struct Assembler<'a> {
    tape: Vec<i32>,
    pos: usize,
    expand: bool,
    labels: HashMap<Ident<'a>, LabelDef<'a>>,
    locals: HashMap<Ident<'a>, LabelDef<'a>>,
    lit_uses: BTreeMap<Auto<'a>, Vec<LabelRef<'a>>>,
    macro_count: usize,
}

impl<'a> Assembler<'a> {
    pub fn new(tape_size: usize, expand: bool) -> Assembler<'a> {
        Assembler {
            tape: vec![0; tape_size],
            pos: 0,
            expand,
            labels: HashMap::new(),
            locals: HashMap::new(),
            lit_uses: BTreeMap::new(),
            macro_count: 0,
        }
    }

    fn goto(&mut self, pos: usize) {
        self.pos = pos;
    }

    fn get_pos(&self) -> usize {
        self.pos
    }

    fn push_tape(&mut self, v: i32) {
        if self.pos >= self.tape.len() {
            panic!("TAPE SIZE EXCEEDED");
        }
        self.tape[self.pos] = v;
        self.pos += 1;
    }

    fn push_string(&mut self, s: &str, span: Span<'a>) -> Result<usize> {
        let bytes = s.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'\\' {
                i += 1;
                let escaped = match bytes[i] {
                    b'n'  => b'\n',
                    b'r'  => b'\r',
                    b't'  => b'\t',
                    b'\\' => b'\\',
                    b'\'' => b'\'',
                    b'0'  => b'\0',
                    other => return error!(format!("invalid escape character '\\{}'", other as char), span),
                };
                self.push_tape(escaped as i32);
            } else {
                self.push_tape(bytes[i] as i32);
            }
            i += 1
        }

        Ok(i)
    }

    pub fn assemble(mut self, stmts: &[ast::Stmt<'a>]) -> Result<Vec<i32>> {
        self.assemble_stmts(stmts)?;

        self.solve_locals()?;
        self.add_auto_lbls()?;

        #[derive(PartialEq, Eq)]
        enum SolveState {
            Labels,
            Nums,
            Strs,
        }

        let mut prev_state = SolveState::Labels;
        let mut prev_lvl = 0;
        let mut prev_val = 0;

        let mut prev_lbl_name = "__none";
        let mut prev_num = 0;
        let mut prev_str = "";

        for (auto, uses) in std::mem::take(&mut self.lit_uses) {
            match auto {
                Auto::Lbl(lbl, ref_lvl) => {
                    if prev_lbl_name != lbl.0 {
                        prev_lvl = 0;
                        if let Some(def) = self.labels.get(&lbl) {
                            prev_val = def.pos as i32;
                        } else {
                            return error!(format!("label \"{}\" was not defined", lbl.0), uses[0].span.clone());
                        }
                    }

                    for _ in prev_lvl + 1..=ref_lvl {
                        let pos = self.get_pos();
                        self.push_tape(prev_val);
                        prev_val = pos as i32;
                    }
                    prev_lbl_name = lbl.0;
                    prev_lvl = ref_lvl;
                },
                Auto::Num(num, ref_lvl) => {
                    if prev_state != SolveState::Nums || prev_num != num {
                        prev_state = SolveState::Nums;
                        prev_val = num;
                        prev_lvl = 0;
                    }

                    for _ in prev_lvl + 1..=ref_lvl {
                        let pos = self.get_pos();
                        self.push_tape(prev_val);
                        prev_val = pos as i32;
                    }
                    prev_lvl = ref_lvl;
                    prev_num = num;
                },
                Auto::Str(s, ref_lvl) => {
                    if prev_state != SolveState::Strs || prev_str != s {
                        prev_state = SolveState::Strs;
                        prev_val = self.get_pos() as i32;

                        self.push_string(s, uses[0].span.clone())?;

                        prev_lvl = 1;
                    }

                    for _ in prev_lvl + 1..=ref_lvl {
                        let pos = self.get_pos();
                        self.push_tape(prev_val);
                        prev_val = pos as i32;
                    }
                    prev_lvl = ref_lvl;
                    prev_str = s;
                },
            }

            // println!("Solving uses of {}, {:?}", auto, uses);
            for lbl_ref in &uses {
                self.tape[lbl_ref.pos] = prev_val;
            }
        }

        Ok(self.tape)
    }

    fn assemble_stmts(&mut self, stmts: &[ast::Stmt<'a>]) -> Result<usize> {
        let mut count = 0;
        for stmt in stmts {
            count += self.assemble_stmt(stmt)?;
        }
        Ok(count)
    }

    fn assemble_stmt(&mut self, stmt: &ast::Stmt<'a>) -> Result<usize> {
        use ast::Stmt::*;

        match stmt {
            Label(lbl) if Ident::from(lbl.inner).is_local() => {
                self.add_local_lbl(lbl)?;
                Ok(0)
            },
            Label(lbl) => {
                self.add_global_lbl(lbl)?;
                if self.expand {
                    print!("{}", lbl.0);
                    if lbl.1 > 0 {
                        print!("_{:02x}", lbl.1);
                    }
                    println!(":");
                }
                Ok(0)
            },
            Inst(inst) => self.assemble_inst(inst),
            Lit(lit)   => {
                if self.expand {
                    if self.labels.len() > 0 {
                        print!("\t");
                    }
                    println!("{}", lit);
                }
                self.assemble_lit(lit)
            },
            Org(val)  => {
                if val.inner < 0 {
                    self.goto(self.tape.len() + val.inner as usize);
                } else {
                    self.goto(val.inner as usize);
                }
                Ok(0)
            },
        }
    }

    fn assemble_inst(&mut self, inst: &ast::Inst<'a>) -> Result<usize> {
        use ast::Op::*;
        use ast::{ Arg, Lit };

        let mut count = 0;
        match inst.op {
            Add | Mul | Cle | Ceq | Beq |
            Cpy | Jmp | Put | Ptn | Hlt => {
                // The maximum number of arguments in a single instruction is 3.
                let mut arg_vals = [0; 3];

                let mut desugared_inst = inst.clone();

                // First resolve all of the derefs
                for (i, arg) in inst.args.iter().enumerate() {
                    let span = arg.span();
                    match arg {
                        Arg::Lit(lit) => {
                            if let Lit::Deref(box deref) = ast::reduce_lit(lit) {

                                let macro_lbl = self.unique_lbl(".__deref_arg", span);
                                let gen = self.assemble_deref_arg(&deref, &macro_lbl)?;

                                let pos = self.get_pos();
                                self.add_local_lbl_to(&macro_lbl, pos + i + 1)?;

                                count += gen;
                                desugared_inst.args[i] = ast::Arg::Lbl(macro_lbl);
                            }
                        },
                        _ => (),
                    }
                }

                // Now get arg values.
                for (i, arg) in inst.args.iter().enumerate() {
                    arg_vals[i] = self.assemble_arg(arg, i)?;
                }

                if self.expand {
                    if self.labels.len() > 0 {
                        print!("\t");
                    }
                    println!("{}", desugared_inst);
                }

                self.push_tape(inst.op as i32);
                for i in 0..inst.args.len() {
                    self.push_tape(arg_vals[i]);
                }
                count += 1 + inst.args.len();
            },


            Psh => {
                let arg = inst.args[0].clone();
                let sp = ("sp", 0);
                count += self.assemble_stmts(stmts! { inst.span.clone() =>
                    [Cpy arg (% @* [sp])]
                    [Add (% [sp]) (% @& (# -1)) (% [sp])]
                })?;
            },
            Pop => {
                let arg = inst.args[0].clone();
                let sp = ("sp", 0);
                count += self.assemble_stmts(stmts! { inst.span.clone() => 
                    [Add (% [sp]) (% @& (# 1)) (% [sp])]
                    [Cpy (% @* [sp]) arg]
                })?;
            },
            Cal => {
                let jmp_back = self.unique_lbl(".__ret", inst.span.clone());
                let procedure_lbl = inst.args[0].clone();

                // This version puts the return address in the stack.
                count += self.assemble_stmts(stmts! { inst.span.clone() => 
                    [Psh (% @& jmp_back.clone())]
                    [Jmp procedure_lbl]
                    [label jmp_back]
                })?;
            },
            Ret => {
                let tmp = ("__tmp", 0);
                // This version pops the return address from the stack.
                count += self.assemble_stmts(stmts! { inst.span.clone() => 
                    [Pop (% [tmp])]
                    [Jmp (% [tmp])]
                })?;
            },
        }
        Ok(count)
    }

    fn assemble_deref_arg(&mut self, lit: &ast::Lit<'a>, macro_lbl: &ast::Label<'a>) -> Result<usize> {
        use ast::{ Lit, Spanned };

        match lit {
            Lit::Num(Spanned { span, .. }) |
            Lit::Chr(Spanned { span, .. }) |
            Lit::Str(Spanned { span, .. }) =>
                error!("cannot dereference this type", span.clone()),

            Lit::Lbl(_) |
            Lit::Deref(_) =>
                self.assemble_stmts(stmts! { lit.span() =>
                    [Cpy (% lit.clone()) (% [macro_lbl.inner])]
                }),

            Lit::Ref(_) =>
                unreachable!("In this case it means that there would be a *&. \
                              But this should already have been reduced by this step."),
        }
    }

    fn assemble_arg(&mut self, arg: &ast::Arg<'a>, arg_idx: usize) -> Result<i32> {
        use ast::{ Arg, Lit };

        let arg_pos = self.get_pos() + arg_idx + 1;
        match arg {
            Arg::Lbl(lbl) => {
                self.add_local_lbl_to(lbl, arg_pos)?;
                Ok(arg_pos as i32)
            },
            Arg::Lit(lit) => {
                match lit {
                    Lit::Num(num)   => Ok(num.inner),
                    Lit::Chr(chr)   => Ok(chr.inner as i32),
                    Lit::Lbl(lbl)   => Ok(self.get_label(lbl, arg_pos) as i32),
                    Lit::Ref(box r) => self.get_value(r, 1, arg_pos).map(|v| v as i32),
                    Lit::Str(s)     => error!("string literal in argument position is not allowed", s.span()),
                    Lit::Deref(_)   => Ok(EMPTY_DEFAULT),
                }
            },
        }
    }

    fn assemble_lit(&mut self, lit: &ast::Lit<'a>) -> Result<usize> {
        use ast::Lit;

        match lit {
            Lit::Num(num)     => {
                self.push_tape(**num);
                Ok(1)
            },
            Lit::Chr(chr)     => {
                self.push_tape(**chr as i32);
                Ok(1)
            },
            Lit::Str(s)       => {
                self.push_string(s.inner, s.span.clone())
            },
            // TODO: Remove this requirement.
            Lit::Lbl(lbl)     => {
                let val = self.get_label(lbl, self.get_pos()) as i32;
                self.push_tape(val);
                Ok(1)
            },
            Lit::Ref(box r)   => {
                let val = self.get_value(r, 1, self.get_pos())? as i32;
                self.push_tape(val);
                Ok(1)
            },
            Lit::Deref(box d) => return error!("derefs not allowed here", d.span())
        }
    }

    fn get_value(&mut self, mut lit: &ast::Lit<'a>, mut ref_lvl: u32, use_pos: usize) -> Result<Position> {
        use ast::Lit;

        loop {
            match lit {
                Lit::Chr(chr)   => {
                    break self.lit_uses
                        .entry(Auto::Num(chr.inner as i32, ref_lvl))
                        .or_default()
                        .push(LabelRef::new(use_pos, chr.span()));
                    },
                Lit::Num(num)   => {
                    break self.lit_uses
                        .entry(Auto::Num(num.inner, ref_lvl))
                        .or_default()
                        .push(LabelRef::new(use_pos, num.span()));
                    },
                Lit::Str(s)     => {
                    break self.lit_uses
                        .entry(Auto::Str(s.inner, ref_lvl))
                        .or_default()
                        .push(LabelRef::new(use_pos, s.span()));
                    },
                Lit::Lbl(lbl)   => {
                    break self.lit_uses
                        .entry(Auto::Lbl(Ident::from(lbl.inner), ref_lvl))
                        .or_default()
                        .push(LabelRef::new(use_pos, lbl.span()));
                    },
                Lit::Ref(box r) => {
                    lit = r;
                    ref_lvl += 1;
                },
                Lit::Deref(d)   => {
                    return error!("derefs are not allowed here", d.span())
                },
            }
        }
        Ok(0)
    }

    fn get_label(&mut self, lbl: &ast::Label<'a>, pos: usize) -> Position {
        let labels = if Ident::from(lbl.inner).is_local() { &mut self.locals } else { &mut self.labels };

        let ident = Ident::from(lbl.inner);
        if let Some(def) = labels.get(&ident).cloned() {
            def.pos
        } else {
            let lbl_ref = LabelRef::new(pos, lbl.span());
            self.lit_uses
                .entry(Auto::Lbl(ident, 0))
                .or_default()
                .push(lbl_ref);

            0
        }
    }

    #[inline]
    fn add_local_lbl(&mut self, lbl: &ast::Label<'a>) -> Result<()> {
        self.add_local_lbl_to(lbl, self.get_pos())
    }

    fn add_local_lbl_to(&mut self, lbl: &ast::Label<'a>, to: usize) -> Result<()> {
        use std::collections::hash_map::Entry::*;

        match self.locals.entry(Ident::from(lbl.inner)) {
            Occupied(_)   => error!("label defined twice", lbl.span.clone()),
            Vacant(entry) => {
                entry.insert(LabelDef::new(to, lbl.span.clone()));
                Ok(())
            },
        }
    }

    fn add_global_lbl(&mut self, lbl: &ast::Label<'a>) -> Result<()> {
        use std::collections::hash_map::Entry::*;

        let curr_pos = self.get_pos();

        match self.labels.entry(Ident::from(lbl.inner)) {
            Occupied(_)   => error!("label defined twice", lbl.span.clone()),
            Vacant(entry) => {
                entry.insert(LabelDef::new(curr_pos, lbl.span.clone()));
                self.solve_locals()
            },
        }
    }

    fn solve_locals(&mut self) -> Result<()> {
        use std::collections::btree_map::Entry::*;

        // Removes all uses of local labels in values and replaces them with uses of actual
        // numbers.
        for (lbl, def) in self.locals.drain() {
            let local_lbl_uses: Vec<_> = self.lit_uses
                .drain_filter(|k, _| (Auto::Lbl(lbl, 0)..=Auto::Lbl(lbl, u32::MAX)).contains(k))
                .collect();

            for (auto, uses) in local_lbl_uses {
                match self.lit_uses.entry(Auto::Num(def.pos as i32, auto.unwrap_lbl().1)) {
                    Occupied(entry) => entry.into_mut().extend(uses),
                    Vacant(entry)   => { entry.insert(uses); },
                }
            }
        }

        Ok(())
    }

    fn add_auto_lbls(&mut self) -> Result<()> {
        use std::collections::hash_map::Entry::*;

        match self.labels.entry(Ident("__end", 0)) {
            Occupied(entry) =>
                error!("cannot define auto label",
                    entry.get().span
                        .as_ref()
                        .unwrap()
                        .clone()),

            Vacant(entry)   => {
                let end = self.tape.len();
                entry.insert(LabelDef::auto(end));
                Ok(())
            },
        }
    }

    fn unique_ident<'b: 'a>(&mut self, name: &'b str) -> Ident<'a> {
        self.macro_count += 1;
        Ident::new(name, self.macro_count)
    }

    fn unique_lbl<'b: 'a>(&mut self, name: &'b str, span: Span<'a>) -> ast::Label<'a> {
        self.macro_count += 1;
        ast::Spanned::new((name, self.macro_count), span)
    }
}

