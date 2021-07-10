use pest::Span;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Spanned<'a, T> {
    pub inner: T,
    pub span: Span<'a>,
}

impl<'a, T> Spanned<'a, T> {
    pub fn span(&self) -> Span<'a> { self.span.clone() }
}

impl<'a, T> Spanned<'a, T> {
    pub fn new(inner: T, span: Span<'a>) -> Spanned<'a, T> {
        Spanned { inner, span }
    }

    pub fn to_inner(self) -> T {
        self.inner
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Op {
    Hlt = 0,
    Add = 1,
    Mul = 2,
    Cle = 3,
    Ceq = 4,
    Jmp = 5,
    Beq = 6,
    Cpy = 7,
    Put = 8,
    Ptn = 9,


    Psh,
    Pop,
    Cal,
    Ret,
}

impl Op {
    pub fn nargs(&self) -> usize {
        match self {
            Op::Add => 3,
            Op::Mul => 3,
            Op::Cle => 3,
            Op::Ceq => 3,
            Op::Beq => 2,
            Op::Cpy => 2,
            Op::Jmp => 1,
            Op::Put => 1,
            Op::Ptn => 1,
            Op::Hlt => 0,

            Op::Psh => 1,
            Op::Pop => 1,
            Op::Cal => 1,
            Op::Ret => 0,
        }
    }
}

pub struct Prog<'a> {
    pub stmts: Vec<Stmt<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Label(Label<'a>),
    Inst(Inst<'a>),
    Lit(Lit<'a>),
    Macro(Macro<'a>),
    Include(Str<'a>),
    Define(Ident<'a>, Lit<'a>),
}

#[derive(Debug, Clone)]
pub struct Macro<'a> {
    pub name: Ident<'a>,
    pub args: Vec<MacroArg<'a>>,
    pub stmts: Vec<Stmt<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub enum MacroArg<'a> {
    Lit(Ident<'a>),
    Reg(Ident<'a>),
}

#[derive(Debug, Clone)]
pub struct Ident<'a> {
    pub content: &'a str,
    pub span: Span<'a>,
}

pub type Label<'a> = Spanned<'a, (&'a str, usize)>;
pub type Num<'a> = Spanned<'a, i32>;
pub type Str<'a> = Spanned<'a, &'a str>;
pub type Chr<'a> = Spanned<'a, char>;

#[derive(Debug, Clone)]
pub struct Inst<'a> {
    pub op: Op,
    pub args: Vec<Arg<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub enum Lit<'a> {
    Lbl(Label<'a>),
    Num(Num<'a>),
    Chr(Chr<'a>),
    Str(Str<'a>),
}

impl<'a> Lit<'a> {
    pub fn span(&self) -> Span<'a> {
        match self {
            Lit::Lbl(lbl) => lbl.span.clone(),
            Lit::Num(num) => num.span.clone(),
            Lit::Str(s)   => s.span.clone(),
            Lit::Chr(c)   => c.span.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Arg<'a> {
    Imm(Lit<'a>),
    Lbl(Label<'a>),
    Reg(Ident<'a>),
    RegImm(Ident<'a>, Num<'a>),
}

impl<'a, T> std::ops::Deref for Spanned<'a, T> {
    type Target = T;
    fn deref(&self) -> &T { &self.inner }
}

impl<'a, T> std::ops::DerefMut for Spanned<'a, T> {
    fn deref_mut(&mut self) -> &mut T { &mut self.inner }
}

impl<'a, T> std::convert::AsRef<T> for Spanned<'a, T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

pub fn mk_lbl<'a>(name: &'a str, span: Span<'a>) -> Label<'a> {
    Spanned::new((name, 0), span)
}

impl<'a> Inst<'a> {
    pub fn new(op: Op, args: Vec<Arg<'a>>, span: Span<'a>) -> Inst<'a> {
        Inst { op, args, span }
    }
}

impl<'a> Arg<'a> {
    pub fn span(&self) -> Span<'a> {
        match self {
            Arg::Lbl(lbl) => lbl.span(),
            Arg::Lit(lit) => lit.span(),
        }
    }
}

impl<'a> From<Label<'a>> for Arg<'a> {
    fn from(v: Label<'a>) -> Arg<'a> { Arg::Lbl(v) }
}

impl<'a> From<Lit<'a>> for Arg<'a> {
    fn from(v: Lit<'a>) -> Arg<'a> { Arg::Lit(v) }
}

impl<'a> From<Num<'a>> for Lit<'a> {
    fn from(v: Num<'a>) -> Lit<'a> { Lit::Num(v) }
}

impl<'a> From<Chr<'a>> for Lit<'a> {
    fn from(v: Chr<'a>) -> Lit<'a> { Lit::Chr(v) }
}

impl<'a> From<Str<'a>> for Lit<'a> {
    fn from(v: Str<'a>) -> Lit<'a> { Lit::Str(v) }
}

impl<'a> From<Label<'a>> for Lit<'a> {
    fn from(v: Label<'a>) -> Lit<'a> { Lit::Lbl(v) }
}

use std::fmt;
use std::fmt::{ Display, Formatter };

impl<'a> Display for Stmt<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Label(lbl) => write!(f, "{}:", lbl.inner.0),
            Stmt::Inst(inst) => Display::fmt(inst, f),
            Stmt::Lit(lit)   => Display::fmt(lit, f),
            Stmt::Org(num)   => write!(f, ".org {}", num.inner),
        }
    }
}

impl<'a> Display for Inst<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ", self.op)?;

        for arg in &self.args {
            write!(f, "{} ", arg)?;
        }

        Ok(())
    }
}

impl<'a> Display for Arg<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Arg::Lbl(Spanned { inner: (name, id), .. }) => {
                write!(f, "<{}", name)?;
                if *id > 0 {
                    write!(f, "_{:02x}", id)?;
                }
                write!(f, ">")
            },
            Arg::Lit(lit) => Display::fmt(lit, f),
        }
    }
}

impl<'a> Display for Lit<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Chr(chr) => write!(f, "'{}'", chr.inner),
            Lit::Str(s)   => write!(f, "\"{}\"", s.inner),
            Lit::Num(num) => write!(f, "{}", num.inner),
            Lit::Ref(r)   => write!(f, "&{}", r),
            Lit::Deref(d) => write!(f, "*{}", d),
            Lit::Lbl(Spanned { inner: (name, id), .. }) => {
                write!(f, "'{}", name)?;
                if *id > 0 {
                    write!(f, "_{:02x}", id)?;
                }
                Ok(())
            },
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = match self {
            Op::Hlt => "hlt",
            Op::Add => "add",
            Op::Mul => "mul",
            Op::Cle => "cle",
            Op::Ceq => "ceq",
            Op::Jmp => "jmp",
            Op::Beq => "beq",
            Op::Cpy => "cpy",
            Op::Put => "put",
            Op::Ptn => "ptn",

            Op::Psh => "psh",
            Op::Pop => "pop",
            Op::Cal => "cal",
            Op::Ret => "ret",
        };

        write!(f, "{}", name)
    }
}

impl<'a, T: Display> Display for Spanned<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.inner, f)
    }
}
