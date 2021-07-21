use pest::Span;
use pest::iterators::Pairs;
use architecture_utils as risc0;

use crate::parser::ParserRule;

macro_rules! spanned {
    () => { };
    (pub struct $name:ident<$life:lifetime $(, $ts:tt)*> { $(pub $member:ident : $ty:ty,)* } $($rest:tt)* ) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct $name<$life $(, $ts)*> {
            $(pub $member: $ty,)*
            pub span: Span<$life>,
        }

        impl<$life $(, $ts)*> $name<$life $(, $ts)*> {
            #[inline]
            #[allow(dead_code)]
            pub fn new($($member : $ty,)* span: Span<$life>) -> $name<$life $(, $ts)*> {
                $name {
                    $($member,)*
                    span,
                }
            }

            #[inline]
            #[allow(dead_code)]
            pub fn span(&self) -> Span<$life> {
                self.span.clone()
            }
        }

        spanned! { $($rest)* }
    };
}

macro_rules! impl_span {
    () => { };

    (struct $name:ident<$life:lifetime $(, $ts:tt)*> => $expr:expr; $($rest:tt)*) => {
        impl<$life $(, $ts)*> $name<$life $(, $ts)*> {
            #[inline]
            #[allow(dead_code)]
            pub fn span(&self) -> Span<$life> {
                $expr
            }
        }

        impl_span! { $($rest)* }
    };

    (enum $name:ident<$life:lifetime $(, $ts:tt)*> => match { $($pat:pat => $expr:expr,)+ } $($rest:tt)*) => {
        impl<$life $(, $ts)*> $name<$life $(, $ts)*> {
            #[inline]
            #[allow(dead_code)]
            pub fn span(&self) -> Span<$life> {
                use $name::*;

                match self {
                    $(
                        $pat => $expr,
                    )+
                }
            }
        }

        impl_span! { $($rest)* }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prog<'a> {
    pub stmts: Vec<Stmt<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt<'a> {
    Label(Label<'a>),
    Inst(Inst<'a>),
    Lit(Lit<'a>),
    Macro(Macro<'a>),
    Include(Str<'a>),
    Define(Ident<'a>, Lit<'a>),
    Str(Str<'a>),
}

impl<'a> Stmt<'a> {
    pub fn as_label(&self) -> Option<&Label<'a>> {
        if let Self::Label(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/*
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroArg<'a> {
    Imm(Ident<'a>),
    Reg(Ident<'a>),
}
*/

spanned! {
    pub struct Reg<'a> {
        pub addr: risc0::Reg,
    }

    pub struct Macro<'a> {
        pub name: Ident<'a>,
        pub args: Vec<Ident<'a>>,
        pub contents: Pairs<'a, ParserRule>,
    }

    pub struct Ident<'a> {
        pub content: &'a str,
    }

    pub struct Num<'a> {
        pub val: i16,
    }

    pub struct Str<'a> {
        pub content: &'a str,
    }

    pub struct Chr<'a> {
        pub chr: char,
    }

    pub struct Inst<'a> {
        pub ident: Ident<'a>,
        pub args: Vec<Arg<'a>>,
    }

    pub struct BinExpr<'a> {
        pub lhs: Box<Expr<'a>>,
        pub operator: Ident<'a>,
        pub rhs: Box<Expr<'a>>,
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Label<'a> {
    Local(Ident<'a>),
    Global(Ident<'a>),
    Macro(Ident<'a>, usize),
}

impl<'a> Label<'a> {
    #[inline]
    pub fn local(ident: Ident<'a>) -> Label<'a> {
        Label::Local(ident)
    }

    #[inline]
    pub fn global(ident: Ident<'a>) -> Label<'a> {
        Label::Global(ident)
    }

    #[inline]
    pub fn expanded(ident: Ident<'a>, id: usize) -> Label<'a> {
        Label::Macro(ident, id)
    }

    #[inline]
    pub fn ident(&self) -> &Ident<'a> {
        match self {
            Label::Local(ident)  |
            Label::Global(ident) |
            Label::Macro(ident, ..) => ident,
        }
    }

    #[inline]
    pub fn span(&self) -> Span<'a> {
        self.ident().span()
    }
}

// TODO: remove this type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit<'a> {
    Expr(Expr<'a>),
    Str(Str<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg<'a> {
    Imm(Lit<'a>),
    Reg(Reg<'a>),
    RegImm(Reg<'a>, Lit<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<'a> {
    Num(Num<'a>),
    Lbl(Label<'a>),
    Bin(BinExpr<'a>),
    Chr(Chr<'a>),
}

impl_span! {
    enum Arg<'a> => match {
        Imm(lit) => lit.span(),
        Reg(reg) => reg.span(),
        RegImm(imm, _) => imm.span(),
    }

    enum Lit<'a> => match {
        Expr(expr) => expr.span(),
        Str(s)     => s.span(),
    }

    enum Expr<'a> => match {
        Num(num) => num.span(),
        Lbl(lbl) => lbl.span(),
        Bin(bin) => bin.span(),
        Chr(chr) => chr.span(),
    }
}

pub fn precedence_of(op: &str) -> i32 {
    match op {
        "|"         => 1,
        "&"         => 2,
        ">>" | "<<" => 3,
        "+"  | "-"  => 4,
        "*"  | "/"  => 5,
        s           => unreachable!("unexpected operator '{}'", s),
    }
}


use std::fmt;
use std::fmt::{ Display, Formatter };

impl<'a> Display for Ident<'a> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.content, f)
    }
}

impl<'a> Display for Str<'a> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.content)
    }
}

impl<'a> Display for Macro<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "@macro {} ", self.name)?;

        for (i, arg) in self.args.iter().enumerate() {
            write!(f, "{}", arg)?;

            if i < self.args.len() - 1 {
                write!(f, ", ")?;
            }
        }

        Ok(())
    }
}

/*
impl<'a> Display for MacroArg<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            MacroArg::Imm(ident) => write!(f, "#{}", ident),
            MacroArg::Reg(ident) => write!(f, "%{}", ident),
        }
    }
}
*/

impl<'a> Display for Stmt<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Label(lbl)        => write!(f, "{}:", lbl),
            Stmt::Inst(inst)        => Display::fmt(inst, f),
            Stmt::Lit(lit)          => Display::fmt(lit, f),
            Stmt::Macro(mac)        => Display::fmt(mac, f),
            Stmt::Include(s)        => write!(f, "@include {}", s),
            Stmt::Define(name, def) => write!(f, "@define {} {}", name, def),
            Stmt::Str(s)            => write!(f, "{}", s),
        }
    }
}

impl<'a> Display for Inst<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ", self.ident)?;

        for arg in &self.args {
            write!(f, "{} ", arg)?;
        }

        Ok(())
    }
}

impl<'a> Display for Arg<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Arg::Imm(lit)         => write!(f, "{}", lit),
            Arg::Reg(reg)         => write!(f, "{}", reg),
            Arg::RegImm(reg, imm) => write!(f, "{}({})", reg, imm),
        }
    }
}

impl<'a> Display for Reg<'a> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.addr, f)
    }
}

impl<'a> Display for Label<'a> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.ident(), f)
    }
}

impl<'a> Display for Num<'a> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.val, f)
    }
}

impl<'a> Display for Lit<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Expr(expr) => write!(f, "{}", expr),
            Lit::Str(s)     => write!(f, "\"{}\"", s.content),
        }
    }
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        fn write_prec(f: &mut Formatter, expr: &Expr, prec: i32) -> fmt::Result {
            match expr {
                Expr::Num(num) => write!(f, "{}", num.val),
                Expr::Chr(chr) => write!(f, "'{}'", chr.chr),
                Expr::Lbl(lbl) => write!(f, "{}", lbl),
                Expr::Bin(BinExpr { lhs, operator, rhs, .. }) => {
                    let cur_prec = precedence_of(operator.content);
                    if cur_prec < prec {
                        write!(f, "(")?;
                    }

                    write_prec(f, lhs.as_ref(), cur_prec)?;
                    write!(f, " {} ", operator.content)?;
                    write_prec(f, rhs.as_ref(), cur_prec)?;

                    if cur_prec < prec {
                        write!(f, ")")?;
                    }

                    Ok(())
                }
            }
        }

        write_prec(f, self, 0)
    }
}
