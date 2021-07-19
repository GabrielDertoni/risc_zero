use pest::Span;
use pest::iterators::Pairs;

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
            pub fn new($($member : $ty,)* span: Span<$life>) -> $name<$life $(, $ts)*> {
                $name {
                    $($member,)*
                    span,
                }
            }

            #[inline]
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
            pub fn span(&self) -> Span<$life> {
                $expr
            }
        }

        impl_span! { $($rest)* }
    };

    (enum $name:ident<$life:lifetime $(, $ts:tt)*> => match { $($pat:pat => $expr:expr,)+ } $($rest:tt)*) => {
        impl<$life $(, $ts)*> $name<$life $(, $ts)*> {
            #[inline]
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Op {
    Noop,
    Add,
    Mult,
    Mov,
    Div,
    Cle,
    Clt,
    Addi,
    Lui,
    Jmp,
    Beq,
    Bne,
    Ldb,
    Stb,
    Ldw,
    Stw,
}

impl Op {
    pub fn nargs(&self) -> usize {
        use Op::*;

        match self {
            Noop => 0,
            Add  => 2,
            Mult => 2,
            Mov  => 2,
            Div  => 2,
            Cle  => 2,
            Clt  => 2,
            Addi => 2,
            Lui  => 2,
            Jmp  => 2,
            Beq  => 2,
            Bne  => 2,
            Ldb  => 2,
            Stb  => 2,
            Ldw  => 2,
            Stw  => 2,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegAddr {
    Tmp,
    Hi,
    Lo,
    Sp,
    Adr,
    Acc,
    Flags,
    R1, R2, R3, R4, R5, R6, R7, R8, R9,
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

    pub fn as_inst(&self) -> Option<&Inst<'a>> {
        if let Self::Inst(v) = self {
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
        pub addr: RegAddr,
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

    #[inline]
    pub fn is_local(&self) -> bool {
        matches!(self, Label::Local(..))
    }

    #[inline]
    pub fn is_global(&self) -> bool {
        matches!(self, Label::Global(..))
    }

    #[inline]
    pub fn is_expanded(&self) -> bool {
        matches!(self, Label::Macro(..))
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

impl<'a> Expr<'a> {
    pub fn as_num(&self) -> Option<&Num<'a>> {
        if let Self::Num(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_chr(&self) -> Option<&Chr<'a>> {
        if let Self::Chr(v) = self {
            Some(v)
        } else {
            None
        }
    }
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

impl<'a> Display for RegAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use RegAddr::*;

        let s = match self {
            Tmp   => "$tmp",
            Hi    => "$hi",
            Lo    => "$lo",
            Sp    => "$sp",
            Adr   => "$adr",
            Acc   => "$acc",
            Flags => "$flg",
            R1    => "$r1",
            R2    => "$r2",
            R3    => "$r3",
            R4    => "$r4",
            R5    => "$r5",
            R6    => "$r6",
            R7    => "$r7",
            R8    => "$r8",
            R9    => "$r9",
        };

        write!(f, "{}", s)
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


impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Op::*;

        let name = match self {
            Noop => "noop",
            Add  => "add",
            Mult => "mult",
            Mov  => "mov",
            Div  => "div",
            Cle  => "cle",
            Clt  => "clt",
            Addi => "addi",
            Lui  => "lui",
            Jmp  => "jmp",
            Beq  => "beq",
            Bne  => "bne",
            Ldb  => "ldb",
            Stb  => "stb",
            Ldw  => "ldw",
            Stw  => "stw",
        };

        write!(f, "{}", name)
    }
}
