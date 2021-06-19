# Risc Zero

## Esboço da arquitetura

### Visão geral

- Endereçamento de 16 bits
- 8 registradores no total (16 bits)

### Registradores

- 16 registradores expostos
    - TMP (Usado pelo montador)
    - HI (High)
    - LO (Low)
    - SP (Stack pointer)
    - ADR (Address)
    - ACC (Acumulador)
    - FL (Flags)
    - R1..R9
- Registradores ocultos: *program counter.*

## Instruções

### Instruções aritméticas:

- [R] `ADD <reg1> <reg2>` - `reg1 += reg2`
- [I] `ADDI <reg1> <im>` - `reg1 += im`
- [R] `MULT <reg1> <reg2>` - `reg1 *= reg2`
- [R] `DIV <reg1> <reg2>` - `HI = reg1 / reg2, LO = reg1 % reg2`
- [R] `MOV <reg1> <reg2>` - `reg1 := reg2`

### Instruções bit-a-bit:

- [R] `AND <reg1> <reg2>` - `reg1 &= reg2`
- [I] `ANDI <reg1> <im>` - `reg1 &= im`
- [R] `OR <reg1> <reg2>` - `reg1 |= reg2`
- [R] `NOT <reg1>` - `reg1 = ~reg1`
- [R] `SHL <reg1> <reg2>` - `reg1 <<= reg2`
- [R] `SHR <reg1> <reg2>` - `reg1 >>= reg2`

### Instruções de desvio:

- [J] `BEQ <reg1>` - `if (!ZERO) goto reg1`
- [J] `BNE <reg1>` - `if (ZERO) goto reg1`
    - [J] `JMP <reg1>` - `ANDI LF ZERO, BEQ reg1`

### Instruções de comparação:

- [R] `CEQ <reg1> <reg2>` - `ZERO := !(reg1 == reg2)`
- [R] `CLT <reg1> <reg2>` - `ZERO := !(reg1 < reg2)`

### Instruções de acesso à memória:

- [M] `LDB <reg1> <im> <reg2>` - `reg1 := *(reg2 + im)`
- [M] `STB <reg1> <im> <reg2>` - `*(reg2 + im) := reg1`
- [M] `LDW <reg1> <im> <reg2>` - `reg1 := *(int16_t *)(reg2 + im)`
- [M] `STW <reg1> <im> <reg2>` - `*(int16_t *)(reg2 + im) := reg1`
- [I] `LUI <reg1> <im>` - `reg1 = im << 8`

### Input/Output

- [R] `GET <reg1>` - `reg1 = getchar()`
    - GETD (Get decimal)
    - GETS (Get string)
- [R] `PUT <reg1>` - `putchar(reg1)`
    - PUTD (Put decimal)
    - PUTS (Put string) Esboço da arquitetura



## Layout das instruções:

Tipo | OpCode | Reg1 | Reg2 | Im | Opts
-----|--------|------|------|----|-----
R    | 4      | 4    | 4    | -  | 4
I    | 4      | 4    | -    | 8  | -
M    | 4      | 4    | 4    | 5  | -
J    | 4      | 4    | -    | -  | 8


