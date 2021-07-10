" Vim syntax file
" Language: Tape
" Maintainer: Gabriel Dertoni
" Latest Revision: 30 Maio 2021

if exists("b:current_syntax")
  finish
endif

syntax case ignore

" hlt                - exit(0)
" add add1 add2 dest - tape[dest] = tape[add1]  + tape[add2]
" mul add1 add2 dest - tape[dest] = tape[add1]  * tape[add2]
" cle add1 add2 dest - tape[dest] = tape[add1]  < tape[add2]
" ceq add1 add2 dest - tape[dest] = tape[add1] == tape[add2]
" jmp add1           - ip = tape[add1]
" beq add1 add2      - ip = tape[add1] ? tape[add2] : ip
" cpy add1 dest      - tape[dest] = tape[add1]
" put add1           - putchar(tape[add1])
" ptn add1           - printf("%d", tape[add1])

" Instructions
syn keyword tapeInstruction hlt add mul cle ceq jmp beq cpy put ptn

" Pseudo instructions
syn keyword tapeInstruction psh pop cal ret

syntax match tapeComment /;.*/
syntax match tapeRef /&/
syntax match tapeDeref /*/

syntax region tapeString start=/"/ skip=/\\"/ end=/"/
syntax match tapeLabelRef /'\w\+\( \|\r\|$\)/
syntax match tapeLocalRef /'\.\w\+\( \|\r\|$\)/
syntax match tapeChar /'\\\?\w'/

syntax match tapeNumber /\<[-]\?\d\+\>/

syntax match tapeOrg "\.org"
syntax match tapeLocalLabel /\.\w\+:/
syntax match tapeLabel /\w\+:/

hi def link tapeComment     Comment
hi def link tapeNumber      Number
hi def link tapeString      String
hi def link tapeLabelRef    Constant
hi def link tapeLocalRef    Constant
hi def link tapeChar        String
hi def link tapeLabel       Function
hi def link tapeLocalLabel  Label
hi def link tapeMainLabel   MainLabel
hi def link tapeRegister    Identifier
hi def link tapeOrg         Structure
hi def link tapeInstruction Statement
hi def link tapeRef         Type
hi def link tapeDeref       Type
