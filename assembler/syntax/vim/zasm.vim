" Vim syntax file
" Language: Zasm
" Maintainer: Gabriel Dertoni
" Latest Revision: 30 Maio 2021

if exists("b:current_syntax")
  finish
endif

syntax case ignore

" syn match zasmArgList /\s\w\+\s\zs[^,]\+\(,[^,]\+\)*/ contains=zasmLabelRef,zasmLocalRef

" Instructions
syn match zasmInstruction /\s\zs\w\+/

" Markers
syn match zasmMacro /@macro/ nextgroup=zasmInstruction
syn match zasmInclude /@include/ nextgroup=zasmInstruction
syn match zasmDefine /@define/ nextgroup=zasmInstruction

syn match zasmMacroLit /#\w\+/
syn match zasmMacroReg /%\w\+/

" Registers
syn match zasmRegister /\$\(\w\|\d\)\+/
 
syn keyword zasmTodo contained TODO FIXME XXX NOTE
syntax match zasmComment /;.*/ contains=zasmTodo
 
syntax region zasmString start=/"/ skip=/\\"/ end=/"/
 
syntax match zasmLabelRef /\(\w\+\s\|,\)\s*\zs\w\+/
syntax match zasmLocalRef /\.\(\w\|\d\|_\)\+/
 
syntax match zasmChar /'\\\?\w'/
 
syntax match zasmNumber /\<\(0b\|0o\|0d\|0x\)\?[-]\?\d\+\>/
 
" syntax match zasmOrg "\.org"
syntax match zasmLocalLabel /\.\w\+:/
syntax match zasmLabel /\w\+:/

hi def link zasmComment     Comment
hi def link zasmTodo        Todo
hi def link zasmNumber      Number
hi def link zasmString      String
hi def link zasmChar        String
hi def link zasmLabel       Function
hi def link zasmLocalLabel  Label
hi def link zasmRegister    Bold
hi def link zasmOrg         Structure
hi def link zasmInstruction Identifier
hi def link zasmInclude     Include
hi def link zasmMacro       Define
hi def link zasmDefine      Define
hi def link zasmMacroLit    Constant
hi def link zasmMacroReg    Bold

hi def link zasmLabelRef    Label
hi def link zasmLocalRef    Label

