# Tapec

Um compilador para a linguagem de programação Tape.

# Compilando e Instalando

- Clone esse repositório.
- Assegure-se de ter instalado as [ferramentas do rust](https://www.rust-lang.org/tools/install)
- Entre no repositório `cd zasmc`.
- Compile e instale binário `cargo install --path .`
- Se o diretório do cargo já estiver no seu PATH, você deverá ser capaz de usar
    o compilador. Use `zasmc --version` para verificar se a instalação foi bem
    sucedida.

# A Linguagem de Programação Tape

## Instruções básicas

```
hlt                      - Termina a execução imediatamente
add <add1> <add2> <dest> - tape[dest] = tape[add1]  + tape[add2]
mul <add1> <add2> <dest> - tape[dest] = tape[add1]  + tape[add2]
cle <add1> <add2> <dest> - tape[dest] = tape[add1]  < tape[add2]
ceq <add1> <add2> <dest> - tape[dest] = tape[add1] == tape[add2]
jmp <add1>               - ip = tape[add1]
beq <add1> <add2>        - ip = tape[add1] ? tape[add2] : ip
cpy <add1> <dest>        - tape[dest] = tape[add1]
put <add1>               - putchar(tape[add1])
ptn <add1>               - printf("%d", tape[add1])
```

## Comentários

Um comentário começa com ';' e vai até o final da linha.

```asm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Isso é apenas um comentário ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
```

## Literais

Literais são valores que serão convertidos diretamente para valores na fita.

```asm
0                   ; O valor 0
'a'                 ; O valor 97
"Hello, world\n\0"  ; Será convertido para mais de um valor.
```

## Labels

### Globais

Um label representa simplesmente uma posição na fita. Ele pode ser usado como
argumento de instruções, por exemplo, e será substiuido pelo endereço de onde o
label foi definido em tempo de compilação.

```asm
put 'char_h  ; Imprimimos o conteúdo do endereço de memória do label "char_h"
hlt          ; Encerra o programa

char_h: 'h'  ; Definimos o label "char_h" e colocamos o valor ASCII de 'h' em
             ; seu endereço
```

Toda vez que formos definir um label usamos a sintaxe `<nome>:` e toda vez que
formos referenciar um label usamos uma aspas simples antes do nome `'<nome>`.

Um label global pode ser utilizado a partir de qualquer parte do código.

### Locais

Labels locais sempre precisam começar com um ponto antes do nome e estão
diretamente relacionados ao label global anterior a eles. Esses labels podem se
repetir desde que estejam sobre labels globais diferentes.

```asm
main:                   ; Inicia um contexto
.loop:                  ; Label local de um escopo "main"
    cpy char_A tmp      ; Copia 'A' para a "variável" tmp.
    put tmp             ; Imprime o valor de tmp. No caos, 'A'.
    jmp .loop           ; Cria um loop infinito. A necessidade desse '&' se tornará aparente mais afrente.

outro:
.loop:                  ; Não interfere com outro label .loop.
    cpy char_B tmp
    put tmp
    jmp .loop

tmp: 0
char_A: 'A'
char_B: 'B'
```
