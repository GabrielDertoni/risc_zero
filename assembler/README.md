# Tapec

Um compilador para a linguagem de programação Tape.

# Compilando e Instalando

- Clone esse repositório.
- Assegure-se de ter instalado as [ferramentas do rust](https://www.rust-lang.org/tools/install)
- Entre no repositório `cd tapec`.
- Compile e instale binário `cargo install --path .`
- Se o diretório do cargo já estiver no seu PATH, você deverá ser capaz de usar
    o compilador. Use `tapec --version` para verificar se a instalação foi bem
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
    cpy 'char_A 'tmp    ; Copia 'A' para a "variável" tmp.
    put 'tmp            ; Imprime o valor de tmp. No caos, 'A'.
    jmp &'.loop         ; Cria um loop infinito. A necessidade desse '&' se tornará aparente mais afrente.

outro:
.loop:                  ; Não interfere com outro label .loop.
    cpy 'char_B 'tmp
    put 'tmp
    jmp &'.loop

tmp: 0
char_A: 'A'
char_B: 'B'
```

### Em argumentos

Em algumas situações é necessário obter o endereço de onde estará armazenado
certo argumento de uma instrução para podermos modificá-la. Para isso usamos
labels de argumento que são definidos no formato `<label>` incluindo `<` e `>`.
Labels de argumento sempre precisam ser locais, então na prática sempre
começarão com um `.`.

```asm
main:
    cpy 'ptr '.arg           ; Copia o valor de ponteiro para .arg.
    put <.arg>               ; Agora .arg possui 'string e o put pode imprimir o 'H' (primeiro caractere de 'string).

ptr: 'string                 ; Ponteiro para o endereço de string.
string: "Hello, World\n\0"
```

## Usando endereços

Para usar um valor como a constante 1, por exemplo, não podemos simplesmete
escrever o 1 como argumento da instrução, já que isso significaria o endereço 1
e na verdade queremos o número 1. Para resolver isso devemos criar labels para
cada constante utilizada.

```asm
ceq 'zero 'one 'result ; Compara 0 com 1 e coloca o resultado em result.

result: 0
zero: 0
one: 1
```

Entretanto, para facilitar esse processo, podemos utilizar diretamente as
constates desejadas desde que declaremos que queremos usar o **endereço** delas.
Assim, o mesmo código anterior pode ser reescrito como

```asm
ceq: &0 &1 'result

result: 0
```

**Atenção**: Cada vez que você utilizar `&0`, o compilador irá criar um novo
label que armazena o valor 0 e colocar no lugar de `&0` esse label. Ou seja, se
tiver uma constante que é muito utilizada, utilize o método tradicional com
labels diretamente.

Também podemos pegar o endereço de labels com `&'label`. Numa instrução jump
isso sempre é necessário, visto que se usarmos o label diretamente, o jump
tentaria acessar o endereço do label em busca de um endereço para onde pular.

### Endereços de endereços

O compilador atualmente possui suporte para declara quantos labels quanto
necessário quando usando endereçamento. Ou seja, podemos usar `&&0` se por algum
motivo quisermos um endereço a um endereço de `0`.

# Alguns exemplos básicos

## Hello world

```asm
main:
    put &'h'
    put &'e'
    put &'l'
    put &'l'
    put &'o'
    put &','
    put &' '
    put &'w'
    put &'o'
    put &'r'
    put &'l'
    put &'d'
    put &'!'
    put &'\n'
    hlt
```

## Hello World com ponteiros

Apesar de um pouco mais longa em código, essa abordagem é mais flexível, basta
mudar o label `string` para imprimir algo completamente diferente.

```asm
main:
.loop:
    cpy 'ptr '.cpy_arg     ; Efetivamente dereferencia o ponteiro e coloca o valor em a
    cpy <.cpy_arg> 'a      ; Na inicialização, o primeiro argumento do cpy será -1

    ceq &0 'a 'tmp         ; Compara se o resgistrado a possui o valor 0 e coloca o resultado em tmp
    beq 'tmp &'.loop_end   ; Se tmp for 1, termina o loop. Senão, continue
    put 'a                 ; Imprime o conteúdo de a.
    add &1 'ptr 'ptr       ; Incrementa o ponteiro.
    jmp &'.loop            ; Volta ao início do loop.

.loop_end:
    hlt

tmp: 0                     ; Armazena o resultado de comparações.
a: 0                       ; Registrador A.
ptr: 'string               ; Aponta para o próximo caractere a ser impresso.
string: "Hello, world!\n\0"
```

## Dereferenciando

O truque demonstrado na seção anterior pode ser abstraido através do
dereferenciamento. Na verdade, o compilador ainda irá gerar uma instrução `cpy`
a mais para cada uso de dereferenciamento e usará o truque demonstrado acima,
mas isso é completamente transparente ao programador.

```asm
main:
    put **'double_ptr              ; Imprime 'H'
    hlt

double_ptr: 'ptr
ptr: 'string
string: "Hello, world!\n\0"
```

## TODOs

- Adicionar palavras chave para organização. `.org`
- Adicionar pseudoinstrução `peek`.
- Maybe there is an issue with global labels.


## TODOs (refactor)
- Change instructions in AST to be enum like. Ex. `Inst::Add(1, 2, 3)`.
