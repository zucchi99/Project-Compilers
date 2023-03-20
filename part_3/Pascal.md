# Pascal

Insert a link to the official documentation of the language.
![Sintassi Pascal](https://doc.tmssoftware.com/biz/scripter/guide/pascal.html#function-and-procedure-declaration)

Pascal è composto da 5 elementi base:

* `Program`, elemento di livello più alto
* `Declarations`, blocco per le dichiarazioni di variabili
* `Statements`, blocco per le istruzioni
* `Functions and Procedures`, sum()
* `Expressions`, 5 * 2 + 3

## Esempio di programma

    program AddTwoNumbers;

    var
        num1, num2, sum: integer;

    begin
        writeln('Enter two numbers: ');
        readln(num1);
        readln(num2);
        sum := num1 + num2;
        writeln('The sum of ', num1, ' and ', num2, ' is ', sum);
    end.

## Strutture da considerare

In particolare andremo ad osservare:

* Blocchi di codice
* `Function` e `Procedure`
* Iterazioni
* Condizioni
* Array
* Puntatori
* Commenti

### Blocco di codice

Un blocco Pascal è un insieme di istruzioni che vengono eseguite in sequenza. Un blocco è racchiuso tra le parole chiave `begin` e `end`.  
Le istruzione sono tutte separate da un punto e virgola. Ad esempio:
Ad esempio:

    begin
        writeln('Hello, world!');
        writeln('Hello, world!');
    end.

Il blocco principale termina sempre con la keyword `end.`, se manca il punto il programma restituisce errore.

Le dichiarazione di variabili avvengono dopo la keyword `var` e sono separate da un punto e virgola e non sono istruzioni. 
Le `dichiarazioni di variabili` usano il simbolo `:` per indicare il tipo della variabile, mentre le `assegnazioni` usano il simbolo `:=` per indicare l'assegnazione. Ad esempio:

    var
        a: integer;
        b: integer;
    begin
        a := 1;
        b := 2;
        writeln('Hello, world!');
        writeln('Hello, world!');
    end.

### Function vs Procedure

Devono essere dichiarate prima di essere usate, banalmente devono essere messe precedentemente al blocco principale.

La differenza è che la `procedure` non ritorna un valore, mentre la `function` è una funzione che ritorna un valore. Ad esempio:

    function add(a: integer; b: integer): integer;
    begin
      add := a + b;
    end;

Per ritornare un valore da una funzione, si usa una variabile con lo stesso nome della funzione.

NON LI CONSIDEREREMO PERCHÈ RIGUARDANO PASCAL, ma ci sono anche altri modi di ritornare un valore da una funzione. Ad esempio:

    function add(a: integer; b: integer): integer;
    begin
        result := a + b;
    end;

Se non ritorniamo un valore, usiamo la keyword `procedure`. Ad esempio:

    procedure add(a: integer; b: integer);
    begin
        writeln(a + b);
    end;

### Iterazioni

In Pascal le iterazioni possono essere:

* determinate, con `for`

        var
            i: integer;
        begin
            for i := 1 to 10 do
                writeln(i);
        end;

* indeterminate, con `while`

        var
            i: integer;
        begin
            i := 1;
            while i <= 10 do
            begin
                writeln(i);
                i := i + 1;
            end;
        end;


### Condizioni

Le condizioni sono usate per eseguire un blocco di codice solo se una condizione è vera. Ad esempio:
    
    var
        a: integer;
    begin
        a := 1;
        if a = 1 then
            writeln('a is 1');
    end;

### Array

In Pascal gli array sono dichiarati con la keyword `array`. Ad esempio:

    var
        a: array[1..10] of integer;
    begin
        a[1] := 1;
        a[2] := 2;
        writeln(a[1]);
        writeln(a[2]);
    end;

### Puntatori

I puntatori sono usati per memorizzare l'indirizzo di una variabile. Ad esempio:

    var
        a: integer;
        b: ^integer;
    begin
        a := 1;
        b := @a;
        writeln(b^);
    end;

### Commenti

In pascal i commenti sono:

* `//` per un commento su una sola linea
* `(*` e `*)` per un commento su più linee
* `{` e `}` per un commento su più linee

È possibile fare un nesting dei commenti se si usano commenti diversi.
Purtroppo BNFC non supporta i commenti nested.

### Priorità

In Pascal le operazioni vengono eseguite secondo la seguente priorità, includendo le parentesi e le funzioni:

1. `^`
1. `*`, `/`, `div`, `mod`, `and`
1. `+`, `-`, `or`, `xor`
1. `=`, `<>`, `<`, `<=`, `>`, `>=`
1. `not`
1. `and` e `or`

Dove il livello 1 ha la priorità più alta.

### Albero di sintassi

1. `Program`
1. `Declarations`
    1. `Var` block
        1. variables
        1. Consts
        1. `Function`
        1. `Procedure`
    1. `Function`
        1. `Declarations` block
        1. `statements` block
    1. `Procedure`
        1. `Declarations` block
        1. `statements` block
1. `Main` `statements` block

Quindi:

    Program = "program" Ident ";" Block "."

    Block = [Declaration] Statements

    Declaration = VarDeclaration
                | ConstDeclaration
                | FunctionDeclaration
                | ProcedureDeclaration

    ConstDeclaration       = "const" [Ident "=" Expression ";"]
    VarDeclaration         = "var" [Ident ":" Type ";"]
    FunctionDeclaration    = "function" Ident "(" [Ident ":" Type] ")" ":" Type ";" Block ";"
    ProcedureDeclaration   = "procedure" Ident "(" [Ident ":" Type] ")" ";" Block ";"

    Statements = "begin" [Statement ";"] "end"