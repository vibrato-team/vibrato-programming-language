# Vibrato
Imperative programming language based on music theory.

## Index
1. [Program (example)](#program-example)
2. [Expressions and data types](#expressions-and-data-types)
    1. [Whole](#whole)
    2. [Half](#half)
    3. [Quarter](#quarter)
    4. [Eight](#eight)
    4. [32th](#32th)
    5. [64th](#64th)
    6. [Melodies](#melodies)
    7. [Sample](#sample)
3. [Instructions](#instructions)
    1. [Assignment](#assignment)
    2. [Block](#block)
    3. [Entry](#entry)
    4. [Exit](#exit)
    5. [If/else conditional](#if/else-conditional)
    6. [Determinate iteration](#determinate-iteration)
    7. [Indeterminate iteration](#indeterminate-iteration)
    8. [Stop and Next](#stop-and-next)
    9. [Sharps and Flats](#sharps-and-flats)
    10. [Tracks](#tracks)
    11. [New and Free](#new-and-free)
4. [Scope rules](#scope-rules)
5. [Syntax](#syntax)
    1. [Identifiers](#identifiers) 
    2. [Rests](#rests)
    3. [Chords](#chords)
    4. [Legato](#legato)
    5. [Operators](#operators)
6. [Examples](#examples)
7. [Extras](#extras)
    1. [Function overload](#function-overload)
    2. [Arpeggio](#arpeggio)
    3. [MIDI file generation](#MIDI-file-generation)

___
_It is recommended to use the [Fira Code] font (https://github.com/tonsky/FiraCode) for a better experience when programming in Vibrato._
___

## Program (example)
```vibrato
moderato() {
    |> ("Hello World!")|
}
```

```vibrato
moderato() {
    n: quarter|
    m: quarter|
    @ (n, m)|
    |> (n * m)|
}
```

## Expresiones y tipos de datos
### Whole
They are the notes that last a full measure and can have the values `maj` or `min`. Supports logical operators. Default value: `min`.
Example:
```vibrato
b0: whole <-> maj|
b1: whole <-> min or b0|
```

### Half
They are the notes that last half a measure and their values belong to the ASCII character set. They occupy 1 byte. Default value: `\0`.
Example:
```vibrato
c0: half <-> 'z'|
c1: half <-> '~'|
```

### Quarter
They are the notes that last a quarter of a measure and their values belong to the range [-2^31, 2^31 - 1] of the integers complement 2. Supports the arithmetic operators `+`, `-`, `*`, `/` and `mod`. Default value: `0`.
Example:
```vibrato
x0: quarter <-> x1 * x2 + 4|
```

### Eight
They are the notes that last an eighth measure and their values belong to the range [-2^63, 2^63 - 1] of the integers complement 2. Supports the arithmetic operators `+`, `-`, `*`, `/` and `mod`. Default value: `0`.
Example:
```vibrato
x0: quarter <-> 100000000000|
```

### 32th
They are the notes that last a thirty-second measure and their values are real numbers **simple precision**, whose range is ±1.18×10^−38 to ±3.4×10^38. They occupy 4 bytes. Default value: `0.0`.
Example:
```vibrato
f0: 32th <-> 3.0|
f1: 32th <-> 3.141592|
```

### 64th
They are the notes that last a sixty-fourth measure and their values are real numbers **double precision**, whose range is ±2.23×10^−308 to ±1.80×10^308. They occupy 8 bytes. Default value: `0.0`.
Example:
```vibrato
sf0: 64th <-> 3.0|
sf1: 64th <-> 1.6180339887|
```

### Melodies
A `Melody` is an array of notes of the same musical figure (white, round, black, etc.) consecutive in memory. Receives as a parameter between `< >` the type of unique musical figure that it will accept. The literals of melodies are of the form `[value_0, value_1, ..., value_n]`, `"c_0c_1 ...c_n"` if it is white or `Melody<type>(initial_size)` to declare a melody with an initial size. Default value: `[]`.
Example:
```vibrato
arr: Melody<quarter> <-> [1, 2, 3, 4]|
brr: Melody<half> <-> "abcdefg"|
crr: Melody<whole> <-> Melody<whole> (4)|
```

The values within the literals created with the last syntax will be the default if there is a default value for the type.

### Sample
A sample is a variable that points or references another variable by storing its memory address. If the variable `x` points to the variable `y`, it is said that "`x` is a sample of `y`" or "`x` samples `y`". Default value: the `TT` token, also called _TriTono_ (equivalent to `NULL` in C).
Example:
```vibrato
x: sample<Melody<whole>> <-> new Melody<whole> (n) |
```
The example declares a variable `x` which is a sample of a round melody of size `n`.

To dereference the unary operator suffix `!` must be used.



## Instructions
Assignments, declarations of a variable, IO instructions and uses of `free` must end in `|`. These types of instructions will be called _statements_.

### Assignment
```vibrato
variable <-> expression|
```
Executing this instruction has the effect of evaluating the expression on the right side and storing it in the variable on the left side.

### Block
A block is an instruction that has a sequence of instructions inside.
```
{
    <instruction_0>
    <instruction_1>
    ...
    <instruction_n>
}
```

### Entry
```vibrato
@ (var_0, var_1, ..., var_n)|
```
It allows obtaining data written by the user via standard input, storing the values in each of the variables passed to the record instruction, denoted by `@`. The values are obtained from the tokens of the line entered by the user.

This instruction works only with variables of type whole, half, quarter, eight, 32th and 64th.

### Exit
```vibrato
|> (var_0, var_1, ..., var_n)|
```
It prints on standard output the variables passed to the play instruction, denoted by `|>`, separated by a space.


### If/else conditional
The if/else conditional instruction verifies a condition and executes `instruction_0` if condition is `maj`, otherwise it executes `instruction_1`.
```vibrato
if(Condition) <instruction_0>|
else <instruction_1>|
```
`Condition` will always be an expression of `whole` type.

### Determinate iteration
The determined iteration will repeat a block according to the beginning, end and jump. Start and jump are optional. After `loop`, the name of the iteration variable must be indicated, followed by `:` and the type of data, which in this case is only of the whole type, that is, `quarter` and `eight`, are by default `quarter` type.
```
loop <id>:<type> {
    |>("Hard Rock Sofa!")|
} in (beginning, end, jump)
```
`start, end and jump` will always be arithmetic expressions.
`end` is exclusive, for example:
```vibrato
loop x {
    |>(x)|
} in (1,5,1)
```
```
1
2
3
4
```
`start` is optional and by default is 0.
`jump` is optional and by default is 1.

### Indeterminate iteration
The indeterminate iteration will repeat an instruction block as long as `condition` is `maj`, otherwise the cycle ends.
```
loop(condition){
    |>("Hard Rock Sofa!")|
}
```
`condition` is always an expression of `whole` type.

### Stop and Next
They work to explicitly stop the iterations of a cycle or move on to the next iteration without executing the rest of the code in the block. The stop instruction is denoted by the `|]` token and the next instruction is denoted by the `>>` token.
```
loop {
    if (b0) {
        >>
    } else {
        |]
    }
} in (10)
```

### Sharps and flats
This instruction is suffixed. The sharps `#` add one unit to the variable in use.
On the other hand, the flats `&` subtract one unit from the variable in use.
```vibrato
sb: quarter <-> 1|
sb#|
|>(sb)|
sb&|
|>(sb)|
```
```
2
1
```

### Tracks
Functions can be created for better handling and easier understanding of it, in a modular and simple way. The functions are known as tracks in Vibrato.

They are declared as follows
```vibrato
track <track_name>(parameters_list): <data_type_return> {
    ...
}
```
where `track` is the word reserved for declaring a function, followed by a valid identifier. Then the list of parameters `(parameters_list)`, first passing the name of the parameter followed by its type of data, being of the form `foo: quarter, bar: half`. If the prefix `>` is fixed to the type, the argument will be passed by reference. For example `track f(x: quarter, y_ref: >quarter)`.

Finally, the type of data that the function will return. If a return type is not specified, the track is a procedure.

Inside the keys are the instructions that will be executed as such as a Block. It should always have the reserved word `||` at the end of the expression to return, or only on a new line in case of not returning anything.
```vibrato
track intro(): whole {
    maj ||
}
track intro'(foo: whole) {
    ||
}
```
To call a function, it must be done as follows:
```vibrato
play <track_name> with (parameters_list)|
```
Example:
```
play intro with (a0, "the weeknd")|
```

### New and Free
Memory can be reserved or released dynamically according to the need of the programmer. There are the reserved words `new` and` free`.

To use `new` it must be indicated a literal of `chord` or `melody` or something of the form `scalar_type(expression)`, where `scalar_type` is a scalar type (black, white, etc.) and the expression will be used to initialize the variable. `New` returns an expression of `sample` type.

For `free` it must be used the identifier of a `sample`.

Example:
```vibrato
ptr: sample<Melody<quarter>>|
n: quarter|
@ (n)|
ptr <-> new Melody<quarter> (n)|
...
free ptr|
```

## Scope rules

To use a variable it must be previously declared in the block in which we are working. It is not possible to nest functions.
```
ra: quarter <-> 1|
ra2: quarter <-> 2|
|>(ra + ra2)|
```

If a variable with the same name of a variable external to the block in use is declared, it hides the external variable until the end of the block.
```
ra: quarter <-> 1
loop ra:quarter {
    |>(ra)|
} in (3)
|>(ra)|
```

Prints:
```
0
1
2

1
```

## Sintaxis
### Identificadores
Un identificador de variable es una cadena de caracteres de cualquier longitud compuesta únicamente de las letras desde la `A` hasta la `Z` (mayúsculas o minúsculas), los dígitos del `0` al `9`, y el caracter `_`. Puede tener al final cero o más `'`.

Los identificadores no pueden comenzar por un dígito y son sensibles a mayúsculas.

### Rests (Silencios)
Los silencios son líneas o bloques de texto que son ignoradas durante la ejecución y sirven para documentar el código fuente. A cada silencio se le asocia una figura musical para especificar la duración del mismo. Los distintos tipos de silencios son:

#### De una línea

- Silencio de blanca
```
-- blablabla
```

- Silencio de negra
```
~ blablabla
```

#### De varias líneas
- Silencio de corchea
```
*/ blablabla
blabalbla /*
```

- Silencio de semicorchea
```
**/ blablablabla
blabalbal /**
```

- Silencio de fusa
```
***/ blablabla
blablabal /***
```

- Silencio de semifusa
```
****/ blablabla
blabalbalbla /****
```

### Chords (Acordes)
Un acorde es una estructura de datos que se utiliza para organizar y almacenar distintos tipos de datos. La estructura general es la siguiente:

```vibrato
chord Identificador {
    id_1: tipo_1,
    id_2: tipo_2,
    ...
    id_n: tipo_n
}
```

Para un literal de acorde se usa la siguiente sintaxis:
```vibrato
Identificador (parametro_1, parametro_2, ... parametro_n)
```
Por ejemplo:
```vibrato
x <-> Sol (3.0, 'c', c0)|
```
donde `x` es una variable de tipo `Sol`, `c0` es una variable de tipo caracter y `Sol` está definido así:
```vibrato
chord Sol {
    x: 64th,
    y: half,
    z: half
}
```

Para acceder a un atributo de un acorde se usa el `.` al estilo C. Ejemplo:
```vibrato
val: 64th <-> sol.x|
```

### Legato
Un legato es una estructura de datos que se utiliza para almacenar uno de los tipos de datos presentes en la misma. La estructura general es la siguiente:
```vibrato
legato Identificador {
    id_1: tipo_1,
    id_2: tipo_2,
    ...
    id_n: tipo_n
}
```

### Operadores
Los operadores de cada tipo se muestran en orden descendente de precedencia.
#### Aritméticos
- Negativo `-` (unario)
- Modulo `mod`, División `/`, Multiplicación `*`
- Potencia `^`
- Suma `+`, Resta `-`

#### Lógicos
- Negación `not`
- Conjunción `and`
- Disyunción `or`

#### De comparación
- Igual a `=`, Distinto a `/=`
- Menor que `<`, Mayor que `>`, Menor o igual que `<=`, Mayor o igual que `>=`

#### Para melodías
- indexación:
```
val: whole <-> melodia[idx]|
```

#### Para acordes
- Acceder atributo: `acorde.atributo`

#### Para samples
- Dereferenciar: `sample!`

#### Precedencia de operadores
El orden de evaluación de operaciones en Vibrato es: dereferencia, acceder atributo de acorde, operadores sobre bool, operadores comparativos, operadores unarios aritméticos, operadores aritméticos restantes, operadores unarios restantes, operadores sobre melodías restantes, respetando el orden de precedencia de cada operador en cada una de ellas

## Preludio
El programador tendrá acceso a las siguientes funciones _built-in_:

### `to_ascii` y `from_ascii`
```vibrato
|> (to_ascii('A'))|
|> (from_ascii(66))|
```
Imprime
```
65
B
```
### length 
El operador unario prefijo `length` permite obtener la longitud de una melodía, retornando un valor de tipo `quarter`.
Ejemplo:
```vibrato
|> (length ['a', 'b', 'c'])|
```
Imprime
```
3
```
### concat
El operador binario infijo `<|>`, también llamado _concat_, recibe dos melodías del mismo tipo y las concatena en la primera dimensión.
Ejemplo:
```vibrato
arr: Melody<Melody<whole>> <-> [[maj, min], [maj, maj, min], [min]]|
brr: Melody<Melody<whole>> <-> [[min, min, min, min], [maj]]|

crr: Melody<Melody<whole>> <-> arr <|> brr|    -- crr = [[maj, min], [maj, maj, min], [min], [min, min, min, min], [maj]]
```
## Ejemplos
Ver carpeta [examples](https://github.com/vibrato-team/vibrato-programming-language/tree/master/examples)

## Extras

### Sobrecarga de funciones
Se podrá declarar más de una función con mismo identificador pero distintos argumentos.

### Crear Operadores
Se va a poder definir operadores entre tipos de datos solo si el operador no esta definido

### `from_legato`
Funcionque toma un legato y devuelve un string con el tipo de dato que se este usando en el legato.
```vibrato
|> (from_legato(L))|
```
```
"quarter"
```

### Arpeggio
Una variable de tipo `Arpeggio<tipo_0>`, donde `tipo_0` es un tipo cualquiera, es un diccionario que mapea `Melody<half>` a `tipo_0`. La sintaxis para **crear** un arpeggio sería así:
```vibrato
dict: Arpeggio<quarter> <-> {
    "abc" -> 0|
    "efg" -> n|
    ...
}|
```

A un arpeggio se le pueden agregar nuevos pares dinámicamente de la siquiente manera:
```vibrato
dict <~> ("v1br4t0", 140)|
dict <~> ("tr3m0l0", 300)|
```

Para acceder a un valor se hace de la siguiente manera:
```vibrato
x: quarter <-> dict["efg"]|
``` 

### Generación de archivo MIDI
___
#### Nota
Como MVP se podría ignorar todo tipo de iteraciones, _jumps_ y valores de expresiones en tiempo de compilación. Se le asigna unos pitch aleatorios a las variables y se lee de arriba hacia abajo siguiendo las demás reglas mencionadas abajo.
___

Al compilar un programa pasandole el flag `--midi` o `-m` al compilador, se generará un archivo `.midi` con una _pieza musical_ inspirada en el código fuente. La figura musical y valor de cada variable y comentario ayudarán a componer dicha pieza. 

Las **reglas** a seguir para generar el MIDI son tentativamente las siguientes: 

- la figura musical determinará la duración de la nota y el valor se mapeará a un _pitch_
- si la variable no se le puede determinar un valor en tiempo de compilación, se escogerá un pitch aleatorio
- se ignorará la semántica de los ifs y se procesarán todas las instrucciones dentro de un `if` como si estuviesen fuera del mismo
- para los `loop` se repetirá el contenido 2 veces, pero si el valor de la expresión es posible de determinar en tiempo de compilación dicho valor será el número de veces que se repetirá el contenido.
- si una instrucción utiliza una expresión cuyo valor se conoce en tiempo de compilación, ese valor será el pitch. Si no se conoce su valor, tendrá un pitch aleatorio.
- si la instrucción es una asignación, solo sonará el lado derecho.
- el procesamiento o interpretación del código será comenzando desde la primera línea hasta la última.
- si se invoca un track, el contenido del mismo sonará siguiendo las reglas antes mencionadas.

Por ejemplo, si se compila con `--midi` el programa

```vibrato
**/ Este es un ejemplo ilustrativo,
útil para demostrar la importancia de la elección de las figuras musicales. /**

moderato() {
    n0: quarter|
    n1: quarter|
    mrr: Melody<32th> <-> new Melody<32th>(10)|

    @ (n0, n1)|
    
    i: quarter <-> 0|
    loop {
        mrr[i] <-> n0 mod 10|
        n0 <-> n0*n1|
        i#|                 ~ Aumentar indice en uno
    }x(10)
}
```

se obtendrá una canción que
- comenzará con 3 (número de líneas del bloque de comentario) silencios de semicorchea
- luego 2 negras cuyo pitch es aleatorio
- luego 10 fusas cuyos pitches son aleatorios
- luego sonará otra vez 2 negras con pitch aleatorio
- luego una negra con pitch 0
- luego sonará 10 veces
    - una fusa con pitch aleatorio
    - una negra con pitch aleatorio
    - una negra con el pitch de `i` + 1
    - un silencio de negra
