# Vibrato
Imperative programming language based on music theory.

## Index
1. [Program (example)](#program-example)
2. [Expressions and data types](#expressions-and-data-types)
    1. [Whole](#whole)
    2. [Half](#half)
    3. [Quarter](#quarter)
    4. [Eighth](#eighth)
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

### Eighth
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

## Syntax
### Identifiers
A variable identifier is a string of characters of any length consisting solely of the letters from `A` to `Z` (upper or lower case), the digits from `0` to `9`, and the character `_` . It can have at the end zero or more `'`.

Identifiers cannot start with a digit and are case sensitive.

### Rests
Silences are lines or blocks of text that are ignored during execution and are used to document the source code. Each silence is associated with a musical figure to specify its duration. The different types of silences are:

#### Of one line

- Silence of whole note
```
-- blahblahblah
```

- Silence of half note
```
~ blahblahblah
```

#### Multi-line
- Silence of eighth note
```
*/ blahblahblah
blahblahblah /*
```

- Silence sixteenth
```
**/ blahblahblahblah
blahblahblah /**
```

- Silence of thirty-second note
```
***/ blahblahblah
blahblahblah /***
```

- Silence of sixty-fourth
```
****/ blahblahblah
blahblahblahblah /****
```

### Chords
A chord is a data structure that is used to organize and store different types of data. The general structure is as follows:

```vibrato
chord Identifier {
    id_1: type_1,
    id_2: type_2,
    ...
    id_n: type_n
}
```

For a chord literal the following syntax is used:
```vibrato
Identifier (parameter_1, parameter_2, ... parameter_n)
```
For example:
```vibrato
x <-> Sol (3.0, 'c', c0)|
```
where `x` is a variable of type `Sol`, `c0` is a variable of type character and `Sol` is defined as follows:
```vibrato
chord Sol {
    x: 64th,
    y: half,
    z: half
}
```

To access an attribute of a chord, `.` style C most be used. For example:
```vibrato
val: 64th <-> sol.x|
```

### Legato
A legato is a data structure that is used to store one of the types of data present in it. The general structure is as follows:
```vibrato
legato Identifier {
    id_1: type_1,
    id_2: type_2,
    ...
    id_n: type_n
}
```

### Operators
Operators of each type are displayed in descending order of precedence.
#### Arithmetic
- Negative `-` (unary)
- Module `mod`, Division `/`, Multiplication `*`
- Power `^`
- Add `+`, Subtract `-`

#### Logic
- Denial `not`
- Conjunction `and`
- Disjunction `or`

#### For comparison
- Equal to `=`, other than `/=`
- Less than `<`, Greater than `>`, Less than or equal to `<=`, Greater than or equal to `>=`

#### For melodies
- Indexing:
```
val: whole <-> melody[idx]|
```

#### For chords
- Access attribute: `chord.attribute`

#### For samples
- Dereference: `sample!`

#### Operator precedence
The order of evaluation of operations in Vibrato is: dereference, access chord attribute, bool operators, comparative operators, arithmetic unary operators, remaining arithmetic operators, remaining unary operators, operators on remaining melodies, respecting the order of precedence of each operator in each of them.

## Prelude
The programmer will have access to the following built-in functions:

### `to_ascii` and `from_ascii`
```vibrato
|> (to_ascii('A'))|
|> (from_ascii(66))|
```
Prints
```
65
B
```
### Length 
The unary operator prefix `length` allows to obtain the length of a melody, returning a value of `quarter` type.
For example:
```vibrato
|> (length ['a', 'b', 'c'])|
```
Prints
```
3
```
### Concat
The binary operator infix `<|>`, also called _concat_, receives two melodies of the same type and concatenates them in the first dimension.
For example:
```vibrato
arr: Melody<Melody<whole>> <-> [[maj, min], [maj, maj, min], [min]]|
brr: Melody<Melody<whole>> <-> [[min, min, min, min], [maj]]|

crr: Melody<Melody<whole>> <-> arr <|> brr|    -- crr = [[maj, min], [maj, maj, min], [min], [min, min, min, min], [maj]]
```
## Examples
See folder [examples](https://github.com/vibrato-team/vibrato-programming-language/tree/master/examples)

## Extras

### Function overload
More than one function can be declared with the same identifier but different arguments.

### Create operators
It will be possible to define operators between data types only if the operator is not defined.

### `from_legato`
Function that takes a legato and returns a string with the type of data that is being used in the legato.
```vibrato
|> (from_legato(L))|
```
```
"quarter"
```

### Arpeggio
A variable of type `Arpeggio <type_0>`, where `type_0` is any type, is a dictionary that maps `Melody <half>` to `type_0`. The syntax for **creating** an arpeggio would be like this:
```vibrato
dict: Arpeggio<quarter> <-> {
    "abc" -> 0|
    "efg" -> n|
    ...
}|
```

To an arpeggio, new pairs can be added dynamically in the following way:
```vibrato
dict <~> ("v1br4t0", 140)|
dict <~> ("tr3m0l0", 300)|
```

To access a value:
```vibrato
x: quarter <-> dict["efg"]|
``` 

### MIDI file generation
___
#### Note
As MVP, all kinds of iterations, _jumps_ and values of expressions could be ignored at compile time. The variables are assigned a random pitch and have to be read from top to bottom following the other rules mentioned below.
___

When compiling a program by passing the `--midi` or` -m` flag to the compiler, a `.midi` file will be generated with a _piece of music_ inspired by the source code. The musical figure and value of each variable and comment will help to compose this piece.

The ** rules ** to follow to generate the MIDI are tentatively the following:

- The musical figure will determine the duration of the note and the value will be mapped to a pitch.
- If the variable cannot be determined at compile time, a random pitch will be chosen.
- The semantics of the ifs will be ignored and all the instructions within an `if` will be processed as if they were outside it.
- For the `loops` the content will be repeated 2 times, but if the value of the expression is impossible to determine at compile time, that value will be the number of times the content will be repeated.
- If an instruction uses an expression whose value is known at compile time, that value will be the pitch. If its value is unknown, it will have a random pitch.
- If the instruction is an assignment, only the right side will sound.
- The processing or interpretation of the code will be done starting from the first line to the last.
- If a track is invoked, its content will sound following the rules mentioned before.

For example, if the program is compiled with `--midi`

``` vibrato
**/ This is an illustrative example,
Useful to demonstrate the importance of the choice of musical figures. /**

moderato() {
    n0: quarter|
    n1: quarter|
    mrr: Melody<32th> <-> new Melody<32th>(10)|

    @ (n0, n1)|
    
    i: quarter <-> 0|
    loop {
        mrr[i] <-> n0 mod 10|
        n0 <-> n0*n1|
        i#|                 ~ Increase index by one
    }x(10)
}
```

It will be obtained a song that:
- starts with 3 (number of lines in the comment block) silences of sixteenth note
- then 2 quarter notes whose pitch is random
- then 10 thirty-second notes whose pitches are random
- then again 2 quarter notes with random pitch
- then a quarter note with pitch 0
- then it will play 10 times:
     - a thirty-second note with random pitch
     - a quarter note with random pitch
     - a quarter note with the pitch of `i` + 1
     - a silence of quarter note
