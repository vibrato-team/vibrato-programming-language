# Vibrato
Lenguaje de programación imperativo basado en teoría musical.

## Índice
1. [Programa (ejemplo)](#programa-ejemplo)
2. [Expresiones y tipos de datos](#expresiones-y-tipos-de-datos)
    1. [Whole (Redonda)](#whole-redonda)
    2. [Half (Blanca)](#half-blanca)
    3. [Quarter (Negra)](#quarter-negra)
    4. [32th (Fusa)](#32th-fusa)
    5. [64th (Semifusa)](#64th-semifusa)
    6. [Melodies (Melodías)](#melodies-melodías)
    7. [Sample](#sample)
3. [Instrucciones](#instrucciones)
    1. [Asignación](#asignación)
    2. [Bloque](#bloque)
    3. [Entrada](#entrada)
    4. [Salida](#salida)
    5. [Condicional if/else](#condicional-if--else)
    6. [Iteración determinada](#iteración-determinada)
    7. [Iteración indeterminada](#iteración-indeterminada)
    8. [Stop y Next](#stop-y-next)
    9. [Sostenidos y Bemoles](#sostenidos-y-bemoles)
    10. [Tracks](#tracks)
    11. [New y Free](#new-y-free)
4. [Reglas de alcance](#reglas-de-alcance)
5. [Sintaxis](#sintaxis)
    1. [Identificadores](#identificadores) 
    2. [Rests (Silencios)](#rests-silencios)
    3. [Chords (Acordes)](#chords-acordes)
    4. [Legato](#legato)
    5. [Operadores](#operadores)
6. [Ejemplos](#ejemplos)
7. [Extras](#extras)
    1. [Sobrecarga de funciones](#sobrecarga-de-funciones)
    2. [Arpeggio](#arpeggio)
    3. [Generación de archivo MIDI](#generación-de-archivo-midi)

___
_Recomendado utilizar la fuente [Fira Code](https://github.com/tonsky/FiraCode) para una mejor experiencia al programar en Vibrato._
___

## Programa (ejemplo)
```vibrato
moderato() {
    |> ("Hello World!")
}
```

```vibrato
moderato() {
    n: quarter|
    m: quarter|
     (n, m)|
    |> (n * m)
}
```

## Expresiones y tipos de datos
### Whole (Redonda)
Son las notas que duran un compás completo y pueden tener los valores `maj` o `min`. Soporta los operadores lógicos. Valor default: `min`.
Ejemplo:
```vibrato
b0: whole <-> maj|
b1: whole <-> min or b0
```

### Half (Blanca)
Son las notas que duran medio compás y sus valores pertenecen al conjunto de caracteres ASCII. Ocupan 1 Byte. Valor default: `\0`.
Ejemplo:
```vibrato
c0: half <-> 'z'|
c1: half <-> '~'
```

### Quarter (Negra)
Son las notas que duran un cuarto de compás y sus valores pertenecen al rango [-2^31, 2^31 - 1] de los enteros complemento a 2. Soporta los operadores aritméticos `+`, `-`, `*`, `/` y `mod`. Valor default: `0`.
Ejemplo:
```vibrato
x0: quarter <-> x1 * x2 + 4
```

### Eighth (Corchea)
Son las notas que duran un octavo de compás y sus valores pertenecen al rango [-2^63, 2^63 - 1] de los enteros complemento a 2. Soporta los operadores aritméticos `+`, `-`, `*`, `/` y `mod`. Valor default: `0`.
Ejemplo:
```vibrato
x0: quarter <-> 100000000000
```

### 32th (Fusa)
Son las notas que duran un treintavo de compás completo y sus valores son numeros reales **precision simple**, cuyo rango es ±1.18×10^−38 to ±3.4×10^38. Ocupan 4 bytes. Valor default: `0.0`.
Ejemplo:
```vibrato
f0: 32th <-> 3.0|
f1: 32th <-> 3.141592
```

### 64th (Semifusa)
Son las notas que duran un treintavo de compás completo y sus valores son numeros reales **precision doble**, cuyo rango es ±2.23×10^−308 to ±1.80×10^308. Ocupan 8 bytes. Valor default: `0.0`.
Ejemplo:
```vibrato
sf0: 64th <-> 3.0|
sf1: 64th <-> 1.6180339887
```

### Melodies (Melodías)
Una `Melody` es un arreglo de notas de una misma figura musical (blanca, redonda, negra, etc.) consecutivas en memoria. Recibe como parametro entre `< >` el tipo de figura musical único que aceptará. Los literales de melodías son de la forma `[valor_0, valor_1, ..., valor_n]`,`"c_0c_1...c_n"` si es de blancas o `Melody<tipo>(tamano_inicial)` para declarar una melodía con un tamaño inicial. Valor default: `[]`.

Ejemplo:
```vibrato
arr: Melody<quarter> <-> [1, 2, 3, 4]|
brr: Melody<half> <-> "abcdefg"|
crr: Melody<whole> <-> Melody<whole> (4)
```

Los valores dentro de los literales creados con la última sintaxis serán los default en caso de existir un valor default para el tipo.

### Sample
Un sample es una variable que apunta o referencia a otra variable almacenando su dirección de memoria. Si la variable `x` apunta a la variable `y`, se dice que "`x` es un sample de `y`" o "`x` _samplea_ a `y`". Valor default: el token `TT`, también llamado _TriTono_ (equivale al `NULL` en C).
Ejemplo:
```vibrato
x: sample<Melody<whole>> <-> new Melody<whole> (n) |
```
En el ejemplo se declara una variable `x` que es un sample de una melodía de redondas de tamaño `n`.

Para dereferenciar se usa el operador unario sufijo `!`.



## Instrucciones

### Asignación
```vibrato
variable <-> expresion
```
Ejecutar esta instrucción tiene el efecto de evaluar la expresión del lado derecho y almacenarla en la variable del lado izquierdo.

### Bloque
Un bloque es una instrucción que tiene dentro una secuencia de instrucciones finalizadas por `|`. El `|` es opcional para la última instrucción del bloque.
```
{
    <instrucción_0>|
    <instrucción_1>|
    ...
    <instrucción_n>|
}
```

### Entrada
```vibrato
@ (var_0, var_1, ..., var_n)
```
Permite obtener datos escritos por el usuario vía entrada estándar, almacenando los valores en cada una de las variables pasadas a la instrucción de record, denotada por `@`. Los valores se obtienen de los tokens de la línea ingresada por el usuario.

Esta instrucción funciona únicamente con variables de tipo whole, half, quarter, eight, 32th y 64th.

### Salida
```vibrato
|> (var_0, var_1, ..., var_n)
```
Imprime en salida estándar las variables pasadas a la instrucción de play, denotada por `|>`, separadas por un espacio. 


### Condicional if / else
La instruccion condicional if/else verifica una condicion y ejecuta `instruccion_0` si condicion es `maj`, de lo contrario ejecuta `instruccion_1`. 
```vibrato
if(Condicion) <instrucción_0>
else <instrucción_1>
```
`Condicion` siempre va a ser una expresión de tipo `whole`.

### Iteración determinada
La iteracion determinada va a repetir un bloque segun el inicio, fin y salto. Inicio y salto son opcionales. luego de `loop` se debe indicar el nombre de la variable de iteracion, seguido de `:` y el tipo de dato, que en este caso solo de tipo entero, es decir, `quarter` y `eight`, por default agarra tipo `quarter`.
```
loop <id>:<tipo> {
    |>("Hard Rock Sofa!")
} in (inicio, fin, salto)
```
`inicio, fin y salto` siempre van a ser expresiones aritmeticas.
`fin` es exclusivo, por ejemplo:
```vibrato
loop x {
    |>(x)
} in (1,5,1)
```
```
1
2
3
4
```

### Iteración indeterminada
La iteracion indeterminada va a repetir un bloque de instrucciones mientras `condicion` sea `maj`, en caso contrario, se termina el ciclo. 
```
loop(condicion){
    |>("Hard Rock Sofa!")
}
```
`condicion` siempre va a ser una expresion de tipo `whole`.

### Stop y Next
Sirven para detener explícitamente las iteraciones de un ciclo o pasar a la siguiente iteración sin ejecutar el resto del código en el bloque. La instrucción de stop se denota por el token `|]` y la instrucción de next se denota por el token `>>`.
```
loop {
    if (b0) {
        >>
    } else {
        |]
    }
} in (10)
```

### Sostenidos y Bemoles
Esta intrucción va en forma sufija. Los Sostenidos `#` suman una unidad a la variable en uso. 
Por otro lado los Bemoles `&` restan una unidad a la variable en uso.
```vibrato
sb: quarter <-> 1
sb#
|>(sb)
sb&
|>(sb)
```
```
2
1
```

### Tracks
Se pueden crear funciones para mejor manejo y mas facil entendimiento del mismo, de una forma modular y sencilla. A las funciones se les conoce como tracks en Vibrato. 

Se declaran de la siguiente manera
```vibrato
track <nombre_track>(lista_parametros): <tipo_dato_retorno> {
    ...
}
```
donde `track` es la palabra reservada para declarar una funcion, seguido de un identificador válido. Luego la lista de parametros `(lista_parametros)`, pasando primero el nombre del parametro seguido de su tipo de dato, quedando de la forma `foo: quarter, bar: half`. Si al tipo se le arregla el prefijo `>`, el argumento se pasará por referencia. Por ejemplo `track f(x: quarter, y_ref: >quarter)`. 

Por último el tipo de dato que va a retornar la función. Si no se especifica un tipo de retorno el track es un procedimiento.

Dentro de las llaves tenemos las instrucciones que van a ser ejecutas tal cual como un Bloque. Siempre debe llevar la palabra reservada `||` al final de la expresion a retornar, o solo en una linea nueva en caso de no retornar nada.

```vibrato
track intro(): whole {
    maj ||
}
track intro'(foo: whole) {
    ||
}
```
Para hacer el llamado a una funcion, se debe hacer de la siguiente forma:
```vibrato
play <nombre_track> with (lista_parametros)
```
Ejemplo:
```
play intro with (a0, "the weeknd")|
```

### New y Free
Se puede reservar o liberar memoria dinamicamente según la necesidad de programador. Existen las palabras reservadas `new` y `free`.

Para usar `new` debes indicarle un literal de `chord` o `Melody` o algo de la forma `tipo_escalar(expresión)`, donde `tipo_escalar` es un tipo escalar (negra, blanca, etc.) y la expresión será para inicializar la variable. `new` retorna una expresión de tipo `sample`.

Para `free` debes usar el identificador de un `sample`.

Ejemplo:
```vibrato
ptr: sample<Melody<quarter>>|
n: quarter|
@ (n)|
ptr <-> new Melody<quarter> (n)|
...
free ptr|
```

## Reglas de alcance

Para utilizar una variable debe estar previamente declarada en el bloque en el que se este trabajando. No es posible anidar funciones.
```
ra: quarter <-> 1
ra2: quarter <-> 2
|>(ra + ra2)
```

Si se declara una variable con un mismo de una variable externa al bloque en uso, esta esconde la variable externa hasta el final del bloque
```
ra: quarter <-> 1
loop ra:quarter {
    |>(ra)
} in (3)
|>(ra)
```

Imprime:
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
    id_1: tipo_1|
    id_2: tipo_2|
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
    x: 64th|
    y: half|
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
    id_1: tipo_1|
    id_2: tipo_2|
    ...
    id_n: tipo_n
}
```

### Operadores
Los operadores de cada tipo se muestran en orden descendente de precedencia.
#### Aritméticos
- Negativo `-` (unario)
- Modulo `mod`, División `/`, Multiplicación `*`
- Potencia `**`
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

#### Precedencia de operadores
El orden de evaluación de operaciones en Vibrato es: dereferencia, acceder atributo de acorde, operadores sobre bool, operadores comparativos, operadores aritméticos, operadores unarios restantes, operadores sobre melodías restantes, respetando el orden de precedencia de cada operador en cada una de ellas

## Preludio
El programador tendrá acceso a las siguientes funciones _built-in_:

### `to_ascii` y `from_ascii`
```vibrato
|> (to_ascii('A'))|
|> (from_ascii(66))
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
}
```

A un arpeggio se le pueden agregar nuevos pares dinámicamente de la siquiente manera:
```vibrato
dict <~> ("v1br4t0", 140)|
dict <~> ("tr3m0l0", 300)
```

Para acceder a un valor se hace de la siguiente manera:
```vibrato
x: quarter <-> dict["efg"]
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
