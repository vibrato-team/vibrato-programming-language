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
4. [Reglas de alcance](#reglas-de-alcance)
5. [Sintaxis](#sintaxis)
    1. [Identificadores](#identificadores) 
    2. [Rests (Silencios)](#rests-silencios)
    3. [Operadores](#operadores)
    4. [Chords (Acordes)](#chords-acordes)
    5. [Legato](#legato)
6. [Ejemplos](#ejemplos)
7. [Extras](#extras)
    1. [Arpeggio](#arpeggio)
    2. [Generación de archivo MIDI](#generación-de-archivo-midi)

## Programa (ejemplo)
```vibrato
main() {
    play ("Hello World!")
}
```

```vibrato
main() {
    n: quarter|
    m: quarter|
    record (n, m)|
    play (n * m)
}
```

## Expresiones y tipos de datos
### Whole (Redonda)
Son las notas que duran un compás completo y pueden tener los valores `maj` o `min`. Soporta los operadores lógicos.
Ejemplo:
```vibrato
b0: whole <-> maj|
b1: whole <-> min or b0
```

### Half (Blanca)
Son las notas que duran medio compás y sus valores pertenecen al conjunto de caracteres ASCII. Ocupan 1 Byte.
Ejemplo:
```vibrato
c0: half <-> 'z'|
c1: half <-> '~'
```

### Quarter (Negra)
Son las notas que duran un cuarto de compás y sus valores pertenecen al rango [-2^31, 2^31 - 1] de los enteros complemento a 2. Soporta los operadores aritméticos `+`, `-`, `*`, `/` y `mod`.
Ejemplo:
```vibrato
x0: quarter <-> x1 * x2 + 4
```

### Eighth (Corchea)
Son las notas que duran un octavo de compás y sus valores pertenecen al rango [-2^63, 2^63 - 1] de los enteros complemento a 2. Soporta los operadores aritméticos `+`, `-`, `*`, `/` y `mod`.
Ejemplo:
```vibrato
x0: quarter <-> 100000000000
```

### 32th (Fusa)
Son las notas que duran un treintavo de compás completo y sus valores son numeros reales **presicion simple**, cuyo rango es ±1.18×10^−38 to ±3.4×10^38. Ocupan 4 bytes.
Ejemplo:
```vibrato
f0: 32th <-> 3.0|
f1: 32th <-> 3.141592
```

### 64th (Semifusa)
Son las notas que duran un treintavo de compás completo y sus valores son numeros reales **presicion doble**, cuyo rango es ±2.23×10^−308 to ±1.80×10^308. Ocupan 8 bytes.
Ejemplo:
```vibrato
sf0: 64th <-> 3.0|
sf1: 64th <-> 1.6180339887
```

### Melodies (Melodías)
Una `Melody` es un arreglo de notas de una misma figura musical (blanca, redonda, negra, etc.) consecutivas en memoria. Recibe como parametro entre `< >` el tipo de figura musical único que aceptará. Se puede declarar un tamaño inicial de la melodía mediante la sintaxis `Melody<tipo> (n)`, donde `n` es una expresión aritmética de tipo `quarter` o `eighth`. Los literales de melodías son de la forma `[valor_0, valor_1, ..., valor_n]` o `"c_0c_1...c_n"` si es de blancas.

Ejemplo:
```vibrato
arr: Melody<quarter> <-> [1, 2, 3, 4]|
brr: Melody<32th> <-> [0.5, 0.4, 0.3]|
crr: Melody<whole> <-> Melody<whole> (4)
```

### Sample
Un sample es una variable que apunta o referencia a otra variable almacenando su dirección de memoria. Si la variable `x` apunta a la variable `y`, se dice que "`x` es un sample de `y`" o "`x` _samplea_ a `y`".
Ejemplo:
```vibrato
x: sample<Melody<whole>> <-> new Melody<whole> (n) |
```
En el ejemplo se declara una variable `x` que es un sample de una melodía de redondas de tamaño `n`.



## Instrucciones

### Asignación
```vibrato
variable <-> expresion
```
Ejecutar esta instrucción tiene el efecto de evaluar la expresión del lado derecho y almacenarla en la variable del lado izquierdo.

### Bloque
Un bloque es una instrucción que tiene dentro una secuencia de instrucciones finalizadas por `|`.
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
record (var_0, var_1, ..., var_n)
```
Permite obtener datos escritos por el usuario vía entrada estándar, almacenando los valores en cada una de las variables pasadas a `record`. Los valores se obtienen de los tokens de la línea ingresada por el usuario.

Esta instrucción funciona únicamente con variables de tipo whole, half, quarter, eight, 32th y 64th.

### Salida
```vibrato
play (var_0, var_1, ..., var_n)
```
Imprime en salida estándar las variables pasadas a `play` separadas por un espacio. 


### Condicional if / else
La instruccion condicional if/else verifica una condicion y ejecuta instruccion_0 si condicion es True, de lo contrario ejecuta la instruccion_1. 
```vibrato
if(Condicion){
    <instrucción_0>
} 
else{
    <instrucción_1>
}
```
`Condicion` siempre va a ser una expresion booleana.

### Iteracion determinada
La iteracion determinada va a repetir un bloque de instrucciones segun la cantidad de `repeticiones`. 
```
{
    play("Hard Rock Sofa!")
} x (repeticiones)
```
`repeticiones` siempre va a ser una expresion aritmetica

### Iteracion indeterminada
La iteracion indeterminada va a repetir un bloque de instrucciones mientras `condicion` sea True, en caso contrario, se termina el ciclo. 
```
loop(condicion){
    play("Hard Rock Sofa!")
}
```
`condicion` siempre va a ser una expresion booleana

### stop y resume
Sirven para detener explícitamente un las iteraciones de un ciclo o pasar a la siguiente iteración sin ejecutar el resto del código en el bloque.

### Funciones
Se pueden crear funciones para mejor manejo y mas facil entendimiento del mismo, de una forma modular y sencilla. 

Se declaran de la siguiente manera
```vibrato
track <nombre_track>(lista_parametros): <tipo_dato_retorno> {
    ...
}
```
donde `track` es la palabra reservada para declarar una funcion, seguido de un nombre valido para las funciones(iguales a los identificadores de variables). Luego la lista de parametros (lista_parametros), donde primero el nombre del parametro seguido de su tipo de dato, queda de la forma `foo: quarter, bar: half`.  Por ultimo el tipo de dato que va a retornar la funcion, puede retornar nada.

Dentro de las llaves tenemos las instrucciones que van a ser ejecutas tal cual como un Bloque. Siempre debe llevar la palabra reservada `||` al final de la expresion a retornar, o solo en una linea nueva en caso de no retornar nada.

```vibrato
track intro(): whole {
    Maj ||
}
track intro'(foo: whole) {
    ||
}
```
Para hacer el llamado a una funcion, se debe hacer de la siguiente forma:
```vibrato
play <nombre_track> track with (lista_parametros)
```

### New y Free
Se puede reservar o liberar memoria dinamicamente segun la necesidad de programador. Existen las palabras reservadas `new` y `free`.

Para usar new debes indicarle un literal de `chord` o `Melody` o algo de la forma `tipo_escalar(expresión)`, donde `tipo_escalar` es un tipo escalar (negra, blanca, etc.) y la expresión será para inicializar la variable. Para free debes usar el id del sample.

## Reglas de alcance

Para utilizar una variable debe estar previamente declarada en el bloque en el que se este trabajando. No es posible anidar funciones.
```
ra: quarter <-> 1
ra2: quarter <-> 2
play(ra + ra2)
```

Si se declara una variable con un mismo de una variable externa al bloque en uso, esta esconde la variable externa hasta el final del bloque
```
ra: quarter <-> 1
{
    ra: quarter <-> 0
    ra#
    play(ra)
} x (3)
play(ra)
```

Imprime:
```
1
2
3

1
```

## Sintaxis
### Identificadores
Un identificador de variable es una cadena de caracteres de cualquier longitud compuesta únicamente de las letras desde la `A` hasta la `Z` (mayúsculas o minúsculas), los dígitos del `0` al `9`, y el caracter `_`. Puede tener al final cero o más `'`.

Los identificadores no pueden comenzar por un dígito y son sensibles a mayúsculas: la variable `var` es diferente a la variable `Var`, que a su vez son distintas a la variable `VAR`.

### Rests (Silencios)
Los silencios son líneas o bloques de texto que son ignoradas durante la ejecución y sirven para documentar el código fuente. A cada silencio se le asocia una figura musical para especificar la duración del mismo. Los distintos tipos de silencios son:

#### De una línea
- Silencio de redonda
```
- blablabla
```

- Silencio de blanca
```
-- blablabla
```

- Silencio de negra
```
~ blablabla
```




## Sintaxis 
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
- Igual a `=`, Distinto a `!=`
- Menor que `<`, Mayor que `>`, Menor o igual que `<=`, Mayor o igual que `>=`

#### Precedencia de operadores
El orden de evaluación de operaciones en Vibrato es: operadores sobre bool, operadores comparativos, operadores aritméticos, unarias, respetando el orden de precedencia de cada operador en cada una de ellas

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

Para "instanciar" un acorde se usa la siguiente sintaxis:
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

## Ejemplos

## Extras

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

Por ahora se tienen en mente un enfoque para la creación de la pieza musical que consiste en una especia de "interpretación simplista y aleatoria" del código fuente.

Las **reglas** a seguir para generar el MIDI son tentativamente las siguientes: 

- la figura musical determinará la duración de la nota y el valor se mapeará a un _pitch_
- si la variable no se le puede determinar un valor en tiempo de compilación, se escogerá un pitch aleatorio
- se ignorará la semántica de los ifs y se procesarán todas las instrucciones dentro de un `if` como si estuviesen fuera del mismo
- para los `loop` y `{}x(<expresión dinámica>)` se repetirá el contenido 2 veces, pero si el valor de la expresión es posible de determinar en tiempo de compilación dicho valor será el número de veces que se repetirá el contenido.
- si una instrucción utiliza una expresión cuyo valor se conoce en tiempo de compilación, ese valor será el pitch. Si no se conoce su valor, tendrá un pitch aleatorio.
- si la instrucción es una asignación, solo sonará el lado derecho.
- el procesamiento o interpretación del código será comenzando desde la primera línea hasta la última.
- si se invoca un track, el contenido del mismo sonará siguiendo las reglas antes mencionadas.

Por ejemplo, si se compila con `--midi` el programa

```vibrato
**/ Este es un ejemplo ilustrativo,
útil para demostrar la importancia de la elección de las figuras musicales. /**

main() {
    n0: quarter|
    n1: quarter|
    mrr: Melody<32th> <-> new Melody<32th>(10)|

    record (n0, n1)|
    i: quarter <-> 0|
    {
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
