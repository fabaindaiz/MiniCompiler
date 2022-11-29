# Entrega3

Fabián Díaz & Sergio Gálvez

## Objetivos extra

Entrega 2: Convención de llamada a función x86-64
Entrega 3: Pattern-matching de tuplas
Entrega 4: Recursión

## Modificaciones generales

### Asm
En Asm se definen los registros e instrucciones assembly.
Tambien se define la traducción a strings.

- Sin cambios

### Ast
En Ast se definen las primitivas y operaciones de la representación intermedia.
Tambien se define el Pretty printing para los frameworks de testeo.

- Sin cambios

### Compile
En Compile se define la compilación de expresiones y el pipeline de compilación.
Tambien se realizala compilacion de las expresiones a una lista de instrucciones.

- Se añadio el manejo de memoria mediante el GC en sys.c
- Se modificaron algunas etapas de la compilación para adecuarse al GC

### Lib
En lib se definen los ambientes de compilación para variables y funciones
Tambien se define el manejo de errores y otras funciones útiles
Tambien se definen todos los snippets para compilación de funciones

- Se movieron algunas funciones hacia Util
- Se añadio una función que permite llamar a try_gc para solicitar memoria

### Util
En util se definen todas las funciones útiles que se utilizan para el compilador

- Se movieron algunas funciones desde Lib

### Interp
En Interp se define un interprete para la representación intermetdia del compilador.
El interprete define un ambiente y funciones de lift para realizar las operaciones.

- Sin cambios

### Parse
En Parse de define el parseo desde archivos y strings.
Este transforma las sexp de CCSexp en expresiones de nuestra representación.

- Sin cambios

### Run-time (sys.c)
En Run-Time se encuentra la función principal de c que llama al código assembly
Tambien tiene funciones utiles y otras que pueden ser llamadas desde assembly

- Se movieron algunas funciones dhacia GC y Lib
- Se modifico la función principal para inicializar el GC y la memoria

### GC (gc.c)
En GC se encuentra las funciones involucradas en el funcionamiento del GC
Tambien de controlan los parámetros con que funcionará el GC

- Se movieron algunas funciones desde Run-time
- Se completaron las funciones involucradas en el GC

### Lib (lib.c)
En Lib se tiene las funciones básicas para el funcionamiento del codigo assembly
Tambien tiene funciones utiles y de manejo de errores que pueden ser llamadas

- Se movieron algunas funciones desde Run-time

### Tests
Los test estan divididos entre bbctest y test en exec.
Los test de bbctest pruebas el proceso completo de compilación.
Los test en exec prueban funciones especificas del compilador.

- Se añadió un par de test para verificar el comportamiento del GC

## Observaciones
- Se logró implementar todo lo mencionado en el enunciado
- El compilador no genera warnings al ejecutar el buildeo
