# Entrega2

Fabián Díaz & Sergio Gálvez

## Objetivos extra

Entrega 2: Convención de llamada a función x86-64

## Modificaciones generales

### Asm
En Asm se definen los registros e instrucciones assembly.
Tambien se define la traducción a strings.

- Se añadieron los nuevos registros e instrucciones a usar
- Se anadio la traduccion de las nuevas instrucciones a strings

### Ast
En Ast se definen las primitivas y operaciones de la representación intermedia.
Tambien se define el Pretty printing para los frameworks de testeo.

- Se añadieron las nuevas primitivas y expresiones solicitadas
- Se añadió el Pretty printing de las nuevas expresiones y tipos

### Compile
En Compile se define la compilación de expresiones y el pipeline de compilación.
Tambien se realizala compilacion de las expresiones a una lista de instrucciones.

- Se añadieron al compilador las nuevas expresiones y operaciones solicitadas

### Lib
En lib se definen los ambientes de compilación para variables y funciones
Tambien se define el manejo de errores y otras funciones útiles
Tambien se definen todos los snippets para compilación de funciones

- Se añadió la verificación de tipos y el manejo de errores con c
- Se añadieron los snippets para la convención de llamada a función x86-64

### Interp
En Interp se define un interprete para la representación intermetdia del compilador.
El interprete define un ambiente y funciones de lift para realizar las operaciones.

- No se hicieron modificaciones en este archivo

### Parse
En Parse de define el parseo desde archivos y strings.
Este transforma las sexp de CCSexp en expresiones de nuestra representación.

- Se añadieron los nuevos tipos y expresiones a la funcion de parse

### Tests
Los test estan divididos entre bbctest y test en exec.
Los test de bbctest pruebas el proceso completo de compilación.
Los test en exec prueban funciones especificas del compilador.

- Se añadieron nuevos bbctests para las nuevas expresiones del compilador

- No se añadieron nuevos test normales dentro de la carpeta execs

## Observaciones
- Se logró implementar todo lo mencionado en el enunciado
- El interprete no genera warnings al ejecutar el buildeo
