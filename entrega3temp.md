# Entrega3

Fabián Díaz & Sergio Gálvez

<<<<<<< HEAD
## Objetivos extra

Entrega 2: Convención de llamada a función x86-64
Entrega 3: Pattern-matching de tuplas

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
- Se añadieron nuevos tipos para las funciones y programas
- Se añadió el Pretty printing de las nuevas expresiones y tipos

### Compile
En Compile se define la compilación de expresiones y el pipeline de compilación.
Tambien se realizala compilacion de las expresiones a una lista de instrucciones.

- Se añadieron al compilador las nuevas expresiones y operaciones solicitadas
- Se añadieron los manejos necesarios para usar el heap y hacer mutaciones

### Lib
En lib se definen los ambientes de compilación para variables y funciones
Tambien se define el manejo de errores y otras funciones útiles
Tambien se definen todos los snippets para compilación de funciones

- Se añadió la verificación de tipos y el manejo de errores para tuplas

### Interp
En Interp se define un interprete para la representación intermetdia del compilador.
El interprete define un ambiente y funciones de lift para realizar las operaciones.

- Se añadieron verificaciones de tipos y manejo de errores de aridad

### Parse
En Parse de define el parseo desde archivos y strings.
Este transforma las sexp de CCSexp en expresiones de nuestra representación.

- Se añadieron los nuevos tipos y expresiones a la funcion de parse
- Se añadieron nuevas funcionalidades mediante azucar sintáctico

### Tests
Los test estan divididos entre bbctest y test en exec.
Los test de bbctest pruebas el proceso completo de compilación.
Los test en exec prueban funciones especificas del compilador.

- Se añadieron nuevos bbctests para las nuevas expresiones del compilador
- Se añadieron multiples test internos sobre el manejo de tuplas
=======
## Modificaciones generales

### Asm

- Se añade instruccion movq, que en realidad es mov pero especifica tamaño de palabra para evitar warnings de assembler

### Ast

- Se añadieron tuplas y las operaciones set y get

### Compile

- Se crean traducciones a assembly de tuple, set y get. considerando los runtime checks pedidos para set y get

### lib

- Se extienden funciones de error para tratar nuevos errores
- Se pasa la función "num_expr" de compile a lib y se extiende para contar tamaño de pila para set y tuple

### Interp

- Se actualizo el codigo usando el código de referencia

### Parse

- Se modifica el parse de let para hacer "pattern-matching" sobre tuplas

### Run-time (sys.c)

- Se extiende la funcion get_value para poder decodificar y printear tuplas
- Se modifican funcion de error para incluir tuplas
- Se crea nueva funcion de error que recibe 2 argumentos para errores de bad indexing en tuplas

### Tests

- Se agregaron los tests de referencia
- Se crea test en run-test para verificar nueva funcionalidad del parse
- Se testean nuevos type checking en carpeta errors
- Se testean funcionalidades y errores para tuplas en carpeta tuples
>>>>>>> e10d9fbec788d94c80a4424f12044b2d0c484caf

## Observaciones
- Se logró implementar todo lo mencionado en el enunciado
- El interprete no genera warnings al ejecutar el buildeo
