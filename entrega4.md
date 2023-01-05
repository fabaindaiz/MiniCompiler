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

- Se añadieron los nuevos registros e instrucciones a usar
- Se anadio la traduccion de las nuevas instrucciones a strings
- Se añadio el tipo de dato Any para almacenar labels en un el heap para llamar lambdas

### Ast
En Ast se definen las primitivas y operaciones de la representación intermedia.
Tambien se define el Pretty printing para los frameworks de testeo.

- Se añadieron las nuevas primitivas y expresiones solicitadas
- Se añadieron nuevos tipos para las funciones y programas
- Se añadió el Pretty printing de las nuevas expresiones y tipos
- Se añadieron funciones lambda y la aplicacion de lambdas

### Compile
En Compile se define la compilación de expresiones y el pipeline de compilación.
Tambien se realizala compilacion de las expresiones a una lista de instrucciones.

- Se añadieron al compilador las nuevas expresiones y operaciones solicitadas
- Se añadieron los manejos necesarios para usar el heap y hacer mutaciones
- Se crean traducciones a assembly de lambda, LamApp y LetRec. considerando los runtime checks pedidos para la clausura

### Lib
En lib se definen los ambientes de compilación para variables y funciones
Tambien se define el manejo de errores y otras funciones útiles
Tambien se definen todos los snippets para compilación de funciones

- Se añadió la verificación de tipos y el manejo de errores para clausuras
- Se extienden funciones de error para tratar nuevos errores
- Se extiende la función "num_expr" para contar tamaño de pila para lambda, LamApp y LetRec

- Se añadieron funciones para el manejo del empaquetamiento de la clausura
- Se añadieron funciones para el manejo de variables libres y ambientes relacionados
- Se corrigio un bug en la aplicación de funciones normales que sobreescribia registros

### Interp
En Interp se define un interprete para la representación intermetdia del compilador.
El interprete define un ambiente y funciones de lift para realizar las operaciones.

- Se añadieron verificaciones de tipos y manejo de errores de clausuras
- Se actualizo el codigo usando el código de referencia

### Parse
En Parse de define el parseo desde archivos y strings.
Este transforma las sexp de CCSexp en expresiones de nuestra representación.

- Se añadieron los nuevos tipos y expresiones a la funcion de parse
- Se modifica el parse de let para hacer "letrec" sobre lambdas

### Run-time (sys.c)
En Run-Time se encuentra la función principal de c que llama al código assembly
Tambien tiene funciones utiles y de manejo de errores que pueden ser llamadas

- Se extiende la funcion get_value para poder decodificar y printear clausuras
- Se modifican funcion de error para incluir clausuras

### Tests
Los test estan divididos entre bbctest y test en exec.
Los test de bbctest pruebas el proceso completo de compilación.
Los test en exec prueban funciones especificas del compilador.

- Se añadieron nuevos bbctests para las nuevas expresiones del compilador
- Se agregaron los tests de referencia
- Se crea test en run-test para verificar nueva funcionalidad del parse
- Se testean nuevos type checking en carpeta errors
- Se testean funcionalidades y errores para tuplas en carpeta tuples

## Observaciones
- Se logró implementar todo lo mencionado en el enunciado
- El interprete no genera warnings al ejecutar el buildeo
