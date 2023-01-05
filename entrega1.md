# Entrega1

Fabián Díaz & Sergio Gálvez

## Modificaciones generales

### Asm
En Asm se definen los registros e instrucciones assembly.
Tambien se define la traducción a strings y la función gensym.

- Se añadieron los nuevos registros e instrucciones a usar
- Se anadio la traduccion de las instrucciones a strings
- Se añadio el gensym para asignar nombres a los labels

### Ast
En Ast se definen las primitivas y operaciones de la representación intermedia.
Tambien se define el Pretty printing para los frameworks de testeo.

- Se añadieron las nuevas primitivas y expresiones solicitadas
- Se añadió el Pretty printing de las nuevas expresiones

### Compile
En Compile se define el ambiente y el pipeline de compilación.
Tambien se realizala compilacion de las expresiones a una lista de instrucciones.

- Se modifico el ambiente para soportar reg_offset según el tamaño del ambiente
- Se añadieron al compilador las constantes y el soporte para los nuevos tipos
- Se añadieron al compilador las nuevas expresiones y operaciones solicitadas
- Se añadio la evaluación lazy aplicable a primitivas binarias compatibles

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

- Se añadió un bbctest básico por cada operación del compilador
- Se añadieron multiples bbctest para los casoso del let binding

- No se añadieron nuevos test normales dentro de la carpeta execs

## Observaciones
- Se logró implementar todo lo mencionado en el enunciado
- El interprete no genera warnings al ejecutar el buildeo
