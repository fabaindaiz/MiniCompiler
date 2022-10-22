# Entrega3

Fabián Díaz & Sergio Gálvez

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

## Observaciones
- Se logró implementar todo lo mencionado en el enunciado
- El interprete no genera warnings al ejecutar el buildeo
