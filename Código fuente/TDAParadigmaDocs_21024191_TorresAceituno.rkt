#lang racket
;TDA Paradigmadocs
(require "TDADate.rkt")
(require "TDAUser.rkt")

;---CONSTRUCTORES---
;descripción: Función que crea la plataforma con paradigmadocs
;dom: String X Date X EncryptFunction X DecryptFunction
;rec: Lista
(define (paradigmadocs nombrePlataforma fecha encryptFn decryptFn)
  (if (and (string? nombrePlataforma)
           (fecha? fecha))
      (list nombrePlataforma fecha (list) (list) (list) encryptFn decryptFn)
      null)
  )


;---PERTINENCIA---
;descripción: Función que verifica el formato de paradigmadocs
;dom: Lista
;rec: Booleano
(define (paradigmadocs? paradigmadocs)
  (list? paradigmadocs)
  )


;---SELECTORES---
;descripción: Función que selecciona el nombre de la plataforma
;dom: Lista
;rec: String
(define (getPlataforma paradigmadocs)
  (first paradigmadocs)
  )

;descripción: Función que selección la fecha
;dom: Lista
;rec: Fecha
(define (getFechaP paradigmadocs)
  (second paradigmadocs)
  )

;descripción: Función que selecciona la primera lista, correspondiente alos usuarios registrados
;dom: Lista paradigmadocs
;rec: Lista
(define (getLista1 paradigmadocs)
  (third paradigmadocs)
  )

;descripción: Función que selecciona la segunda lista, correspondiente al estado de inicio de sesión
;dom: Lista paradigmadocs
;rec: Lista
(define (getLista2 paradigmadocs)
  (fourth paradigmadocs)
  )

;descripción: Función que selecciona el primer elemento de una lista de estado de inicio de sesión
;dom: Lista
;rec: String
(define seleccionarNombre car)

;descripción: Función que selecciona la tercera lista, correspondiente a información de acceso
;dom: Lista paradigmadocs
;rec: Lista
(define (getLista3 paradigmadocs)
  (fifth paradigmadocs)
  )

;descripción: Función que selecciona función de encriptación
;dom: Paradigmadocs
;rec: Función
(define (getEncrypt paradigmadocs)
  (sixth paradigmadocs)
  )

;descripción: Función que selecciona la función de decriptación
;dom: Paradigmadocs
;rec: Función
(define (getDecrypt paradigmadocs)
  (seventh paradigmadocs)
  )


;---MODIFICADORES---
;descripción: Función que modifica la primera lista vacía a paradigmadocs
;dom: Lista x Lista
;rec: Lista
(define (setLista1 paradigmadocs_1 NewLista1)
  (if (and (paradigmadocs? paradigmadocs_1)
           (list? NewLista1))
     (list (getPlataforma paradigmadocs_1) (getFechaP paradigmadocs_1) NewLista1 (getLista2 paradigmadocs_1) (getLista3 paradigmadocs_1) (getEncrypt paradigmadocs_1) (getDecrypt paradigmadocs_1))
     paradigmadocs_1)
  )

;descripción: Función que modifica la segunda lista de paradigmadocs
;dom: Lista x Lista
;rec: Lista
(define (setLista2 paradigmadocs_1 NewLista2)
  (if (and (paradigmadocs? paradigmadocs_1)
           (list? NewLista2))
     (list (getPlataforma paradigmadocs_1) (getFechaP paradigmadocs_1) (getLista1 paradigmadocs_1) NewLista2 (getLista3 paradigmadocs_1) (getEncrypt paradigmadocs_1) (getDecrypt paradigmadocs_1))
     paradigmadocs_1)
  )

;descripción: Función que modifica la tercera lista de paradigmadocs
;dom: Lista x Lista
;rec: Lista
(define (setLista3 paradigmadocs_1 NewLista3)
  (list (getPlataforma paradigmadocs_1) (getFechaP paradigmadocs_1) (getLista1 paradigmadocs_1) (getLista2 paradigmadocs_1) (cons NewLista3 (getLista3 paradigmadocs_1)) (getEncrypt paradigmadocs_1) (getDecrypt paradigmadocs_1))
  )

;descripción: Función que reimplanta la tercera lista de paradigmadocs
;dom: Lista x Lista
;rec: Lista
(define (setLista3_implante paradigmadocs_1 NewLista3)
  (list (getPlataforma paradigmadocs_1) (getFechaP paradigmadocs_1) (getLista1 paradigmadocs_1) (getLista2 paradigmadocs_1) NewLista3 (getEncrypt paradigmadocs_1) (getDecrypt paradigmadocs_1))
  )

;descripción: Función que modifica el nombre de una plataforma ya creada
;dom: Lista x String
;rec: Lista
(define (setNewNombrePlataforma paradigmadocs_1 newNombrePlataforma)
  (if (and (paradigmadocs? paradigmadocs_1)
           (string? newNombrePlataforma))
      (list newNombrePlataforma (getFechaP paradigmadocs_1) (getLista1 paradigmadocs_1) (getLista2 paradigmadocs_1) (getLista3 paradigmadocs_1) (getEncrypt paradigmadocs_1) (getDecrypt paradigmadocs_1))
      paradigmadocs_1)
  )

;descripción: Función que modifica una fecha (no tiene uso realmente)
;dom: Lista x Fecha
;rec: Lista
(define (setNewFechaP paradigmadocs_1 fecha)
  (if (and (paradigmadocs? paradigmadocs_1)
           (fecha? fecha))
      (list (getPlataforma paradigmadocs_1) fecha (getLista1 paradigmadocs_1) (getLista2 paradigmadocs) (getLista3 paradigmadocs_1) (getEncrypt paradigmadocs_1) (getDecrypt paradigmadocs_1))
      paradigmadocs_1)
  )


;---ORAS FUNCIONES---
;descripción: Función que encripta un texto con símbolos extraños
;dom: String
;rec: String
(define encryptFn (lambda (s)
                    (list->string (reverse (string->list (string-append (list->string (reverse (string->list (string-append (list->string (reverse (string->list s)))
                                                                                                                            "☎ ☏ εїз ♨ ◊ ♢ ♥ ♤ ♧ ♧ ♤ ♧ ♫ ♬ ♪ ♩დ ღ ♡ ❣ ❤ ❥ ❦ ❧ ♥ ɞ☠ ☤ ☥ ☦ ☧ ☨ ☩ ☪ ☫ ☬ ☮ ☭ ☯ ☸ ☽ ☾ ♕ ♚ ♛ ✙ ✚ ✛ ✜ ✝ ✞ ✟ ✠ ✡ ✢ 卍 卐 卍 웃")))) "▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆▆ˍ▆ˍ▆ˍ▆ˍ▆ˍ▆ˍ▆ˍ▆ˍ▆ˍ▆")))))
  )

;descripción: Función que deecripta un texto (deben estar enlazadas ambas funciones para que se cumpla la decodificación del mensaje (la función no es universal, sino que es específica)
;dom: String
;rec: String
(define decryptFn (lambda (s)
                    (list->string (reverse (list-tail (reverse (list-tail (reverse (string->list s)) 117)) 46)) ))
  )

;descripción: Función que elimina usuarios repetidos
;dom: Lista Usuarios
;rec: Lista Usuarios
;recursión: natural (elegida por requerimiento)
(define (removerRegistradosDuplicados lst)
  (cond
    [(empty? lst) empty]
    [else (cons (first lst) (removerRegistradosDuplicados (filter (lambda (x)
                                                                    (not (equal? (car (first lst)) (car x)))) lst)))]
    )
  )

;descripción: Función que verifica si un usuario está registrado para ejecutar operaciones
;dom: Lista Usuarios X String X String
;rec: Booleano
;recursión: de cola (elegida por su fácil implementación y no dejar estados pendientes)
(define (verificado? listaUsuarios username_1 password_1)
    (if (empty? listaUsuarios)
        #f
        (if (and (eq? (getNombre (car listaUsuarios)) username_1)
                 (eq? (getContrasenna (car listaUsuarios)) password_1))
            #t
            (verificado? (cdr listaUsuarios) username_1 password_1)))
    )

;descripción: Función que saca la fecha de creación de un usuario registrado
;dom: Lista Registro X String
;rec: Fecha
;recursión: de cola (elegida por su fácil implementación y no dejar estados pendientes)
(define (fechaDeCreacionUser? listaRegistrados usuario)
  (if (empty? listaRegistrados)
      null
      (if (eq? (car (car listaRegistrados)) usuario)
          (third (car listaRegistrados))
          (fechaDeCreacionUser? (cdr listaRegistrados) usuario)
          )
      )
  )  


;To import
(provide (all-defined-out))
