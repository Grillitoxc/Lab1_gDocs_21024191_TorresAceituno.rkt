#lang racket
(require "TDAParadigmaDocs.rkt")
(require "TDAUser.rkt")
(require "TDADate.rkt")

;descripción: Función que registra a un usuario
;dom: Paradigmadocs X date X string X string
;rec: ParadigmaDocs
;recursión: Natural
(define (register paradigmaDocs date username password)
  (if (empty? (getLista1 paradigmaDocs))
      (setLista1New paradigmaDocs (usuario username password date))
      0)
  )

; condicion => eq? (car LISTA_X) username)
;                  si son iguales devuelvo paradigmaDocs sin agregarle ese usuario
;                  caso false (append paradigmaDocs (usuario username password date))

;(define (noSeRepite? paradigmaDocs_2 username)
  ;caso base
 ; (if (= (paradigmaDocs_2) null)
;      #t

;(append paradigmaDocs (list (usuario username password date)))

(define Gdocs (paradigmaDocs "gDocs" (fecha 25 10 2021) encryptFn encryptFn))

(define gDocs1 (register Gdocs (fecha 25 10 2021) "user1" "pass1"))

; ((user pass fecha) (user2 pass2 fecha2)            