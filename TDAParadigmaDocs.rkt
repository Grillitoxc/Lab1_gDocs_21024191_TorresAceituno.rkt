#lang racket
;TDA ParadigmaDocs
(require "TDADate.rkt")
(require "TDAUser.rkt")

;Representacion: (entero X entero X entero)
;(list dia mes año)

;---CONSTRUCTORES---
;descripción: Función que crea la plataforma con paradigmaDocs
;dom: String X Date X EncryptFunction X DecryptFunction
;rec: Lista
(define (paradigmaDocs nombrePlataforma fecha encryptFn decryptFn)
  (if (and (string? nombrePlataforma)
           (fecha? fecha))
      (list nombrePlataforma fecha encryptFn decryptFn)
      null)
  )


;---PERTINENCIA---
;descripción: Función que verifica el largo de paradigmaDocs como argumentos válidos
;dom: Lista
;rec: Booleano
(define (paradigmaDocs? paradigmaDocs)
  (and (list? paradigmaDocs)
       (= (length paradigmaDocs) 4))
  )


;---SELECTORES---
;descripción: Función que selecciona el nombre de la plataforma
;dom: Lista
;rec: String
(define (getPlataforma paradigmaDocs)
  (if (paradigmaDocs? paradigmaDocs)
      (car paradigmaDocs)
      null)
  )

;descripción: Función que selección la fecha
;dom: Lista
;rec: Fecha
(define (getFecha paradigmaDocs)
  (if (paradigmaDocs? paradigmaDocs)
      (car(cdr paradigmaDocs))
      null)
  )

;descripción: Función que selecciona la función que ecripta
;dom: Lista
;rec:
(define (getEncrypt paradigmaDocs)
  (if (paradigmaDocs? paradigmaDocs)
      (caddr paradigmaDocs)
      null)
  )

;descripción: Función que selecciona la función que decripta
;dom: Lista
;rec:
(define (getDecrypt paradigmaDocs)
  (if (paradigmaDocs? paradigmaDocs)
      (cadddr paradigmaDocs)
      null)
  )


;---MODIFICADORES---
;descripción: Función que modifica el nombre de una plataforma ya creada
;dom: Lista x String
;rec: Lista
(define (setNewNombrePlataforma paradigmaDocs_1 newNombrePlataforma)
  (if (and (paradigmaDocs? paradigmaDocs_1)
           (string? newNombrePlataforma))
      (paradigmaDocs newNombrePlataforma (getFecha paradigmaDocs_1) (getEncrypt paradigmaDocs_1) (getDecrypt paradigmaDocs_1))
      paradigmaDocs_1)
  )

;descripción: Función que modifica una fecha (no tiene uso realmente)
;dom: Lista x Fecha
;rec: Lista
(define (setNewFechaP paradigmaDocs_1 fecha)
  (if (and (paradigmaDocs? paradigmaDocs_1)
           (fecha? fecha))
      (paradigmaDocs (getPlataforma paradigmaDocs_1) fecha (getEncrypt paradigmaDocs_1) (getDecrypt paradigmaDocs_1))
      paradigmaDocs_1)
  )


;---ORAS FUNCIONES---
;descripción: Función que encripta/decripta un texto (se usan para lo mismo)
;dom: Lista
;rec: Lista
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))


;To import
(provide (all-defined-out))
;---EJEMPLOS DE CADA FUNCIÓN---
;(define Gdocs (paradigmaDocs "Gdocs" (fecha 12 12 2020) encryptFn encryptFn))