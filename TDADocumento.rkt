#lang racket
;TDA Documento
(require "TDADate.rkt")

;Representación: (string X fecha X string X string X entero X lista)
;(titulo fecha contenido autor)

;---CONSTRUCTORES---
;descripción: Función que crea la un documento con sus parámetros específicos
;dom:
;rec:
(define (documento titulo fecha contenido autor idDoc accesos)
  (if (and (string? titulo)
           (fecha? fecha)
           (string? autor)
           (string? contenido)
           )
      (list titulo fecha contenido autor idDoc accesos)
      null)
  )


;---PERTINENCIA---
;descripción: Función que verifica el formato de documento
;dom: Lista
;rec: Booleano
(define (documento? documento)
  (list? documento)
  )


;---SELECTORES---
;descripción: Función para seleccionar el título de un documento
;dom: Lista Documento
;rec: String
(define (getTitulo documento)
  (first documento)
  )

;descripción: Función que selecciona la fecha de creación del documento
;dom: Lista Documento
;rec: Lista fecha
(define (getFechaD documento)
  (second documento)
  )

;descripción: Función que selecciona el contenido del documento
;dom: Lista Documento
;rec: Contenido del documento
(define (getContenido documento)
  (third documento)
  )

;descripción: Función que selecciona el autor de un documento
;dom: Lista Documento
;rec: String
(define (getAutor documento)
  (fourth documento)
  )

;descripción: Función que selecciona el id del documento
;dom: Lista Documento
;rec: Entero
(define (getidDoc documento)
  (fifth documento)
  )

;descripción: Función que selecciona la lista de accesos del documento
;dom: Lista Documento
;rec: Lista de accesos
(define (getAccesos documento)
  (sixth documento)
  )


;---MODIFICADORES---
;descripción: Función que cambia el titulo del documento
;dom: Lista documento X string
;rec: Lista documento
(define (setTitulo documento newTitulo)
  (if (string? newTitulo)
      (list newTitulo (getFechaD documento) (getContenido documento) (getAutor documento) (getidDoc documento) (getAccesos documento))
      documento)
  )

;descripción: Función que cambia el contenido de un documento
;dom: Lista documento X string
;rec: Lista documento
(define (setContenido documento newContenido)
  (list (getTitulo documento) (getFechaD documento) newContenido (getAutor documento) (getidDoc documento) (getAccesos documento))
  )

;descripción: Función que modifica el autor del documento
;dom: Lista documento X string
;rec: Lista documento
(define (setAutor documento newAutor)
  (if (string? newAutor)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) newAutor (getidDoc documento) (getAccesos documento))
      documento)
  )

;descripción: Función que modifica el número de id del documento
;dom: Lista documento X Entero
;rec: Lista documento
(define (setidDoc documento idDoc)
  (if (number? idDoc)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) (getAutor documento) idDoc (getAccesos documento))
      documento)
  )
  
;descripción: Función que agrega una lista de acceso
;dom: Lista documento X Entero
;rec: Lista documento
(define (setAccesos documento accesos)
  (if (list? accesos)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) (getAutor documento) (getidDoc documento) (cons (getAccesos documento) accesos))
      documento)
  )



;To import
(provide (all-defined-out))

