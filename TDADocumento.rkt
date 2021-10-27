#lang racket
;TDA Documento
(require "TDADate.rkt")

;Representación: (string X fecha X string X string)
;(titulo fecha contenido autor)

;---CONSTRUCTORES---
;descripción: Función que crea la un documento con sus parámetros específicos
;dom:
;rec:
(define (documento titulo fecha contenido autor)
  (if (and (string? titulo)
           (fecha? fecha)
           (string? autor)
           (string? contenido))
      (list titulo fecha contenido autor)
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


;---MODIFICADORES---
;descripción: Función que cambia el titulo del documento
;dom: Lista documento X string
;rec: Lista documento
(define (setTitulo documento newTitulo)
  (if (string? newTitulo)
      (list newTitulo (getFechaD documento) (getContenido documento) (getAutor documento))
      documento)
  )

;descripción: Función que cambia el contenido de un documento
;dom: Lista documento X string
;rec: Lista documento
(define (setContenido documento newContenido)
  (list (getTitulo documento) (getFechaD documento) newContenido (getAutor documento))
  )

;descripción: Función que modifica el autor del documento
;dom:
;rec:
(define (setAutor documento newAutor)
  (if (string? newAutor)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) newAutor)
  documento)
  )


;To import
(provide (all-defined-out))
