#lang racket
;TDA Documento
(require "TDADate.rkt")
(require "TDAParadigmadocs.rkt")

;Representación: (string X fecha X string X string X entero X lista)
;(titulo fecha contenido autor)

;---CONSTRUCTORES---
;descripción: Función que crea un documento con sus parámetros específicos
;dom: String X fecha X String X String X Entero X Lista
;rec: Lista Documento
(define (documento titulo fecha contenido autor idDoc accesos historial)
  (if (and (string? titulo)
           (fecha? fecha)
           (string? autor)
           (string? contenido)
           )
      (list titulo fecha contenido autor idDoc accesos historial)
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

;descripción: Función que selecciona el historial de versiones del documento
;dom: Lista Documento
;rec: Lista de historial
(define (getHistorial documento)
  (seventh documento)
  )


;---MODIFICADORES---
;descripción: Función que cambia el titulo del documento
;dom: Lista documento X string
;rec: Lista documento
(define (setTitulo documento newTitulo)
  (if (string? newTitulo)
      (list newTitulo (getFechaD documento) (getContenido documento) (getAutor documento) (getidDoc documento) (getAccesos documento) (getHistorial documento))
      documento)
  )

;descripción: Función que cambia el contenido de un documento
;dom: Lista documento X string
;rec: Lista documento
(define (setContenido documento newContenido)
  (list (getTitulo documento) (getFechaD documento) newContenido (getAutor documento) (getidDoc documento) (getAccesos documento) (getHistorial documento))
  )

;descripción: Función que modifica el autor del documento
;dom: Lista documento X string
;rec: Lista documento
(define (setAutor documento newAutor)
  (if (string? newAutor)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) newAutor (getidDoc documento) (getAccesos documento) (getHistorial documento))
      documento)
  )

;descripción: Función que modifica el número de id del documento
;dom: Lista documento X Entero
;rec: Lista documento
(define (setidDoc documento idDoc)
  (if (number? idDoc)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) (getAutor documento) idDoc (getAccesos documento) (getHistorial documento))
      documento)
  )
  
;descripción: Función que agrega una lista de accesos
;dom: Lista documento X Entero
;rec: Lista documento
(define (setAccesos documento accesos)
  (if (list? accesos)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) (getAutor documento) (getidDoc documento) (cons accesos (getAccesos documento)) (getHistorial documento))
      documento)
  )

;descripción: Función que implanta una lista de accesos vacía
;dom: Lista documento X Entero
;rec: Lista documento
(define (setAccesos_implante documento)
  (list-set documento 5 null)
  )

;descripción: Función que agrega una lista de acceso
;dom: Lista documento X Entero
;rec: Lista documento
(define (setHistorial documento historial)
  (if (list? historial)
      (list (getTitulo documento) (getFechaD documento) (getContenido documento) (getAutor documento) (getidDoc documento) (getAccesos documento) (cons historial (getHistorial documento)))
      documento)
  )


;---OTRAS FUNCIONES---
;descripción: Función que verifica si un documento está en la lsita de documentos mediante un id
;dom: Lista documentos X Entero
;rec: Booleano
;recursión: de cola
(define (buscarDoc listaDocs idDoc)
  (if (empty? listaDocs)
      #f
      (if (eq? (getidDoc (car listaDocs)) idDoc)
          #t
          (buscarDoc (cdr listaDocs) idDoc)
          )
      )
  )

;descripción: Función que busca el autor del documento por su id
;dom: Lista documentos X Entero
;rec: String
;recursión: de cola
(define (buscarUserDoc listaDocs idDoc)
  (if (empty? listaDocs)
      #f
      (if (eq? (getidDoc (car listaDocs)) idDoc)
          (getAutor (car listaDocs))
          (buscarUserDoc (cdr listaDocs) idDoc)
          )
      )
  )

;descripción: Función que retorna un documento específico por su id
;dom: Lista documentos X Entero
;rec: Lsita documento
;recursión: de cola
(define (seleccionarDoc listaDocs idDoc)
  (if (empty? listaDocs)
      #f
      (if (eq? (getidDoc (car listaDocs)) idDoc)
          (car listaDocs)
          (seleccionarDoc (cdr listaDocs) idDoc)
          )
      )
  )

;descripción: Función que limpia la lista de accesos de un documento de paradigmadocs (formato)
;dom: Lista documento
;rec: Lista documento
(define (limpiarListAccesos documento)
  (list (getTitulo documento) (getFechaD documento) (getContenido documento) (getAutor documento) (getidDoc documento) (car (getAccesos documento)) (getHistorial documento))
  )


;To import
(provide (all-defined-out))
