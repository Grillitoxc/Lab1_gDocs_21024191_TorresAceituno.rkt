#lang racket
(require "TDAParadigmaDocs_21024191_TorresAceituno.rkt")
(require "TDAUser_21024191_TorresAceituno.rkt")
(require "TDADate_21024191_TorresAceituno.rkt")
(require "TDADocumento_21024191_TorresAceituno.rkt")
(require "TDAAcceso_21024191_TorresAceituno.rkt")


;-REGISTER-
;descripción: Función que registra a un usuario en la plataforma, de tal forma que no se repita ningún usuario
;dom: Paradigmadocs X date X string X string
;rec: Paradigmadocs
;recursión: natural (removerRegistradosDuplicados) (elegida por requisito)
(define (register paradigmadocs date username password)
      (setLista1 paradigmadocs (removerRegistradosDuplicados (reverse (cons (usuario username password date) (getLista1 paradigmadocs)))))
  )


;-LOGIN-
;descripción: Función que autentica a un usuario registrado y le permite ejecutar un comando específico
;dom: paradigmadocs X string X string X function 
;rec: Paradigmadocs
;recursión: de cola (verificado?) (elegida por su fácil implementación y no dejar estados pendientes)
(define (login paradigmadocs username password operation)
  (if (verificado? (getLista1 paradigmadocs) username password)
      (cond
        [(eq? operation create)(operation (setLista2 paradigmadocs (list username)))]
        [(eq? operation share)(operation (setLista2 paradigmadocs (list username)))]
        [(eq? operation add)(operation (setLista2 paradigmadocs (list username)))]
        [(eq? operation restoreVersion)(operation (setLista2 paradigmadocs (list username)))]
        [(eq? operation revokeAllAccesses)(operation (setLista2 paradigmadocs (list username)))]
        [(eq? operation search)(operation (setLista2 paradigmadocs (list username)))]
        [(eq? operation paradigmadocs->string)(operation (setLista2 paradigmadocs (list username)))]
        [(eq? operation delete)(operation (setLista2 paradigmadocs (list username)))]
        [(eq? operation searchAndReplace)(operation (setLista2 paradigmadocs (list username)))]
        [else paradigmadocs]
        )
      (cond
        [(eq? operation create)(operation paradigmadocs)]
        [(eq? operation share)(operation paradigmadocs)]
        [(eq? operation add)(operation paradigmadocs)]
        [(eq? operation restoreVersion)(operation paradigmadocs)]
        [(eq? operation revokeAllAccesses)(operation paradigmadocs)]
        [(eq? operation search)(operation paradigmadocs)]
        [(eq? operation paradigmadocs->string)(operation paradigmadocs)]
        [(eq? operation delete)(operation paradigmadocs)]
        [(eq? operation searchAndReplace)(operation paradigmadocs)]
        [else paradigmadocs]
        )
      )
  )


;-CREATE-
;descripción: Función que crea un documento con los datos entregados. También se encarga de encriptar el contenido de este
;dom: paradigmadocs X date X String (titulo documento) X String  (contenido) 
;rec: Paradigmadocs
(define (create paradigmadocs)
  (lambda(date nombre contenido)
    (if (null? (getLista2 paradigmadocs))
        paradigmadocs
        (setLista3 (setLista2 paradigmadocs null)
                   (documento nombre date ((getEncrypt paradigmadocs) contenido) (seleccionarNombre (getLista2 paradigmadocs)) (length (getLista3 paradigmadocs)) null null date))
        )
    )
  )


;-SHARE-
;descripción: Función que le da distintos tipos de accesos a usuarios que previamente se hayan registrado. En caso de que el usuario sea el propietario del documento, este no puede darse permisos
;dom: Paradigmadocs X Entero X Lista de accesos
;rec: Paradigmadocs
;recursión: de cola (buscarDoc, seleccionarDoc, buscarUserDoc) (elegida por su fácil implementación y no dejar estados pendientes)
(define (share paradigmadocs)
  (lambda(idDoc access . accesses)
    (if (null? (getLista2 paradigmadocs))
        paradigmadocs
        (if (buscarDoc (getLista3 paradigmadocs) idDoc)
            (setLista3_implante (setLista2 paradigmadocs null)
                                (reverse (list-set (reverse (getLista3 paradigmadocs)) idDoc (limpiarListAccesos (setAccesos (seleccionarDoc (getLista3 paradigmadocs) idDoc)
                                                                                                         (filtrarAccesos paradigmadocs (unirAccess access accesses) (buscarUserDoc (getLista3 paradigmadocs) idDoc)))))))
            paradigmadocs)
        )
    )
  )


;-ADD-
;descripción: Función que agrega un contenido al documento considerando que esto sólo lo puede hacer el propietario/autor de un documento o aquellos usuarios que tengan el permiso de escritura.
;             Además, el documento con su contenido pasa a ser la versión actual de este, mientras que se pasa a un historial de versiones el documento anterior.
;dom: Paradigmadocs X Entero X Fecha X String
;rec: Paradigmadocs
;recursión: de cola (buscarDoc, seleccionarDoc, filtrarUsuariosEscritura, esPropietario?) (elegida por su fácil implementación y no dejar estados pendientes)
(define (add paradigmadocs)
  (lambda(idDoc date contenidoTexto)
    (if (null? (getLista2 paradigmadocs))
        paradigmadocs
        (if (or (esPropietario? (getLista3 paradigmadocs) (seleccionarNombre (getLista2 paradigmadocs)) idDoc)
                (filtrarUsuariosEscritura (getAccesos (seleccionarDoc (getLista3 paradigmadocs) idDoc)) (seleccionarNombre (getLista2 paradigmadocs)) ))
            (if (buscarDoc (getLista3 paradigmadocs) idDoc)
                (setLista3_implante (setLista2 paradigmadocs null)
                                    (reverse (list-set (reverse (getLista3 paradigmadocs)) idDoc (setHistorial (seleccionarDoc (getLista3 paradigmadocs) idDoc) (documento
                                                                                                                                                                 (getTitulo (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                                                                                 (getFechaD (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                                                                                 (getContenido (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                                                                                 (getAutor (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                                                                                 idDoc
                                                                                                                                                                 null
                                                                                                                                                                 null
                                                                                                                                                                 (getFechaCreacion (seleccionarDoc (getLista3 paradigmadocs) idDoc))) contenidoTexto date idDoc paradigmadocs))))
                paradigmadocs
                )
            paradigmadocs
            )
        )
    )
  )


;-RESTOREVERSION-
;descripción: Función que restaura una versión anterior de un documento. Sólo el propietario del documento puede hacer esta acción.
;             Además, la versión actual del documento pasa a ser parte del historial, mientras que se actualiza la nueva versión actual con la restaurada
;dom: Paradigmadocs X Entero X Entero
;rec: Paradigmadocs
;recursión: de cola (esPropietario?) (elegida por su fácil implementación y no dejar estados pendientes)
(define (restoreVersion paradigmadocs)
  (lambda(idDoc idVersion)
    (if (null? (getLista2 paradigmadocs))
        paradigmadocs
        (if (esPropietario? (getLista3 paradigmadocs) (seleccionarNombre (getLista2 paradigmadocs)) idDoc)
            (if (and (< idVersion (length (getHistorial (seleccionarDoc (getLista3 paradigmadocs) idDoc))))
                     (> idVersion -1))
                (setLista3_implante (setLista2 paradigmadocs null)
                                    (reverse (list-set (reverse (getLista3 paradigmadocs)) idDoc (setHistorial_restauracion
                                                                                                  (seleccionarDoc (getLista3 paradigmadocs) idDoc) (documento
                                                                                                                                                    (getTitulo (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                                                                    (getFechaD (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                                                                    (getContenido (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                                                                    (getAutor (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                                                                    idDoc
                                                                                                                                                    null
                                                                                                                                                    null
                                                                                                                                                    (getFechaCreacion (seleccionarDoc (getLista3 paradigmadocs) idDoc))) (list-ref (reverse (getHistorial (seleccionarDoc (getLista3 paradigmadocs) idDoc))) idVersion) idVersion))))
                paradigmadocs
                )
            paradigmadocs
            )
        )
    )
  )


;-REVOKEALLACCESSES-
;descripción: Función que elimina todos accesos de todos los documentos
;dom: Paradigmadocs
;rec: Paradigmadocs
(define (revokeAllAccesses paradigmadocs)
  (if (null? (getLista2 paradigmadocs))
      paradigmadocs
      (setLista3_implante (setLista2 paradigmadocs null) (map setAccesos_implante (getLista3 paradigmadocs))))
  )


;-SEARCH-
;descripción: Función que permite al usuario buscar documentos propios o que le hayan sido compartidos que contengan un texto específico. Esta búsqueda se hace en la versión activa y en el historial de versiones
;dom: Paradigmadocs
;rec: Lista de documentos
;recursión: de cola (buscarDocsPropietario) (elegida por su fácil implementación y no dejar estados pendientes)
(define (search paradigmadocs)
  (lambda (searchText)
    (if (null? (getLista2 paradigmadocs))
          null
          (if (null? (buscarDocsPropietario (getLista3 paradigmadocs) (seleccionarNombre (getLista2 paradigmadocs))))
              null
              #f
              )
          )
    )
  )


;-PARADIGMADOCS->STRING
;descripción: Función que recibe una plataforma del estilo paradigmadocs y muestra toda la información guardada en caso de que el usuario no se logee dentro de login.
;             Caso contrario, muestra la información respectiva al usuario que se logeó (documentos, fecha de creación de la cuenta, etc)
;dom: Paradigmadocs
;rec: String
(define (paradigmadocs->string paradigmadocs)
  (if (null? (getLista2 paradigmadocs))
      (string-append
                "\n ► Esta es la plataforma de documentos llamada: " (getPlataforma paradigmadocs)
                "\n ► Fecha de creación de la plataforma: " (fecha->string (getFechaP paradigmadocs))
                "\n ► Los documentos alcenados en esta plataforma son:\n " (darFormatoDocs (getLista3 paradigmadocs) paradigmadocs)   
                 )
      (string-append
                "\n ► Usuario ingresado: " (seleccionarNombre (getLista2 paradigmadocs))
                "\n ► Fecha de creación de la cuenta: " (fecha->string (fechaDeCreacionUser? (getLista1 paradigmadocs) (seleccionarNombre (getLista2 paradigmadocs))))
                "\n ► Los documentos los cuales este usuario tiene acceso son: " (darFormatoDocs_login_access (buscarDocsAccesos (getLista3 paradigmadocs) (seleccionarNombre (getLista2 paradigmadocs))) paradigmadocs)
                "\n ► Los documentos los cuales este usuario es propietario son: " (darFormatoDocs_login_propiedad (buscarDocsPropietario (getLista3 paradigmadocs) (seleccionarNombre (getLista2 paradigmadocs))) paradigmadocs)
                "\n"
                 )
      )
  )


;-DELETE-
;descripción: Funcipón que permite elimitar los N caracteres de la versión activa de un documento y guarda la versión que estaba antes en el historial
;dom: Paradigmadocs X Entero X Fecha X Entero
;rec: Paradigmadocs
;recursión: de cola (esPropietario?, filtrarUsuariosEscritura) (elegida por su fácil implementación y no dejar estados pendientes)
(define (delete paradigmadocs)
  (lambda (idDoc date numberOfCharacters)
    (if (null? (getLista2 paradigmadocs))
        paradigmadocs
        (if (or (esPropietario? (getLista3 paradigmadocs) (seleccionarNombre (getLista2 paradigmadocs)) idDoc)
                (filtrarUsuariosEscritura (getAccesos (seleccionarDoc (getLista3 paradigmadocs) idDoc)) (seleccionarNombre (getLista2 paradigmadocs))))
            (if (<= (string-length (decryptFn (getContenido (seleccionarDoc (getLista3 paradigmadocs) idDoc)))) numberOfCharacters)
                (setLista3_implante (setLista2 paradigmadocs null)
                                    (reverse (list-set (reverse (getLista3 paradigmadocs)) idDoc (setHistorial_delete
                                                                                                  (seleccionarDoc (getLista3 paradigmadocs) idDoc)
                                                                                                  ""
                                                                                                  (documento
                                                                                                   (getTitulo (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                   (getFechaD (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                   (getContenido (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                   (getAutor (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                   idDoc
                                                                                                   null
                                                                                                   null
                                                                                                   (getFechaCreacion (seleccionarDoc (getLista3 paradigmadocs) idDoc)))
                                                                                                  date
                                                                                                  paradigmadocs))))    
                (setLista3_implante (setLista2 paradigmadocs null)
                                    (reverse (list-set (reverse (getLista3 paradigmadocs)) idDoc (setHistorial_delete
                                                                                                  (seleccionarDoc (getLista3 paradigmadocs) idDoc)
                                                                                                  ((getEncrypt paradigmadocs) (list->string (reverse (list-tail (reverse (string->list ((getDecrypt paradigmadocs) (getContenido (seleccionarDoc (getLista3 paradigmadocs) idDoc))))) numberOfCharacters))))
                                                                                                  (documento
                                                                                                   (getTitulo (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                   (getFechaD (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                   (getContenido (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                   (getAutor (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                   idDoc
                                                                                                   null
                                                                                                   null
                                                                                                   (getFechaCreacion (seleccionarDoc (getLista3 paradigmadocs) idDoc)))
                                                                                                  date
                                                                                                  paradigmadocs))))
                )
            paradigmadocs
            )
        )
    )
  )


;-SEARCHANDREPLACE-
(define (searchAndReplace paradigmadocs)
  (lambda (idDoc date searchText replaceText)
    (if (null? (getLista2 paradigmadocs))
        paradigmadocs
        (if (buscarDoc (getLista3 paradigmadocs) idDoc)
            (if (or (esPropietario? (getLista3 paradigmadocs) (seleccionarNombre (getLista2 paradigmadocs)) idDoc)
                (filtrarUsuariosEscritura (getAccesos (seleccionarDoc (getLista3 paradigmadocs) idDoc)) (seleccionarNombre (getLista2 paradigmadocs))))
                (if (string-contains? ((getDecrypt paradigmadocs) (getContenido (seleccionarDoc (getLista3 paradigmadocs) idDoc))) searchText)
                    (setLista3_implante (setLista2 paradigmadocs null)
                                    (reverse (list-set (reverse (getLista3 paradigmadocs)) idDoc (setHistorial_delete
                                                                                                  (seleccionarDoc (getLista3 paradigmadocs) idDoc)
                                                                                                  ((getEncrypt paradigmadocs) (string-replace ((getDecrypt paradigmadocs) (getContenido (seleccionarDoc (getLista3 paradigmadocs) idDoc))) searchText replaceText))
                                                                                                  (documento
                                                                                                   (getTitulo (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                   (getFechaD (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                   (getContenido (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                   (getAutor (seleccionarDoc (getLista3 paradigmadocs) idDoc))
                                                                                                   idDoc
                                                                                                   null
                                                                                                   null
                                                                                                   (getFechaCreacion (seleccionarDoc (getLista3 paradigmadocs) idDoc)))
                                                                                                  date
                                                                                                  paradigmadocs))))
                    paradigmadocs
                    )
                paradigmadocs
                )
            paradigmadocs
            )
        )
    )
  )


;---EJEMPLOS DE CADA FUNCIÓN---
; GENERANDO PARADIGMADOCS
(define Gdocs000 (paradigmadocs "Gdocs" (fecha 25 10 2021) encryptFn decryptFn))

; 1) REGISTER 
(define Gdocs011 (register (register (register Gdocs000 (fecha 25 4 2021) "user1" "pass1") (fecha 22 4 2021) "user2" "pass2") (fecha 25 4 2021) "user3" "pass3"))
(define Gdocs012 (register Gdocs000 (fecha 15 4 2021) "user" "pass"))
(define Gdocs013 (register (register Gdocs000 (fecha 25 3 2021) "user" "pass") (fecha 25 3 2021) "user" "pass2"))

; 2) LOGIN
(define Gdocs021 (login Gdocs011 "user3" "pass3" share))
(define Gdocs022 ((login Gdocs011 "user1" "pass1" create) (fecha 10 10 2021) "doc1" "contenido1"))
(define Gdocs023 ((login Gdocs022 "user2" "pass2" create)(fecha 10 10 2021) "doc2" "contenido2"))

; 3) CREATE
(define Gdocs031 ((login Gdocs011 "user3" "pass3" create) (fecha 10 10 2021) "primerTitulo" "contenido"))
(define Gdocs032 ((login Gdocs012 "user" "pass" create) (fecha 10 10 2021) "doc1" "contenido1"))
(define Gdocs033 ((login Gdocs032 "user" "pass" create) (fecha 10 10 2021) "doc2" "contenido2"))

; 4) SHARE
(define Gdocs041 ((login Gdocs023 "user1" "pass1" share) 0 (access "user3" #\r) (access "user2" #\w) (access "user2" #\c)))
(define Gdocs042 ((login Gdocs023 "user1" "pass1" share) 0 (access "user1" #\r) (access "user2" #\c)))
(define Gdocs043 ((login Gdocs023 "user1" "pass1" share) 1 (access "user3" #\r) (access "user3" #\w) (access "user2" #\c)))

; 5) ADD
(define Gdocs051 ((login Gdocs041 "user2" "pass2" add) 0 (fecha 11 11 2021) " Contenido extra"))
(define Gdocs052 ((login Gdocs051 "user2" "pass2" add) 0 (fecha 12 11 2021) " Más contenido"))
(define Gdocs053 ((login Gdocs041 "user3" "pass3" add) 0 (fecha 11 11 2021) " Contenido extra"))

; 6) RESTOREVERSION
(define Gdocs061 ((login Gdocs052 "user1" "pass1" restoreVersion) 0 0))
(define Gdocs062 ((login Gdocs052 "user1" "pass1" restoreVersion) 0 1))
(define Gdocs063 ((login Gdocs052 "user1" "pass1" restoreVersion) 0 5))

; 7) REVOKEALLACCESSES
(define Gdocs071 (login Gdocs051 "user1" "pass1" revokeAllAccesses))
(define Gdocs072 (login Gdocs042 "user1" "pass1" revokeAllAccesses))
(define Gdocs073 (login Gdocs043 "user0" "pass1" revokeAllAccesses))

; 8) SEARCH
(define Gdocs081 ((login Gdocs052 "user3" "pass3" search) "Contenido"))

; 9) PARADIGMADOCS->STRING
(define Gdocs091 (login Gdocs052 "user1" "pass1" paradigmadocs->string))
(define Gdocs092 (login Gdocs052 "user2" "pass2" paradigmadocs->string))
(define Gdocs093 (login Gdocs052 "user4" "pass4" paradigmadocs->string))

; 10) DELETE
(define Gdocs101 ((login Gdocs052 "user1" "pass1" delete) 0 (fecha 13 11 2021) 5))
(define Gdocs102 ((login Gdocs052 "user1" "pass1" delete) 0 (fecha 13 11 2021) 10))
(define Gdocs103 ((login Gdocs052 "user1" "pass1" delete) 0 (fecha 13 11 2021) 40))

; 11) SEARCHANDREPLACE
(define Gdocs111 ((login Gdocs052 "user1" "pass1" searchAndReplace) 0 (fecha 14 11 2021) "Contenido extra Más contenido" "TEXTO REEMPLAZADO"))
(define Gdocs112 ((login Gdocs052 "user4" "pass4" searchAndReplace) 0 (fecha 14 11 2021) "Contenido extra Más contenido" "TEXTO REEMPLAZADO"))
(define Gdocs113 ((login Gdocs052 "user2" "pass2" searchAndReplace) 0 (fecha 14 11 2021) "contenido" "YA NO ES CONTENIDOOOOOO"))
