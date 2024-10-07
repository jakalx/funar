#lang deinprogramm/sdp/beginner

(require deinprogramm/sdp/image)

(define x (+ 23 42))
(define y
  (+ 23
     (* 42 2)))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "green"))
(define overlay1 (overlay star1 circle1)) ; overlay (put images over each other

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))

; Abstraktion von Dingen nebeneinander / übereinander
; Voraussetzung: 2 ähnliche Code-Stellen
; 
(define z (+ x y))

; lexical binding
; vom Vorkommen aus von innen nach außen suchen
; die erste Bindung (lambda, define) ist es
(define f
  (lambda (z)
    (+ z 1)))

; Konstruktionsanleitung
; - Kurzbeschreibung
; - Signaturdeklaration

; quadratisches kachelmuster aus 2 Kacheln zusammensetzen
(: tile (image image -> image))

(check-expect (tile star1 circle1)
              (above
               (beside star1 circle1)
               (beside circle1 star1)))

(define tile
  (lambda (a b)
    (above
     (beside a b)
     (beside b a))))

(tile star1 circle1)
