#lang deinprogramm/sdp/beginner

; Datenanalyse

; Uhrzeit besteht aus/hat folgende Eigenschaften:
; - Stunde *UND*
; - Minute
; Zusammengesetzte Daten (Product-Type)
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour   (integer-from-to 0 23))
  (time-minute (integer-from-to 0 59)))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; current time 10:54 am
(define time1 (make-time 10 54))
(define time2 (make-time 14 32))

; Minuten seit Mitternacht
(: minutes-since-midnight (time -> natural))
(check-expect (minutes-since-midnight time1)
              654)
(check-expect (minutes-since-midnight time2)
              872)

; Schablone, entsteht aus der Signatur der Funktion
#;(define minutes-since-midnight
  (lambda (t)
    ; zusammengesetzte Daten als Input
    ...(time-hour t)...
    ...(time-minute t)...))
(define minutes-since-midnight
  (lambda (t)  
    (+ (* (time-hour t) 60)
       (time-minute t))))

; aus Minuten-seit-Mitternacht die Uhrzeit berechnen
(: time-from-minutes (natural -> time))
(check-expect (time-from-minutes 654) time1)
(check-expect (time-from-minutes 872) time2)

(define time-from-minutes
  (lambda (total)
    (make-time (quotient total 60)
               (remainder total 60))))

; Haustier:
; - Hund *ODER*
; - Katze *ODER*
; - Schlange
; Fallunterscheidung (Sum-Types)
; hier: Aufzählung (enumeration): "dog" "cat" "snake"
(define pet
  (signature (enum "dog"
                   "cat"
                   "snake")))


; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

(define cute?
  (lambda (pet)
    ; Fallunterscheidung -> Verzweigung
    ; für jeden Fall einen Zweig, werden sequentiell geprüft
    (cond
      ; Zweig: (Bedingung Ergebnis)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Tiere auf dem texanischen Highway
; - Gürteltier *ODER*
; - Papagei

; Gürteltier:
; - lebendig?
; - Gewicht
; eigentlich Repräsentation des Zustands eines Gürteltiers zu einem bestimmten Zeitpunkt
(define-record dillo
  make-dillo
  dillo?
  (dillo-alive? boolean)
  (dillo-weight number))

; lebendiges Gürteltier mit 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier mit 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(check-property
 (for-all ((d dillo))
   (not (dillo-alive? (run-over-dillo d)))))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern (mit einer spezifizierbaren Menge Futter)
;   ein lebendiges Gürteltier nimmt um die angegegbene Menge an Gewicht zu
;   ein totes Gürteltier nimmt nicht zu und bleibt tot

(: feed-dillo (dillo natural -> dillo))
(check-expect (feed-dillo (make-dillo #t 10) 10) (make-dillo #t 20))
(check-expect (feed-dillo (make-dillo #f 10) 10) (make-dillo #f 10))

(define feed-dillo
  (lambda (dillo amount-of-food)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                   (+ amount-of-food weight)
                   weight))))

(check-property
 (for-all ((food natural) (d dillo))
   (==> (dillo-alive? d)
       (equal? (dillo-weight (feed-dillo d food))
               (+ food (dillo-weight d))))))

; Papagei
; - Satz *UND*
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

(define parrot1 (make-parrot "hello" 1))
(define parrot2 (make-parrot "goodbye!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))
(check-expect (run-over-parrot parrot1) (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

(define animal
  (signature (mixed dillo
                    parrot)))

; Highway animal überfahren
(: run-over-animal (animal -> animal))
(check-expect (run-over-animal dillo1) (run-over-dillo dillo1))
(check-expect (run-over-animal parrot1) (run-over-parrot parrot1))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

(check-property
 (for-all ((a animal))
   (define new-a (run-over-animal a))
   (and (equal? (dillo? a) (dillo? new-a))
        (equal? (parrot? a) (parrot? new-a)))))

; Listen (recursive data type)
(define-record empty-list
  make-empty-list
  empty?)
(define empty (make-empty-list))

(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

(define-record (cons-list-of element)
  cons
  cons?
  (first element)
  (rest (list-of element)))

(define list1 (cons 5 empty))
(define list2 (cons 2 (cons 5 empty)))
(define list3 (cons 8 list2))

(define list-of-numbers
  (signature (list-of number)))

; Elemente einer Liste addieren
(: list-sum (list-of-numbers -> number))
(check-expect (list-sum list3) 15)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list) (+ (first list) (list-sum (rest list)))))))

; Produkt der liste
(: list-product (list-of-numbers -> number))
(check-expect (list-product list2) 10)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))
; % - Signaturvariable (magic)

(: fold-list ((%a %b -> %b) %b (list-of %a) -> %b))
(check-expect (fold-list * 1 list2) 10)

(define fold-list
  (lambda (alg init list)
    (cond
      ((empty? list) init)
      ((cons? list) (alg (first list) (fold-list alg init (rest list)))))))

(define map-list-via-fold
  (lambda (f list)
    (fold-list (lambda (x xs) (cons (f x) xs))
               empty
               list)))

(define concat-list
  (lambda (as bs)
    (fold-list cons bs as)))
(check-expect (concat-list (cons 1 empty) (cons 2 empty)) (cons 1 (cons 2 empty)))

; extract all numbers fulfilling a predicate
; % - Signaturvariable (magic)
(: filter-list ((%element -> boolean) (list-of %element) -> (list-of %element)))
(check-expect (even-numbers (cons 2 (cons 3 empty))) (cons 2 empty))
(check-expect (odd-numbers (cons 2 (cons 3 empty))) (cons 3 empty))

(define filter-list
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list)) (cons (first list) (filter-list p? (rest list)))
           (filter-list p? (rest list)))))))

(define even-numbers
  (lambda (list) (filter-list even? list)))
(define odd-numbers
  (lambda (list) (filter-list odd? list)))

; Map a function over a list of elements
(: map-list ((%a -> %b) (list-of %a) -> (list-of %b)))
(check-expect (map-list (lambda (a) (+ 1 a)) list2) (cons 3 (cons 6 empty)))

(define map-list
  (lambda (op list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (op (first list))
        (map-list op (rest list)))))))