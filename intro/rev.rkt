#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (append-element (rev (rest list)) (first list))))))

(: append-element ((list-of %a) %a -> (list-of %a)))
(check-expect (append-element (list 1 2 3) 4)
              (list 1 2 3 4))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (append-element (rest list) element))))))

(check-expect (rev2 (list 1 2 3 4) empty)
              (list 4 3 2 1))

(define rev2
  (lambda (list acc) ; accumulator
    (cond
      ((empty? list) acc)
      ((cons? list)
       (define acc2 (cons (first list) acc))
       (rev2 (rest list) acc2)))))