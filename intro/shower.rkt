#lang deinprogramm/sdp/beginner

; Duschprodukte ist eins der folgenden
; - Seife
; - Shampoo
; - Mixtur (besteht aus einer Mixtur aus gleichen Teilen zweier Duschprodukte)
; gemischte Daten
(define shower-product
  (signature (mixed soap
                    shampoo
                    mixture)))

; Seife hat folgende Eigenschaften
; - pH-Wert
(define-record soap
  make-soap
  soap?
  (soap-ph number))

(define soap1 (make-soap 7.0))

; Haartyp
; - Schuppen
; - trocken
; - fettig
(define hairtype
  (signature (enum "dandruff" "dry" "oily")))

; Shampoo hat folgende Eigenschaften
; - Haartyp
(define-record shampoo
  make-shampoo
  shampoo?
  (shampoo-hairtype hairtype))

(define shampoo1 (make-shampoo "dandruff"))

#|
; Duschgel
; - Seife
; - Shampoo
(define-record showergel
  make-showergel
  showergel?
  (showergel-soap soap)
  (showergel-shampoo shampoo))

(define showergel1 (make-showergel soap1 shampoo1))
|#

; Mixtur aus unterschiedlichen Duschprodukten
(define-record mixture
  make-mixture
  mixture?
  (mixture-product1 shower-product)
  (mixture-product2 shower-product))

(define mix1 (make-mixture soap1 shampoo1))

(: shower-product-soap-proportion (shower-product -> number))
(check-expect (shower-product-soap-proportion soap1)
              1)
(check-expect (shower-product-soap-proportion shampoo1)
              0)

(define shower-product-soap-proportion
  (lambda (product)
    (cond
      ((soap? product) 1)
      ((shampoo? product) 0)
      ((mixture? product)
       (/
        (+
        (shower-product-soap-proportion (mixture-product1 product))
        (shower-product-soap-proportion (mixture-product2 product)))
        2)))))