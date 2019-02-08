#lang racket

(require sxml)

(define test-file "test.annotations.xml")

(define (get-sxml filename)
  (ssax:xml->sxml (open-input-file filename) '()))

(define find-text
  (sxpath "/document/annotations/annotation/TEXT"))

(define (print-text text-list)
  (map (lambda (txt)
         (display (string-append (sxml:text txt) "\n\n")))
       text-list))

(define find-time
  (sxpath "/document/annotations/annotation/segment/movingRegion/rectRegion"))
