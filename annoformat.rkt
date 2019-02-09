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

(define (extract-t rect-region)
  (second (fifth (second rect-region))))

(define (t-list xml)
  (map extract-t
       (every-other (find-time xml))))

(define test-dir (build-path (find-system-path 'orig-dir)
                             "CIR Annotations\\"))

(define (do-files directory proc)
  (map (lambda (path)
         (call-with-input-file path proc))
       (directory-list directory
                       #:build? #t)))

(define (every-other li)
  (let ((c 0))
    (filter (lambda (x)
              (begin
                (set! c (+ c 1))
                (odd? c)))
            li)))

;; some tests on general annotation structure

(define (text-count in-file)
  (length (find-text (ssax:xml->sxml in-file '()))))

(define (time-count in-file)
  (length (find-time (ssax:xml->sxml in-file '()))))

(define (text-per-timestamp xml)
  (= (length (find-text xml))
     (* 2 (length (find-time xml)))))

(define (count-text-per-timestamp in-file)
  (text-per-timestamp (ssax:xml->sxml in-file '())))
