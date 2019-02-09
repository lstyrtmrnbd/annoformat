#lang racket

(require sxml)

(define test-file "test.annotations.xml")

(define (get-sxml filename)
  (ssax:xml->sxml (open-input-file filename) '()))

(define find-text
  (sxpath "/document/annotations/annotation/TEXT"))

(define (break-text text-list)
  (map (lambda (txt)
         (string-append (sxml:text txt) "\n\n"))
       text-list))

;; testing
(define (print-text text-list)
  (map (lambda (txt)
         (display (string-append (sxml:text txt) "\n\n")))
       text-list))

(define find-time
  (sxpath "/document/annotations/annotation/segment/movingRegion/rectRegion"))

(define (every-other li)
  (let ((c 0))
    (filter (lambda (x)
              (begin
                (set! c (+ c 1))
                (odd? c)))
            li)))

(define (extract-t rect-region)
  (second (fifth (second rect-region))))

(define (break-time xml)
  (map (lambda (rr)
         (string-append (extract-t rr) "\n"))
       (every-other (find-time xml))))

;; zips timecodes with line-broken text blurbs
(define (combine-time-text xml)
  (map list
       (break-time xml)
       (break-text (find-text xml))))

(define (display-time-text ttl out)
  (map (lambda (tt)
         (begin
           (display (first tt) out)
           (display (second tt) out)))
       ttl))

;; for the big run
(define input-dir (build-path (find-system-path 'orig-dir)
                              "CIR Annotations\\"))

(define output-dir "CIR Annotations Reformatted\\")

(define (in-out in-path out-path)
  (call-with-input-file in-path
    (lambda (input)
      (call-with-output-file out-path
        (lambda (output)
          (display-time-text (combine-time-text (ssax:xml->sxml input))
                             output))))))

;; takes input path and generates output path
(define (transform-path in-path)
  (let-values ([(base filename drop) (split-path in-path)])
    (let-values ([(parent olddir drop) (split-path base)])
      (build-path parent output-dir (path-replace-extension filename ".txt")))))

(define (do-files directory proc)
  (map proc
       (directory-list directory
                       #:build? #t)))

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
