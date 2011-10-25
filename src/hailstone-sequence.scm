#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 streams))
(use-modules (ice-9 getopt-long))

(define even?
  (lambda (number)
    (= (modulo number 2) 0)))

(define odd?
  (lambda (number)
    (not (even? number))))

(define (hailstone number)
  (define (stone-it number)
    (cond ((odd? number) (+ 1 (* 3 number)))
          ((even? number) (/ number 2))))

  (if (= number 1)
      (list number)
      (append (list number) (hailstone (stone-it number)))))

(define (make-hailstone-sequence limit)
  (let ((numbers (make-stream (lambda (state)
                                (if (>= state limit)
                                    state
                                    (cons state
                                          (+ state 1)))) 1)))
    (define (move-on!)
      (let ((new-stream (stream-cdr numbers)))
        (set! numbers new-stream)))

    (define (dispatch op)
      (cond ((eq? op 'hailstone) (let ((sequence (hailstone
                                                  (stream-car numbers))))
                                        (move-on!)
                                        sequence))

            ((eq? op 'longest) (stream-fold (lambda (sequence previous)
                                              (cond ((or (null? previous)
                                                         (>= (length sequence)
                                                             (cadr previous)))
                                                     (list (car sequence)
                                                           (length sequence)))
                                                    (else previous)))
                                            '()
                                            (stream-map hailstone numbers)))))

    dispatch))


(define (time-diff t1 t2)
  (cons (- (car t1) (car t2))
        (- (cdr t1) (cdr t2))))

(define-syntax measure-time
  (syntax-rules ()
    ((_ exp)
     (let ((past (gettimeofday)))
       (let ((value exp))
         (let ((now (gettimeofday)))
           (list value (time-diff now past))))))))

(define (longest-hailstone-until limit)
  (let ((hailstones (make-hailstone-sequence limit)))
    (hailstone (car (hailstones 'longest)))))


(define (main args)
  (let* ((option-spec '((version (single-char #\v) (value #f))
                        (help    (single-char #\h) (value #f))
                        (longest (single-char #\l) (value #t))))
         (options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f))
         (longest-limit (string->number (option-ref options 'longest "100"))))
    (if (or version-wanted help-wanted)
        (begin
          (if version-wanted
              (display "hailstone-sequence - version 0.5\n"))
          (if help-wanted
              (display "\
hailstone-sequence [options]
   -v, --version     Display version
   -h, --help        Display this help
   -l, --longest N   Display the longest hailstone sequence for any 'n' (< N)
")))
        (begin
          (display (longest-hailstone-until longest-limit))
          (newline)))))
