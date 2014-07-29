; test-kit
(define (assert-equal expected actual)
  (if (= actual expected)
      (begin (display "; OK") 
             (newline)
             "; ALL DONE")
      (begin (display "; ERROR. Expected ")
             (display expected)
             (display " but was ")
             (display actual)
             (newline) ". Bad...")))

; helpers
(define nil '())

(define vect-len vector-length)
(define vect-ref vector-ref)

(define (vect-empty? array)
  (if (= (vect-len array) 0)
      #t
      #f))

; binary chop: itarative version
(define (chop num array)
   (if (vect-empty? array)
       -1
       (do ((bottom 0)
            (top (- (vect-len array) 1))
            (middle nil)
            (found-pos nil)
            (middle-num nil))
           ((> bottom top)
            (if (null? found-pos) 
                -1
                found-pos))       
          (set! middle (truncate (/ (+ top bottom) 2)))
          (set! middle-num (vect-ref array middle))        
          (cond ((= middle-num num) (set! found-pos middle) (set! top -2)) ; let u burn in hell!
                ((< middle-num num) (set! bottom (+ middle 1)))
                ((< num middle-num) (set! top (- middle 1)))))))

; test asserts
(define (test-chop)
  (begin 
         (assert-equal -1 (chop 3 #()))           
         (assert-equal -1 (chop 3 #(1)))
         (assert-equal -1 (chop 3 #(4)))
         (assert-equal  0 (chop 1 #(1)))
    
         (assert-equal  0 (chop 1 #(1 3)))
         (assert-equal  1 (chop 3 #(1 3)))
         (assert-equal -1 (chop 4 #(1 3)))
         (assert-equal -1 (chop 0 #(1 3)))
    
         (assert-equal  0 (chop 1 #(1 3 5)))
         (assert-equal  1 (chop 3 #(1 3 5)))
         (assert-equal  2 (chop 5 #(1 3 5)))
         (assert-equal -1 (chop 0 #(1 3 5)))
         (assert-equal -1 (chop 2 #(1 3 5)))
         (assert-equal -1 (chop 4 #(1 3 5)))
         (assert-equal -1 (chop 6 #(1 3 5)))
   
         (assert-equal  0 (chop 1 #(1 3 5 7)))
         (assert-equal  1 (chop 3 #(1 3 5 7)))
         (assert-equal  2 (chop 5 #(1 3 5 7)))
         (assert-equal  3 (chop 7 #(1 3 5 7)))
         (assert-equal -1 (chop 0 #(1 3 5 7)))
         (assert-equal -1 (chop 2 #(1 3 5 7)))
         (assert-equal -1 (chop 4 #(1 3 5 7)))
         (assert-equal -1 (chop 6 #(1 3 5 7)))
         (assert-equal -1 (chop 8 #(1 3 5 7)))
  
         (assert-equal -1 (chop 8 #(1 3 5 7 9)))
         (assert-equal -1 (chop 6 #(1 3 5 7 9 11)))
         (assert-equal -1 (chop 8 #(1 3 5 7 9 11)))
         (assert-equal -1 (chop 10 #(1 3 5 7 9 11)))
         (assert-equal -1 (chop 12 #(1 3 5 7 9 11)))
  
         (assert-equal -1 (chop 4 #(1 3 5 7 9 11 13)))))

(test-chop)
