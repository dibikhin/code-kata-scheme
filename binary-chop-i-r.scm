; test-kit
(define vect-len vector-length)

(define vect-ref vector-ref)

(define (vect-empty? array)
  (if (= (vect-len array) 0)
      #t
      #f))
  
(define (assert-equal expected actual)
  (if (= actual expected)
      "OK"
      (begin (display actual)
             (display " is not equal to ")
             (display expected)
             (newline) "bad...")))

; binary chop: itarative process, recursive calls
(define (chop-iter num array buttom top)
  (if (> buttom top)
      -1
      (let ((middle (truncate (/ (+ top buttom) 2))))
           (let ((middle-num (vect-ref array middle)))
                (cond ((= middle-num num) middle)
                      ((< middle-num num) (chop-iter num array (+ middle 1) top))
                      ((< num middle-num) (chop-iter num array buttom (- middle 1))))))))
          
(define (chop num array)
  (if (vect-empty? array)
      -1
      (chop-iter num array 0 (- (vect-len array) 1))))

; test asserts
(define (test-chop)
  (begin (assert-equal -1 (chop 3 #()))           
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
  
         (assert-equal -1(chop 4 #(1 3 5 7 9 11 13)))))

(test-chop)
