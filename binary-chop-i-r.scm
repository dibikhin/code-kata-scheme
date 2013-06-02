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
      (let ((middle (/ (+ top buttom) 2))) ;integer required !
           (let ((middle-num (vect-ref array middle)))
                (cond ((= middle-num num) middle)
                      ((< middle-num num) (chop-iter num array (+ middle 1) top))
                      ((< num middle-num) (chop-iter num array buttom (- middle 1))))))))
          
(define (chop num array)
  (if (vect-empty? array)
      -1
      (chop-iter num array 0 (- (vect-len array) 1))))

(define (test-chop)
  (begin (assert-equal -1 (chop 3 #()))
         (assert-equal -1 (chop 4 #(1)))
         (assert-equal -1 (chop 2 #(5)))
         (assert-equal  0 (chop 1 #(1)))
         (assert_equal -1 (chop 0 #(1 3)))))

(test-chop)
