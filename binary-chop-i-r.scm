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
      (begin
    (display actual)
    (display " is not equal to ")
    (display expected)
    (newline) "bad...")))

; binary chop: itarative process, recursive calls
(define (chop-iter num array buttom top)
  (if (> buttom top)
      -1
      (let ((middle (/ (+ top buttom) 2)))
            middle)))
            
			
; (let ((middle-num (vect-ref array middle)))
                  ; (if (= middle-num num)
				      ; middle))			
				  
(define (chop num array)
  (if (vect-empty? array)
      -1
      (chop-iter num array 0 (- (vect-len array) 1))))

(define (test-chop)
  (begin
    (assert-equal -1 (chop 3 #()))
    (assert-equal -1 (chop 4 #(1))
    (assert-equal -1 (chop 2 #(5))))))

(test-chop)