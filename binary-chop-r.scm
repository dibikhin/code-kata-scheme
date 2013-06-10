; test-kit
(define vect-len vector-length)
(define vect-ref vector-ref)
(define vect-map vector-map)

(define (vect-empty? array)
  (if (= (vect-len array) 0)
      #t
      #f))

(define (sub-vect array from to)
  (if (> from to)
      #()
      (subvector array from to)))
  
(define (assert-equal expected actual)
  (if (= actual expected)
      "OK"
      (begin (display "Expected ")
             (display expected)
             (display " but was ")
             (display actual)
             (newline) ". Bad...")))

; prepare array
(define (index-vect array) 
  (let ((count -1))
    (vect-map (lambda (item)
              (set! count (+ count 1))
              (cons item count))
            array)))

; binary chop: recursive process, recursive calls
(define (chop-recur num array)
  (if (vect-empty? array)
      -1
      (let ((middle (truncate (/ (- (vect-len array) 1) 2))))
           (let ((middle-num (car (vect-ref array middle))))
                (cond ((= middle-num num) (cdr (vect-ref array middle)))
                      ((< middle-num num) (chop-recur num (sub-vect array (+ middle 1) (vect-len array))))
                      ((< num middle-num) (chop-recur num (sub-vect array 0 middle ))))))))
          
(define (chop num array)
  (if (vect-empty? array)
      -1
      (chop-recur num (index-vect array))))
	
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
