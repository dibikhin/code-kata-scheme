;
(define nil '())

; tree
(define (make-node left item rigth)
  (cons item (cons left rigth)))

(define (left node)
  (if (and (not (null? node))
           (pair? node))
      (car (cdr node))
      nil))

(define (item node)
  (if (and (not (null? node))
           (pair? node))
      (car node)
      nil))

(define (right node)
  (if (and (not (null? node))
           (pair? node))
      (cdr (cdr node))
      nil))

; tree debug
(define (print tree indent)
  (if (not (null? tree))
      (begin (display indent)
             (display (number->string (value (item tree))))
             (newline)
             (print (left tree) (string-append indent "---"))
             (print (right tree) (string-append indent "---")))))

; item
(define (make-item value index)
  (cons value index))

(define (value item)
  (car item))

(define (index item)
  (cdr item))

; vector
(define vect-len vector-length)
(define vect-ref vector-ref)
(define vect-map vector-map)

(define (vect-empty? array)
  (if (= (vect-len array) 0)
      #t
      #f))

; vect->tree
(define (vect->tree array)
  (define (iter array buttom top)
    (if (> buttom top)
        nil
        (let ((middle (truncate (/ (+ top buttom) 2))))
             (let ((item (vect-ref array middle)))
                (make-node 
                  (iter array buttom (- middle 1))
                  item
                  (iter array (+ middle 1) top))))))
  (if (vect-empty? array)
      nil
      (iter array 0 (- (vect-len array) 1))))

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

; prepare array
(define (index-vect array) 
  (let ((count -1))
    (vect-map (lambda (number)
              (set! count (+ count 1))
              (make-item number count))
            array)))

; binary chop tree
(define (chop num array)
  (define (chop-tree number tree value index)
    (if (null? tree) 
        -1        
        (cond ((= number (value (item tree))) (index (item tree)))
              ((> number (value (item tree))) (chop-tree number (right tree) value index))
              ((< number (value (item tree))) (chop-tree number (left tree) value index)))))
  (if (vect-empty? array)
      -1
      (chop-tree num (vect->tree (index-vect array)) value index)))

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
