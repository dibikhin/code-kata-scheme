(define (id entry)
    (vector-ref vector 0))

(define (max-val entry)
    (vector-ref vector 1))

(define (min-val entry)
    (vector-ref vector 2))
    
(define (spread entry)
    (abs
        (- 
            (max-val entry)
            (min-val entry))))

(define (vector-first vector)
	(vector-ref vector 0))

(define (min-spread-id entry-set)
    (id
		(vector-first
			(vector-sort
				(vector-map spread entry-set)))))

(define entry-set 
	#(
		#(1 88 59)
		#(2 79 63)
		#(3 77 55)))

(min-spread-id entry-set)