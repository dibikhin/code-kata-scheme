(define (id entry)
    ())

(define (max-val entry)
    ())

(define (min-val entry)
    ())
    
(define (spread entry)
    (abs
        (- 
            (max-val entry)
            (min-val entry))))

(define (find-min spread entry-set) 
	(vector-first
		(vector-sort
			(vector-map spread entry-set))))

(define (min-spread-entry entry-set)
    (find-min spread entry-set))

(define (min-spread-id entry-set) 
    (id
        (min-spread-entry entry-set)))

; 1 88 59
; 2 79 63
; 3 77 55