(define (min-spread-day-num data) 
    (day-num
        (min-spread-entry data)))

(define (day-num entry)
    ())
    
(define (min-spread-entry data)
    (min spread data))

(define (spread entry)
    (abs
        (- 
            (max-temp entry)
            (min-temp entry))))