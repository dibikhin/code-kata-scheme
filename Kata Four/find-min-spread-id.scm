; tested in MIT/GNU Scheme only

; data structs due to #(3 77 55)
(define (id vect)
    (vector-ref vect 0))

(define (max-val vect)
    (vector-ref vect 1))

(define (min-val vect)
    (vector-ref vect 2))
    
(define (spread vect)
    (abs
        (- 
            (max-val vect)
            (min-val vect))))

; vector helpers
(define (vector-first vect)
	(vector-ref vect 0))

(define (vect-empty? array)
  (if (= (vect-len array) 0)
      #t
      #f))

; main worker
(define (vector-sort-by get-value vect-of-vects)
	(sort vect-of-vects
		(lambda (x y)
			(let 
				((x-value (get-value x)) 
				 (y-value (get-value y)))
				(cond 
					((= x-value y-value) 0)
					((< x-value y-value) #t)
					((> x-value y-value) #f))))))

(define (find-min-spread-id vect-of-vects)
	(id
		(vector-first
			(vector-sort-by spread vect-of-vects))))

(define temperatures
	#(
		#(1 88 59)
		#(2 79 63)
		#(3 77 55)
		#(4 77 59)
		#(5 90 66)
		#(6 81 61)
		#(7 73 57)
		#(8 75 54)
		#(9 86 32)
		#(10 84 64)
		#(11 91 59)
		#(12 88 73)
		#(13 70 59)
		#(14 61 59)
		#(15 64 55)
		#(16 79 59)
		#(17 81 57)
		#(18 82 52)
		#(19 81 61)
		#(20 84 57)
		#(21 86 59)
		#(22 90 64)
		#(23 90 68)
		#(24 90 77)
		#(25 90 72)
		#(26 97 64)
		#(27 91 72)
		#(28 84 68)
		#(29 88 66)
		#(30 90 45)))

(define soccer-goals
	#(
		#("Arsenal"       79 36)
		#("Liverpool"     67 30)
		#("Manchester_U"  87 45)
		#("Newcastle"     74 52)
		#("Leeds"         53 37)
		#("Chelsea"       66 38)
		#("West_Ham"      48 57)
		#("Aston_Villa"   46 47)
		#("Tottenham"     49 53)
		#("Blackburn"     55 51)
		#("Southampton"   46 54)
		#("Middlesbrough" 35 47)
		#("Fulham"        36 44)
		#("Charlton"      38 49)
		#("Everton"       45 57)
		#("Bolton"        44 62)
		#("Sunderland"    29 51)
		#("Ipswich"       41 64)
		#("Derby"         33 63)
		#("Leicester"     30 64)))

(find-min-spread-id temperatures)

(find-min-spread-id soccer-goals)