(define % (lambda (x y)  (- x (* y (truncate (/ x y))))))

(define dualrail-bit
  (lambda (x i)
    (let ((tmp 
	   (string-append x
			  (string-append ".d["
					 (string-append (number->string i)  "]")
					 )
			  )
	   ))
      (list (string-append tmp ".f") (string-append tmp ".t"))
      )
    )
  )

(define assert-var-bit
  (lambda (x i v)
    (let ((tmp (dualrail-bit x i)))
      (begin
	(assert (car tmp) (- 1 v))
	(assert (cadr tmp) v)
	)
      )
    )
  )

(define assert-var-int
  (lambda (x width value)
    (letrec ((assert-one-bit
	      (lambda (v i)
		(cond
		 ((=? i width) #t)
		 (#t (begin
		       (assert-var-bit x i (% v 2))
		       (assert-one-bit (truncate (/ v 2)) (+ i 1))
		       )
		     )
		 )
		)
	      ))
      (assert-one-bit value 0)
      )
    )
  )
	     
(define assert-channel-valid
  (lambda (ch width value)
    (assert-var-int (string-append ch ".d") width value)
    )
  )

(define assert-channel-neutral
  (lambda (ch width)
    (letrec ((helper
	      (lambda (i)
		(cond
		 ((=? i width) #t)
		 (#t (let ((tmp (dualrail-bit (string-append ch ".d") i)))
			(begin
			  (assert (car tmp) 0)
			  (assert (cadr tmp) 0)
			  (helper (+ 1 i))
			  )
			)
		     )
		 )
		)
	      )
	     )
      (helper 0)
      )
    )
  )

			
(define set-channel-neutral
  (lambda (ch width)
    (letrec ((ch-name (string-append ch ".d"))
	     (helper
	      (lambda (i)
		(cond
		 ((=? i width) #t)
		 (#t (let ((tmp (dualrail-bit ch-name i)))
		       (begin
			 (set (car tmp) 0)
			 (set (cadr tmp) 0)
			 (helper (+ 1 i))
			 )
		       )
		     )
		 )
		)
	      ))
      (helper 0)
      )
    )
  )

(define set-channel-valid
  (lambda (ch width val)
    (letrec ((ch-name (string-append ch ".d"))
	     (helper
	      (lambda (i v)
		(cond
		 ((=? i width) #t)
		 (#t (let ((tmp (dualrail-bit ch-name i)))
		       (begin
			 (set (car tmp) (- 1 (% v 2)))
			 (set (cadr tmp) (% v 2))
			 (helper (+ 1 i) (truncate (/ v 2)))
			 )
		       )
		     )
		 )
		)
	      ))
      (helper 0 val)
      )
    )
  )

(define assert-var-bool  (lambda (x v) (assert-var-int x 1 v)))

