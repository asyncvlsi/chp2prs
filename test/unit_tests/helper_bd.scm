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

(define assert-data-bit
  (lambda (x i v)
    (let ((tmp (string-append x (string-append ".d[" (string-append (number->string i)  "]")))))

        (assert tmp v)
      )
    )
  )

(define assert-data-int
  (lambda (x width value)
    (letrec ((assert-one-data-bit
          (lambda (v i)
        (cond
         ((=? i width) #t)
         (#t (begin
               (assert-data-bit x i (% v 2))
               (assert-one-data-bit (truncate (/ v 2)) (+ i 1))
              )
         )
        )
        )
      ))
      (assert-one-data-bit value 0)
    )
  )
)
         
(define assert-channel-valid
  (lambda (ch width value)
    (begin
      (assert-data-int ch width value)
      (assert (string-append ch ".r") 1)
    )
  )
)

(define assert-channel-neutral
  (lambda (ch width)
    (assert (string-append ch ".r") 0)
    )
  )

            
(define set-channel-neutral
  (lambda (ch width)
    (letrec ((ch-name (string-append ch ".d"))
         (helper
          (lambda (i)
            (cond
              ((=? i width) #t)
              (#t (let ((tmp (string-append ch (string-append ".d[" (string-append (number->string i) "]" )))))
                    (begin
                      (set tmp 0)
                      (helper (+ 1 i))
                    )
                  )
              )
            )
          )
        ))
        (begin 
          (helper 0)
          (set (string-append ch ".r") 0)
        )
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
              (#t (let ((tmp (string-append ch (string-append ".d[" (string-append (number->string i)  "]")))))
                (begin
                  (set tmp (% v 2))
                  (helper (+ 1 i) (truncate (/ v 2)))
                )
              ))
            )
         )
        ))
      (begin
        (helper 0 val)
        (set (string-append ch ".r") 1)
      )
    )
  )
)

(define assert-var-bool  (lambda (x v) (assert-var-int x 1 v)))

