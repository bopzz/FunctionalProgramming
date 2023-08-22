(defun .member (X L)
    (cond
        ((null L) nil)
        ((not (null L)) (cond
            ((equalp X (car L)) t) 
            ((not (equalp X (car L))) (.member X (cdr L)))
        ))
    )
)    

(defun .append (X L1)
    (cond
        ((null L1) (cons X '()))
        ((not (null L1)) (cons (car L1) (.append X (cdr L1))))
    )
)

(defun .remove_all_helper (X L1 L2)
    (cond
        ((null L1) L2)
        ((not (null L1)) (cond             
            ((not (equalp X (car L1))) (.remove_all_helper X (cdr L1) (.append (car L1) L2)))
            ((equalp X (car L1)) (.remove_all_helper X (cdr L1) L2))
        ))
    )
)

(defun .remove-all (X L)
    (cond
        ((null L) nil)
        ((not (null L)) (.remove_all_helper X L '()))
    )
)

(defun .foldl (L F Z) 
    (cond
        ((null L) Z)
        ((not (null L)) (
            .foldl (cdr L) F (funcall F Z (car L))
        ))
    )
)

(defun .sum (L)
    (cond
        ((null L) 0)
        ((not (null L)) (+ (car L) (.sum(cdr L))))
    )
)

(defun .add-element (X L)
    (cond 
        ((null L) (cons X '()))
        ((not (null L)) (cond
            ((equalp t (.member X L)) L)
            ((not (equalp t (.member X L))) (cons X L))
        ))
    )
)

(defun .intersection_helper (S1 S2 S3)
    (cond 
        ((or (null S1) (null S2)) S3)
        ((and (not (null S1)) (not (null S2))) (cond
            ((equalp t (.member (car S1) S2)) (.intersection_helper (cdr S1) S2 (.append (car S1) S3)))
            ((not (equalp t (.member (car S1) S2))) (.intersection_helper (cdr S1) S2 S3))
        )))        
)

(defun .intersection (S1 S2)
    (.intersection_helper S1 S2 '())
)

(defun .supersetp (S1 S2)
    (cond
        ((and (null S1) (null S2) t))
        ((null S1) nil)
        ((null S2) t)
        ((not (null S2)) (cond
            ((equalp t (.member (car S2) S1)) (.supersetp S1 (cdr S2)))
            ((not (equalp t (.member (car S2) S1))) nil)
        ))
    )
)

(defun .cardinality (S)
    (cond
        ((null S) 0)
        ((not (null S)) (+ 1 (.cardinality(cdr S)))) 
    )
)

(defun .factorial (N)
    (cond 
        ((equalp N 0) 1)
        ((not (equalp N 0)) (* N (.factorial(- N 1))))
    )
)

(defun .remainder (X Y)
    (cond 
        ((equalp X Y) 0)
        ((< X 0) nil)
        ((> X 0) (cond
            ((> X Y) (.remainder (- X Y) Y))
            ((< X Y) X)    
        ))
    )
)

(defun .getMin (X Y)
    (cond
        ((>= X Y) Y)
        ((< X Y) X)
    )
)

(defun .gcd (X Y)
    (cond
        ((equalp X 0) Y)
        ((equalp Y 0) X)
        ((equalp (.remainder X Y) 0) (.getMin X Y))
        ((not (equalp (.remainder X Y) 0)) (cond
            ((equalp X (.getMin X Y)) (.gcd X (.remainder Y X)))
            ((equalp Y (.getMin X Y)) (.gcd Y (.remainder X Y)))
        ))
    )
)

(defun .pow (X Y)
    (cond
        ((equalp X 0) 0)
        ((equal Y 0) 1)
        ((> Y 0) (* X (.pow X (- Y 1))))  
        ((< Y 0) (/ 1 (* X (.pow X (- (- 0 Y) 1)))))
    )
)

(defun .with-annual-interest (P R N)
    (cond
        ((equalp P 0) 0)
        ((equalp N 0) P)
        ((not (equal N 0)) (* P (.pow (+ 1 R) N)))
    )
)