



(defn make-product
  " construct the product of m1 and m2 "
  [m1 m2]
  (cond (and (= m1 1)
             (variable? m2)) m2
        (and (= m2 1)
             (not (number? m1))) m1
        (or (= m1 0)
            (= m2 0)) 0
        :else (list '* m1 m2)))
  

(defn sum?
  " is e a sum? "
  [e]
  (and (list? e)
       (= (first e) '+)))

(defn addend
  " gets addend (first term in an addition) of the sum s "
  [s]
  (second s))

(defn augend
  " gets augend (second term in an addition) of the sum s "
  [s]
  (cond (nil? (first (rest (rest (rest s))))) (last s)
  :else  (list '* (first (rest (rest s))) (first (rest (rest (rest s)))))))

(defn product?
  " is e a product? "
  [e]
  (and (list? e)
       (= (first e) '*)))

(defn multiplier
  " multiplier of the product p "
  [p]
  (second p))


(defn multiplicand
  " multiplicand of the product p "
  [p]
  (cond (nil? (first (rest (rest (rest p))))) (last p)
  :else  (list '* (first (rest (rest p))) (first (rest (rest (rest p)))))))


(defn exponentiation? 
  [exp] 
   (and (list? exp) (= (first exp) '**))) 

(defn base 
  [exp]
   (second exp))

(defn exponent 
  [exp] 
   (second (next exp))) 

(defn make-exponentiation 
  [base exp]
   (cond (= (number? base) 1) 1
         (= (number? exp) 1) base
         (= (number? exp) 0) 1
         :else (list '** base exp))) 

(defn deriv
  " takes the derivative of exp with respect to var "
  [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp) (make-sum
                        (make-product (multiplier exp)
                                      (deriv (multiplicand exp) var))
                        (make-product (deriv (multiplier exp) var)
                                      (multiplicand exp)))
        (exponentiation? exp) (make-product  
                                (make-product (exponent exp)  
                                  (make-exponentiation (base exp)  
                                    (make-sum (exponent exp) -1))) 
                                (deriv  (base exp) var))
        :else (  (throw (Exception. "unknown expression type -- DERIV")))))
