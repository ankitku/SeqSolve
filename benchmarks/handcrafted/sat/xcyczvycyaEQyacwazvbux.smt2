(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)
(declare-fun u () String)
(declare-fun v () String)
(declare-fun w () String)

(assert (= (str.++ x "c" y "c" z v y "c" y "a") (str.++ y "ac" w "a" z v "b" u x)))

(check-sat)
(get-model)
