(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)

(assert (= (str.++ x "cdabggcd") (str.++ "ab" y x y)))

(check-sat)
(get-model)
