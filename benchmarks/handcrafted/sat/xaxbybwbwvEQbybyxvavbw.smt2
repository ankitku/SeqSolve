(set-logic QF_S)
 
(declare-fun x () String)
(declare-fun y () String)
(declare-fun z () String)
(declare-fun v () String)
(declare-fun w () String)

(assert (= (str.++ x "a" x "b" y "b" w "b" w v) (str.++ "b" y "b" y x v "a" v "b" w)))

(check-sat)
(get-model)


