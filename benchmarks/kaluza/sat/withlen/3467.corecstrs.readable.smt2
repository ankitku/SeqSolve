(declare-fun I0_2 () Int)
(declare-fun I1_2 () Int)
(declare-fun I2_2 () Int)
(declare-fun PCTEMP_LHS_1 () String)
(declare-fun PCTEMP_LHS_2 () String)
(declare-fun T1_2 () String)
(declare-fun T2_2 () String)
(declare-fun T3_2 () String)
(declare-fun var_0xINPUT_125802 () String)
(assert (= I0_2 (- 48 30)))
(assert (>= 30 0))
(assert (>= 48 30))
(assert (<= 48 I1_2))
(assert (= I2_2 I1_2))
(assert (= I0_2 (str.len PCTEMP_LHS_1)))
(assert (= var_0xINPUT_125802 (str.++ T1_2 T2_2)))
(assert (= T2_2 (str.++ PCTEMP_LHS_1 T3_2)))
(assert (= 30 (str.len T1_2)))
(assert (= I1_2 (str.len var_0xINPUT_125802)))
(assert (= PCTEMP_LHS_2 PCTEMP_LHS_1))
(check-sat)
