(Let (Pattern_Var x) (Apply (Apply (InfixOp +) (Integer 36)) (Integer 22))
 (Let (Pattern_Var y) (Apply (Apply (InfixOp -) (Variable x)) (Integer 16))
  (Let (Pattern_Var z) (Integer 1337)
   (Match
    ((subject
      (Apply
       (Apply (InfixOp +)
        (Apply (Apply (InfixOp +) (Variable y)) (Variable x)))
       (Variable z)))
     (cases
      (((Pattern_Int 1437) (Integer 111)) ((Pattern_Int 1337) (Integer 222))
       (Pattern_Wildcard (Integer 999)))))))))