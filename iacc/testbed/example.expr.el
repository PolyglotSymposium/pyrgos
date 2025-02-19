(Let (Pattern_Var x) (ApplyInfix (Add (Integer 36) (Integer 22)))
 (Let (Pattern_Var y) (ApplyInfix (Sub (Variable x) (Integer 16)))
  (Let (Pattern_Var z) (Integer 1337)
   (Match
    ((subject
      (ApplyInfix
       (Add (Variable y) (ApplyInfix (Add (Variable x) (Variable z))))))
     (cases
      (((Pattern_Int 1437) (Integer 111)) ((Pattern_Int 1337) (Integer 222))
       (Pattern_Wildcard (Integer 999)))))))))