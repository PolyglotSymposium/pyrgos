(IR1_Match
 ((subject
   (IR1_Apply (IR1_Apply (IR1_InfixOp +) (IR1_Integer 36)) (IR1_Integer 22)))
  (cases
   (((IR1_Pat_Terminal (TPat_Var x))
     (IR1_Match
      ((subject
        (IR1_Apply (IR1_Apply (IR1_InfixOp -) (IR1_Variable x))
         (IR1_Integer 16)))
       (cases
        (((IR1_Pat_Terminal (TPat_Var y))
          (IR1_Match
           ((subject (IR1_Integer 1337))
            (cases
             (((IR1_Pat_Terminal (TPat_Var z))
               (IR1_Match
                ((subject
                  (IR1_Apply
                   (IR1_Apply (IR1_InfixOp +)
                    (IR1_Apply (IR1_Apply (IR1_InfixOp +) (IR1_Variable y))
                     (IR1_Variable x)))
                   (IR1_Variable z)))
                 (cases
                  (((IR1_Pat_Terminal (TPat_Int 1437)) (IR1_Integer 111))
                   ((IR1_Pat_Terminal (TPat_Int 1337)) (IR1_Integer 222))
                   ((IR1_Pat_Terminal TPat_Wildcard) (IR1_Integer 999)))))))))))))))))))))