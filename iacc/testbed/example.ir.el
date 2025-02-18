(IR_Let (IR_PrimCall (IR_Add (IR_Integer 36) (IR_Integer 22)))
 (IR_Let (IR_PrimCall (IR_Sub (IR_Variable 0) (IR_Integer 16)))
  (IR_Let (IR_Integer 1337)
   (IR_Let
    (IR_PrimCall
     (IR_Add (IR_Variable 1)
      (IR_PrimCall (IR_Add (IR_Variable 2) (IR_Variable 0)))))
    (IR_IfThenElse
     ((condition (IR_PrimCall (IR_Eq (IR_Variable 0) (IR_Integer 1437))))
      (branch1 (IR_Integer 111)) (branch2 (IR_Integer 999))))))))