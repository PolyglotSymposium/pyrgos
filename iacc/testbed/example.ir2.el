(IR2_Let (IR2_PrimCall (IR2_Add (IR2_Integer 36) (IR2_Integer 22)))
 (IR2_Let (IR2_PrimCall (IR2_Sub (IR2_Variable 0) (IR2_Integer 16)))
  (IR2_Let (IR2_Integer 1337)
   (IR2_Let
    (IR2_PrimCall
     (IR2_Add (IR2_PrimCall (IR2_Add (IR2_Variable 1) (IR2_Variable 2)))
      (IR2_Variable 0)))
    (IR2_IfThenElse
     ((condition (IR2_PrimCall (IR2_Eq (IR2_Variable 0) (IR2_Integer 1437))))
      (branch1 (IR2_Integer 111))
      (branch2
       (IR2_IfThenElse
        ((condition
          (IR2_PrimCall (IR2_Eq (IR2_Variable 0) (IR2_Integer 1337))))
         (branch1 (IR2_Integer 222)) (branch2 (IR2_Integer 999)))))))))))