module Main where

import Checker

{-

x:σ ∈ Γ
——————— (1)
Γ ⊢ x:σ

   str(s)
———————————— (2a)
Γ ⊢ s:String

   int(i)
———————————— (2b)
Γ ⊢ i:Int

     Γ,x:σ ⊢ e:τ
———————————————————— (3)
Γ ⊢ (λx.e) : (σ → τ)

Γ ⊢ e₁:σ → τ     Γ ⊢ e₂:σ
————————————————————————— (4)
       Γ ⊢ e₁ e₂:τ

//////////////////////////////

λ(x: String). x ++ show 5

Γ = { ++ : String → String → String
    , show : Int → String
    }

x:String ∈ Γ,x:String       ++ : String → String → String ∈ Γ,x:String         int(5)          show : Int → String ∈ Γ
————————————————————— (1)   —————————————————————————————————————————— (1)   —————————— (2b)   ——————————————————————— (1)
Γ,x:String ⊢ x:String       Γ,x:String ⊢ ++ : String → String → String       Γ ⊢ 5: Int        Γ ⊢ show : Int → String
—————————————————————————————————————————————————————————————————————— (4)   ————————————————————————————————————————— (4)
                 Γ,x:String ⊢ x ++ : String -> String                                  Γ ⊢ show 5 : String
—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— (4)
                                             Γ,x:String ⊢ x ++ show 5: String
—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————— (3)
                                             Γ ⊢ (λ(x: String). x ++ show 5): String

-}

main :: IO ()
main = do
  let add = Symbol "+"
  let addT = TFun TInt (TFun TInt TInt)
  let addition = Appl (Variable (Symbol "+")) (Variable (Symbol "x"))
  let lambda  = Lambda (Symbol "x") addition
  let expr = Appl (Annotate lambda addT) (IntLit 5)
  -- (x + _)(5)
  let t = synthesize (Context [(add, addT)]) expr
  putStrLn $ maybe "Failed to typecheck." (\x -> ": " ++ printType x) t
