module Builtins where

import Syntax

typesOfBuiltins :: [(Symbol, TypeExpr)]
typesOfBuiltins =
  [ ("bimap", forall "a" $ forall "b" $ forall "c" $ forall "d" $ ("a" :->: "c") :->: ("b" :->: "d") :->: (TCons "a" "b") :->: TCons "c" "d")
  , ("cons!", forall "a" $ forall "b" $ (TSExpr :->: "a") :->: (TSExpr :->: "b") :->: TSExpr :->: TCons "a" "b")
  , ("specific-keyword!", TypeScheme "a" (Just TKeyword) $ TSpecificKeyword "a" :->: TSExpr :->: TSpecificKeyword "a")
  , ("cons", forall "a" $ forall "b" $ "a" :->: "b" :->: TCons "a" "b")
  , ("nil!", Monotype $ TSExpr :->: TNil)
  ]
