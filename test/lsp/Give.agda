module Give where

open import Common.Nat
open import Common.Equality

add : Nat -> Nat -> Nat
add zero y = y
add (suc x) y = suc {! !}
