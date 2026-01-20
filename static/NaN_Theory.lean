/-
NaN_Theory.lean
Postulado: um não-número não pode ser um número.
Formalização por tipagem (lógica negativa válida).
-/

-- ===============================
-- 1. Domínios
-- ===============================

-- Domínio dos números naturais
def NatNumber := Nat

-- Domínio estendido com um marcador de não-número
inductive ExtNumber
| num : NatNumber → ExtNumber
| NaN : ExtNumber

open ExtNumber

-- ===============================
-- 2. Predicados de pertencimento
-- ===============================

-- É um número natural?
def isNat : ExtNumber → Prop
| num _ => True
| NaN   => False

-- ===============================
-- 3. Teoremas fundamentais
-- ===============================

-- 1 é um número natural
theorem one_is_number : isNat (num 1) := by
  trivial

-- NaN não é um número natural
theorem NaN_is_not_number : ¬ isNat NaN := by
  intro h
  cases h

-- ===============================
-- 4. Desigualdade estrutural
-- ===============================

-- NaN nunca pode ser igual a um número natural
theorem NaN_ne_one : NaN ≠ num 1 := by
  intro h
  cases h

-- ===============================
-- 5. Forma conceitual (capacidade negativa)
-- ===============================

-- Se algo não é número, não pode ser igual a um número
theorem non_number_ne_number :
  ∀ x n, isNat x = False → x ≠ num n := by
  intro x n h
  cases x with
  | num k =>
      simp at h
  | NaN =>
      intro hEq
      cases hEq
