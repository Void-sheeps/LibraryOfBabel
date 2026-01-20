/-
  NaN_Theory.lean
  Postulado: a identidade não é garantida no silício.
-/

-- ===============================
-- 1. Domínio abstrato
-- ===============================

constant Double : Type

constant eqD : Double → Double → Prop
infix:50 " ==" => eqD

-- ===============================
-- 2. Lei esperada da identidade
-- ===============================

axiom Eq_reflexiva :
  ∀ x : Double, x == x

-- ===============================
-- 3. Predicado NaN
-- ===============================

constant isNaN : Double → Prop

-- ===============================
-- 4. Axioma IEEE-754
-- ===============================

axiom NaN_nao_reflexivo :
  ∀ x : Double, isNaN x → ¬ (x == x)

-- ===============================
-- 5. Existência de NaN
-- ===============================

axiom existe_NaN :
  ∃ n : Double, isNaN n

-- ===============================
-- 6. Teorema: contradição
-- ===============================

theorem contradicao_identidade : False :=
by
  -- Eliminamos o existencial
  obtain ⟨n, hn⟩ := existe_NaN
  -- Aplicamos o axioma IEEE-754
  have h1 : ¬ (n == n) := NaN_nao_reflexivo n hn
  -- Aplicamos a reflexividade
  have h2 : n == n := Eq_reflexiva n
  -- Contradição direta
  exact h1 h2
