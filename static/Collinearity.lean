import Mathlib.Analysis.SpecialFunctions.Log.Basic
import Mathlib.Data.Real.Basic
import Mathlib.LinearAlgebra.Basic

open Real

-- Logs em bases fixas
def f (x : ℝ) : ℝ := log x / log 60
def g (x : ℝ) : ℝ := log x / log 10

-- Embedding no plano
def Φ (x : ℝ) : ℝ × ℝ := (f x, g x)

-- Vetor direcional fixo
def v : ℝ × ℝ := (1 / log 60, 1 / log 10)

-- Lema central: colinearidade
theorem embedding_is_radial
  {x : ℝ} (hx : x > 0) :
  Φ x = (log x) • v := by
  unfold Φ f g v
  -- Produto escalar em ℝ × ℝ é componente a componente
  ext <;> field_simp
