import Mathlib.Analysis.SpecialFunctions.Log.Basic
import Mathlib.Data.Real.Basic
import Mathlib.LinearAlgebra.Basic

open Real

-- 1. Definições de logs em bases fixas
noncomputable def f (x : ℝ) : ℝ := log x / log 60
noncomputable def g (x : ℝ) : ℝ := log x / log 10

-- 2. Embedding no plano ℝ × ℝ
noncomputable def Φ (x : ℝ) : ℝ × ℝ := (f x, g x)

-- 3. Vetor direcional fixo (normalizado pelo inverso dos logs das bases)
noncomputable def v : ℝ × ℝ := (1 / log 60, 1 / log 10)

-- 4. Teorema: O embedding é radial (escalar do vetor diretor)
theorem embedding_is_radial {x : ℝ} (hx : x > 0) :
  Φ x = (log x) • v := by
  -- Expande as definições para expor a estrutura subjacente
  unfold Φ f g v
  -- 'ext': Aplica extensionalidade (divide a igualdade de vetores em igualdades de componentes)
  -- '<;>': Aplica a próxima tática em todos os sub-objetivos gerados
  -- 'field_simp': Simplifica operações de corpo (transforma divisão em mult. por inverso)
  ext <;> field_simp
