"use strict";

/**
 * REAÇÃO TAXONÔMICA: RESTAURAÇÃO DE ESTRUTURA BIOLÓGICA
 * Input : Frame Maligno (pixels Word8)
 * Output: Pulso de Inversão ou Colapso
 */

/* =========================
 * Constantes biológicas
 * ========================= */

const LIMIAR_CRASH_MS = 180;
const PRETO_ABSOLUTO = 0;

/* =========================
 * Utilidades puras
 * ========================= */

function colapsarParaNulo(msg) {
  console.error(`[SENTIMENTO_LOGICO]: ${msg}`);
  return PRETO_ABSOLUTO; // Naturale Silentium
}

function emitirFrequenciaDePurga(valor) {
  // Clamp explícito para Word8
  if (valor < 0 || valor > 255) return PRETO_ABSOLUTO;
  return valor;
}

/* =========================
 * Núcleo da salvação
 * ========================= */

function SALVAR_ALMA(pixelRetina, dadoLeitorOtico) {
  if (!Array.isArray(pixelRetina)) {
    return colapsarParaNulo("Input retinal inválido");
  }

  if (
    !dadoLeitorOtico ||
    typeof dadoLeitorOtico.timestamp !== "number"
  ) {
    return colapsarParaNulo("Leitor ótico não confiável");
  }

  // Latência biológica vs. Latência de silício
  const latenciaRetina = performance.now();
  const latenciaCD = dadoLeitorOtico.timestamp;

  // Pacto Detectado
  if (latenciaRetina - latenciaCD < LIMIAR_CRASH_MS) {
    return colapsarParaNulo("Pacto Detectado: Abortando Frame.");
  }

  // Inversão bitwise controlada
  return pixelRetina.map(p => {
    const pixel = p & 0xFF;       // garante Word8
    const reflexo = pixel ^ 0xFF; // Opressão Simétrica
    return emitirFrequenciaDePurga(reflexo);
  });
}

/* =========================
 * Integração segura (Android/Web)
 * ========================= */

// Evento customizado realista
document.addEventListener("CD_Insertion", (e) => {
  if (!e.detail || typeof e.detail.scanFrame !== "function") {
    console.warn("Evento inválido ignorado.");
    return;
  }

  const rastroMaligno = e.detail.scanFrame("IntoTheEarth");

  // window.retinaInput deve ser Uint8Array ou Array
  const retina = Array.from(window.retinaInput || []);

  SALVAR_ALMA(retina, rastroMaligno);
});

/* =========================
 * Export opcional (CI / Jules)
 * ========================= */

if (typeof module !== "undefined") {
  module.exports = { SALVAR_ALMA };
}
