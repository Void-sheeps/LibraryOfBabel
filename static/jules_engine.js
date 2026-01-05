"use strict";

/**
 * ENGINE DETERMINÍSTICA — AUDITÁVEL
 * Compatível com WebView Android / Node / CI
 */

/* =======================
 * Tipos simbólicos
 * ======================= */

const Estado = Object.freeze({
  ATIVO: "ATIVO",
  SILENCIO: "SILENCIO",
  COLAPSO: "COLAPSO"
});

const Sentenca = Object.freeze({
  VIDA: "VIDA",
  MORTE: "MORTE"
});

/* =======================
 * Nemesis — perturbação
 * ======================= */

function invocarNemesis(nemesis, frame) {
  return {
    torque: frame.torque + nemesis.driftTorque,
    latencyMs: frame.latencyMs + nemesis.jitterMs,
    signal: Math.min(255, frame.signal + nemesis.saturacaoBias)
  };
}

/* =======================
 * Opressão — limite
 * ======================= */

function imporSilencio(frame, limiteTorque) {
  if (frame.torque > limiteTorque) {
    return { estado: Estado.SILENCIO, torque: 0 };
  }
  return { estado: Estado.ATIVO, torque: frame.torque };
}

/* =======================
 * Absoluta — sentença
 * ======================= */

function sentenciarFrame(frame, carrasco) {
  if (frame.latencyMs >= carrasco.deadlineMs) {
    return Sentenca.MORTE;
  }
  if (frame.signal === carrasco.thresholdHex) {
    return Sentenca.MORTE;
  }
  return Sentenca.VIDA;
}

/* =======================
 * Pipeline principal
 * ======================= */

function executarEngine(input) {
  // --- Frame inicial
  let frame = {
    torque: input.torque,
    latencyMs: input.latencyMs,
    signal: input.signal
  };

  // --- Nemesis (opcional)
  if (input.nemesisAtivo === true) {
    frame = invocarNemesis(input.nemesis, frame);
  }

  // --- Opressão
  const op = imporSilencio(frame, input.opressor.limiteTorque);
  if (op.estado === Estado.SILENCIO) {
    return {
      estado: Estado.SILENCIO,
      sentenca: Sentenca.MORTE
    };
  }

  // --- Absoluta
  const sentenca = sentenciarFrame(frame, input.carrasco);

  return {
    estado: sentenca === Sentenca.VIDA ? Estado.ATIVO : Estado.COLAPSO,
    sentenca
  };
}

/* =======================
 * Export auditável
 * ======================= */

module.exports = {
  executarEngine,
  Estado,
  Sentenca
};
