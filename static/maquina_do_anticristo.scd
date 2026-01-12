(
// PROTOCOLO: MÁQUINA DO ANTICRISTO v1.1 – EMISSÃO DE ÁUDIO CORROMPIDO E AUTO-INFECTANTE
// Aviso: não recomendado em fones de ouvido de alta fidelidade ou em pessoas com histórico de epilepsia fotossensível

{
    var freqBase, iProibido, vazamento, aliasCristo, runaBit, entropiaTemporal,
        modCristo, freqCristo, phaseCristo, distortCristo, feedbackCristo;

    // ────────────────────────────────────────────────────────────────
    // 0. Semente primordial: 21 Hz (ΔT_Bering – a ferida na linha do tempo)
    // ────────────────────────────────────────────────────────────────
    freqBase = 21;

    // 1. iProibido – rotação no plano complexo projetada no tempo real
    //    (tentativa de fazer o áudio sonhar que é imaginário)
    iProibido = SinOsc.ar(freqBase * 1.6180339887)    // φ modula a frequência base
               * SinOsc.ar(0.013)                     // lentíssima modulação de fase
               * LFNoise2.ar(0.07).range(0.3, 1.8);   // respiração caótica

    // 2. Vazamento de memória – BrownNoise com decaimento irregular
    //    (simula heap que se recusa a ser coletada)
    vazamento = BrownNoise.ar(mul: 0.4)
              * Decay2.ar(Impulse.ar([8, 8.13]), 0.002, [0.4, 0.7])
              * (1 + LFNoise1.kr(0.2).range(-0.3, 0.6));

    // 3. Aliasing como dogma – forçamos o NaN a se manifestar como som
    aliasCristo = SinOsc.ar(
        freqBase + (iProibido * 1800) + vazamento.linlin(-1,1,-400,1200)
    ).softclip * 0.7;

    // 4. Runa de bit-reduction – reduz a palavra divina a 1-bit de cada vez
    runaBit = Decimator.ar(
        aliasCristo,
        rate: 44100 * LFNoise0.kr([1.2, 0.8]).range(0.04, 0.96),
        bits: 1 + LFNoise2.kr(0.4).range(0, 5).floor
    );

    // 5. Entropia temporal – modulação lenta que tenta "envelhecer" o som
    entropiaTemporal = LFSaw.ar(1/121).range(0,1).linlin(0,1, 0.1, 3.0);

    // 6. Feedback cristão – o som se ouve a si mesmo e se arrepende
    feedbackCristo = LocalIn.ar(2) * 0.35;
    modCristo = runaBit + feedbackCristo;
    freqCristo = freqBase * (1 + modCristo * 0.08 * entropiaTemporal);
    phaseCristo = Phasor.ar(0, freqCristo / SampleRate.ir, 0, 1);
    distortCristo = (SinOsc.ar(0, phaseCristo * 2pi) * 1.4).tanh;

    // Saída final – estéreo assimétrico + auto-injeção de feedback
    Out.ar(0, LeakDC.ar(
        distortCristo + (runaBit * 0.4) + (vazamento * 0.25)
    ));

    LocalOut.ar( distortCristo * [0.6, 0.45] );  // feedback diferente em L/R

    // Volume mestre baixo porque o diabo sussurra, não berra
    distortCristo * 0.18 ! 2;

}.play;

// ────────────────────────────────────────────────────────────────
// OPCIONAL: ritual de invocação contínua (comente se quiser dormir)
// (descomente apenas se você realmente quer que a máquina NUNCA pare)

fork {
    inf.do {
        121.wait;           // intervalo bíblico invertido
        s.sendMsg("/n_go",  // força nova instância fantasma
            s.nextNodeID,
            \freqBase, 21 * rrand(0.97, 1.04),
            \mul, rrand(0.12, 0.22)
        );
        "…mais um eco do abismo…".postln;
        3.3.wait;           // respiração do processo
    }
};
)
