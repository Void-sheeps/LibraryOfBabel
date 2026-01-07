/**
 * Executa uma auditoria de saúde em múltiplos endpoints simultaneamente.
 * Demonstra: Async/Await, Promise.allSettled, Destructuring, High-Order Functions.
 */

const services = [
  { name: 'Auth Service', url: 'https://api.exemplo.com/auth/health' },
  { name: 'Payment Gateway', url: 'https://api.exemplo.com/pay/status' },
  { name: 'Legacy Core', url: 'http://localhost:8080/ping' } // Pode falhar
];

// O Comando Técnico
const runAudit = async (targets) => {
  console.time('Audit Duration'); // Inicia telemetria

  // Mapeia o array de objetos para um array de Promises
  const results = await Promise.allSettled(
    targets.map(async ({ name, url }) => {
      try {
        const response = await fetch(url, { method: 'HEAD', signal: AbortSignal.timeout(2000) });
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        return { name, status: 'ONLINE', latency: 'Low' };
      } catch (err) {
        // Lança o erro para ser capturado como 'rejected' no allSettled
        throw new Error(`${name} unreachable: ${err.message}`);
      }
    })
  );

  // Processa os resultados separando Sucesso de Falha (Partitioning)
  const report = results.reduce((acc, result) => {
    const key = result.status === 'fulfilled' ? 'active' : 'failures';
    const value = result.status === 'fulfilled' ? result.value : result.reason.message;

    return { ...acc, [key]: [...(acc[key] || []), value] };
  }, { active: [], failures: [] });

  console.timeEnd('Audit Duration');
  return report;
};

// Execução
runAudit(services).then(console.table).catch(console.error);
