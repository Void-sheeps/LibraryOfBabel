import enum

class State(enum.Enum):
    Stable = "Stable"
    MetaStable = "MetaStable"
    CognitiveOverflow = "CognitiveOverflow"

class Agent:
    def __init__(self, reflective: bool, capacity: float, realityTesting: bool):
        self.reflective = reflective
        self.capacity = capacity
        self.realityTesting = realityTesting

class Algorithm:
    def __init__(self, engagementWeight: float, retentionWeight: float, predictivityWeight: float, demand: float):
        self.engagementWeight = engagementWeight
        self.retentionWeight = retentionWeight
        self.predictivityWeight = predictivityWeight
        self.demand = demand

def optimize(a: Algorithm, e: float, r: float, p: float) -> float:
    return a.engagementWeight * e + a.retentionWeight * r + a.predictivityWeight * p

def cognitive_load(a: Algorithm, h: Agent) -> float:
    return a.demand - h.capacity

def evaluate_state(a: Algorithm, h: Agent, t: float) -> State:
    load = cognitive_load(a, h)
    threshold = 1.0
    if not h.reflective:
        return State.Stable
    if not h.realityTesting:
        return State.CognitiveOverflow
    if load <= 0:
        return State.Stable
    if load > 0 and t < threshold:
        return State.MetaStable
    return State.CognitiveOverflow

def system_judgement(a: Algorithm, h: Agent) -> str:
    if h.reflective and a.demand > h.capacity:
        return "DESIGN_DEFICIT: algoritmo unsafe para agentes reflexivos"
    return "SYSTEM_OK: comportamento esperado"

# Exemplo concreto
agent_lucido = Agent(reflective=True, capacity=5.0, realityTesting=True)
algoritmo_plataforma = Algorithm(engagementWeight=1.0, retentionWeight=1.0, predictivityWeight=1.0, demand=7.5)

t_exposicao = 0.8
print(evaluate_state(algoritmo_plataforma, agent_lucido, t_exposicao).value)
print(system_judgement(algoritmo_plataforma, agent_lucido))
