import numpy as np
import matplotlib.pyplot as plt
from typing import List, Tuple

# --- Constants ---
NUM_POINTS: int = 500000
DISCARD_INITIAL: int = 100
CONTRACTION_FACTOR: float = 0.5
NUM_VERTICES: int = 3
FIG_SIZE: Tuple[int, int] = (10, 10)
POINT_SIZE: float = 0.2
TITLE_FONT_SIZE: int = 14

# Equilateral triangle vertices
VERTICES: np.ndarray = np.array([[0, 0], [1, 0], [0.5, np.sqrt(3) / 2]])

# Unequal probabilities: vertex 0 = 60%, 1 = 30%, 2 = 10%
PROBABILITIES: List[float] = [0.6, 0.3, 0.1]
COLORS: List[str] = ['red', 'green', 'blue']

# --- Main Simulation ---
if __name__ == "__main__":
    point: np.ndarray = np.array([0.5, 0.5])
    x: List[float] = []
    y: List[float] = []
    c: List[str] = []

    for i in range(NUM_POINTS):
        # Weighted choice based on probabilities
        vertex_idx: int = np.random.choice(NUM_VERTICES, p=PROBABILITIES)
        point = (point + VERTICES[vertex_idx]) * CONTRACTION_FACTOR

        if i > DISCARD_INITIAL:
            x.append(point[0])
            y.append(point[1])
            c.append(COLORS[vertex_idx])

    # --- Plotting ---
    plt.figure(figsize=FIG_SIZE)
    plt.scatter(x, y, c=c, s=POINT_SIZE)
    plt.axis('equal')
    plt.axis('off')
    plt.title('Chaos Game with Unequal Probabilities (60% red)\n'
              'The Law of Large Numbers still guarantees an asymmetrical convergence',
              fontsize=TITLE_FONT_SIZE)
    plt.savefig('weighted_sierpinski.png')
