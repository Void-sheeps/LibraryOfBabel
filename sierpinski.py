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
POINT_ALPHA: float = 0.8
TITLE_FONT_SIZE: int = 14

# Equilateral triangle vertices
VERTICES: np.ndarray = np.array([[0, 0], [1, 0], [0.5, np.sqrt(3) / 2]])
COLORS: List[str] = ['red', 'green', 'blue']

# --- Main Simulation ---
if __name__ == "__main__":
    # Initial random point
    point: np.ndarray = np.array([0.5, 0.5])

    # Arrays to store points and colors
    x: List[float] = []
    y: List[float] = []
    c: List[str] = []

    for i in range(NUM_POINTS):
        # Choose a vertex randomly (equal probability 1/3)
        vertex_idx: int = np.random.randint(0, NUM_VERTICES)
        chosen_vertex: np.ndarray = VERTICES[vertex_idx]
        chosen_color: str = COLORS[vertex_idx]

        # Move the point halfway to the vertex
        point = (point + chosen_vertex) * CONTRACTION_FACTOR

        # Store for plotting (skip initial points)
        if i > DISCARD_INITIAL:
            x.append(point[0])
            y.append(point[1])
            c.append(chosen_color)

    # --- Plotting ---
    plt.figure(figsize=FIG_SIZE)
    plt.scatter(x, y, c=c, s=POINT_SIZE, alpha=POINT_ALPHA)
    plt.axis('equal')
    plt.axis('off')
    plt.title('Chaos Game - Colored Sierpinski Triangle\n'
              f'{NUM_POINTS} points - Convergence by the Law of Large Numbers',
              fontsize=TITLE_FONT_SIZE)
    plt.savefig('sierpinski.png')
