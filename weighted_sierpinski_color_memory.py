import numpy as np
import matplotlib.pyplot as plt
from typing import List, Tuple

# ========================
# Simulation Parameters
# ========================
NUM_POINTS: int = 500000
DISCARD_INITIAL: int = 100
CONTRACTION_FACTOR: float = 0.5
NUM_VERTICES: int = 3
FIG_SIZE: Tuple[int, int] = (12, 10)
POINT_SIZE: float = 0.1
POINT_ALPHA: float = 0.9
TITLE_FONT_SIZE: int = 14
TITLE_PAD: int = 20

# Equilateral triangle vertices
VERTICES: np.ndarray = np.array([
    [0.0, 0.0],
    [1.0, 0.0],
    [0.5, np.sqrt(3) / 2]
])

# Unequal probabilities for each vertex
PROBABILITIES: List[float] = [0.6, 0.3, 0.1]

# Vertex colors in RGB format (values between 0 and 1)
VERTEX_COLORS_RGB: np.ndarray = np.array([
    [1.0, 0.0, 0.0],  # Red
    [0.0, 1.0, 0.0],  # Green
    [0.0, 0.0, 1.0]   # Blue
])

# ========================
# Main Simulation
# ========================
if __name__ == "__main__":
    # Initial point (approximate center)
    point: np.ndarray = np.array([0.5, 0.5])
    # Initial neutral color (medium gray)
    current_color: np.ndarray = np.array([0.5, 0.5, 0.5])

    # Lists to store final points and colors
    x_coords: List[float] = []
    y_coords: List[float] = []
    colors_list: List[np.ndarray] = []

    for i in range(NUM_POINTS):
        # Choose a vertex randomly with the defined probabilities
        idx: int = np.random.choice(NUM_VERTICES, p=PROBABILITIES)

        # Geometric update: move the point halfway to the chosen vertex
        point = (point + VERTICES[idx]) * CONTRACTION_FACTOR

        # Color update with memory: moving average between current color and vertex color
        current_color = (current_color + VERTEX_COLORS_RGB[idx]) / 2.0

        # Store the point and its color only after discarding the initial ones
        if i >= DISCARD_INITIAL:
            x_coords.append(point[0])
            y_coords.append(point[1])
            colors_list.append(current_color.copy())

    # ========================
    # Visualization
    # ========================
    plt.figure(figsize=FIG_SIZE)
    plt.scatter(x_coords, y_coords, c=colors_list, s=POINT_SIZE, alpha=POINT_ALPHA)

    # Aesthetic settings
    plt.axis('equal')
    plt.axis('off')
    plt.title('Chaos Game with Color Memory and Unequal Probabilities\n'
              'P(red)=60%, P(green)=30%, P(blue)=10%\n'
              'Gradients reveal probabilistic density with history',
              fontsize=TITLE_FONT_SIZE, pad=TITLE_PAD)

    plt.tight_layout()
    plt.savefig('weighted_sierpinski_color_memory.png')
