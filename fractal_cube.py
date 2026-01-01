import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from typing import List, Tuple

# --- Constants ---
NUM_POINTS: int = 300000
DISCARD_INITIAL: int = 100
CONTRACTION_FACTOR: float = 0.5
NUM_VERTICES: int = 8
FIG_SIZE: Tuple[int, int] = (12, 10)
POINT_SIZE: float = 0.5
POINT_ALPHA: float = 0.7
TITLE_FONT_SIZE: int = 14

# 8 vertices of the unit cube
VERTICES: np.ndarray = np.array([
    [0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0],
    [0, 0, 1], [1, 0, 1], [1, 1, 1], [0, 1, 1]
])

# --- Functions ---
def hsv_to_rgb(h: float, s: float = 1.0, v: float = 1.0) -> Tuple[float, float, float]:
    """Converts HSV color to RGB color."""
    import colorsys
    return colorsys.hsv_to_rgb(h, s, v)

# --- Main Simulation ---
if __name__ == "__main__":
    # Generate a rainbow of colors around the cube
    vertex_colors: List[Tuple[float, float, float]] = [hsv_to_rgb(i / float(NUM_VERTICES)) for i in range(NUM_VERTICES)]

    # Initial point at the center
    point: np.ndarray = np.array([0.5, 0.5, 0.5])

    x: List[float] = []
    y: List[float] = []
    z: List[float] = []
    c: List[Tuple[float, float, float]] = []

    for i in range(NUM_POINTS):
        idx: int = np.random.randint(0, NUM_VERTICES)
        chosen_vertex: np.ndarray = VERTICES[idx]
        chosen_color: Tuple[float, float, float] = vertex_colors[idx]

        # Contraction
        point = (point + chosen_vertex) * CONTRACTION_FACTOR

        # Discard initial points
        if i > DISCARD_INITIAL:
            x.append(point[0])
            y.append(point[1])
            z.append(point[2])
            c.append(chosen_color)

    # --- Plotting ---
    fig = plt.figure(figsize=FIG_SIZE)
    ax = fig.add_subplot(111, projection='3d')
    ax.scatter(x, y, z, c=c, s=POINT_SIZE, alpha=POINT_ALPHA, depthshade=True)

    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('Z')
    ax.set_title('3D Chaos Game - Fractal Attractor in a Cube (Constant Volume)\n'
                 f'{NUM_POINTS} points', fontsize=TITLE_FONT_SIZE)
    ax.set_box_aspect([1, 1, 1])  # Equal proportion
    plt.savefig('fractal_cube.png')
