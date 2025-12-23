import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Parâmetros
num_points = 300000
# 8 vértices do cubo unitário
vertices = np.array([
    [0,0,0], [1,0,0], [1,1,0], [0,1,0],
    [0,0,1], [1,0,1], [1,1,1], [0,1,1]
])

# Cores em HSV para gradiente bonito em 3D (convertidas para RGB)
def hsv_to_rgb(h, s=1.0, v=1.0):
    import colorsys
    return colorsys.hsv_to_rgb(h, s, v)

vertex_colors = [hsv_to_rgb(i/8.0) for i in range(8)]  # Arco-íris ao redor do cubo

# Ponto inicial no centro
point = np.array([0.5, 0.5, 0.5])

x, y, z, c = [], [], [], []

for i in range(num_points):
    idx = np.random.randint(0, 8)
    chosen_vertex = vertices[idx]
    chosen_color = vertex_colors[idx]

    # Contração 0.5
    point = (point + chosen_vertex) / 2.0

    # Descartar primeiros pontos
    if i > 100:
        x.append(point[0])
        y.append(point[1])
        z.append(point[2])
        c.append(chosen_color)

# Plot 3D
fig = plt.figure(figsize=(12, 10))
ax = fig.add_subplot(111, projection='3d')
ax.scatter(x, y, z, c=c, s=0.5, alpha=0.7, depthshade=True)

ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
ax.set_title('Jogo do Caos 3D - Atrator Fractal em Cubo (Volume Constante)\n'
             f'{num_points} pontos', fontsize=14)
ax.set_box_aspect([1,1,1])  # Proporção igual
plt.savefig('fractal_cube.png')
