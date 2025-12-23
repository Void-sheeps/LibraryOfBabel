import numpy as np
import matplotlib.pyplot as plt

# Parâmetros
num_points = 500000  # Quantos mais pontos, melhor a convergência (lei dos grandes números)
vertices = np.array([[0, 0], [1, 0], [0.5, np.sqrt(3)/2]])  # Triângulo equilátero
colors = ['red', 'green', 'blue']  # Teoria das cores: um para cada vértice

# Ponto inicial aleatório
point = np.array([0.5, 0.5])

# Arrays para armazenar pontos e cores
x, y, c = [], [], []

for i in range(num_points):
    # Escolhe vértice aleatoriamente (probabilidade igual 1/3)
    vertex_idx = np.random.randint(0, 3)
    chosen_vertex = vertices[vertex_idx]
    chosen_color = colors[vertex_idx]

    # Move o ponto pela metade da distância até o vértice (fator de contração 0.5)
    point = (point + chosen_vertex) / 2.0

    # Armazena para plotagem (pula os primeiros para evitar artefatos iniciais)
    if i > 100:
        x.append(point[0])
        y.append(point[1])
        c.append(chosen_color)

# Plot
plt.figure(figsize=(10, 10))
plt.scatter(x, y, c=c, s=0.2, alpha=0.8)
plt.axis('equal')
plt.axis('off')
plt.title('Jogo do Caos - Triângulo de Sierpinski Colorido\n'
          f'{num_points} pontos - Convergência pela Lei dos Grandes Números',
          fontsize=14)
plt.savefig('sierpinski.png')
