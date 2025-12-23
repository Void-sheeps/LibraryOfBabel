import numpy as np
import matplotlib.pyplot as plt

num_points = 500000
vertices = np.array([[0, 0], [1, 0], [0.5, np.sqrt(3)/2]])
# Probabilidades desiguais: vértice 0 = 60%, 1 = 30%, 2 = 10%
probabilities = [0.6, 0.3, 0.1]
colors = ['red', 'green', 'blue']

point = np.array([0.5, 0.5])
x, y, c = [], [], []

for _ in range(num_points):
    # Escolha ponderada pelas probabilidades
    vertex_idx = np.random.choice(3, p=probabilities)
    point = (point + vertices[vertex_idx]) / 2.0

    if len(x) > 100:
        x.append(point[0])
        y.append(point[1])
        c.append(colors[vertex_idx])

plt.figure(figsize=(10, 10))
plt.scatter(x, y, c=c, s=0.2)
plt.axis('equal')
plt.axis('off')
plt.title('Jogo do Caos com Probabilidades Desiguais (60% vermelho)\n'
          'Lei dos Grandes Números ainda garante convergência assimétrica',
          fontsize=14)
plt.savefig('weighted_sierpinski.png')
