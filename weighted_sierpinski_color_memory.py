import numpy as np
import matplotlib.pyplot as plt

# ========================
# Parâmetros da simulação
# ========================
num_points = 500000                    # Número total de pontos (quanto maior, melhor a convergência)
discard_initial = 100                  # Descartar os primeiros pontos (evita artefatos iniciais)

# Vértices do triângulo equilátero
vertices = np.array([
    [0.0, 0.0],                        # Vértice 0 (inferior esquerdo)
    [1.0, 0.0],                        # Vértice 1 (inferior direito)
    [0.5, np.sqrt(3)/2]                # Vértice 2 (topo)
])

# Probabilidades desiguais para cada vértice
# Ordem: vértice 0 (vermelho), vértice 1 (verde), vértice 2 (azul)
probabilities = [0.6, 0.3, 0.1]

# Cores dos vértices em formato RGB (valores entre 0 e 1)
vertex_colors_rgb = np.array([
    [1.0, 0.0, 0.0],   # Vermelho
    [0.0, 1.0, 0.0],   # Verde
    [0.0, 0.0, 1.0]    # Azul
])

# ========================
# Inicialização
# ========================
point = np.array([0.5, 0.5])           # Ponto inicial (centro aproximado)
current_color = np.array([0.5, 0.5, 0.5])  # Cor inicial neutra (cinza médio)

# Listas para armazenar os pontos e cores finais
x_coords = []
y_coords = []
colors_list = []

# ========================
# Loop principal do Jogo do Caos
# ========================
for i in range(num_points):
    # Escolhe um vértice aleatoriamente com as probabilidades definidas
    idx = np.random.choice(3, p=probabilities)

    # Atualização geométrica: move o ponto pela metade da distância até o vértice escolhido
    point = (point + vertices[idx]) / 2.0

    # Atualização da cor com memória: média móvel entre cor atual e cor do vértice
    current_color = (current_color + vertex_colors_rgb[idx]) / 2.0

    # Armazena o ponto e sua cor apenas após descartar os iniciais
    if i >= discard_initial:
        x_coords.append(point[0])
        y_coords.append(point[1])
        colors_list.append(current_color.copy())  # .copy() garante valores independentes

# ========================
# Visualização
# ========================
plt.figure(figsize=(12, 10))
plt.scatter(x_coords, y_coords, c=colors_list, s=0.1, alpha=0.9)

# Configurações estéticas
plt.axis('equal')       # Mantém proporção correta do triângulo
plt.axis('off')         # Remove eixos para visual limpo
plt.title('Jogo do Caos com Memória de Cor e Probabilidades Desiguais\n'
          'P(vermelho)=60%, P(verde)=30%, P(azul)=10%\n'
          'Gradientes revelam densidade probabilística com histórico',
          fontsize=14, pad=20)

plt.tight_layout()
plt.savefig('weighted_sierpinski_color_memory.png')
