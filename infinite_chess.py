import chess
import numpy as np
from typing import List, Tuple, Dict

# --- Constants ---
TOTAL_PLIES: int = 20001
NUM_WALKS: int = 1000
STEPS_PER_WALK: int = 100
RANDOM_SEED: int = 42
DELTAS: List[int] = [-2, -1, 0, 1, 2]
PROBABILITIES: List[float] = [0.05, 0.2, 0.5, 0.2, 0.05]
START_POS: Tuple[int, int] = (7, 0)


def run_deterministic_simulation() -> str:
    """
    Runs a deterministic chess simulation for a specified number of moves,
    always choosing the first legal move.
    """
    board = chess.Board()
    completed_moves = 0

    for i in range(TOTAL_PLIES):
        legal_moves = list(board.legal_moves)
        if not legal_moves:
            print(f"Game over at move {i} due to no legal moves.")
            if board.is_checkmate():
                print("Reason: Checkmate")
            elif board.is_stalemate():
                print("Reason: Stalemate")
            else:
                print("Reason: Unknown")
            completed_moves = i
            break

        move = legal_moves[0]
        board.push(move)
        completed_moves = i + 1

    final_fen = board.fen()
    print(f"FEN after {completed_moves} moves: {final_fen}")
    return final_fen


def parse_fen_to_board_dict(fen_string: str) -> Dict[Tuple[int, int], str]:
    """
    Parses a FEN string's piece placement into a dictionary mapping
    (x, y) coordinates to piece characters. (0,0) corresponds to a1.
    """
    board = {}
    parts = fen_string.split(' ')
    rows = parts[0].split('/')
    for y, row in enumerate(rows):
        x = 0
        for char in row:
            if char.isdigit():
                x += int(char)
            else:
                board[(x, 7 - y)] = char
                x += 1
    return board


def run_monte_carlo_simulation(fen_string: str) -> None:
    """
    Performs a Monte Carlo simulation based on the provided analysis.
    """
    initial_board_state = parse_fen_to_board_dict(fen_string)

    np.random.seed(RANDOM_SEED)

    final_positions_k = []
    nan_transitions = 0
    total_transitions = NUM_WALKS * STEPS_PER_WALK

    for _ in range(NUM_WALKS):
        current_pos = START_POS
        board_copy = initial_board_state.copy()

        if START_POS in board_copy:
            board_copy.pop(START_POS)
        else:
            print(f"Error: No piece at the starting position {START_POS} to simulate.")
            return

        for _ in range(STEPS_PER_WALK):
            axis = np.random.choice([0, 1])
            delta = np.random.choice(DELTAS, p=PROBABILITIES)

            if delta == 0:
                continue

            is_nan = False
            # Check intermediate squares for collision
            step = np.sign(delta)
            for i in range(1, abs(delta) + 1):
                if axis == 0: # Moving along x-axis
                    check_pos = (current_pos[0] + i * step, current_pos[1])
                else: # Moving along y-axis
                    check_pos = (current_pos[0], current_pos[1] + i * step)

                if check_pos in board_copy:
                    is_nan = True
                    break

            if is_nan:
                nan_transitions += 1
            else:
                path_vector = [0, 0]
                path_vector[axis] = delta
                current_pos = (current_pos[0] + path_vector[0], current_pos[1] + path_vector[1])

        final_k = current_pos[0] + 64 * current_pos[1]
        final_positions_k.append(final_k)

    mean_pos = np.mean(final_positions_k)
    std_dev_pos = np.std(final_positions_k)
    nan_percentage = (nan_transitions / total_transitions) * 100

    print("\n--- Monte Carlo Simulation Results ---")
    print(f"Mean final position (k): {mean_pos:.2f}")
    print(f"Standard deviation of final position: {std_dev_pos:.2f}")
    print(f"Percentage of NaN transitions: {nan_percentage:.2f}%")


if __name__ == "__main__":
    final_fen_state = run_deterministic_simulation()
    run_monte_carlo_simulation(final_fen_state)
