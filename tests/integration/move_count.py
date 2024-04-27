import os
import tempfile

import chess.pgn

import subprocess


def run_command(pgn_file_path: str):
    command = ["swipl", "-t", "halt", "-f", "-q", "-O", "../../src/main.pl", "--", pgn_file_path, "TEST"]
    process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    output = stdout.decode()
    return output.splitlines()


def generate_possible_pgns(pgn_file_path):
    with open(pgn_file_path) as pgn_file:
        game = chess.pgn.read_game(pgn_file)

    board = game.board()
    for move in game.mainline_moves():
        board.push(move)

    possible_moves = list(board.legal_moves)
    possible_pgns = []

    for move in possible_moves:
        new_game = chess.pgn.Game()

        for original_move in game.mainline_moves():
            new_game = new_game.add_variation(original_move)

        new_game = new_game.add_variation(move)

        pgn = str(chess.pgn.Game().from_board(new_game.board()))
        pgn = pgn.splitlines()[-1]
        possible_pgns.append(pgn)

    return possible_pgns


def create_temp_files(pgn_file_path):
    with open(pgn_file_path) as pgn_file:
        game = chess.pgn.read_game(pgn_file)

    moves = list(game.mainline_moves())
    temp_files = []

    for i in range(len(moves)):
        temp_file = tempfile.NamedTemporaryFile(delete=False)
        temp_files.append(temp_file.name)

        new_game = chess.pgn.Game()
        board = new_game.board()
        for move in moves[:i + 1]:
            board.push(move)
            new_game = new_game.add_variation(move)

        pgn = str(chess.pgn.Game().from_board(board))
        pgn = pgn.splitlines()[-1]
        temp_file.write(pgn.encode())
        temp_file.close()

    return temp_files


def compare_sets(set1, set2):
    for el in set1 - set2:
        print(f"Missing: {el}")

    for el in set2 - set1:
        print(f"Extra: {el}")

    return set1 == set2


def test_pgn(file: str):
    first = set(generate_possible_pgns(file))
    second = set(run_command(file))
    return compare_sets(first, second)


#print(test_pgn("../../src/craycray.pgn"))

temps = create_temp_files("../../src/craycray.pgn")

t = ['a', 'b']
for i, temp in enumerate(temps):
    print(f"======================== move {i // 2 + 1}.{t[i % 2]} ========================")
    print(test_pgn(temp))

# Clean up
for temp in temps:
    os.remove(temp)


