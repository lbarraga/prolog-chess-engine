"""
This script is used to test the "all possible moves" functionality.
When the program is called with the TEST argument, all the possible moves are generated.

The entry function is `test_all_files_in_directory`, which takes a directory path as an argument.
For each .pgn file in the directory, it calls `test_pgn_all_moves`. This function generates all possible moves for each
point in the game. This is then compared to the output of the program when it is called with the TEST argument.
"""
import os
import tempfile

import chess.pgn

import subprocess


def run_command(pgn_file_path: str):
    command = ["swipl", "-t", "halt", "-f", "-q", "-O", "../../../src/main.pl", "--", pgn_file_path, "TEST"]
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


def test_pgn_all_moves(file: str):
    print(f"Testing {file}")
    temps = create_temp_files(file)

    t = ['a', 'b']
    for i, temp in enumerate(temps):
        print(f"move {i // 2 + 1}.{t[i % 2]}: {test_pgn(temp)}")

    # Clean up
    for temp in temps:
        os.remove(temp)


def test_all_files_in_directory(directory):
    for filename in os.listdir(directory)[50:]:
        if filename.endswith(".pgn"):  # Only process .pgn files
            file_path = os.path.join(directory, filename)
            test_pgn_all_moves(file_path)


if __name__ == '__main__':
    test_all_files_in_directory("../test_pgns")
