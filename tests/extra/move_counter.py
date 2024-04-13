import chess.pgn
import os


def analyze_moves(pgn_file, output_dir):
    # Load PGN file
    with open(pgn_file) as file:
        pgn = chess.pgn.read_game(file)

    # Initialize a chess board
    board = pgn.board()

    # Create output file name (replace .pgn with .txt)
    output_file = os.path.join(output_dir, os.path.basename(pgn_file).replace('.pgn', '.txt'))

    # Open the output file to write analysis
    with open(output_file, 'w') as output:
        for i, move in enumerate(pgn.mainline_moves()):
            # Get number of legal moves for White and Black

            white_moves = len(list(board.legal_moves))
            board.push(move)
            output.write(f"{white_moves} ")

        if i % 2 == 0:
            output.write("0")




    print(f"Analysis saved to {output_file}")


def process_pgn_files_in_directory(input_dir, output_dir):
    # List all files in the input directory
    pgn_files = [f for f in os.listdir(input_dir) if f.endswith('.pgn')]

    for pgn_file in pgn_files:
        pgn_file_path = os.path.join(input_dir, pgn_file)
        analyze_moves(pgn_file_path, output_dir)


# Usage
input_directory = 'test_pgns'
output_directory = 'test_move_counts'

process_pgn_files_in_directory(input_directory, output_directory)
