import chess.pgn
import os

# Define the input and output directories
input_dir = 'test_pgns'
output_dir = 'test_fens'

# Create the output directory if it doesn't exist
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

# Loop through all files in the input directory
for filename in os.listdir(input_dir):
    if filename.endswith('.pgn'):  # Ensure we're processing .pgn files
        input_filepath = os.path.join(input_dir, filename)
        output_filepath = os.path.join(output_dir, os.path.splitext(filename)[0] + '.fen')

        with open(input_filepath) as pgn_file:
            game = chess.pgn.read_game(pgn_file)  # Read the game from the .pgn file
            board = game.end().board()  # Navigate to the last position
            fen = board.fen().split(' ')[0]  # Get only the board part of the FEN string

            with open(output_filepath, 'w') as fen_file:
                fen_file.write(fen)  # Write the board part of the FEN string to the .fen file

        print(f'Processed {filename} -> {os.path.basename(output_filepath)}')

print('All PGN files have been processed.')
