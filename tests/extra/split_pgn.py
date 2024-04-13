def split_pgn_file(input_file_path):
    with open(input_file_path, 'r') as file:
        content = file.readlines()

    game_lines = []
    game_count = 0

    for line in content:
        # Check if the line is the start of a new game
        if line.startswith('[Event ') and game_lines:
            # Write the current game to a file before starting a new one
            with open(f'test_pgns/fisher_game_{game_count}.pgn', 'w') as game_file:
                game_file.writelines(game_lines)
            game_lines = []  # Reset for the next game
            game_count += 1

        game_lines.append(line)

        # Check for the last game in the file
        if line.strip() in ['1-0', '0-1', '1/2-1/2', '*'] and game_count == len(content) - 1:
            with open(f'test_pgns/fisher_game_{game_count}.pgn', 'w') as game_file:
                game_file.writelines(game_lines)

    print(f'Successfully split into {game_count + 1} separate PGN files.')


# Replace 'your_pgn_file.pgn' with the path to your PGN file
split_pgn_file('test_pgns/Fischer.pgn')
