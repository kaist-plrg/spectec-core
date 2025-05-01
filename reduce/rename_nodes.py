import os
import sys
import pandas as pd
import re

def main():
    if len(sys.argv) != 3 or sys.argv[2] not in ['--dry-run', '--run']:
        print("Usage: python rename_script.py /path/to/folder --dry-run|--run")
        sys.exit(1)

    folder_path = sys.argv[1]
    mode = sys.argv[2]

    # Load the mapping CSV
    mapping_df = pd.read_csv('reduce/renumber_mappings.csv')

    # Build mapping dictionary {num1: num2}
    mapping = dict(zip(mapping_df.iloc[:, 0], mapping_df.iloc[:, 1]))

    # Loop over files in the folder
    for filename in os.listdir(folder_path):
        match = re.match(r'r_(\d+)_.*\.p4', filename)
        if match:
            num1 = int(match.group(1))
            if num1 in mapping:
                num2 = mapping[num1]
                new_filename = filename.replace(f'r_{num1}_', f'r2_{num2}_')
                old_path = os.path.join(folder_path, filename)
                new_path = os.path.join(folder_path, new_filename)

                if mode == '--dry-run':
                    print(f'[DRY RUN] Would rename: {filename} → {new_filename}')
                elif mode == '--run':
                    os.rename(old_path, new_path)
                    print(f'Renamed: {filename} → {new_filename}')
            else:
                print(f'Warning: {num1} not found in mapping, skipping {filename}')

if __name__ == "__main__":
    main()

