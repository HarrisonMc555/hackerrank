#!/usr/bin/python3
"""Scramble squares solution"""

from collections import Counter

def left(tile):
    """left"""
    return tile[3]

def right(tile):
    """right"""
    return tile[1]

def top(tile):
    """top"""
    return tile[0]

def bottom(tile):
    """bottom"""
    return tile[2]

def is_valid_grid(grid):
    """is_valid_grid"""
    for i, row in enumerate(grid):
        for j, tile in enumerate(row):
            if j + 1 < len(row) and right(tile) != left(row[j+1]):
                return False
            if i + 1 < len(grid) and j < len(grid[i+1]) and \
               bottom(tile) != top(grid[i+1][j]):
                return False
    return True


def add_to_grid(grid, tile, ncols):
    """add_to_grid"""
    for row in grid:
        if len(row) < ncols:
            row.append(tile)
            return grid
    grid.append([tile])
    return grid

def grid_pop(grid):
    """grid_pop"""
    last_row = grid[-1]
    del last_row[-1]
    if not last_row:
        del grid[-1]

def tile_strings(tile):
    """tile_strings"""
    row1 = '\\ {} /'.format(top(tile))
    row2 = '{} X {}'.format(left(tile), right(tile))
    row3 = '/ {} \\'.format(bottom(tile))
    return [row1, row2, row3]

def grid_string(grid):
    """grid_string"""
    lines = []
    for row in grid:
        tiles_rows = [tile_strings(tile) for tile in row]
        for j in range(3):
            lines.append(' '.join(tile_rows[j] for tile_rows in tiles_rows))
            # s += ' '.join(tile_rows[j] for tile_rows in tiles_rows) + '\n'
    return '\n'.join(lines)

def find_valid_grid(grid, rem_tiles_counters, ncols):
    """find_valid_grid"""
    if not is_valid_grid(grid):
        return None
    if all(count == 0 for count in rem_tiles_counters.values()):
        return grid
    for tile in [t for t in rem_tiles_counters if rem_tiles_counters[t] > 0]:
        add_to_grid(grid, tile, ncols)
        if is_valid_grid(grid):
            rem_tiles_counters[tile] -= 1
            next_grid = find_valid_grid(grid, rem_tiles_counters, ncols)
            if next_grid is not None:
                return next_grid
            rem_tiles_counters[tile] += 1
        else:
            pass
        grid_pop(grid)
    return None

def solve():
    """solve"""
    ncols, nrows = [int(s) for s in input().split()]
    ntotal = nrows * ncols
    tiles_list = [input().split() for _ in range(ntotal)]
    tiles_counters = Counter(tuple(tile) for tile in tiles_list)
    grid = find_valid_grid([], tiles_counters, ncols)
    if grid is None:
        print('No valid grid found')
    else:
        print(grid_string(grid))


def main():
    """main"""
    solve()

if __name__ == '__main__':
    main()
