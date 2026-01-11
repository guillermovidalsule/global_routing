import matplotlib.pyplot as plt
import pandas as pd
import argparse
from matplotlib.collections import LineCollection

def visualize(edge_file, results_file):
    with open(edge_file, 'r') as f:
        content = f.read().split()
        size_rows = int(content[0]) 
        size_cols = int(content[1])
        start_row = int(content[2])
        start_col = int(content[3])

    end_row = start_row + size_rows - 1
    end_col = start_col + size_cols - 1

    fig, ax = plt.subplots(figsize=(max(size_cols, 8), max(size_rows, 8)))

    for y in range(start_col, end_col + 1):
        ax.axvline(y, color='lightgray', linestyle='-', linewidth=0.5, zorder=1)
    for x in range(start_row, end_row + 1):
        ax.axhline(x, color='lightgray', linestyle='-', linewidth=0.5, zorder=1)

    nodes_x = [] # Rows
    nodes_y = [] # Cols
    for r in range(start_row, end_row + 1):
        for c in range(start_col, end_col + 1):
            nodes_y.append(c) 
            nodes_x.append(r)
    
    ax.scatter(nodes_y, nodes_x, s=30, facecolors='none', edgecolors='blue', zorder=2, alpha=0.3)

    try:
        df = pd.read_csv(results_file)
        cmap = plt.get_cmap('tab20')
        net_segments = {}

        for _, row in df.iterrows():
            r1, c1 = int(row['X1']), int(row['Y1'])
            r2, c2 = int(row['X2']), int(row['Y2'])
            
            nets = str(row['Nets']).strip().split()
            for net_id_str in nets:
                net_id = int(net_id_str)
                if net_id not in net_segments:
                    net_segments[net_id] = []
                net_segments[net_id].append([(c1, r1), (c2, r2)])

        for net_id, segments in net_segments.items():
            lc = LineCollection(segments, colors=[cmap(net_id % 20)], 
                                linewidths=4, label=f'Net {net_id}', zorder=3)
            ax.add_collection(lc)

    except FileNotFoundError:
        print(f"Results file {results_file} not found.")

    ax.set_xticks(range(start_col, end_col + 1))
    ax.set_yticks(range(start_row, end_row + 1))
    
    ax.set_xlim(start_col - 0.5, end_col + 0.5)
    ax.set_ylim(start_row - 0.5, end_row + 0.5)
    
    ax.set_aspect('equal')
    
    plt.title(f"VLSI Router: Row(X) x Col(Y) = {size_rows}x{size_cols}")
    plt.xlabel("Horizontal Column Index (Ada Y)")
    plt.ylabel("Vertical Row Index (Ada X)")
    
    plt.show()

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('edge_file')
    parser.add_argument('results_file')
    args = parser.parse_args()
    visualize(args.edge_file, args.results_file)
