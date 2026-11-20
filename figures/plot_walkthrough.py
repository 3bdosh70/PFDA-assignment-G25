"""Plot row counts and median scores using the published audio-feature bins."""
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

folder = Path(__file__).resolve().parent
data = pd.read_csv(folder.parent / "data/processed/spotify_tracks_clean.csv")
labels = ["0–.2", ".2–.4", ".4–.6", ".6–.8", ".8–1"]
def band(column):
    return pd.cut(data[column], [0, .2, .4, .6, .8, 1.01], labels=labels, right=False)

groups = data.groupby([band("acousticness"), band("danceability")], observed=False)["popularity"]
fig, axes = plt.subplots(1, 2, figsize=(11, 5), layout="constrained")
for ax, table, title, palette in zip(axes, [groups.median(), groups.size()],
        ["Median popularity score", "Track-genre rows per cell"], ["viridis", "Blues"]):
    matrix = table.unstack().to_numpy()
    img = ax.imshow(matrix, origin="lower", cmap=palette)
    ax.set(xticks=range(5), xticklabels=labels, yticks=range(5), yticklabels=labels,
           xlabel="Danceability", ylabel="Acousticness", title=title)
    for (y, x), value in np.ndenumerate(matrix):
        rgb = img.cmap(img.norm(value))[:3]
        light = np.dot(rgb, [.299, .587, .114])
        ax.text(x, y, f"{value:,.0f}", ha="center", va="center", color="black" if light > .55 else "white")
fig.suptitle("How popularity varies across audio profiles", fontsize=18, weight="bold")
fig.savefig(folder / "walkthrough.png", dpi=160)
print(groups.median().unstack().to_string())
print(groups.size().unstack().to_string())
