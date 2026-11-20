"""Rebuild the two README charts derived from the published cleaned CSV.

Run from the repository root: python figures/build_readme_charts.py
This presentation script does not run or replace the project's R analysis.
"""

import os
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "data" / "processed" / "spotify_tracks_clean.csv"
OUT = ROOT / "figures"
BLUE = "#446a91"
GREEN = "#1d9d76"
INK = "#172b43"
MUTED = "#5c6d7e"


def save(fig: plt.Figure, name: str) -> None:
    fig.savefig(OUT / name, format="svg", bbox_inches="tight", facecolor="white")
    if preview_dir := os.environ.get("README_CHART_PREVIEW_DIR"):
        preview = Path(preview_dir)
        preview.mkdir(parents=True, exist_ok=True)
        fig.savefig(preview / name.replace(".svg", ".png"), dpi=150, bbox_inches="tight", facecolor="white")
    plt.close(fig)


def popularity_distribution(data: pd.DataFrame) -> None:
    scores = data["popularity"].dropna()
    fig, ax = plt.subplots(figsize=(9, 4.1))
    fig.subplots_adjust(left=0.09, right=0.98, top=0.79, bottom=0.18)
    bins = np.arange(-0.5, 105.5, 5)
    ax.hist(scores, bins=bins, color=BLUE, edgecolor="white", linewidth=0.5)
    median = scores.median()
    ax.axvline(median, color=GREEN, linewidth=2.2, label=f"Median: {median:.0f}")
    ax.set_xlim(-1, 101)
    ax.set_xticks(np.arange(0, 101, 10))
    ax.set_xlabel("Popularity score (0–100)", color=INK)
    ax.set_ylabel("Track-genre rows", color=INK)
    ax.grid(axis="y", color="#e7edf3")
    ax.set_axisbelow(True)
    ax.spines[["top", "right", "left"]].set_visible(False)
    ax.tick_params(length=0, colors=MUTED)
    ax.legend(loc="upper right", frameon=False, labelcolor=GREEN)
    zero_share = (scores == 0).mean() * 100
    fig.text(0.09, 0.94, "Popularity is widely spread", fontsize=18, weight="bold", color=INK)
    fig.text(
        0.09,
        0.875,
        f"{len(scores):,} cleaned rows · {zero_share:.1f}% have score 0 · middle 50%: 17–50",
        fontsize=11,
        color=MUTED,
    )
    save(fig, "popularity-distribution.svg")


def genre_median_examples(data: pd.DataFrame) -> None:
    summary = (
        data.groupby("track_genre", as_index=False)["popularity"]
        .agg(rows="size", median="median", mean="mean")
        .sort_values(["median", "mean", "track_genre"])
    )
    # Show examples from both tails, while the README states the selection rule.
    chosen = pd.concat([summary.head(5), summary.tail(5)]).sort_values(
        ["median", "mean", "track_genre"]
    )
    fig, ax = plt.subplots(figsize=(9, 5.25))
    fig.subplots_adjust(left=0.22, right=0.93, top=0.82, bottom=0.12)
    ypos = np.arange(len(chosen))
    colors = [BLUE] * 5 + [GREEN] * 5
    ax.barh(ypos, chosen["median"], color=colors, height=0.72)
    ax.set_yticks(ypos, chosen["track_genre"])
    ax.invert_yaxis()
    ax.set_xlim(0, 78)
    ax.set_xticks(np.arange(0, 71, 10))
    ax.set_xlabel("Median popularity score", color=INK)
    ax.grid(axis="x", color="#e7edf3")
    ax.set_axisbelow(True)
    ax.spines[["top", "right", "left"]].set_visible(False)
    ax.tick_params(length=0, colors=MUTED)
    for i, row in enumerate(chosen.itertuples()):
        ax.text(row.median + 1.2, i, f"{row.median:.0f}  ·  n={row.rows:,}", va="center", color=INK, fontsize=9)
    fig.text(0.10, 0.95, "Genre labels show different score profiles", fontsize=18, weight="bold", color=INK)
    fig.text(
        0.10,
        0.89,
        "Five lowest and five highest median genre rows from the 114 labels",
        fontsize=11,
        color=MUTED,
    )
    save(fig, "genre-median-examples.svg")


def main() -> None:
    data = pd.read_csv(DATA)
    required = {"popularity", "track_genre", "track_id"}
    if missing := required - set(data):
        raise ValueError(f"Missing columns: {sorted(missing)}")
    if len(data) != 113_305 or data["track_id"].nunique() != 89_553:
        raise ValueError("Published cleaned data changed; recheck README figures before use")
    popularity_distribution(data)
    genre_median_examples(data)


if __name__ == "__main__":
    main()
