# Spotify song popularity analysis in R

A team project exploring which audio and track characteristics relate to Spotify popularity. The published dataset has **113,305 rows**, **89,553 unique track IDs**, and **114 genres**. Repeated track IDs mean rows are not all independent songs.

## At a glance

The team audited and cleaned the data, explored audio features and categories, and built prediction workflows in R. This chart shows one **descriptive** comparison from the published output tables. It does not show that explicit content causes a change in popularity.

![Horizontal bar chart: mean popularity is 33.0 for non-explicit rows and 36.5 for explicit rows](figures/popularity-by-explicit.svg)

*Mean popularity by explicit flag in the published dataset: 33.0 across 103,630 non-explicit rows and 36.5 across 9,675 explicit rows. Source: [published summary table](outputs/tables/popularity_by_explicit.csv).*

```mermaid
flowchart LR
    Data[Spotify track rows] --> Audit[Audit and clean]
    Audit --> Explore[Explore audio features and categories]
    Explore --> Model[Build prediction models]
    Model --> Review[Compare outputs and limitations]
```

## Find the work

| Start here | What it contains |
| --- | --- |
| [Group25.R](Group25.R) | The combined workflow for the published version. |
| [scripts/](scripts) | Separate analysis stages and supporting code. |
| [outputs/tables/](outputs/tables) | Published descriptive summaries. |
| [data/raw/](data/raw) | Dataset and its description. |

## Run the published workflow

From the repository root, use R with `dplyr`, `ggplot2`, and `rpart` installed:

```bash
Rscript --vanilla Group25.R
```

The combined script expects `data/raw/spotify_tracks_data.csv` in this repository. It writes generated outputs locally. The separate scripts are useful for reading the stages; `scripts/Main.R` covers the audit, preparation, and general exploration stages rather than the complete combined analysis.

## Reading the results responsibly

The chart above is a dataset summary, not a causal result or a verified prediction score. Some track IDs appear in more than one genre. This README describes the **currently published code and tables**; newer local work has not yet been merged into this group repository. The project belongs to the team, and individual contributions should not be inferred from this overview.
