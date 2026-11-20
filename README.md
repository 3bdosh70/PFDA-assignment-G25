# Spotify song popularity analysis in R

A team project exploring which audio and track characteristics relate to Spotify popularity. The published dataset has **113,305 rows**, **89,553 unique track IDs**, and **114 genres**. Repeated track IDs mean rows are not all independent songs.

## At a glance

The team audited and cleaned the data, explored audio features and categories, and implemented prediction code in R. The published tables linked below support descriptive findings; this overview does not claim a verified prediction score. The chart shows one **descriptive** comparison, not evidence that explicit content causes a change in popularity.

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
| [Data audit](scripts/01_import_and_audit.R) and [preparation](scripts/02_data_preparation.R) | The first steps in the analysis. |
| [Exploration](scripts/03_general_exploration.R) and [prediction code](scripts/07_objective4_prediction.R) | Separate stages to read after the audit. |
| [outputs/tables/](outputs/tables) | Published descriptive summaries. |
| [data/raw/](data/raw) | Dataset and its description. |

## Run the published workflow

From the repository root, use R with `dplyr`, `ggplot2`, and `rpart` installed:

```bash
Rscript --vanilla Group25.R
```

The combined script expects `data/raw/spotify_tracks_data.csv` in this repository. It runs the full analysis and writes generated outputs locally, so use the linked scripts and published tables for a quicker review. `scripts/Main.R` covers the audit, preparation, and general exploration stages rather than the complete combined analysis.

## Reading the results responsibly

The chart above is a dataset summary, not a causal result or a verified prediction score. Some track IDs appear in more than one genre. This is shared team work; the repository history and files provide the detail behind the overview, and this README does not assign individual credit for each stage.
