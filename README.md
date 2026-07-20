# Pitch Stuff+ Model

This project builds an explainable Stuff+ style model for evaluating pitch quality from TrackMan-style pitch data.

The public version uses synthetic data. No private team files, player reports, TrackMan exports, or proprietary materials are included.

## Overview

Stuff+ is a way to grade the physical quality of a pitch independent of traditional box score results. This project scores each pitch using traits such as velocity, movement, spin, extension, and separation from the pitcher's fastball.

The model is intentionally transparent. Instead of hiding the logic in a black-box model, each pitch type has a small set of weighted traits that reflect what usually makes that pitch effective.

A score of 100 is league-average within pitch type. Scores above 100 are better, and scores below 100 are worse.

## Features

- Standardizes TrackMan-style pitch data
- Cleans pitch type labels
- Creates pitch traits and fastball separation variables
- Scores individual pitches on a Stuff+ scale
- Summarizes grades by pitcher and pitch type
- Builds arsenal-level pitcher grades
- Adds short development notes for each pitch
- Exports clean CSV outputs for review

## Repository Structure

```text
pitch-stuff-plus-model/
├── README.md
├── data/
│   └── sample_pitches.csv
├── R/
│   ├── helpers.R
│   ├── clean_pitches.R
│   ├── stuff_weights.R
│   ├── score_stuff.R
│   └── reports.R
├── analysis/
│   └── run_stuff_plus.R
└── outputs/
    ├── pitch_grades.csv
    ├── arsenal_grades.csv
    ├── pitch_leaderboard.csv
    └── validation_summary.csv
```

## Data

The included sample dataset is synthetic TrackMan-style pitch data. Each row represents one pitch and includes:

- pitcher and pitcher team
- pitch type
- velocity
- spin rate
- induced vertical break
- horizontal break
- extension
- release point
- plate location
- swing, whiff, called strike, and CSW flags
- batted-ball contact flags when available

The same workflow can be applied to real pitch data if the columns follow the expected TrackMan-style structure.

## Methodology

1. Standardize pitch data

The cleaning step handles inconsistent pitch type labels and converts key columns into usable numeric or logical fields.

2. Create pitch traits

Each pitch receives standardized trait values within its pitch type. Examples include:

- velocity
- induced vertical break
- horizontal movement
- spin rate
- extension
- velocity separation from fastball
- movement separation from fastball

Standardizing within pitch type prevents fastballs and breaking balls from being compared on the wrong scale.

3. Score each pitch

Each pitch type has its own weighted trait table. For example, four-seam fastballs place more weight on velocity and ride, while changeups place more weight on fastball separation and movement.

Raw scores are converted to a plus scale:

```text
Stuff+ = 100 + 10 * z-score
```

4. Summarize pitchers

The model creates pitch-level, pitch-type-level, and arsenal-level summaries. Arsenal grades are weighted by pitch usage.

## Example Usage

Run the full workflow from a fresh R session:

```r
source("analysis/run_stuff_plus.R")
```

The script writes outputs to the `outputs/` folder.

## Example Outputs

The project creates four main output files:

```text
outputs/pitch_grades.csv
outputs/arsenal_grades.csv
outputs/pitch_leaderboard.csv
outputs/validation_summary.csv
```

These outputs show each pitcher's best pitches, overall arsenal quality, and basic validation checks against CSW, whiff rate, and hard-hit rate allowed.

## Requirements

This project uses R and the following packages:

- dplyr
- tidyr
- stringr
- readr
- purrr

Install them with:

```r
install.packages(c("dplyr", "tidyr", "stringr", "readr", "purrr"))
```

## Limitations

This is a public portfolio version of a Stuff+ style model, not a proprietary professional model.

The current version does not fully account for:

- batter quality
- count and game context
- pitch location intent
- catcher target
- pitch sequencing
- park effects
- handedness matchups
- tunneling between pitches

Future versions could add non-linear modeling, handedness splits, pitch usage recommendations, and validation against future performance.

## Project Motivation

This project was built to show how raw pitch-level data can be turned into an interpretable player evaluation tool. It combines baseball domain knowledge, feature engineering, plus-scale scoring, and reproducible R workflows.
