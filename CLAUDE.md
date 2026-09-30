# TidyTuesday (R)

Weekly #TidyTuesday analyses in R.

## Layout
- One folder per week: `YYYY/YYYY-MM-DD/` (date of the TidyTuesday release), containing the script (named after the dataset, e.g. `health.R`), its figure(s) as PNG, and a short `README.md` in French showing the figure.
- Root `README.md` holds an index table with one row per week: add a row for each new week.
- Scripts run from the project root (renv is activated by `.Rprofile`); write outputs with `here::here("YYYY", "YYYY-MM-DD", "<file>.png")`.

## Conventions
- Data loaded with `tidytuesdayR::tt_load(<year>, week = <n>)`.
- tidyverse + native pipe `|>`, 4-space indent, code comments in English, figure labels in French.
- Dependencies managed with renv: after adding a package, run `renv::snapshot()` and commit `renv.lock`.
- Small personal project: commit directly on `main`.
