# Synthetic Control Methods for Estimating the Effect of Landscape Fire Smoke on Mortality

This repository contains the analysis code for a study estimating the causal effect of acute landscape fire smoke (LFS) PM2.5 exposure on mortality using synthetic control methods. Health data are drawn from the [Multi-Country Multi-City Collaborative Research Network](https://mccstudy.lshtm.ac.uk) (MCC) database, and wildfire-derived air pollution data are from [Xu et al. (2023)](https://www.nature.com/articles/s41586-023-06398-6). The analysis compares six synthetic control estimators under two data-generating processes via simulation, and applies the best-performing methods to an empirical case study episode.

---

## Repository structure

| Path | Contents |
|---|---|
| `_targets.R` | Full analysis pipeline — start here |
| `functions.R` | Sources all functions in `R/` into the pipeline environment |
| `R/` | Functions called by the pipeline (documented with roxygen2-style headers) |
| `archive/R/` | Alternative methods explored but not used in the final pipeline |
| `data/` | Input data — not tracked by git (see [Data](#data) below) |
| `Output/` | Generated figures and tables (written by the pipeline) |
| `renv/` | Package lockfile for reproducibility |
| `run.sh` | Example jobscript for running the pipeline on an HPC cluster |

---

## Methods overview

Six synthetic control estimators are compared:

- **ADH** — Abadie, Diamond & Hainmueller (2010) constrained regression weights
- **ADH subset** — ADH applied to the L2-nearest subset of the donor pool
- **DIFP** — Doudchenko & Imbens (2016) elastic net with intercept
- **PSC** — Abadie & L'Hour (2021) penalised synthetic control
- **DID** — Difference-in-differences using the nearest control unit
- **1-NN matching** — Single nearest-neighbour matching on pre-treatment outcomes

Estimator performance is assessed via a simulation study under two data-generating processes: a **negative binomial GLM** (fitted city-by-city with a natural spline time trend and temperature) and a **low-rank factor model**. Treatment assignment is evaluated under both empirical and uniform-random assignment mechanisms, with and without outcome de-meaning.

Uncertainty in the empirical case study is quantified using **conformal inference** (block permutation, L1 norm), constructing period-specific confidence intervals by inverting a null hypothesis grid.

---

## Data

Both input datasets should be placed in a `data/` folder within the project root before running the pipeline. This folder is not tracked by git.

- **MCC mortality data** — Daily city-level counts of all-cause, cardiovascular, respiratory, and non-external mortality, with mean temperature and city metadata. Access is restricted to MCC network collaborators; see the [MCC network website](https://mccstudy.lshtm.ac.uk) for information on data access.

- **LFS pollution data** — Daily city-level predicted fire and non-fire PM2.5 concentrations matched to MCC cities, from [Xu et al. (2023)](https://www.nature.com/articles/s41586-023-06398-6).

---

## Outputs

Running the pipeline writes all outputs to the `Output/` directory:

- `Output/Figures/Simulation/` — Density plots of normalised treatment effect estimates, line plots comparing real vs. simulated outcome trajectories, scatter plots of bias vs. mean outcome level, and pre- vs. post-treatment RMSPE plots.
- `Output/Tables/Simulation/` — LaTeX tables summarising ATT bias and standard deviation by method and DGP.
- `Output/Figures/Main/` — Empirical case study treatment effect estimates with conformal inference confidence intervals.

---

## Reproducing the analysis

**Requirements:** R 4.5.1 (the version used to develop this analysis; see `renv/renv.lock` for the full package manifest).

1. Clone the repository to your local machine.
2. Place the input datasets in a `data/` folder within the project root (see [Data](#data) above).
3. Open the R project and install `renv` if not already available:
   ```r
   install.packages("renv")
   ```
4. Restore the package environment from the lockfile (this installs all required packages at the correct versions):
   ```r
   renv::restore()
   ```
5. Run the pipeline:
   ```r
   targets::tar_make()
   ```

The pipeline will execute all data preparation, simulation, and estimation steps and write outputs to `Output/`. Note that the simulation study (500 replicates × 6 estimators × 4 DGP/assignment combinations) is computationally intensive. It is advisable to run this on an HPC cluster — an example SGE jobscript is provided in `run.sh`.

To inspect the pipeline dependency graph without running it:
```r
targets::tar_visnetwork()
```

---

## Citation

Citation will be added upon publication.

---

## Contact

For questions about the analysis or issues running the code, please open a [GitHub issue](https://github.com/calum-kennedy-98/MCC_Synth/issues) or contact [calum.kennedy.25@ucl.ac.uk](mailto:calum.kennedy.25@ucl.ac.uk).
