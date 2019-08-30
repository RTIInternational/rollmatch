rollmatch <img src="man/figures/200px-Rti-logo.png" align="right" />
========================================================

![travis-ci build status](https://travis-ci.org/RTIInternational/rollmatch.svg?branch=master) ![CRAN statusbadge](https://www.r-pkg.org/badges/version/rollmatch) [![DOI](https://zenodo.org/badge/105259002.svg)](https://zenodo.org/badge/latestdoi/105259002)

### Rolling Entry Matching R Package

`rollmatch` is an R package designed to match study participants that start interventions at different times (rolling entry) with an appropriate comparison for the purpose of impact evaluation. Rolling entry presents several challenges for comparison group selection:

* A small number of entrants in a period can hinder propensity score models
* Precipitating events prior to entry may be hard to pin down, but generate changes in dynamic variables (e.g., utilization, spending)

This method, called Rolling Entry Matching, assigns potential comparison non-participants multiple counterfactual entry periods which allows for matching of participant and non-participants based on data immediately preceding each participant's specific entry period, rather than using data from a fixed pre-intervention period.

#### References
* For more details on Rolling Entry Matching, please reference [Witman et al. 2018](https://onlinelibrary.wiley.com/doi/abs/10.1111/1475-6773.13086).
* For more details on the `rollmatch` R package, please reference [Jones et al. 2019](https://journal.r-project.org/archive/2019/RJ-2019-005/index.html).

### Video

[![Watch the Intro Video](https://img.youtube.com/vi/_U1bDrL_f-M/0.jpg)](https://www.youtube.com/watch?v=_U1bDrL_f-M)

### Installation

```r
# Install from CRAN:
install.packages("rollmatch")

# Or try out the development version on GitHub:
# install.packages("devtools")
devtools::install_github("RTIInternational/rollmatch")
```

### Usage

```r
# Load rollmatch
library(rollmatch)

# Load sample dataset
data(package="rollmatch", "rem_synthdata_small")

# Reduce the input dataset size, for matching 
reduced_data <- reduce_data(data = rem_synthdata_small, treat = "treat",
                            tm = "quarter", entry = "entry_q",
                            id = "indiv_id", lookback = 1)

# Choose confounding variables hypothesized to be associated with both treatment and outcome
fm <- as.formula(treat ~ qtr_pmt + yr_pmt + age)
vars <- all.vars(fm)

# Calculate propensity socres for the reduced data
scored_data <- score_data(reduced_data = reduced_data,
                          model_type = "logistic", match_on = "logit",
                          fm = fm, treat = "treat",
                          tm = "quarter", entry = "entry_q", id = "indiv_id")

# Run the rolling entry matching algorithm
output <- rollmatch(scored_data, data=rem_synthdata_small, treat = "treat",
                    tm = "quarter", entry = "entry_q", id = "indiv_id",
                    vars = vars, lookback = 1, alpha = .2,
                    standard_deviation = "average", num_matches = 3,
                    replacement = TRUE)

# View output matches and diagnostics
output
```


