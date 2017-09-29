rollmatch <img src="man/figures/200px-Rti-logo.png" align="right" />
========================================================

### Rolling Entry Matching R Package

rollmatch is an R package designed to match study participants that start interventions at different times (rolling entry) with an appropriate comparison for the purpose of impact evaluation. Rolling entry presents several challenges for comparison group selection:

* A small number of entrants in a period can hinder propensity score models
* Precipitating events prior to entry may be hard to pin down, but generate changes in dynamic variables (e.g., utilization, spending)

This method, called Rolling Entry Matching, assigns potential comparison non-participants multiple counterfactual entry periods which allows for matching of participant and non-participants based on data immediately preceding each participant's specific entry period, rather than using data from a fixed pre-intervention period.

### Installation

```r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("RTIInternational/rollmatch")
```

### Usage

```r
# Load sample dataset
data(package="rollmatch", "rem_synthdata_small")

# Choose confounding variables hypothesized to be associated with both treatment and outcome
formula <- as.formula(treat ~ qtr_pmt + yr_pmt + age)

# Run rollmatch
r_match <- rollmatch(formula, data = rem_synthdata_small, tm = "quarter",
                       entry = "entry_q", id = "indiv_id", caliper = 0.2)

# View output matches and diagnostics
r_match
```


