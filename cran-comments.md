## Test environments
* local OS X install (10.12 "Sierra"), R 3.3.3
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results
There were no `ERRORs`, `WARNINGs` or `NOTEs` when run locally on OS X.

When checking with win-builder through `devtools::build_win()`, there was 1 `NOTE`:

> * checking CRAN incoming feasibility ... NOTE
>   Maintainer: 'Rob Chew <rchew@rti.org>'
> 
>   New submission

I'm new to this, but I assume that's normal for a new package :)

## Downstream dependencies
I have run R CMD check on downstream dependencies of rollmatch (dplyr, magrittr, and testthat). All packages passed without `ERRORs` or `WARNINGs`.