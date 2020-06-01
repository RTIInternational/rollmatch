## Test environments
* local OS X install (10.12 "Sierra"), R 3.6.2
* ubuntu 14.04, on travis-ci (release & devel) 
* win-builder (devel)

## R CMD check results
There were no `ERRORs` when run locally on OS X or on win-builder

## Downstream dependencies
I have run R CMD check on downstream dependencies of rollmatch (dplyr, magrittr, and testthat). All packages passed without `ERRORs` or `WARNINGs`.
