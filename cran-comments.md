
## Resubmission
This is a resubmission. In this version I have:

* Adopted the CRAN template for the MIT license ad LICENSE file.
* Reduced the size of the CRAN package to < 5 MB (now at 1.8 MB).
* Added a reference in the "Description" field of the DESCRIPTION file.
* Added small executable examples to the internal helper functions.
	* Although we added examples, these functions are not directly accessble to users of the package and are not exported.   

## Test environments
* local OS X install (10.12 "Sierra"), R 3.3.3
* ubuntu 14.04, on travis-ci (release & devel) 
* win-builder (devel)

## R CMD check results
There were no `ERRORs`, `WARNINGs` or `NOTEs` when run locally on OS X.

When checking with win-builder using `devtools::build_win()`, there was 1 `NOTE`:

> * checking CRAN incoming feasibility ... NOTE
>   Maintainer: 'Rob Chew <rchew@rti.org>'
> 
>   New submission

Also, the author last names in the "description" field of the DESCRIPTION file are flagged as potential mis-spelled words.

## Downstream dependencies
I have run R CMD check on downstream dependencies of rollmatch (dplyr, magrittr, and testthat). All packages passed without `ERRORs` or `WARNINGs`.