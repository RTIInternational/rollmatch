
## Resubmission
This is a resubmission. In this version I have:

* Adopted the CRAN template for the MIT license ad LICENSE file.
* Reduced the size of the CRAN package to < 5 MB (now at 1.8 MB).

This past submission, I received an install error during the incoming-pretest, which surprised me since it (1) doesn't fail locally or with winbuilder and (2) didn't occur during my first submission.  This really surprised me, given the relatively small changes I made to the codebase.

`ERROR` received:

> Error in library.dynam(lib, package, package.lib) : 
>  DLL 'dplyr' not found: maybe not installed for this architecture?

I'm going to run again to see if I can recreate the issue -- sorry if this causes you extra work!

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

I'm new to this, but I assume that's normal for a new package :)

## Downstream dependencies
I have run R CMD check on downstream dependencies of rollmatch (dplyr, magrittr, and testthat). All packages passed without `ERRORs` or `WARNINGs`.