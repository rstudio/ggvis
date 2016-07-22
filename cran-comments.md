## Test environments
* local OS X install, R 3.3.0
* ubuntu 12.04 (on travis-ci), R 3.3.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs,  WARNINGs or NOTEs. 

## Downstream dependencies
I have also run R CMD check on all 2 downstream dependencies of ggvis. (https://github.com/rstudio/ggvis/master/revdep/README.md). One package, mlr, had errors, which I believe are related to Java and not to ggvis. (On CRAN, the checks for mlr also have errors.)
