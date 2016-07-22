# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.0 (2016-05-03) |
|system   |x86_64, darwin13.4.0         |
|ui       |X11                          |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |
|date     |2016-07-22                   |

## Packages

|package  |*  |version    |date       |source                           |
|:--------|:--|:----------|:----------|:--------------------------------|
|ggvis    |   |0.4.3      |2016-07-22 |local (@5b391f3)                 |
|testthat |*  |1.0.2.9000 |2016-07-22 |Github (hadley/testthat@46d15da) |

# Check results
1 packages with problems

## mlr (2.8)
Maintainer: Bernd Bischl <bernd_bischl@gmx.net>  
Bug reports: https://github.com/mlr-org/mlr/issues

2 errors | 0 warnings | 0 notes

```
checking examples ... ERROR
Running examples in ‘mlr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plotFilterValues
> ### Title: Plot filter values using ggplot2.
> ### Aliases: plotFilterValues
> 
> ### ** Examples
> 
> fv = generateFilterValuesData(iris.task, method = "chi.squared")
Error in .jnew("weka/core/Attribute", attname[i], .jcast(levels, "java/util/List")) : 
  java.lang.UnsupportedClassVersionError: weka/core/Attribute : Unsupported major.minor version 51.0
Calls: generateFilterValuesData ... read_model_frame_into_Weka -> read_data_into_Weka -> .jnew -> .External
Execution halted
** found \donttest examples: check also with --run-donttest

checking tests ... ERROR
Running the tests in ‘tests/run-base.R’ failed.
Last 13 lines of output:
  makeLearner("classif.__mlrmocklearners__5") did not throw an error.
  
  
  Timing stopped at: 0.021 0.002 0.015 
  Error in ._jobjRef_dollar(x[["jobj"]], name) : 
    no field, method or inner class called 'handled' 
  Calls: test_check ... <Anonymous> -> isTRUE -> $ -> $.Throwable -> ._jobjRef_dollar
  testthat results ================================================================
  OK: 67 SKIPPED: 0 FAILED: 1
  1. Failure: Learner (@test_base_Learner.R#26) 
  
  Error: testthat unit tests failed
  Execution halted
```

