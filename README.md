# SMR-Training

This repository contains the code used to train LIST analysts in R. While written to be contextually relevant to work commonly undertaken in LIST, it may also be relevant to other analysts who use SMR data.


### RStudio Projects

This code uses [RStudio Projects](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects), which are a way of bundling together related files and scripts. RStudio Projects come with a .RProj file, and wherever this file is saved is 
where RStudio sets the working directory, from which other filepaths can be defined relatively using the [here](https://github.com/r-lib/here) package.

Type `getwd()` into the RStudio console to get the working directory for this project.


### SPSS Equivalent Functions ###

Below is an approximate and non-exhaustive list of equivalent functions in R and SPSS which are commonly used in analysis of SMR data. The majority of the R functions below come from the [dplyr](https://github.com/tidyverse/dplyr), [tidyr](https://github.com/tidyverse/tidyr) and [magrittr](https://github.com/tidyverse/magrittr) packages, part of the [tidyverse](https://github.com/tidyverse) suite of packages.

R | SPSS
---|---
`arrange(x)` | `SORT CASES BY X (A)`
`arrange(desc(x))` | `SORT CASES BY X (D)`
`first(x)` | `FIRST(X)`
`last(x)` | `LAST(X)`
`substr(x, 1, 1)` | `SUBSTR(X, 1, 1)`
`filter(x == 2)` | `SELECT IF X = 2`
`filter(x != 2)` | `SELECT IF NOT (X = 2)`
`select(x)` |  `/KEEP X`
`select(-x)` |  `/DROP X`
`mutate(x = 2)` | `COMPUTE X = 2`
`drop_na(x)` | `SELECT IF NOT (SYSMIS(X))`
`left_join(x, y, by = "common_variable")` | `MATCH FILES FILE = X`<br>&nbsp;&nbsp;&nbsp;`/TABLE = Y`<br>&nbsp;&nbsp;&nbsp;`/BY COMMON_VARIABLE`
`data %<>%`<br>&nbsp;&nbsp;&nbsp;`group_by(x) %>%`<br>&nbsp;&nbsp;&nbsp;`summarise(y = sum(y)) %>%`<br>&nbsp;&nbsp;&nbsp;`ungroup()` | `AGGREGATE OUTFILE = *`<br>&nbsp;&nbsp;&nbsp;`/BREAK X`<br>&nbsp;&nbsp;&nbsp;`/Y = SUM(Y)`