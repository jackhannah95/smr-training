# SMR Training

This repository contains the code used to train [LIST](https://www.isdscotland.org/Health%2DTopics/Health%2Dand%2DSocial%2DCommunity%2DCare/Local%2DIntelligence%2DSupport%2DTeam/) analysts in R. While written to be contextually relevant to work commonly undertaken in LIST, it may also be relevant to other analysts who use [SMR](https://www.ndc.scot.nhs.uk/Data%2DDictionary/SMR%2DDatasets/) data.

Please note that this GitHub repository contains the master copy of this training material. Any local copies which exist on the network will not be maintained. 


### Instructions for Running

To download this repository, click the green 'Clone or download' button and then click 'Download ZIP'. Unzip the folder in a location on the network which is accessible via the [RStudio server](http://spsssrv02.csa.scot.nhs.uk:8787/).

To open the project in the RStudio server, click File -> Open Project -> navigate to the folder where the project is saved -> open the `SMR-Training.Rproj` file.


### RStudio Projects

This code uses [RStudio Projects](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects), which are a way of bundling together related files and scripts. RStudio Projects come with a `.Rproj` file, and wherever this file is saved is 
where RStudio sets the working directory, from which other filepaths can be defined relatively using the [here](https://github.com/r-lib/here) package. A new project which follows the recommended structure within PHI can be created using the [phiproject](https://github.com/Health-SocialCare-Scotland/phiproject) package.

Type `getwd()` into the RStudio console to get the working directory for this project.


### SPSS Equivalent Functions ###

The below table contains an approximate and non-exhaustive list of equivalent functions in R and SPSS which are commonly used in analysis of SMR data. The R functions come from the [dplyr](https://github.com/tidyverse/dplyr), [tidyr](https://github.com/tidyverse/tidyr) and [magrittr](https://github.com/tidyverse/magrittr) packages, part of the [tidyverse](https://github.com/tidyverse) collection of packages.

Please note that, where not explicitly stated, it is assumed in the R code listed in the below table that the data have first been piped (`%>%` or `%<>%`) to the function, for example:

- `new_df <- old_df %>%`<br>&nbsp;&nbsp;&nbsp;`arrange(x) %>%`<br>&nbsp;&nbsp;&nbsp;`filter(x = first(x))`

- `df %<>%`<br>&nbsp;&nbsp;&nbsp;`select(x, y) %>%`<br>&nbsp;&nbsp;&nbsp;`mutate(z = x + y)`

R | SPSS
---|---
`arrange(x)` | `SORT CASES BY X (A)`
`arrange(desc(x))` | `SORT CASES BY X (D)`
`first(x)` | `FIRST(X)`
`last(x)` | `LAST(X)`
`filter(x == 2)` | `SELECT IF X = 2`
`filter(x != 2)` | `SELECT IF NOT (X = 2)`
`select(x)` |  `/KEEP X`
`select(-x)` |  `/DROP X`
`mutate(x = 2)` | `COMPUTE X = 2`
`drop_na(x)` | `SELECT IF NOT (SYSMIS(X))`
`df %<>%`<br>&nbsp;&nbsp;&nbsp;`left_join(lookup, by = "common_variable")` | `MATCH FILES`<br>&nbsp;&nbsp;&nbsp;`/FILE = *`<br>&nbsp;&nbsp;&nbsp;`/TABLE = "/PATH/TO/LOOKUP"`<br>&nbsp;&nbsp;&nbsp;`/BY COMMON_VARIABLE`
`df %<>%`<br>&nbsp;&nbsp;&nbsp;`group_by(x) %>%`<br>&nbsp;&nbsp;&nbsp;`summarise(y = sum(y)) %>%`<br>&nbsp;&nbsp;&nbsp;`ungroup()` | `AGGREGATE OUTFILE = *`<br>&nbsp;&nbsp;&nbsp;`/BREAK X`<br>&nbsp;&nbsp;&nbsp;`/Y = SUM(Y)`
