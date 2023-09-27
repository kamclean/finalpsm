## Create the dataset

Firstly we should ensure all variables of interest are in the correct
formats we need. Variables should either be factors or numerical as
appropriate. We will be using the `survival::colon` dataset as the basis
of our example.

    library(tidyverse); library(finalfit); library(survival)

    data <- tibble::as_tibble(survival::colon) %>%
      
      dplyr::filter(etype==2) %>% # Outcome of interest is death
      dplyr::filter(rx!="Obs") %>%  # rx will be our binary treatment variable
      dplyr::select(-etype,-study, -status) %>% # Remove superfluous variables
      
      # Convert into numeric and factor variables
      dplyr::mutate_at(vars(obstruct, perfor, adhere, node4), function(x){factor(x, levels=c(0,1), labels = c("No", "Yes"))}) %>%
      dplyr::mutate(rx = factor(rx),
                    mort365 = cut(time, breaks = c(-Inf, 365, Inf), labels = c("Yes", "No")),
                    sex = factor(sex, levels=c(0,1), labels = c("Female", "Male")),
                    differ = factor(differ, levels = c(1,2,3), labels = c("Well", "Moderate", "Poor")),
                    adhere = as.character(adhere),
                    extent = factor(extent, levels = c(1,2,3, 4), labels = c("Submucosa", "Muscle", "Serosa", "Contiguous Structures")),
                    surg = factor(surg, levels = c(0,1), labels = c("Short", "Long"))) %>%
      
      # Logical value for outcome (for survival analysis)
      dplyr::mutate(status = mort365=="Yes")

## Explore missing data

Before considering imputation, we first need to determine if there is
missing data, and whether the data is missing at random (MAR) or missing
not at random data (MNAR). In general, imputation cannot be used when
data is MNAR (e.g. there is clear bias in what data is missing).

There are excellent outlines of how to explore and determine this using
the `finalfit` package already
<a href="https://finalfit.org/articles/missing.html" target="_blank">here</a>.
We will assume this has been done, and the data is MAR.

## Imputation

Firstly, we want to remove variables we want to ensure the multiple
imputation algorithm is based on the appropriate variables. You should
define:

-   `impute` (**Essential**): The variables you want to be imputed.
    There is significant debate whether outcomes should be imputed or
    not, however outcome is often imputed as best practice as shown
    below.

-   `fixed` (**Optional**): Any variables you want to be present in the
    dataset AND to be used in the imputation algorithm, BUT to remain
    unimputed.

-   `ignore` (**Optional**): Any variables you want to be present in the
    dataset, but to NOT include in the algorithm AND remain unimputed.

All other arguments from the `mice::mice()` function can be used to
customise imputation. For example, the number of imputations to be
performed (`m`).

    var_dep = "mort365"
    var_exp = c("rx", "sex", "age", "obstruct", "nodes",  "surg")
    var_fixed = c("perfor")
    var_ignore = c("adhere")

    impdata <- data %>%
      finalpsm::impute(impute = c(var_dep, var_exp), fixed = var_fixed, ignore = var_ignore, m=5)

A nested dataset is returned containing the original (unimputed) data,
with each imputed dataset nested (below). Each dataset has a unique
number corresponding to it (`m`) and the respective patient (`rowid`).
Use `tidyr::unnest()` to convert into a long format.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">imputed</th>
<th style="text-align: right;">m</th>
<th style="text-align: left;">data</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">original</td>
<td style="text-align: right;">0</td>
<td style="text-align: left;">&lt; tibble &gt;</td>
</tr>
<tr class="even">
<td style="text-align: left;">imputed</td>
<td style="text-align: right;">1</td>
<td style="text-align: left;">&lt; tibble &gt;</td>
</tr>
<tr class="odd">
<td style="text-align: left;">imputed</td>
<td style="text-align: right;">2</td>
<td style="text-align: left;">&lt; tibble &gt;</td>
</tr>
<tr class="even">
<td style="text-align: left;">imputed</td>
<td style="text-align: right;">3</td>
<td style="text-align: left;">&lt; tibble &gt;</td>
</tr>
<tr class="odd">
<td style="text-align: left;">imputed</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">&lt; tibble &gt;</td>
</tr>
<tr class="even">
<td style="text-align: left;">imputed</td>
<td style="text-align: right;">5</td>
<td style="text-align: left;">&lt; tibble &gt;</td>
</tr>
</tbody>
</table>

## Modelling using finalfit

The `finalaux::finalimp()` function works functionally the same as the
`finalfit::finalfit()` function. The dependent, explanatory, and
random\_effect variables can be defined, and the appropriate model is
chosen based on these.

    output <- impdata %>%
      finalpsm::imputemutate(nodes5 = ifelse(nodes>=5, "Yes", "No")) %>%
      finalpsm::finalimp(dependent = var_dep, explanatory = c(var_exp, "nodes5"))

There are 2 outputs:

1.  `$table` which provides a finalfit table ready for output which
    contains both the unimputed and imputed models.

<table>
<colgroup>
<col style="width: 3%" />
<col style="width: 9%" />
<col style="width: 10%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 26%" />
<col style="width: 25%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">label</th>
<th style="text-align: left;">levels</th>
<th style="text-align: left;">Yes</th>
<th style="text-align: left;">No</th>
<th style="text-align: left;">OR_multi_unimp</th>
<th style="text-align: left;">OR_multi_imp</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">7</td>
<td style="text-align: left;">rx</td>
<td style="text-align: left;">Lev</td>
<td style="text-align: left;">29 (53.7)</td>
<td style="text-align: left;">281 (50.2)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr class="even">
<td style="text-align: left;">8</td>
<td style="text-align: left;"></td>
<td style="text-align: left;">Lev+5FU</td>
<td style="text-align: left;">25 (46.3)</td>
<td style="text-align: left;">279 (49.8)</td>
<td style="text-align: left;">1.08 (0.60-1.95, p=0.805)</td>
<td style="text-align: left;">1.05 (0.59-1.88 p=0.870)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">9</td>
<td style="text-align: left;">sex</td>
<td style="text-align: left;">Female</td>
<td style="text-align: left;">26 (48.1)</td>
<td style="text-align: left;">270 (48.2)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr class="even">
<td style="text-align: left;">10</td>
<td style="text-align: left;"></td>
<td style="text-align: left;">Male</td>
<td style="text-align: left;">28 (51.9)</td>
<td style="text-align: left;">290 (51.8)</td>
<td style="text-align: left;">0.91 (0.50-1.64, p=0.757)</td>
<td style="text-align: left;">0.94 (0.52-1.68 p=0.831)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">age</td>
<td style="text-align: left;">Mean (SD)</td>
<td style="text-align: left;">62.5 (10.3)</td>
<td style="text-align: left;">59.7 (12.1)</td>
<td style="text-align: left;">0.97 (0.94-1.00, p=0.036)</td>
<td style="text-align: left;">0.97 (0.94-0.99 p=0.020)</td>
</tr>
<tr class="even">
<td style="text-align: left;">5</td>
<td style="text-align: left;">obstruct</td>
<td style="text-align: left;">No</td>
<td style="text-align: left;">38 (70.4)</td>
<td style="text-align: left;">459 (82.0)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr class="odd">
<td style="text-align: left;">6</td>
<td style="text-align: left;"></td>
<td style="text-align: left;">Yes</td>
<td style="text-align: left;">16 (29.6)</td>
<td style="text-align: left;">101 (18.0)</td>
<td style="text-align: left;">0.42 (0.22-0.82, p=0.010)</td>
<td style="text-align: left;">0.44 (0.23-0.84 p=0.014)</td>
</tr>
<tr class="even">
<td style="text-align: left;">2</td>
<td style="text-align: left;">nodes</td>
<td style="text-align: left;">Mean (SD)</td>
<td style="text-align: left;">4.8 (3.3)</td>
<td style="text-align: left;">3.5 (3.5)</td>
<td style="text-align: left;">1.03 (0.92-1.18, p=0.666)</td>
<td style="text-align: left;">1.02 (0.90-1.16 p=0.709)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">12</td>
<td style="text-align: left;">surg</td>
<td style="text-align: left;">Short</td>
<td style="text-align: left;">37 (68.5)</td>
<td style="text-align: left;">421 (75.2)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr class="even">
<td style="text-align: left;">11</td>
<td style="text-align: left;"></td>
<td style="text-align: left;">Long</td>
<td style="text-align: left;">17 (31.5)</td>
<td style="text-align: left;">139 (24.8)</td>
<td style="text-align: left;">0.69 (0.37-1.32, p=0.245)</td>
<td style="text-align: left;">0.72 (0.39-1.36 p=0.313)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">3</td>
<td style="text-align: left;">nodes5</td>
<td style="text-align: left;">No</td>
<td style="text-align: left;">27 (50.9)</td>
<td style="text-align: left;">422 (77.3)</td>
<td style="text-align: left;">-</td>
<td style="text-align: left;">-</td>
</tr>
<tr class="even">
<td style="text-align: left;">4</td>
<td style="text-align: left;"></td>
<td style="text-align: left;">Yes</td>
<td style="text-align: left;">26 (49.1)</td>
<td style="text-align: left;">124 (22.7)</td>
<td style="text-align: left;">0.22 (0.08-0.54, p=0.001)</td>
<td style="text-align: left;">0.22 (0.09-0.56 p=0.002)</td>
</tr>
</tbody>
</table>

1.  `$model` which provides all the underlying data, models, and metrics
    for the original and imputed datasets.

-   All datasets have an extra column added (`predict`) containing the
    predicted probability (0-1) of the outcome (based on the model).

<table>
<colgroup>
<col style="width: 5%" />
<col style="width: 1%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 83%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">imputed</th>
<th style="text-align: right;">m</th>
<th style="text-align: left;">data</th>
<th style="text-align: left;">model</th>
<th style="text-align: left;">metric</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">original</td>
<td style="text-align: right;">0</td>
<td style="text-align: left;">tibble</td>
<td style="text-align: left;">S3:glm</td>
<td style="text-align: left;">Number in dataframe = 614, Number in model
= 599, Missing = 15, AIC = 345.8, C-statistic = 0.688, H&amp;L =
Chi-sq(8) 8.53 (p=0.384)</td>
</tr>
<tr class="even">
<td style="text-align: left;">imputed</td>
<td style="text-align: right;">1</td>
<td style="text-align: left;">tibble</td>
<td style="text-align: left;">S3:glm</td>
<td style="text-align: left;">Number in dataframe = 614, Number in model
= 614, Missing = 0, AIC = 352.1, C-statistic = 0.692, H&amp;L =
Chi-sq(8) 7.05 (p=0.531)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">imputed</td>
<td style="text-align: right;">2</td>
<td style="text-align: left;">tibble</td>
<td style="text-align: left;">S3:glm</td>
<td style="text-align: left;">Number in dataframe = 614, Number in model
= 614, Missing = 0, AIC = 351.1, C-statistic = 0.694, H&amp;L =
Chi-sq(8) 9.65 (p=0.290)</td>
</tr>
<tr class="even">
<td style="text-align: left;">imputed</td>
<td style="text-align: right;">3</td>
<td style="text-align: left;">tibble</td>
<td style="text-align: left;">S3:glm</td>
<td style="text-align: left;">Number in dataframe = 614, Number in model
= 614, Missing = 0, AIC = 352.3, C-statistic = 0.691, H&amp;L =
Chi-sq(8) 9.81 (p=0.279)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">imputed</td>
<td style="text-align: right;">4</td>
<td style="text-align: left;">tibble</td>
<td style="text-align: left;">S3:glm</td>
<td style="text-align: left;">Number in dataframe = 614, Number in model
= 614, Missing = 0, AIC = 353.8, C-statistic = 0.688, H&amp;L =
Chi-sq(8) 6.71 (p=0.568)</td>
</tr>
<tr class="even">
<td style="text-align: left;">imputed</td>
<td style="text-align: right;">5</td>
<td style="text-align: left;">tibble</td>
<td style="text-align: left;">S3:glm</td>
<td style="text-align: left;">Number in dataframe = 614, Number in model
= 614, Missing = 0, AIC = 354.3, C-statistic = 0.687, H&amp;L =
Chi-sq(8) 6.63 (p=0.578)</td>
</tr>
</tbody>
</table>

## Modelling using non-finalfit packages

However, it may be you want to pool other models that are outwith the
scope of finalfit (or to not use finalfit at all!). This is made easier
in the `finalaux` package by how the imputed data has been structured.

1.  Firstly run your model across all the nested datasets (original and
    imputed) using purrr::map.

2.  Retain only your model fits from the output (a single model nested
    per row of the tibble).

3.  Pool the imputed models using `pool2df` to get a `finalfit::fit2df`
    style table for the unimputed and imputed models.

<!-- -->

    imp_match <- impdata  %>%
      
      # Run your model across every nested dataset ("data")
      dplyr::mutate(model = purrr::map(data, function(x){x %>%
          dplyr::select(-rowid) %>%
          finalpsm::match(strata = "rx",
                          explanatory = c("sex", "age", "obstruct", "nodes",  "surg"),
                          dependent = c("mort365", "time"),
                          method = "full") %>%
          finalpsm::finalpsm(dependent = "mort365", balance = F)})) %>%
      
      # Extract only your model fit from your output (multiple outputs from finalpsm)
      tidyr::unnest(model) %>% 
      dplyr::mutate(type = purrr::map_chr(model, function(x){paste0(class(x), collapse = ", ")})) %>%
      dplyr::filter(type == "glmerMod")

    # Pool Output
    finalpsm::pool2df(imp_match, model = "model")$joined %>% knitr::kable()

<table>
<thead>
<tr class="header">
<th style="text-align: left;">explanatory</th>
<th style="text-align: left;">OR_multi_unimp</th>
<th style="text-align: left;">OR_multi_imp</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">rxLev+5FU</td>
<td style="text-align: left;">0.95 (0.51-1.78, p=0.872)</td>
<td style="text-align: left;">0.90 (0.46-1.76 p=0.755)</td>
</tr>
<tr class="even">
<td style="text-align: left;">sexMale</td>
<td style="text-align: left;">0.78 (0.42-1.48, p=0.451)</td>
<td style="text-align: left;">0.76 (0.37-1.55 p=0.443)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">age</td>
<td style="text-align: left;">0.98 (0.95-1.00, p=0.090)</td>
<td style="text-align: left;">0.97 (0.94-1.01 p=0.148)</td>
</tr>
<tr class="even">
<td style="text-align: left;">obstructYes</td>
<td style="text-align: left;">0.35 (0.17-0.71, p=0.003)</td>
<td style="text-align: left;">0.40 (0.16-0.99 p=0.047)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nodes</td>
<td style="text-align: left;">0.86 (0.79-0.93, p&lt;0.001)</td>
<td style="text-align: left;">0.86 (0.79-0.94 p=0.001)</td>
</tr>
<tr class="even">
<td style="text-align: left;">surgLong</td>
<td style="text-align: left;">0.50 (0.26-0.98, p=0.045)</td>
<td style="text-align: left;">0.72 (0.35-1.47 p=0.361)</td>
</tr>
</tbody>
</table>
