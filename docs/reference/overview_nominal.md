# Calculates descriptive statistics for a numerical variable

This function calculated basic descriptive statistics for a categorical/
nominal variable. Inputs can be summary data, vectors, or a data frame.

## Usage

``` r
overview_nominal(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  cases = NULL,
  outcome_variable_levels = NULL,
  outcome_variable_name = "My Outcome Variable",
  grouping_variable_name = "My Grouping Variable",
  conf_level = 0.95,
  count_NA = FALSE
)
```

## Arguments

- data:

  - for raw data, a data frame or tibble

- outcome_variable:

  - for raw data, either a vector containing factor data or the name of
    a data-frame column containing a factor

- grouping_variable:

  - for raw data, either NULL (default), or the vector of a factor or a
    data-frame column containing a factor

- cases:

  For summary data - A vector of 1 or more counts, integers\>0

- outcome_variable_levels:

  For summary data - An optional vector of group labels, same length as
  cases. If not passed, auto-generated.

- outcome_variable_name:

  Optional friendly name for the outcome variable. Defaults to 'My
  Outcome Variable'. Ignored if a data-frame is passed, this argument is
  ignored.

- grouping_variable_name:

  Optional friendly name for the grouping variable. Defaults to 'My
  Grouping Variable'. Ignored for summary data and for data frames –
  only used if vectors of data are passed.

- conf_level:

  The confidence level for the confidence interval. Given in decimal
  form. Defaults to 0.95.

- count_NA:

  Logical to count NAs (TRUE) in total N or not (FALSE)

## Value

Returns a table of descriptive statistics

- **overview_nominal**

  - *outcome_variable_name* -

  - *outcome_variable_level* -

  - *cases* -

  - *n* -

  - *P* -

  - *P_LL* -

  - *P_UL* -

  - *P_SE* -

  - *P_adjusted* -

  - *ta_LL* -

  - *ta_UL* -

## Examples

``` r
# example code
esci::overview_nominal(esci::data_latimier_3groups, "Group")
#>   outcome_variable_name outcome_variable_level cases   n         P      P_LL
#> 1                 Group                 Reread    95 285 0.3333333 0.2811977
#> 2                 Group                   Quiz    95 285 0.3333333 0.2811977
#> 3                 Group                Prequiz    95 285 0.3333333 0.2811977
#>        P_UL       P_SE P_adjusted     ta_LL     ta_UL
#> 1 0.3900826 0.02777728  0.3356401 0.2899506 0.3813297
#> 2 0.3900826 0.02777728  0.3356401 0.2899506 0.3813297
#> 3 0.3900826 0.02777728  0.3356401 0.2899506 0.3813297
```
