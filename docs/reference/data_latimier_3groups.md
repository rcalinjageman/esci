# Latimier 3Groups - Ch14 - 3 groups in Latimier et al. (2019)

The researchers were interested in how different study approaches might
impact learning. Working in France, they created three independent
groups, each comprising 95 adults. Participants worked online through
seven learning modules about DNA. The Reread group worked through a
module, then worked through it a second time before going on to the next
module. The Quiz group worked through a module, then had to complete a
quiz before going on to the next module. The Prequiz group had to work
through the quiz before seeing the presentation of a module, then went
on to the quiz and presentation of the next module. Participants
received feedback and a brief explanation after answering each question
in a quiz, and could take as long as they wished to work through each
module and quiz. Seven days later, participants completed a final test.
data_latimier_3groups is the full data set. To facilitate different
student exercises, there are also separate data entities for each group
(data_latimier_prequiz, data_latimier_reread, etc.), and for every
*pair* of groups (data_latimier_quiz_prequiz, etc.).

## Usage

``` r
data_latimier_3groups
```

## Format

### `data_latimier_3groups`

A data frame with 285 rows and 3 columns:

- Group:

  factor

- Test%:

  numeric

- Time:

  numeric

## Source

[doi:10.7910/DVN/XPYPMF](https://doi.org/10.7910/DVN/XPYPMF)
