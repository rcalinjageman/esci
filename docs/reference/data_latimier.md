# Sample data for estimate_mdiff_ind_contrast: Test score and times from Latimier et al (2019).

The researchers were interested in how different study approaches might
impact learning. Working in France, they created three independent
groups, each comprising 95 adults. Participants worked online through
seven learning modules about DNA. The Reread group worked through a
module then worked through it a second time, before going on to the next
module. The Quiz group worked through a module then had to complete a
quiz before going on to the next module. The Prequiz group had to work
through the quiz before seeing the presentation of a module, then went
on to the quiz and presentation of the next module. Participants
received feedback and a brief explanation after answering each question
in a quiz, and could take as long as they wished to work through each
module and quiz. Seven days later participants completed a final test.

## Usage

``` r
data_latimier
```

## Format

### `data_latimier` A data frame with 285 rows and 3 columns:

- Group:

  A factor indicating membership in the Reread, Quiz, or Prequiz group

- Test:

  % correct on the final test

- Time:

  Time taken on final test

## Source

<https://osf.io/23yad>

Latimier, A., Riegert, A., Peyre, H., Ly, S. T., Casati, R., & Ramus, F.
(2019). Does pre-testing promote better retention than post-testing?
*Npj Science of Learning*, 4(1), 1–7.
https://doi.org/10.1038/s41539-019-0053-1

## Details

This data set is suitable for estimating the magnitude of difference in
a contrast of independent groups.
