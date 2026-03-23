# Anchor Estimate ma - Ch9 - Many Labs replications of Jacowitz and Kahneman (1995)

To what extent does the wording of a question influence one's judgment?
In their classic study, Jacowitz and Kahneman (1995) asked participants
to estimate how many babies are born each day in the United States.
Participants were given either a low anchor (more than 100 babies/day)
or a high anchor (less than 50,000 babies/day). Those who saw the low
anchor estimated many fewer births/day than those who saw the high
anchor, which suggests that the wording can have a profound influence.
The correct answer, as it happens, is ~11,000 births/day in 2014. To
investigate the extent that these results are replicable, the Many Labs
project repeated this classic study at many different labs around the
world. You can find the summary data for 30 of these labs in the Anchor
Estimate ma data file

## Usage

``` r
data_anchor_estimate_ma
```

## Format

### `data_anchor_estimate_ma`

A data frame with 30 rows and 9 columns:

- Location:

  factor

- M Low:

  numeric

- s Low:

  numeric

- n Low:

  integer

- M High:

  numeric

- s High:

  numeric

- n High:

  integer

- USAorNot:

  factor

- Country:

  factor

## Source

[doi:10.1027/1864-9335/a000178](https://doi.org/10.1027/1864-9335/a000178)
