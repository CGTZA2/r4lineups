# mockdata

A dataframe containing real lineup data.

3 columns x 94 observations.

The first column simply provides the participant number, whilst the
second and third contain data for each participant's choice (i.e., the
position of the lineup member that they identified) and a confidence
score for each choice.

In the field of eyewitness testimony, it is common for participants to
rate how confident they are that they have accurately identified the
perpetrator. Here, confidence is recorded as a percentage.

This lineup has the following parameters:

- 94 mock witness choices

- Target position is 7

- Nominal size is 9

## Usage

``` r
data(mockdata)
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 94
rows and 3 columns.
