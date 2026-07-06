# Compute pAUC with False ID Rate Cutoff

Compute pAUC with False ID Rate Cutoff

## Usage

``` r
.compute_pauc_with_cutoff(roc_data, max_false_id_rate)
```

## Arguments

- roc_data:

  Dataframe with roc data (from make_rocdata)

- max_false_id_rate:

  Maximum false ID rate cutoff

## Value

Numeric pAUC value
