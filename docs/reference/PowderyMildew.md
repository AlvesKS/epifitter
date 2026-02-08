# Dataset powdery mildew disease progress curves

Dataset containing experimental data of disease progress curves of
powdery mildew under different irrigation systems and soil moisture
levels in organic tomato.

## Usage

``` r
data("PowderyMildew")
```

## Format

A data frame with 240 observations on the following 2 variables.

- `irrigation_type`:

  Irrigations Systems: MS = Micro Sprinkler

- `moisture`:

  Levels of soils moisture

- `block`:

  Experimental blocks

- `time`:

  a numeric vector containing the time points

- `sev`:

  a numeric vector containg disease severity data in proportinal scales

## References

Lage, D. A. C., Marouelli, W. A., and Café-Filho, A. C. 2019. Management
of powdery mildew and behaviour of late blight under different
irrigation configurations in organic tomato. Crop Protection.
125:104886.

## Examples

``` r
data(PowderyMildew)
## maybe str(PowderyMildew) ; plot(PowderyMildew) ...
```
