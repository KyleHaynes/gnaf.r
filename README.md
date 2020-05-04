# gnaf.r

An R package to assist with importing G-NAF into R.

## What is G-NAF?

G-NAF is Australia's most trusted authoritative (g)eocoded (n)ational (a)ddress (f)ile.

More from: https://psma.com.au/product/gnaf/

PSMA's G-NAF dataset contains all physical addresses in Australia. It's the most trusted source of geocoded addresses for Australian businesses and governments.

## Where to get G-NAF?
G-NAF is released on a quarterly basis and is available from here:
https://data.gov.au/dataset/ds-dga-19432f89-dc3a-4ef3-b943-5326ef1dbecc/details?q=G-NAF

## What is this repo?
A set of convenience functions to assist with importing G-NAF in to R.

## Dependencies required for these function
- R
- Download G-NAF
- Depending on which function used, RAM.

### Installation of the package

The package is not on CRAN.

```R
# Install `remotes` if it isn't installed
if(!any(installed.packages()[,1] == "remotes")) install.packages("remotes")

# Install gnaf.r
remotes::install_github("KyleHaynes/gnaf.r")
```

## Basic usage

1. Download G-NAF from data.gov.au: https://data.gov.au/dataset/ds-dga-19432f89-dc3a-4ef3-b943-5326ef1dbecc/details?q=G-NAF
    * File is 1.5GB compressed / 7.7GB uncompressed
2. Extract the context of the compressed download to a desired location.
3. Note down the location of extracted directory (and the sibling month/year folder). E.g. "C:/temp/G-NAF/G-NAF FEBRUARY 2020".
4. From R:

```R
library("gnaf.r")

setup("C:/temp/G-NAF/G-NAF FEBRUARY 2020")


```