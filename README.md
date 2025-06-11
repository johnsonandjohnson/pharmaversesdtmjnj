# pharmaversesdtmjnj

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

Generate SDTM datasets aligned with Johnson & Johnson's Clinical and Statistical Programming standards.


## Features

- Generates SDTM datasets that comply with J&J standards
- Built on top of the `pharmaversesdtm` package
- Implements data conversion from pharmaverse format to J&J standards format
- Provides reproducible and consistent test data

## Implemented Datasets

Currently supports the following SDTM domains:
- MH (Medical History)
- DS (Disposition)


## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("johnsonandjohnson/pharmaversesdtmjnj")
```

## Usage

```r
library(pharmaversesdtmjnj)

# Access the SDTM datasets directly
# These datasets are loaded lazily when the package is loaded
head(ds)
head(mh)
```

## Data Sources
Test datasets have been sourced from the [pharmaversesdtm](https://github.com/pharmaverse/pharmaversesdtm) package, which utilized the data from the [CDISC pilot project](https://github.com/cdisc-org/sdtm-adam-pilot-project).
