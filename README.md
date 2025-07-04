# pharmaversesdtmjnj
<!-- start badges -->
[![Check 🛠](https://github.com/johnsonandjohnson/pharmaversesdtmjnj/actions/workflows/check.yaml/badge.svg)](https://github.com/johnsonandjohnson/pharmaversesdtmjnj/actions/workflows/check.yaml)
[![Docs 📚](https://github.com/johnsonandjohnson/pharmaversesdtmjnj/actions/workflows/pkgdown.yaml/badge.svg)](https://johnsonandjohnson.github.io/pharmaversesdtmjnj/)

![GitHub forks](https://img.shields.io/github/forks/johnsonandjohnson/pharmaversesdtmjnj?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/johnsonandjohnson/pharmaversesdtmjnj?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/johnsonandjohnson/pharmaversesdtmjnj)
![GitHub contributors](https://img.shields.io/github/contributors/johnsonandjohnson/pharmaversesdtmjnj)
![GitHub last commit](https://img.shields.io/github/last-commit/johnsonandjohnson/pharmaversesdtmjnj)
![GitHub pull requests](https://img.shields.io/github/issues-pr/johnsonandjohnson/pharmaversesdtmjnj)
![GitHub repo size](https://img.shields.io/github/repo-size/johnsonandjohnson/pharmaversesdtmjnj)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/johnsonandjohnson/pharmaversesdtmjnj/main?color=purple&label=package%20version)](https://github.com/johnsonandjohnson/pharmaversesdtmjnj/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/johnsonandjohnson/pharmaversesdtmjnj?color=red&label=open%20issues)](https://github.com/johnsonandjohnson/pharmaversesdtmjnj/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->


Generate SDTM datasets aligned with Johnson & Johnson's Clinical and Statistical Programming standards.


## Features

- Generates SDTM datasets that comply with J&J standards
- Built on top of the 'pharmaversesdtm' package
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
