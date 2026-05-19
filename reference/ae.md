# ae

ae modified from pharmaversesdtm

## Usage

``` r
ae
```

## Format

A data frame with 1191 rows and 36 variables:

- STUDYID:

  Study Identifier

- DOMAIN:

  Domain Abbreviation

- USUBJID:

  Unique Subject Identifier

- AESEQ:

  Sequence Number

- AESPID:

  Sponsor-Defined Identifier

- AETERM:

  Reported Term for the Adverse Event

- AELLT:

  Lowest Level Term

- AELLTCD:

  Lowest Level Term Code

- AEDECOD:

  Dictionary-Derived Term

- AEPTCD:

  Preferred Term Code

- AEHLT:

  High Level Term

- AEHLTCD:

  High Level Term Code

- AEHLGT:

  High Level Group Term

- AEHLGTCD:

  High Level Group Term Code

- AEBODSYS:

  Body System or Organ Class

- AEBDSYCD:

  Body System or Organ Class Code

- AESOC:

  Primary System Organ Class

- AESOCCD:

  Primary System Organ Class Code

- AESEV:

  Severity/Intensity

- AESER:

  Serious Event

- AEACN:

  Action Taken with Study Treatment

- AEREL:

  Causality

- AEOUT:

  Outcome of Adverse Event

- AESCAN:

  Involves Cancer

- AESCONG:

  Congenital Anomaly or Birth Defect

- AESDISAB:

  Persist or Signif Disability/Incapacity

- AESDTH:

  Results in Death

- AESHOSP:

  Requires or Prolongs Hospitalization

- AESLIFE:

  Is Life Threatening

- AESOD:

  Occurred with Overdose

- AEDTC:

  Date/Time of Collection

- AESTDTC:

  Start Date/Time of Adverse Event

- AEENDTC:

  End Date/Time of Adverse Event

- AESTDY:

  Study Day of Start of Adverse Event

- AEENDY:

  Study Day of End of Adverse Event

- AEENRTPT:

  End Relative to Reference Time Point

## Source

data from pharmaversesdtm.

## See also

`ae`
[`ds`](https://johnsonandjohnson.github.io/pharmaversesdtmjnj/reference/ds.md)
[`dv`](https://johnsonandjohnson.github.io/pharmaversesdtmjnj/reference/dv.md)
[`ho`](https://johnsonandjohnson.github.io/pharmaversesdtmjnj/reference/ho.md)
[`ie`](https://johnsonandjohnson.github.io/pharmaversesdtmjnj/reference/ie.md)
[`mh`](https://johnsonandjohnson.github.io/pharmaversesdtmjnj/reference/mh.md)
[`suppho`](https://johnsonandjohnson.github.io/pharmaversesdtmjnj/reference/suppho.md)
[`ts`](https://johnsonandjohnson.github.io/pharmaversesdtmjnj/reference/ts.md)

## Examples

``` r
head(data("ae"))
#> [1] "ae"
```
