#' @title ds
#'
#' @description ds modified from pharmaversesdtm
#' @source data from pharmaversesdtm.
#'
#' @format A data frame with 850 rows and 14 variables:
#' \describe{
#'  \item{STUDYID}{Study Identifier}
#'  \item{DOMAIN}{Domain Abbreviation}
#'  \item{USUBJID}{Unique Subject Identifier}
#'  \item{DSSEQ}{Sequence Number}
#'  \item{DSSPID}{Sponsor-Defined Identifier}
#'  \item{DSTERM}{Reported Term for the Disposition Event}
#'  \item{DSDECOD}{Standardized Disposition Term}
#'  \item{DSCAT}{Category for Disposition Event}
#'  \item{VISITNUM}{Visit Number}
#'  \item{VISIT}{Visit Name}
#'  \item{DSDTC}{Date/Time of Collection}
#'  \item{DSSTDTC}{Start Date/Time of Disposition Event}
#'  \item{DSSTDY}{Study Day of Start of Disposition Event}
#'  \item{DSSCAT}{Subcategory for Disposition Event}
#' }
#' @seealso \code{\link{ds}} \code{\link{mh}}
#' @keywords datasets ds
#' @name ds
#' @examples
#' head(data("ds"))
"ds"
