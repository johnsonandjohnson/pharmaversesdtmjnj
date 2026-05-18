#' @title ie
#'
#' @description ie modified from pharmaversesdtm
#' @source data from pharmaversesdtm.
#'
#' @format A data frame with 60 rows and 12 variables:
#' \describe{
#'  \item{STUDYID}{Study Identifier}
#'  \item{USUBJID}{Unique Subject Identifier}
#'  \item{VISIT}{Visit Name}
#'  \item{VISITNUM}{Visit Number}
#'  \item{DOMAIN}{Domain Abbreviation}
#'  \item{IESEQ}{Sequence Number}
#'  \item{IECAT}{Inclusion/Exclusion Category}
#'  \item{IETEST}{Inclusion/Exclusion Criterion Test Name}
#'  \item{IETESTCD}{Inclusion/Exclusion Criterion Short Name}
#'  \item{IEORRES}{Result or Finding in Original Units}
#'  \item{IESTRESC}{Character Result/Finding in Standard Format}
#'  \item{IEDTC}{Date/Time of Collection}
#' }
#' @seealso \code{\link{ae}} \code{\link{ds}} \code{\link{dv}} \code{\link{ho}} \code{\link{ie}} \code{\link{mh}} \code{\link{suppho}} \code{\link{ts}}
#' @keywords datasets ie
#' @name ie
#' @examples
#' head(data("ie"))
"ie"

