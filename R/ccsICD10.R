#' ccsICD10 data
#'
#' Fuel economy data from the EPA, 1985-2015. This dataset contains
#' selected varaibles, and removes vehicles with incomplete data (e.g.
#' no drive train data)
#'
#' @format A data frame with variables:
#' \describe{
#' \item{ICD}{ICD-10 short format (without decimal)}
#' \item{CCSCate}{CCS category}
#' \item{ICDDesc}{ICD-10 name}
#' \item{CCSDesc}{CCS name}
#' \item{CSSL1}{CCS L1 category}
#' \item{CSSL1Desc}{CCS L1 name}
#' \item{CSSL2}{CCS L2 category}
#' \item{CSSL2Desc}{CCS L2 name}
#' \item{ICDD}{ICD-10 long format (with decimal)}
#' }
#'
#' @source \url{https://www.hcup-us.ahrq.gov/toolssoftware/ccs10/ccs10.jsp}
#'
"ccsICD10"
