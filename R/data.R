#' Food and Drug Administration CYP Inducers and Inhibitors Data
#'
#' Data obtained from the FDA's table of CYP inducers and inhibitors.
#' Reference data contains information on 6 PGx enzymes:
#' CYP1A2, CYP2D6, CYP2C19, CYP2C9, CYP3A4, and CYP3A5
#'
#' Substances may be strong/moderate/weak inducers/inhibitors for a given enzyme.
#'
#' File created 09/02/2024
#' File updated 13/02/2024
#'
#'
#' @format ## `ref`
#' A data frame with 119 rows and 7 columns:
#' \describe{
#'   \item{substance}{Substance}
#'   \item{CYP1A2, CYP2C19, CYP2C9,  CYP2D6, CYP3A4, CYP3A5}{Effect of substance on each enzyme}
#'   ...
#' }
#' @source <https://www.fda.gov/drugs/drug-interactions-labeling/healthcare-professionals-fdas-examples-drugs-interact-cyp-enzymes-and-transporter-systems>
"ref"
