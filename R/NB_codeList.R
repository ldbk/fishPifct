
#' Read code lists definitions from a xlsx file.
#'
#' @param inputFilePath character: Path of the xlsx format definition file.
#' @param formatNomenclatureName logical: Should the names of the nomenclature to be formated ? (default: FALSE)
#' @param upperCaseCodes logical: Should the codes of the nomenclature to be formated in upper case ? (default: TRUE)
#'
#' @return List of nomenclatures.
#'
#' @examples
#' \dontrun{
#' }
#' @export
#' @author Norbert Billet
#' @importFrom readxl excel_sheets read_excel
readCodeListFromXls <- function(inputFilePath, formatNomenclatureName=FALSE, upperCaseCodes=TRUE) {
  if (missing(inputFilePath)) {
    stop("Missing inputFilePath.")
  }

  # check if file exist
  if(! file.exists(inputFilePath)) {
    stop("Specified inputFilePath not exists.")
  }

  # check for "readxl" package avaibility
  if(! require(readxl)) {
    stop("You have to install the \"readxl\" library.")
  }

  xlSheets <- excel_sheets(path=inputFilePath)

  codeListDb <- list()

  for (currSheet in xlSheets) {
    # must we reformat the name of the noenclature (i.e. the name of the xls sheet) ?
    if (formatNomenclatureName) {
      # yes, we remove existing ":", replace "space" by "_" and "-" as namespace separator "#"
      currNomenclatureName <- gsub(":", "", currSheet, fixed=TRUE)
      currNomenclatureName <- gsub(" ", "_", currNomenclatureName, fixed=TRUE)
      currNomenclatureName <- gsub("-", "#", currNomenclatureName, fixed=TRUE)
      currNomenclatureName <- toupper(currNomenclatureName)
    } else {
      # no, we keep as is
      currNomenclatureName <- currSheet
    }

    # we check if there already was an nomenclature with the same name, if yes => we stop
    if (currNomenclatureName %in% names(codeListDb)) {
      stop("A nomenclature with the same name \"", currNomenclatureName, "\" already exists.")
    }

    # we read the sheet
    currDf <- NULL
    try(expr=currDf <- as.data.frame(read_excel(path=inputFilePath, sheet=currSheet)), silent=TRUE)
    if (is.null(currDf)) {
      stop("Something was wrong with the \"", currSheet, "\" sheet.")
    }

    # we check the columns
    names(currDf) <- toupper(names(currDf))

    # we expect at least a "CODE" column
    codeInd <- which(names(currDf)=="CODE")
    if (length(codeInd) != 1) {
      stop("There is no \"CODE\" column in the \"", currSheet, "\" sheet.")
    }

    # if there is a "DESCRIPTION" column
    descInd <- which(names(currDf)=="DESCRIPTION")
    if (length(descInd) != 1) {
      # no description, we put the CODE as description
      descInd <- codeInd
    }
    currDf <- currDf[, c(codeInd, descInd)]
    names(currDf) <- c("CODE", "DESCRIPTION")

    # remove leading and trailing whitespace
    currDf$CODE <- gsub("^\\s+|\\s+$", "", currDf$CODE)

    # upper case codes
    if(upperCaseCodes) {
      currDf$CODE <- toupper(currDf$CODE)
    }

    # check if codes are unique
    if (any(duplicated(currDf$CODE))) {
      stop("Codes are not unique in the \"", currSheet, "\" sheet.")
    }

    # insert CODE where there is no DESCRIPTION
    currDf$DESCRIPTION[is.na(currDf$DESCRIPTION)] <- currDf$CODE[is.na(currDf$DESCRIPTION)]

    # store in the code lists list

    codeListDb[[currNomenclatureName]] <- currDf
  }

  return(codeListDb)
}
