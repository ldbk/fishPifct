
#' Build a PDF or a HTML report on a data validation against a format definition.
#'
#' @param obj Object to validate, see \code{\link{validateData}}.
#' @param formatDb Format data structure, see \code{\link{validateData}}.
#' @param reportFormat Format of the report, could be "html" (default) or "pdf".
#' @param reportFilePath File path of the output PDF or HTML document.
#' @param title Title of the report.
#' @param author Author of the report.
#'
#' @return File path of the created report.
#'
#' @export
#' @examples
#'
#' #
#' @author Norbert Billet - IRD
#' @importFrom rmarkdown render
renderValidationReport <- function(obj,
                                   formatDb,
                                   reportFormat="html",
                                   reportFilePath,
                                   title="Validation of a dataset against a format",
                                   author="John Doe",
                                   ...) {

  if (missing(obj)) {
    stop("Missing obj parameter.")
  }

  if (missing(formatDb)) {
    stop("Missing formatDb parameter.")
  }

  reportFormat <- tolower(reportFormat)
  if (! reportFormat %in% c("html", "pdf")) {
    stop("reportFormat must be \"html\" or \"pdf\".")
  }

  if (missing(reportFilePath)) {
    reportFilePath <- tempfile(pattern = paste0("dataValidationReport_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_"), fileext = paste0(".", reportFormat))
  }

  assign("currDate", format(Sys.time(), "%Y-%m-%d"))
  assign("title", title)
  assign("author", author)

  render(input = system.file("rmd", "validationReport.Rmd", package="SDEFQuality", mustWork = TRUE),
         output_file = reportFilePath,
         output_format=paste0(reportFormat, "_document"),
         quiet = TRUE,
         ...)
  message("Report generated [", reportFilePath, "]")
  return(reportFilePath)
}
