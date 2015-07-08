# setwd("/home/norbert/Boulot/DCF/Projets/RDB-SC/")
# source("generate_classes.R")
# generateClasses("CS - Exchange format - draft.xlsx")


generateClasses <- function(defFilePath) {
  if(! require(xlsx)) {
    stop("You have to install the \"xlsx\" library")
  }
  
  if(! require(formatR)) {
    stop("You have to install the \"formatR\" library")
  }
  
  SE.def <- read.xlsx(defFilePath, sheetName = "SE", stringsAsFactors=FALSE)
  TR.def <- read.xlsx(defFilePath, sheetName = "TR", stringsAsFactors=FALSE)
  HH.def <- read.xlsx(defFilePath, sheetName = "HH", stringsAsFactors=FALSE)
  SL.def <- read.xlsx(defFilePath, sheetName = "SL", stringsAsFactors=FALSE)
  HL.def <- read.xlsx(defFilePath, sheetName = "HL", stringsAsFactors=FALSE)
  CA.def <- read.xlsx(defFilePath, sheetName = "CA", stringsAsFactors=FALSE)
  
  if (! is.null(SE.def)) SE.def$table <- "SE"
  if (! is.null(TR.def)) TR.def$table <- "TR"
  if (! is.null(HH.def)) HH.def$table <- "HH"
  if (! is.null(SL.def)) SL.def$table <- "SL"
  if (! is.null(HL.def)) HL.def$table <- "HL"
  if (! is.null(CA.def)) CA.def$table <- "CA"
  
  fields_def <- rbind(SE.def, TR.def, HH.def, SL.def, HL.def, CA.def)
  
  names(fields_def) <- tolower(names(fields_def))
  
  tables <- unique(fields_def$table)
  
  buildSlotsList <- function(x) {
    paste0("list(", paste0(x, "=\"data.frame\"", collapse = ", "), ")")
  }
  
  buildDataFrameDef <- function(x) {
    res <- paste0("data.frame(\n")
    
    for (currFieldInd in 1:nrow(x)) {
      res <- paste0(res, x$r.object.name[currFieldInd], "=NA_character_")
      if (currFieldInd < nrow(x)) {
        res <- paste0(res, ", ")
      }
      res <- paste0(res, "\n")
    }
    
    res <- paste0(res, ")")
    
    return(res)
  }
  
  buildPrototypesList <- function(x) {
    res <- "list("
    tables <- unique(x$table)
    for (currTableInd in 1:length(tables)) {
      res <- paste0(res, tables[currTableInd], "=", buildDataFrameDef(x[x$table==tables[currTableInd],]))
      
      if (currTableInd != length(tables)) {
        res <- paste0(res, ", ")
      }
      res <- paste0(res, "\n")
    }
    res <- paste0(res, ")")
    return(res)
  }
  
  res <- paste0("SetClass(\nClass=\"CS\",\n",
                "slots=", buildSlotsList(tables), ",\n",
                "prototype=", buildPrototypesList(fields_def),
                ")")
  
  
  file <- paste0("csPi_data_structure_schema_", format(Sys.time(), "%Y%m%d"), ".R")
  
  writeLines(res, file)
  #tidy_source(text=res, file=file)
  
  
  
}
