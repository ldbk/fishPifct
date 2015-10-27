readDefinition <- function(defFilePath, xlsLibrary="readxl") {
  
  if (! xlsLibrary %in% c("xlsx", "readxl")) {
    stop("You have to choose a valid excel to R library: \"xlsx\" or \"readxl\"")
  }
  
  if (xlsLibrary=="xlsx") {
    if(! require(xlsx)) {
      stop("You have to install the \"xlsx\" library")
    }  
    SE.def <- read.xlsx(defFilePath, sheetName = "SE", stringsAsFactors=FALSE)
    TR.def <- read.xlsx(defFilePath, sheetName = "TR", stringsAsFactors=FALSE)
    HH.def <- read.xlsx(defFilePath, sheetName = "HH", stringsAsFactors=FALSE)
    SL.def <- read.xlsx(defFilePath, sheetName = "SL", stringsAsFactors=FALSE)
    HL.def <- read.xlsx(defFilePath, sheetName = "HL", stringsAsFactors=FALSE)
    CA.def <- read.xlsx(defFilePath, sheetName = "CA", stringsAsFactors=FALSE)
  }
  
  if (xlsLibrary=="readxl") {
    if(! require(readxl)) {
      stop("You have to install the \"readxl\" library")
    }  
    SE.def <- read_excel(defFilePath, sheet = "SE")
    TR.def <- read_excel(defFilePath, sheet = "TR")
    HH.def <- read_excel(defFilePath, sheet = "HH")
    SL.def <- read_excel(defFilePath, sheet = "SL")
    HL.def <- read_excel(defFilePath, sheet = "HL")
    CA.def <- read_excel(defFilePath, sheet = "CA")
  }
  
  colsToKeep <- c("deleted", "key", "name", "r.object.name", "r-object name", "type")
  
  SE.def <- SE.def[, tolower(names(SE.def)) %in% colsToKeep]
  TR.def <- TR.def[, tolower(names(TR.def)) %in% colsToKeep]
  HH.def <- HH.def[, tolower(names(HH.def)) %in% colsToKeep]
  SL.def <- SL.def[, tolower(names(SL.def)) %in% colsToKeep]
  HL.def <- HL.def[, tolower(names(HL.def)) %in% colsToKeep]
  CA.def <- CA.def[, tolower(names(CA.def)) %in% colsToKeep]
  
  if (! is.null(SE.def)) SE.def$table <- "se"
  if (! is.null(TR.def)) TR.def$table <- "tr"
  if (! is.null(HH.def)) HH.def$table <- "hh"
  if (! is.null(SL.def)) SL.def$table <- "sl"
  if (! is.null(HL.def)) HL.def$table <- "hl"
  if (! is.null(CA.def)) CA.def$table <- "ca"
  
  fields_def <- rbind(SE.def, TR.def, HH.def, SL.def, HL.def, CA.def)
  
  names(fields_def) <- tolower(names(fields_def))
  names(fields_def)[which(names(fields_def) == "r-object name")] <- "r.object.name"
  
  fields_def$deleted[is.na(fields_def$deleted)] <- "N"
  fields_def$deleted <- tolower(fields_def$deleted)
  fields_def <- fields_def[tolower(fields_def$deleted) != "y",]
  
  fields_def$key[is.na(fields_def$key)] <- "N"
  fields_def$key <- tolower(fields_def$key)
  
  fields_def$type <- tolower(fields_def$type)
  fields_def$r.type <- NA
  fields_def$r.type[fields_def$type %in% c("string", "sting", "character")] <- "character"
  fields_def$r.type[fields_def$type %in% c("int", "integer")] <- "integer"
  fields_def$r.type[fields_def$type %in% c("real") | substr(fields_def$type, 1, 3) == "dec"] <- "double"
  
  return(fields_def)
}


buildSlotsList <- function(dfNames, additionalSlots) {
  paste0("list(\n", paste0("\t\t", names(additionalSlots), "=\"", additionalSlots, "\"", collapse=",\n"), ",\n", paste0("\t\t", dfNames, "=\"data.frame\"", collapse = ",\n"), "\n\t)")
}

buildDataFrameDef <- function(dfDef) {
  res <- paste0("data.frame(\n")
  
  for (currFieldInd in 1:nrow(dfDef)) {
    res <- paste0(res, "\t\t\t", dfDef$r.object.name[currFieldInd], "=", 
                  switch (dfDef$r.type[currFieldInd],
                          "character"="character()",
                          "integer"="integer()",
                          "double"="double()",
                          # default case:
                          "character()"
                  )
    )
    if (currFieldInd < nrow(dfDef)) {
      res <- paste0(res, ", ")
    }
    res <- paste0(res, "\n")
  }
  
  res <- paste0(res, "\t\t)")
  
  return(res)
}

buildPrototypesList <- function(def, additionalSlotsPrototype=NULL) {
  res <- "list(\n"
  if (length(additionalSlotsPrototype) > 0) {
    res <- paste0(res, paste0("\t\t", names(additionalSlotsPrototype), "=\"", additionalSlotsPrototype, "\"", collapse=",\n"), ",\n")
  }
  tables <- unique(def$table)
  for (currTableInd in 1:length(tables)) {
    currTable <- tables[currTableInd]
    res <- paste0(res, "\t\t", currTable, "=", buildDataFrameDef(dfDef=subset(def, table==currTable)))
    
    if (currTableInd != length(tables)) {
      res <- paste0(res, ", ")
    }
    res <- paste0(res, "\n")
  }
  res <- paste0(res, "\t)")
  return(res)
}

buildPkFct <- function(fields_def) {
  res <- "piPk <- function(table){\n"
  for (currTable in unique(fields_def$table)) {
    res <- paste0(res, "\tif (table==\"", currTable, "\") return(c(", paste0("\"", fields_def$r.object.name[fields_def$table==currTable & fields_def$key=="y"], "\"", collapse=", "), "))\n")
  }
  res <- paste0(res, "}")
  return(res)
}


#' Find the fishPi class from the excel definition file.
#'
#' @param defFilePath path of the excel definition.
#' @param className name of the resulting class, better to set to "fishPi".
#' @param classVersion version code of the generate class, something like "2.1", ...
#' @param additionalSlots additional slots to include not present on the definion excel file. Sea example bellow.
#' @param additionalSlotsPrototype prototype for additional slots.
#' @param outputFilePath path of the generated class definition file.
#' @param eval should the class definition to be evaluated at the end of the generation.
#' @param xlsLibrary R XLS library to use, could be readxl (default) or xlsx.
#'
#' @return file path of the generated file.
#'
#' @examples
#' \dontrun{
#' setwd("/home/norbert/Boulot/DCF/Projets/RDB-SC/")
#' source("generate_classes.R")
#' generateClasses(defFilePath="CS - Exchange format - 2.1.xlsx", 
#'                 className="csPi", 
#'                 classVersion="2.1", 
#'                 additionalSlots=list(desc="character", popData="character", design="character",history="character"), 
#'                 additionalSlotsPrototype=list(desc="Commercial Sampling Data format for the fishPi project", popData="Named population data object", design="Design description",history="modification history"), 
#'                 outputFilePath="csPi_class.R",
#'                 xlsLibrary="readxl",
#'                 eval=TRUE)
#' 
#' testCsPi <- new(Class="csPi")
#' testCsPi
#' }
#' @export
#' @author Laurent Dubroca & Norbert Billet
#' @importFrom readxl read_excel
generateClasses <- function(defFilePath, 
                            className, 
                            classVersion, 
                            additionalSlots=NULL, 
                            additionalSlotsPrototype=NULL, 
                            outputFilePath,
                            eval=FALSE,
                            xlsLibrary="readxl") {
  if (missing(defFilePath)) {
    error("You must provide file path of the definition spreadsheet")
  }
  
  if (missing(className)) {
    error("You must provide a class name")
  }
  
  if (missing(classVersion)) {
    error("You must provide a class version")
  }
  
  if (missing(outputFilePath)) {
    outputFilePath <- paste0(className, "_class_", format(Sys.time(), "%Y%m%d"), ".R")
  }
  
  additionalSlots <- c(list(classVersion="character"), additionalSlots)
  additionalSlotsPrototype <- c(list(classVersion=classVersion), additionalSlotsPrototype)
  
  fields_def <- readDefinition(defFilePath, xlsLibrary)
  
  tables <- unique(fields_def$table)
  
  res <- paste0("###\n# generated on ", Sys.time(), "\n###\n\n")
  res <- paste0(res, "setClass(\n\tClass=\"", className, "\",\n",
                "\tslots=", buildSlotsList(tables, additionalSlots=additionalSlots), ",\n",
                "\tprototype=", buildPrototypesList(fields_def, additionalSlotsPrototype),
                "\n)")
  res <- paste0(res, "\n\n", buildPkFct(fields_def))
  
  writeLines(res, outputFilePath)
  
  if (eval) {
    source(outputFilePath)
  }
  
  return(outputFilePath)
}

# setwd("/home/norbert/Boulot/DCF/Projets/RDB-SC/")
# source("generate_classes.R")
# generateClasses(defFilePath="CS - Exchange format - 2.1.xlsx", 
#                 className="csPi", 
#                 classVersion="2.1", 
#                 additionalSlots=list(desc="character", popData="character", design="character"), 
#                 additionalSlotsPrototype=list(desc="Commercial Sampling Data format for the fishPi project", popData="Named population data object", design="Design description"), 
#                 outputFilePath="csPi_class.R",
#                 xlsLibrary="readxl",
#                 eval=TRUE)
# 
# testCsPi <- new(Class="csPi")
# testCsPi
