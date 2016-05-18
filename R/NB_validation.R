
buildEmptyFormatDb <- function() {
  formatDb <- list()
  # add the metadata table
  formatDb[["format_infos"]] <- data.frame(format_name=character(), format_version=character(), stringsAsFactors=FALSE)
  # add the slots table
  formatDb[["slots"]] <- data.frame(slot_name=character(), mandatory=logical(), definition_table=character(), stringsAsFactors=FALSE)
  # add the "text" types definition table
  formatDb[["text_types"]] <- data.frame(type_name=character(), stringsAsFactors=FALSE)
  # add the "numeric" types definition table
  formatDb[["numeric_types"]] <- data.frame(type_name=character(), is_integer=logical(), min=numeric(), max=numeric(), stringsAsFactors=FALSE)
  # add the "date" types definition table
  formatDb[["date_types"]] <- data.frame(type_name=character(), stringsAsFactors=FALSE)
  # add the "date" types definition table
  formatDb[["codelist_types"]] <- data.frame(type_name=character(), enumeration_table=character(), stringsAsFactors=FALSE)
  # add the "date" types definition table
  formatDb[["logical_types"]] <- data.frame(type_name=character(), stringsAsFactors=FALSE)

  # add the "base" slot: ie for the variables in the root of the object
  formatDb[["slots"]] <- rbind(formatDb[["slots"]],
                               data.frame(slot_name="base", mandatory=TRUE, definition_table="slot_base", stringsAsFactors=FALSE))
  formatDb[["slot_base"]] <- data.frame(column_name=character(), nullable=logical(), mandatory=logical(), pk=logical(), type_name=character(), category=character(), stringsAsFactors=FALSE)

  return(formatDb)
}

writeFormatDbToXls <- function(formatDb, outputFilePath=tempfile(fileext=".xlsx")) {

  if (missing(formatDb)) {
    stop("Missing formatDb.")
  }

  # check for "xlsx" package avaibility
  if(! require(xlsx)) {
    stop("You have to install the \"xlsx\" library")
  }

  # xlsx wb
  wb <- createWorkbook()

  ## metadata
  metadataSheet <- createSheet(wb=wb, sheetName="format_infos")
  addDataFrame(x=formatDb[["format_infos"]], sheet=metadataSheet, row.names=FALSE)

  ## slots list
  slotsSheet <- createSheet(wb=wb, sheetName="slots")
  addDataFrame(x=formatDb[["slots"]], sheet=slotsSheet, row.names=FALSE)

  ## text types
  textTypesSheet <- createSheet(wb=wb, sheetName="text_types")
  addDataFrame(x=formatDb[["text_types"]], sheet=textTypesSheet, row.names=FALSE)

  ## numeric types
  numericTypesSheet <- createSheet(wb=wb, sheetName="numeric_types")
  addDataFrame(x=formatDb[["numeric_types"]], sheet=numericTypesSheet, row.names=FALSE)

  ## date types
  dateTypesSheet <- createSheet(wb=wb, sheetName="date_types")
  addDataFrame(x=formatDb[["date_types"]], sheet=dateTypesSheet, row.names=FALSE)

  ## codelist types
  codelistTypesSheet <- createSheet(wb=wb, sheetName="codelist_types")
  addDataFrame(x=formatDb[["codelist_types"]], sheet=codelistTypesSheet, row.names=FALSE)

  ## logical types
  logicalTypesSheet <- createSheet(wb=wb, sheetName="logical_types")
  addDataFrame(x=formatDb[["logical_types"]], sheet=logicalTypesSheet, row.names=FALSE)

  ## loop loop for slots sheets
  for (currSlotName in formatDb[["slots"]]$slot_name) {
    currDefinitionTableName <- paste0("slot_", currSlotName)
    currSheet <- createSheet(wb=wb, sheetName=currDefinitionTableName)
    addDataFrame(x=formatDb[[currDefinitionTableName]], sheet=currSheet, row.names=FALSE)
  }

  ## loop for codelists sheets
  for (currCodeListRef in formatDb[["codelist_types"]]$enumeration_table) {
    currSheet <- createSheet(wb=wb, sheetName=currCodeListRef)
    addDataFrame(x=formatDb[[currCodeListRef]], sheet=currSheet, row.names=FALSE)
  }

  ## write file
  saveWorkbook(wb, outputFilePath)

  return(outputFilePath)
}

#' Read a format definition from a xlsx def file.
#'
#' @param inputFilePath character: Path of the xlsx format definition file.
#'
#' @return Format definition structure.
#'
#' @examples
#' \dontrun{
#' }
#' @export
#' @author Norbert Billet
#' @importFrom readxl excel_sheets read_excel
readFormatDbFromXls <- function(inputFilePath) {
  if (missing(inputFilePath)) {
    stop("Missing inputFilePath")
  }

  # check for "readxl" package avaibility
  if(! require(readxl)) {
    stop("You have to install the \"readxl\" library")
  }

  xlSheets <- excel_sheets(path=inputFilePath)

  formatDb <- list()

  # read the metadata table
  if (! "format_infos" %in% xlSheets) {
    stop("Missing \"format_infos\" sheet in the format definition file.")
  }
  try(expr=formatDb[["format_infos"]] <- as.data.frame(read_excel(path=inputFilePath, sheet="format_infos")), silent=TRUE)
  if (! "format_infos" %in% names(formatDb)) {
    formatDb[["format_infos"]] <- data.frame(format_name=character(), format_version=character(), stringsAsFactors=FALSE)
  }

  # read the slots table
  if (! "slots" %in% xlSheets) {
    stop("Missing \"slots\" sheet in the format definition file.")
  }
  try(expr=formatDb[["slots"]] <- as.data.frame(read_excel(path=inputFilePath, sheet="slots")), silent=TRUE)
  if (! "slots" %in% names(formatDb)) {
    formatDb[["slots"]] <- data.frame(slot_name=character(), mandatory=logical(), definition_table=character(), stringsAsFactors=FALSE)
  }
  formatDb[["slots"]]$mandatory <- as.logical(formatDb[["slots"]]$mandatory)

  # read the "text" types definition table
  if (! "text_types" %in% xlSheets) {
    stop("Missing \"text_types\" sheet in the format definition file.")
  }
  try(expr=formatDb[["text_types"]] <- as.data.frame(read_excel(path=inputFilePath, sheet="text_types")), silent=TRUE)
  if (! "text_types" %in% names(formatDb)) {
    formatDb[["text_types"]] <- data.frame(type_name=character(), stringsAsFactors=FALSE)
  }

  # read the "numeric" types definition table
  if (! "numeric_types" %in% xlSheets) {
    stop("Missing \"numeric_types\" sheet in the format definition file.")
  }
  try(expr=formatDb[["numeric_types"]] <- as.data.frame(read_excel(path=inputFilePath, sheet="numeric_types")), silent=TRUE)
  if (! "numeric_types" %in% names(formatDb)) {
    formatDb[["numeric_types"]] <- data.frame(type_name=character(), is_integer=logical(), min=numeric(), max=numeric(), stringsAsFactors=FALSE)
  }
  formatDb[["numeric_types"]]$is_integer <- as.logical(formatDb[["numeric_types"]]$is_integer)

  # read the "date" types definition table
  if (! "date_types" %in% xlSheets) {
    stop("Missing \"date_types\" sheet in the format definition file.")
  }
  try(expr=formatDb[["date_types"]] <- as.data.frame(read_excel(path=inputFilePath, sheet="date_types")), silent=TRUE)
  if (! "date_types" %in% names(formatDb)) {
    formatDb[["date_types"]] <- data.frame(type_name=character(), stringsAsFactors=FALSE)
  }

  # read the "codelist_types" types definition table
  if (! "codelist_types" %in% xlSheets) {
    stop("Missing \"codelist_types\" sheet in the format definition file.")
  }
  try(expr=formatDb[["codelist_types"]] <- as.data.frame(read_excel(path=inputFilePath, sheet="codelist_types")), silent=TRUE)
  if (! "codelist_types" %in% names(formatDb)) {
    formatDb[["codelist_types"]] <- data.frame(type_name=character(), enumeration_table=character(), stringsAsFactors=FALSE)
  }

  # read the "logical_types" types definition table
  if (! "logical_types" %in% xlSheets) {
    stop("Missing \"logical_types\" sheet in the format definition file.")
  }
  try(expr=formatDb[["logical_types"]] <- as.data.frame(read_excel(path=inputFilePath, sheet="logical_types")), silent=TRUE)
  if (! "logical_types" %in% names(formatDb)) {
    formatDb[["logical_types"]] <- data.frame(type_name=character(), stringsAsFactors=FALSE)
  }

  # read slots
  for (currSlotDef in formatDb[["slots"]]$definition_table) {
    try(expr=formatDb[[currSlotDef]] <- as.data.frame(read_excel(path=inputFilePath, sheet=currSlotDef)), silent=TRUE)
    if (! currSlotDef %in% names(formatDb)) {
      formatDb[[currSlotDef]] <- data.frame(column_name=character(), nullable=logical(), mandatory=logical(), pk=logical(), type_name=character(), category=character(), stringsAsFactors=FALSE)
    }
    formatDb[[currSlotDef]]$nullable <- as.logical(formatDb[[currSlotDef]]$nullable)
    formatDb[[currSlotDef]]$mandatory <- as.logical(formatDb[[currSlotDef]]$mandatory)
    formatDb[[currSlotDef]]$pk <- as.logical(formatDb[[currSlotDef]]$pk)
  }

  # read code lists
  for (currCodeListEnum in formatDb[["codelist_types"]]$enumeration_table) {
    try(expr=formatDb[[currCodeListEnum]] <- as.data.frame(read_excel(path=inputFilePath, sheet=currCodeListEnum)), silent=TRUE)
    if (! currCodeListEnum %in% names(formatDb)) {
      formatDb[[currCodeListEnum]] <- data.frame(CODE=character(), stringsAsFactors=FALSE)
    }
  }

  return(formatDb)
}

classToCategory <- function(className) {
  switch (className,
          "factor" = "codelist",
          "integer" = "numeric",
          "numeric" = "numeric",
          "character" = "text",
          "UNKNOWN"
  )
}

processDataFrame <- function(df) {
  dfDef <- data.frame(column_name=names(df), nullable=TRUE, mandatory=FALSE, pk=FALSE, stringsAsFactors=FALSE)
  dfDef$type_name <- paste0("type_", dfDef$column_name)
  dfDef$category <- unlist(lapply(names(df), function(x) classToCategory(class(df[, x]))))
  return(dfDef)
}

buildFormatDbFromObject <- function(obj) {
  if (missing(obj)) {
    stop("Missing obj.")
  }

  formatDb <- buildEmptyFormatDb()

  # structure of the data: start from the root
  for (currSlotName in slotNames(obj)) {
    currSlot <- slot(obj, currSlotName)
    # is the slot a data.frame ?
    if (inherits(currSlot, "data.frame")) {
      # yes
      # we add it to the slots definition of the format
      currDefinitionTableName <- paste0("slot_", currSlotName)
      formatDb[["slots"]] <- rbind(formatDb[["slots"]],
                                   data.frame(slot_name=currSlotName, mandatory=FALSE, definition_table=currDefinitionTableName, stringsAsFactors=FALSE))
      formatDb[[currDefinitionTableName]] <- processDataFrame(currSlot)
    } else {
      # no: variable on the "base" slot
      formatDb[["slot_base"]] <- rbind(formatDb[["slot_base"]],
                                       data.frame(column_name=currSlotName, nullable=TRUE, mandatory=FALSE, pk=FALSE, type_name=paste0("type_", currSlotName), category=classToCategory(class(currSlot)), stringsAsFactors=FALSE))
    }
  }

  # types used
  for (currSlotDef in formatDb[["slots"]]$definition_table) {
    for(currRowInd in seq_len(nrow(formatDb[[currSlotDef]]))) {
      currRow <- formatDb[[currSlotDef]][currRowInd, ]
      currCatName <- paste0(currRow$category, "_types")
      # already defnied ?
      if (! currRow$type_name %in% formatDb[[currCatName]]$type_name) {
        # no, we add it
        formatDb[[currCatName]][nrow(formatDb[[currCatName]])+1, "type_name"] <- currRow$type_name
        # if category codelist we add another sheet for the codeList definition
        if (currRow$category == "codelist") {
          currCodeListName <- paste0("codelist_", currRow$column_name)
          formatDb[[currCodeListName]] <- data.frame(CODE=character(), stringsAsFactors=FALSE)
          formatDb[["codelist_types"]][nrow(formatDb[["codelist_types"]]), "enumeration_table"] <- currCodeListName
        }
      }
    }
  }

  return(formatDb)
}

testStructure <- function(obj, formatDb) {
  if (missing(obj)) {
    stop("Missing obj.")
  }

  if (missing(formatDb)) {
    stop("Missing formatDb.")
  }

  reportFileName <- tempfile(fileext=".txt")
  cat("Structure report\n", file=reportFileName)
  cat("***\n", file=reportFileName, append=TRUE, sep="")
  cat("Format tested:\n", file=reportFileName, append=TRUE, sep="")
  cat("format_name: [", formatDb[["format_infos"]]$format_name, "]\n", file=reportFileName, append=TRUE, sep="")
  cat("format_version: [", formatDb[["format_infos"]]$format_version, "]\n", file=reportFileName, append=TRUE, sep="")
  cat("***\n", file=reportFileName, append=TRUE, sep="")

  for (currSlotInd in seq_len(nrow(formatDb[["slots"]]))) {
    currSlot <- formatDb[["slots"]][currSlotInd,]
    cat("Check slot_name:[", currSlot$slot_name, "]\n", file=reportFileName, append=TRUE, sep="")
    defTable <- formatDb[[currSlot$definition_table]]

    # test if the slot exists
    if (currSlot$slot_name == "base") {
      # check the base slot
      currObjSlot <- obj
      slotFound <- TRUE
    } else {
      # check another slot
      if (currSlot$slot_name %in% slotNames(obj)) {
        slotFound <- TRUE
        currObjSlot <- slot(obj, currSlot$slot_name)
      } else {
        slotFound <- FALSE
      }
    }

    if (slotFound) {
      # slot found, we could test for the columns
      for (currColumnInd in seq_len(nrow(defTable))) {
        currColumn <- defTable[currColumnInd, ]
        cat("\tCheck column_name:[", currColumn$column_name, "]\n", file=reportFileName, append=TRUE, sep="")
        # test if the columns exist
        if (currSlot$slot_name == "base") {
          # check in the base slot
          if (currColumn$column_name %in% slotNames(currObjSlot)) {
            currObjColumn <- slot(obj, currColumn$column_name)
            columnFound <- TRUE
          } else {
            columnFound <- FALSE
          }
        } else {
          # check in another slot
          if (currColumn$column_name %in% names(currObjSlot)) {
            columnFound <- TRUE
            currObjColumn <- currObjSlot[, currColumn$column_name]
          } else {
            columnFound <- FALSE
          }
        }
        if (columnFound) {

        } else {
          # column not found, it is a mandatory column ?
          if (currColumn$mandatory) {
            # yes, data structure invalid
            cat("ERROR: column_name:[", currColumn$column_name, "] NOT FOUND\n", file=reportFileName, append=TRUE, sep="")
          }
        }
      }
    } else {
      # slot not found, it is a mandatory slot ?
      if (currSlot$mandatory) {
        # yes, data structure invalid
        cat("ERROR: slot_name:[", currSlot$slot_name, "] NOT FOUND\n", file=reportFileName, append=TRUE, sep="")
      }
    }
  }
  return(reportFileName)
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  return(abs(x - round(x)) < tol)
}

isNumeric <- function(x) {
  return(is.na(x) | ! is.na(try(expr=as.numeric(x), silent=TRUE)))
}

fpKey <- function(tab, colIndex, sep=":-:") {
  key <- tab[, colIndex]
  if (length(colIndex) > 1) {
    key <- apply(key, 1, paste0, collapse=sep)
  }
  key <- gsub("[[:space:]]", "", key)
  return(key)
}


validateCodelist <- function(data, slotName, columnName, refCodeList, ignoreCaseInCodelist=TRUE) {

  if (all(is.na(data))) {
    # all data are null
    dataValidity <- rep_len(TRUE, length(data))
    result <- "INFO"
    message <- "All values are null"
  } else {
    if (length(refCodeList) == 0) {
      dataValidity <- rep_len(FALSE, length(data))
      result <- "ERROR"
      message <- "Empty reference codelist"
    } else {
      if (ignoreCaseInCodelist) {
        data <- toupper(data)
        refCodeList <- toupper(refCodeList)
      }
      dataValidity <- is.na(data) | (data %in% refCodeList)

      if (all(dataValidity, na.rm=TRUE)) {
        # perfect: all are numeric values
        result <- "OK"
        message <- "All values are valid codes"
      } else {
        result <- "ERROR"
        errorPcent <- sum(! dataValidity, na.rm=TRUE) / length(dataValidity) * 100
        currBad <- unique(data[! dataValidity])
        if (length(currBad) > 6) {
          currBad <- c(currBad[1:6], "...")
        }
        message <- paste0(round(errorPcent, digits=2), "% of the values are not valid codes: [", paste0(currBad, collapse="; "), "]")
      }
    }
  }

  report <- data.frame(slot=slotName,
                       column=columnName,
                       test="is valid code list ?",
                       result=result,
                       message=message,
                       stringsAsFactors=FALSE)


  ret <- list()
  ret[["report"]] <- report
  ret[["dataValidity"]] <- dataValidity
  return(ret)
}

validateText <- function(data, slotName, columnName) {

  if (all(is.na(data))) {
    # all data are null
    dataValidity <- rep_len(TRUE, length(data))
    result <- "INFO"
    message <- "All values are null"
  } else {
    dataValidity <- unlist(lapply(as.character(data), is.character))

    if (all(dataValidity, na.rm=TRUE)) {
      # perfect: all are text values
      result <- "OK"
      message <- "All values are text"
    } else {
      result <- "ERROR"
      errorPcent <- sum(! dataValidity, na.rm=TRUE) / length(dataValidity) * 100
      currBad <- unique(data[! dataValidity])
      if (length(currBad) > 6) {
        currBad <- c(currBad[1:6], "...")
      }
      message <- paste0(round(errorPcent, digits=2), "% of the values are not text: [", paste0(currBad, collapse="; "), "]")
    }
  }
  report <- data.frame(slot=slotName,
                       column=columnName,
                       test="is text ?",
                       result=result,
                       message=message,
                       stringsAsFactors=FALSE)


  ret <- list()
  ret[["report"]] <- report
  ret[["dataValidity"]] <- dataValidity
  return(ret)
}

validateNull <- function(data, slotName, columnName) {

  dataValidity <- ! is.na(data)

  if (all(dataValidity, na.rm=TRUE)) {
    # perfect: all are not null
    result <- "OK"
    message <- "All values are not null"
  } else {
    result <- "ERROR"
    errorPcent <- sum(! dataValidity, na.rm=TRUE) / length(dataValidity) * 100

    message <- paste0(round(errorPcent, digits=2), "% of the values are null")
  }
  report <- data.frame(slot=slotName,
                       column=columnName,
                       test="is null ?",
                       result=result,
                       message=message,
                       stringsAsFactors=FALSE)


  ret <- list()
  ret[["report"]] <- report
  ret[["dataValidity"]] <- dataValidity
  return(ret)
}

validateNumeric <- function(data, slotName, columnName, isInteger, rangeMin=NA, rangeMax=NA) {

  if (all(is.na(data))) {
    # all data are null
    dataValidity <- rep_len(TRUE, length(data))
    result <- "INFO"
    message <- "All values are null"
  } else {
    # we test for numeric values
    dataValidity <- try(unlist(lapply(data, isNumeric)))

    if (all(dataValidity, na.rm=TRUE)) {
      # perfect: all are numeric values
      result <- "OK"
      message <- "All values are numeric"
    } else {
      result <- "ERROR"
      errorPcent <- sum(! dataValidity, na.rm=TRUE) / length(dataValidity) * 100
      currBad <- unique(data[! dataValidity])
      if (length(currBad) > 6) {
        currBad <- c(currBad[1:6], "...")
      }
      message <- paste0(round(errorPcent, digits=2), "% of the values are not integer: [", paste0(currBad, collapse="; "), "]")
    }
  }
  report <- data.frame(slot=slotName,
                       column=columnName,
                       test="is numeric ?",
                       result=result,
                       message=message,
                       stringsAsFactors=FALSE)

  if (result == "OK") {
    data <- as.numeric(data)

    # test for integer
    if (isInteger) {
      dataValidityTemp <- is.na(data) | is.wholenumber(data)
      dataValidity <- dataValidity & dataValidityTemp

      if (all(dataValidityTemp, na.rm=TRUE)) {
        # perfect: all are integer values
        result <- "OK"
        message <- "All values are integer"
      } else {
        result <- "ERROR"
        errorPcent <- sum(! dataValidityTemp, na.rm=TRUE) / length(dataValidityTemp) * 100
        currBad <- unique(data[! dataValidityTemp])
        if (length(currBad) > 6) {
          currBad <- c(currBad[1:6], "...")
        }
        message <- paste0(round(errorPcent, digits=2), "% of the values are not integer: [", paste0(currBad, collapse="; "), "]")
      }
      report <- rbind(
        report,
        data.frame(slot=slotName,
                   column=columnName,
                   test="is integer ?",
                   result=result,
                   message=message,
                   stringsAsFactors=FALSE))

    }

    # test for min
    if (! is.na(rangeMin)) {
      dataValidityTemp <- is.na(data) | data >= rangeMin
      dataValidity <- dataValidity & dataValidityTemp

      test <- paste0("is greater or egal than ", rangeMin)
      if (all(dataValidityTemp, na.rm=TRUE)) {
        # perfect: all are gretaer or equal to the min
        result <- "OK"
        message <- paste0("All values are greater or egal than ", rangeMin)
      } else {
        result <- "ERROR"
        errorPcent <- sum(! dataValidityTemp, na.rm=TRUE) / length(dataValidityTemp) * 100
        currBad <- unique(data[! dataValidityTemp])
        if (length(currBad) > 6) {
          currBad <- c(currBad[1:6], "...")
        }
        message <- paste0(round(errorPcent, digits=2), "% of the values are lower than ", rangeMin, ": [", paste0(currBad, collapse="; "), "]")
      }
      report <- rbind(
        report,
        data.frame(slot=slotName,
                   column=columnName,
                   test=test,
                   result=result,
                   message=message,
                   stringsAsFactors=FALSE))

    }

    # test for max
    if (! is.na(rangeMax)) {
      dataValidityTemp <- is.na(data) | data <= rangeMax
      dataValidity <- dataValidity & dataValidityTemp

      test <- paste0("is lower or egal than ", rangeMax)
      if (all(dataValidityTemp, na.rm=TRUE)) {
        # perfect: all are lower or equal to the max
        result <- "OK"
        message <- paste0("All values are lower or egal than ", rangeMax)
      } else {
        result <- "ERROR"
        errorPcent <- sum(! dataValidityTemp, na.rm=TRUE) / length(dataValidityTemp) * 100
        currBad <- unique(data[! dataValidityTemp])
        if (length(currBad) > 6) {
          currBad <- c(currBad[1:6], "...")
        }
        message <- paste0(round(errorPcent, digits=2), "% of the values are greater than ", rangeMax, ": [", paste0(currBad, collapse="; "), "]")
      }
      report <- rbind(
        report,
        data.frame(slot=slotName,
                   column=columnName,
                   test=test,
                   result=result,
                   message=message,
                   stringsAsFactors=FALSE))

    }
  }
  ret <- list()
  ret[["report"]] <- report
  ret[["dataValidity"]] <- dataValidity
  return(ret)
}

#' Validate data against a format definition.
#'
#' @param obj Object to validate, could be a data.frame or a S3/S4 object.
#' @param formatDb Format data structure.
#' @param ignoreCaseInCodelist logical: Should the case of codes of nomenclatures to be ignored ? (default: TRUE)
#' @param report character: Should the report be saved in .csv file (\"files\"), returned in a R session list (\"list\") or both (\"both\") ? (default: \"files\")
#' @param reportDir character: Path where to record the report files, otherwise the path of the per-session temporary directory is used.
#'
#' @return Validation report.
#'
#' @examples
#' \dontrun{
#' }
#' @export
#' @author Norbert Billet
#' @importFrom readxl excel_sheets read_excel
#' @importFrom tools file_ext md5sum
validateBio<- function(obj) {
	obj<-sole.cs
  switch (class(obj),
          "csData" = {
		  slhl<-merge(obj@sl,obj@hl)
		  slhl$indw<-0.003*((slhl$lenCls/10)^3)*slhl$lenNum
		  #pipo<-tbl_df(slhl)%>%group_by(sampType,landCtry,vslFlgCtry,year,proj,
		#			      trpCode,staNum,spp,catchCat,landCat,commCatScl,commCat,
		#			      subSampCat,sex,wt,subSampWt)%>%mutate()
		 # pipo<-tapply(slhl$indw,list(slhl$sampType,slhl$landCtry,slhl$vslFlgCtry,slhl$year,slhl$proj,
		#			      slhl$trpCode,slhl$staNum,slhl$spp,slhl$catchCat,slhl$landCat,slhl$commCatScl,slhl$commCat,
	#			      slhl$subSampCat,slhl$sex,slhl$wt,slhl$subSampWt),sum,na.rm=T)
		 # 

	  },
          "csPi" = {
		  slhl<-merge(obj@sl,obj@hl)

	  },
          {
		  print("Class unknown - no subsample weight check")
	  }
  )
}

test<-function(){
	library(fishPifct)
	data(sole)
	pipo <- csDataTocsPi(sole.cs)
	class(sole.cs)
	validateBio(sole.cs)
	class(pipo)
	validateSampWt(pipo)
	validateBio(data.frame())
	#
	load("../../../analyses/wgparam2016.rdata")
	listsp<-unique(wgparam$Latin.name)
	listsp<-sort(unlist(strsplit(listsp,",")))
	listsp<-listsp[-3]
	require(rfishbase)
	l1<-length_weight(c(listsp))



}

#' Validate data against a format definition.
#'
#' @param obj Object to validate, could be a data.frame or a S3/S4 object.
#' @param formatDb Format data structure.
#' @param ignoreCaseInCodelist logical: Should the case of codes of nomenclatures to be ignored ? (default: TRUE)
#' @param report character: Should the report be saved in .csv file (\"files\"), returned in a R session list (\"list\") or both (\"both\") ? (default: \"files\")
#' @param reportDir character: Path where to record the report files, otherwise the path of the per-session temporary directory is used.
#'
#' @return Validation report.
#'
#' @examples
#' \dontrun{
#' }
#' @export
#' @author Norbert Billet
#' @importFrom readxl excel_sheets read_excel
#' @importFrom tools file_ext md5sum
validateData <- function(obj, formatDb, ignoreCaseInCodelist=TRUE, report="files", reportDir) {
  if (missing(obj)) {
    stop("Missing obj.")
  }

  if (missing(formatDb)) {
    stop("Missing formatDb.")
  }

  if (missing(reportDir)) {
    reportDir <- tempdir()
  }

  if (! report %in% c("files", "list", "both")) {
    stop("\"report\" must be \"files\", \"list\" or \"both\"")
  }

  # check for the formatDb source type
  if (inherits(formatDb, "character")) {
    if (! file.exists(formatDb)) {
      stop("Wrong format definition file path in \"formatDb\" parameter.")
    }

    if(! require(tools)) {
      stop("You have to install the \"tools\" library.")
    }

    formatFilePath <- formatDb
    formatType <- "file"
    formatFileMd5 <- md5sum(formatFilePath)
    formatDb <- readFormatDbFromXls(inputFilePath=formatFilePath)
  } else {
    formatType <- "object"
  }

  # check for the dataset source type
  if (inherits(obj, "character")) {
    # this is a file path
    inputFilePath <- obj
    if (! file.exists(inputFilePath)) {
      stop("Wrong input file path in \"obj\" parameter.")
    }

    sourceType <- "file"

    if(! require(tools)) {
      stop("You have to install the \"tools\" library.")
    }

    # extract the file extension to determine the file type
    fileExt <- file_ext(inputFilePath)
    fileMd5 <- md5sum(inputFilePath)

    if (tolower(fileExt) %in% c("xls", "xlsx")) {
      # Microsoft Excel file

      # check for "readxl" package avaibility
      if(! require(readxl)) {
        stop("You have to install the \"readxl\" library.")
      }

      xlSheets <- excel_sheets(path=inputFilePath)

      # build a list from all the excell sheets
      obj <- list()
      for (currSheet in xlSheets) {
        obj[[currSheet]] <- as.data.frame(read_excel(path=inputFilePath, sheet=currSheet))
      }
    }

    if (tolower(fileExt) %in% c("txt", "csv")) {
      # csv file
      obj <- read.csv(file=inputFilePath, strip.white=TRUE, stringsAsFactors=FALSE)
    }
  } else {
    # source object is a R object
    sourceType <- "object"
  }

  reportInFiles <- ifelse(report %in% c("files", "both"), TRUE, FALSE)
  reportInList <- ifelse(report %in% c("list", "both"), TRUE, FALSE)

  reportMeta <- data.frame(parameter=c("format_name",
                                       "format_version",
                                       "validate_date",
                                       "dataset_container",
                                       "format_container"),
                           value=c(formatDb[["format_infos"]]$format_name,
                                   formatDb[["format_infos"]]$format_version,
                                   format(Sys.time(), format="%Y-%m-%d %H:%M:%S"),
                                   sourceType,
                                   formatType),
                           stringsAsFactors=FALSE)
  if (sourceType=="file") {
    # source is a file, we add extra meta infos
    reportMeta <- rbind(reportMeta, c("dataset_file_path", inputFilePath))
    reportMeta <- rbind(reportMeta, c("dataset_file_md5", fileMd5))
  }

  if (formatType=="file") {
    # format is a file, we add extra meta infos
    reportMeta <- rbind(reportMeta, c("format_file_path", formatFilePath))
    reportMeta <- rbind(reportMeta, c("format_file_md5", formatFileMd5))
  }

  # reportStruct <- data.frame(test=character(), result=character(), message=character(), stringsAsFactors=FALSE)
  reportStruct <- data.frame(slot=character(), column=character(), test=character(), result=character(), message=character(), stringsAsFactors=FALSE)
  reportData <- data.frame(slot=character(), column=character(), test=character(), result=character(), message=character(), stringsAsFactors=FALSE)

  reportFiles <- c()
  reportList <- list()

  for (currSlotInd in seq_len(nrow(formatDb[["slots"]]))) {
    currSlot <- formatDb[["slots"]][currSlotInd,]

    defTable <- formatDb[[currSlot$definition_table]]

    # test if the slot exists
    if (currSlot$slot_name == "base") {
    #if (currSlot$slot_name %in% c("base","classVersion","desc","popData","design")) {
      # check the base slot
      # check the object type
      if (inherits(obj, "data.frame")) {
        # obj is a data.frame
        currObjSlot <- obj
        slotFound <- TRUE
      } else {
        if (inherits(obj, "list")) {
          # obj is a list
          if ("base" %in% names(obj)) {
            slotFound <- TRUE
            currObjSlot <- obj[["base"]]
          } else {
            slotFound <- FALSE
          }
        } else {
          # another S3/S4 obj
          currObjSlot <- obj
          slotFound <- TRUE
        }
      }
    } else {
      # check another slot

      # check the object type
      if (inherits(obj, "list")) {
        # obj is a list
        if (currSlot$slot_name %in% names(obj)) {
          slotFound <- TRUE
          currObjSlot <- obj[[currSlot$slot_name]]
        } else {
          slotFound <- FALSE
        }
      } else {
        # another S3/S4 obj
        if (currSlot$slot_name %in% slotNames(obj)) {
          slotFound <- TRUE
          currObjSlot <- slot(obj, currSlot$slot_name)
        } else {
          slotFound <- FALSE
        }
      }
    }

    if (slotFound) {
      reportStruct <- rbind(reportStruct,
                            data.frame(slot=currSlot$slot_name, column=NA, test=paste0("Slot exists ?"), result="OK", message="Found", stringsAsFactors=FALSE))

      # detailed report df
      currObjSlotReport <- currObjSlot

      # slot found, we could test for the columns
      for (currColumnInd in seq_len(nrow(defTable))) {
        currColumn <- defTable[currColumnInd, ]

        if (currSlot$slot_name == "base") {
          # is the base object a data.frame of a S3/S4 R object ?
          if (inherits(currObjSlot, "data.frame")) {
            # a data frame
            if (currColumn$column_name %in% names(currObjSlot)) {
              columnFound <- TRUE
              currObjColumn <- currObjSlot[, currColumn$column_name]
            } else {
              columnFound <- FALSE
            }
          } else {
            # a S3/S4 object
            # check in the base slot
            if (currColumn$column_name %in% slotNames(currObjSlot)) {
              currObjColumn <- slot(obj, currColumn$column_name)
              columnFound <- TRUE
            } else {
              columnFound <- FALSE
            }
          }
        } else {
          # check in another slot
          if (currColumn$column_name %in% names(currObjSlot)) {
            columnFound <- TRUE
            currObjColumn <- currObjSlot[, currColumn$column_name]
          } else {
            columnFound <- FALSE
          }
        }
	#special case for descriptive slot
    	if (currSlot$slot_name %in% c("classVersion","desc","popData","design")) {
		columnFound<-slotFound
		currObjColumn<- currSlot$slot_name
	}


        if (columnFound) {
          currObjColumnReport <- rep_len(TRUE, length(currObjColumn))

          reportStruct <- rbind(reportStruct,
                                data.frame(slot=currSlot$slot_name, column=currColumn$column_name, test=paste0("Column exists ?"), result="OK", message="Found", stringsAsFactors=FALSE))
          # now check data

          typeDef <- subset(formatDb[[paste0(currColumn$category, "_types")]], type_name==currColumn$type_name)

          localChecked <- FALSE

          # check for codelist
          if (currColumn$category=="codelist") {
            localChecked <- TRUE

            res <- validateCodelist(data=currObjColumn, slotName=currSlot$slot_name, columnName=currColumn$column_name, refCodeList=formatDb[[typeDef$enumeration_table]]$CODE, ignoreCaseInCodelist=ignoreCaseInCodelist)

            reportData <- rbind(reportData, res[["report"]])
            currObjColumnReport <- currObjColumnReport & res[["dataValidity"]]
          }

          # check for text
          if (currColumn$category == "text") {
            localChecked <- TRUE

            res <- validateText(data=currObjColumn, slotName=currSlot$slot_name, columnName=currColumn$column_name)

            reportData <- rbind(reportData, res[["report"]])
            currObjColumnReport <- currObjColumnReport & res[["dataValidity"]]
          }

          # check for numeric
          if (currColumn$category == "numeric") {
            localChecked <- TRUE

            res <- validateNumeric(data=currObjColumn,
                                   slotName=currSlot$slot_name,
                                   columnName=currColumn$column_name,
                                   isInteger=typeDef$is_integer, rangeMin=typeDef$min, rangeMax=typeDef$max)

            reportData <- rbind(reportData, res[["report"]])
            currObjColumnReport <- currObjColumnReport & res[["dataValidity"]]
          }

          if (! localChecked) {
            # TODO
            reportData <- rbind(reportData,
                                data.frame(slot=currSlot$slot_name, column=currColumn$column_name, test="NO TEST", result="ERROR", message=paste0("No test provided for category ", currColumn$category), stringsAsFactors=FALSE))
          }

          # test for NA
          if (! currColumn$nullable) {
            # NA not allowed
            # currObjColumnReportTemp <- is.na(currObjColumn)
            # currObjColumnReport <- currObjColumnReport | currObjColumnReportTemp
            # naInd <- which(currObjColumnReportTemp)
            # if (length(naInd) > 0) {
            #   reportData <- rbind(reportData,
            #                       data.frame(slot=currSlot$slot_name, column=currColumn$column_name, test="nullable", result="ERROR", message="Some null values found", stringsAsFactors=FALSE))
            # } else {
            #   reportData <- rbind(reportData,
            #                       data.frame(slot=currSlot$slot_name, column=currColumn$column_name, test="nullable", result="OK", message="No null values", stringsAsFactors=FALSE))
            # }
            res <- validateNull(data=currObjColumn,
                                   slotName=currSlot$slot_name,
                                   columnName=currColumn$column_name)

            reportData <- rbind(reportData, res[["report"]])
            currObjColumnReport <- currObjColumnReport & res[["dataValidity"]]
          } else {
            # NA allowed
            currObjColumnReport[is.na(currObjColumnReport)] <- FALSE
          }

          # append the currObjColumnReport to the currObjSlotReport
          currObjSlotReport <- cbind(currObjSlotReport, ifelse(currObjColumnReport, "VALID", "INVALID"))
          names(currObjSlotReport)[length(names(currObjSlotReport))] <- paste0(currColumn$column_name, "_validity")
        } else {
          # column not found, it is a mandatory column ?
          if (currColumn$mandatory) {
            # yes, data structure invalid
            reportStruct <- rbind(reportStruct,
                                  data.frame(slot=currSlot$slot_name, column=currColumn$column_name, test=paste0("Column exists ?"), result="ERROR", message="Not found", stringsAsFactors=FALSE))
          } else {
            # not an error, just info
            reportStruct <- rbind(reportStruct,
                                  data.frame(slot=currSlot$slot_name, column=currColumn$column_name, test=paste0("Column exists ?"), result="INFO", message="Not found but not mandatory", stringsAsFactors=FALSE))
          }
        }
      }

      # test for PK
      pkCols <- subset(defTable, pk)$column_name
      if (length(pkCols) > 0) {
        if (all(pkCols %in% names(currObjSlot))) {
          # ok, all PK columns found
          reportStruct <- rbind(reportStruct,
                                data.frame(slot=currSlot$slot_name, column=NA, test="PK columns exists ?", result="OK", message="Found", stringsAsFactors=FALSE))

          pk <- fpKey(tab=currObjSlot[, pkCols], colIndex=pkCols)
          dupPk <- pk[duplicated(pk)]
          pkReport <- pk %in% dupPk
          currObjSlotReport <- cbind(currObjSlotReport, PK_unique= ifelse(pkReport, "INVALID", "VALID"))
          if (sum(pkReport) > 0) {
            # some duplicated PK
            reportStruct <- rbind(reportStruct,
                                  data.frame(slot=currSlot$slot_name, column=NA, test="PK unique ?", result="ERROR", message="Some duplicated key found", stringsAsFactors=FALSE))
          } else {
            # no duplicate PK
            reportStruct <- rbind(reportStruct,
                                  data.frame(slot=currSlot$slot_name, column=NA, test="PK unique ?", result="OK", message="All primary key are unique", stringsAsFactors=FALSE))

          }
        } else {
          # ko, som PK columns not found in the slot
          reportStruct <- rbind(reportStruct,
                                data.frame(slot=currSlot$slot_name, column=NA, test="PK columns exists ?", result="ERROR", message="Some PK columns not found", stringsAsFactors=FALSE))
        }
      }

      # save the reprot of the current slot
      if (reportInFiles) {
        # save the currObjSlotReport
        currObjSlotReportFilePath <- tempfile(pattern=paste0("slot_", currSlot$slot_name, "_"), tmpdir=reportDir, fileext=".csv")
        write.csv(x=currObjSlotReport, file=currObjSlotReportFilePath, row.names=FALSE)
        reportFiles <- c(reportFiles, currObjSlotReportFilePath)
      }
      if (reportInList) {
        # append the currObjSlotReport to the report list
        reportList[[paste0("slot_", currSlot$slot_name)]] <- currObjSlotReport
      }
    } else {
      # slot not found, it is a mandatory slot ?
      if (currSlot$mandatory) {
        # yes, data structure invalid
        reportStruct <- rbind(reportStruct,
                              data.frame(slot=currSlot$slot_name, column=NA, test="Slot exists ?", result="ERROR", message="Not found", stringsAsFactors=FALSE))

      } else {
        # no
        reportStruct <- rbind(reportStruct,
                              data.frame(slot=currSlot$slot_name, column=NA, test="Slot exists ?", result="INFO", message="Not found but not mandatory", stringsAsFactors=FALSE))
      }
    }
  }

  # save the report
  if (reportInFiles) {
    # in files
    reportMetaFilePath <- tempfile(pattern="meta_", tmpdir=reportDir, fileext=".csv")
    write.csv(x=reportMeta, file=reportMetaFilePath, row.names=FALSE)
    reportStructFilePath <- tempfile(pattern="struct_", tmpdir=reportDir, fileext=".csv")
    write.csv(x=reportStruct, file=reportStructFilePath, row.names=FALSE)
    reportDataFilePath <- tempfile(pattern="data_", tmpdir=reportDir, fileext=".csv")
    write.csv(x=reportData, file=reportDataFilePath, row.names=FALSE)
    reportFiles <- c(reportFiles, reportMetaFilePath, reportStructFilePath, reportDataFilePath)
  }
  if (reportInList) {
    # in list
    reportList[["meta"]] <- reportMeta
    reportList[["struct"]] <- reportStruct
    reportList[["data"]] <- reportData
  }

  if (reportInFiles & reportInList) {
    # report == both, we display the report files path and return the report list
    message(paste0(reportFiles, collapse="\n"))
    return(reportList)
  }
  if (reportInFiles) {
    # report == files
    return(reportFiles)
  }
  if (reportInList) {
    # report == list
    return(reportList)
  }
}
