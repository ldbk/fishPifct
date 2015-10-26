validateStructure <- function(piCsObj) {
  validated <- TRUE
  # check if exist no-primary key column present in different slots
# TODO: what about the "recType" ?
  # se=>tr=>hh=>sl=>hl  
  slots <- c("se", "tr", "hh", "sl", "hl")
  columns <- unlist(lapply(slots, function(slotName) {
    if (inherits(slot(piCsObj, slotName), "data.frame")) {
      setdiff(names(slot(piCsObj, slotName)), piPk(slotName))
    }
  }))
  columns <- columns[columns != "recType"]
  dup <- duplicated(columns)
  if (sum(dup) > 0) {
    validated <- FALSE
    message("Duplicated non-primary keys: ", paste0(columns[dup], collapse=", "))
  }
  
  # se=>tr=>ca
  slots <- c("se", "tr", "ca")
  columns <- unlist(lapply(slots, function(slotName) {
    if (inherits(slot(piCsObj, slotName), "data.frame")) {
      setdiff(names(slot(piCsObj, slotName)), piPk(slotName))
    }
  }))
  columns <- columns[columns != "recType"]
  dup <- duplicated(columns)
  if (sum(dup) > 0) {
    validated <- FALSE
    message("Duplicated non-primary keys: ", paste0(columns[dup], collapse=", "))
  }
  return(validated)
}


#####################

findCsJoin <- function(fields, csObj) {
  csTables <- c("se", "tr", "hh", "sl", "hl")
  place <- matrix(data=FALSE, ncol=length(fields), nrow=length(csTables))
  
  if (missing(csObj)) {
    csObj <- new("csPi")
  }
  
  place[1, fields %in% names(csObj@se)] <- TRUE
  place[2, fields %in% names(csObj@tr)] <- TRUE
  place[3, fields %in% names(csObj@hh)] <- TRUE
  place[4, fields %in% names(csObj@sl)] <- TRUE
  place[5, fields %in% names(csObj@hl)] <- TRUE
  
  #test for unknown fields
  if (any(colSums(place) == 0)) {
    stop("unknown field(s): ", paste0("[", fields[colSums(place) == 0], "]", collapse = ", "))
  }
  
  #test if all fields are present on a unique table (no join !)
  res <- rowSums(place)==length(fields)
  if (any(res)) {
    return(csTables[which.max(res)])
  }
  
  rowTab <- 1:nrow(place)
  
  continue <- TRUE
  while (continue) {
    place <- place[, colSums(place) != nrow(place)]
    cutRow <- rowSums(place) != 0
    continue <- any(! cutRow)
    rowTab <- rowTab[cutRow]
    place <- place[cutRow, ]
  }
  
  finalInd <- range(apply(place, 2, which.max))
  
  return(csTables[rowTab[finalInd[1]]:rowTab[finalInd[2]]])
}

getCsNames <- function(csObj, tables) {
  if (missing(tables)) tables <- c("se", "tr", "hh", "sl", "hl")
  return(unique(unlist(lapply(tables, function(x) names(slot(csObj, x))))))
}

buildKey <- function(tab, colIndex, sep=":-:", removeWhite=FALSE) {
  key <- do.call("paste", c(tab[, colIndex], list(sep=sep)))
  if (removeWhite) key <- gsub("[[:space:]]", "", key)
  return(key)
}
