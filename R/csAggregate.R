#' Find the fishPi / CS tables to join to get all specified fields.
#'
#' @param fields vector of names of required fields.
#' @param csObj COST / CS object.
#'
#' @return Vector of CS tables names to join.
#'
#' @seealso \code{\link{COSTaggregate}} to computes summary statistics on a COST object.
#'
#' @export
#'
#' @examples
#' findCsJoin(csObject, c("lenCls", "spp", "trpCode"))
#' findCsJoin(csObject, c("vslLen", "spp"))
#'
#' @author Norbert Billet - IRD
findCsJoin <- function(csObj, fields) {
  csTables <- c("se", "tr", "hh", "sl", "hl")
  place <- matrix(data = FALSE, ncol = length(fields), nrow = length(csTables))
  
  if (missing(csObj)) {
    csObj <- new("csPi")
  }
  
  if (! class(csObj) == "csPi") {
    stop("Provided object is not a fishPi csPi object")
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

#' Splits the fishPi CS object into subsets, computes summary statistics for each, and returns the result in a convenient form.
#'
#' @param csObj to aggregate.
#' @param x field to aggregate.
#' @param by a list of grouping elements.
#' @param aggFun a function to compute the summary statistic.
#' @param ... further arguments passed to or used by aggFun
#'
#' @return A data frame with columns corresponding to the grouping variables in by followed by aggregated columns from x.
#'
#' @export
#'
#' @examples
#' findCsJoin(csObject, c("lenCls", "spp", "trpCode"))
#' findCsJoin(csObject, c("vslLen", "spp"))
#'
#' @author Norbert Billet - IRD
csAggregate <- function(csObj,
                          x,
                          by,
                          aggFun,
                          ...) {
  if (missing(csObj)) {
    stop("You must provide a csObj")
  }
  
  if (! class(csObj) == "csPi") {
    stop("Provided object is not a fishPi csPi object")
  }

  if (missing(x)) {
    stop("You must provide a x.")
  }
  
  if (length(x) > 1) {
    stop("You can only aggregate on one field.")
  }
  
  if (missing(by)) {
    stop("You must provide a by.")
  }
  
  if (! inherits(by, "list")) {
    stop("'by' must be a list.")
  }
  
  fields <- c(x, by)
  csTables <- findCsJoin(csObj, fields)
  
  namesBySlots <- outer(c(x, by), csTables, Vectorize(function(x, y) exists(x=as.character(x), where=slot(csObj, y), inherits=FALSE), SIMPLIFY=TRUE))
  
  
  if (length(csTables) == 1) {
    dataTable <- slot(csObj, csTables)
  } else {
    for (curr in 1:(length(csTables)-1)) {
      if (curr == 1) {
        dataTable <- merge(x = slot(csObj, csTables[curr])[, unique(unlist(c(piPk(csTables[curr]), fields[which(namesBySlots[, curr])])))], 
                           y = slot(csObj, csTables[curr+1])[, unique(unlist(c(piPk(csTables[curr+1]), fields[which(namesBySlots[, curr+1])])))], all.x = TRUE)
      } else {
        dataTable <- merge(x = dataTable, y = slot(csObj, csTables[curr+1])[, unique(unlist(c(piPk(csTables[curr+1]), fields[which(namesBySlots[, curr+1])])))], all.x = TRUE)
      }
    }
  }
  
  if (! x %in% names(dataTable)) {
    stop("Non existing specified x available fields are:\n", paste0(names(dataTable), collapse = ", "))
  }
  
  if (is.null(aggFun)) {
    return(dataTable)
  }
  
  byList <- list()
  for (currBy in seq_len(length(by))) {
    eval(parse('',text=paste0("byList <- c(byList, list(", names(by)[currBy], "=dataTable[, \"", by[[currBy]], "\"]))")))
  }
  aggData <- aggregate.data.frame(x=dataTable[, unlist(x)], by=byList, FUN=aggFun, ...)
  if (inherits(x, "list") & length(names(x)) == 1) {
    names(aggData)[which(names(aggData)=="x")] <- names(x)
  }

  return(aggData)
}