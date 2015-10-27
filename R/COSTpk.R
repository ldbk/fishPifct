#' Return key fields for a given COST data structure.
#'
#' @param costTable the COST table.
#'
#' @return list of key fields for the given COST table.
#'
#' @examples
#' #
#' @author Norbert Billet - IRD
#'
#' @export
COSTPk <- function(costTable = "tr") {
  if (costTable == "cl") {
    return(c('landCtry', 'vslFlgCtry', 'year', 'quarter', 'month', 'area', 'rect', 'subRect', 'taxon', 'landCat', 'commCatScl', 'commCat', 'foCatNat', 'foCatEu5', 'foCatEu6', 'harbour', 'vslLenCat'))
  }
  if (costTable == "ce") {
    return(c('vslFlgCtry', 'year', 'quarter', 'month', 'area', 'rect', 'subRect', 'foCatNat', 'foCatEu5', 'foCatEu6', 'harbour', 'vslLenCat'))
  }
  if (costTable == "tr") {
    return(c('sampType', 'landCtry', 'vslFlgCtry', 'year', 'proj', 'trpCode'))
  }
  if (costTable == "hh") {
    return(c('sampType', 'landCtry', 'vslFlgCtry', 'year', 'proj', 'trpCode', 'staNum'))
  }
  if (costTable == "sl") {
    return(c('sampType', 'landCtry', 'vslFlgCtry', 'year', 'proj', 'trpCode', 'staNum', 'spp', 'catchCat', 'landCat', 'commCatScl', 'commCat', 'subSampCat'))
  }
  if (costTable == "hl") {
    return(c('sampType', 'landCtry', 'vslFlgCtry', 'year', 'proj', 'trpCode', 'staNum', 'spp', 'catchCat', 'landCat', 'commCatScl', 'commCat', 'subSampCat', 'sex', 'lenCls'))
  }
  if (costTable == "ca") {
    return(c('sampType', 'landCtry', 'vslFlgCtry', 'year', 'proj', 'trpCode', 'staNum', 'quarter', 'month', 'spp', 'sex', 'catchCat', 'landCat', 'commCatScl', 'commCat', 'stock', 'area', 'rect', 'subRect', 'lenCls', 'age', 'fishId'))
  }
}

#' Return a string key composed by specified columns of a given table.
#'
#' @param tab the data table.
#' @param colIndex the key columns.
#' @param sep the characters pattern to separate fileds of the key.
#'
#' @return key.
#'
#'
#' @examples
#' #
#' @author Norbert Billet - IRD
#'
#' @export
fpKey <- function(tab, colIndex, sep=":-:") {
  key <- tab[, colIndex]
  if (length(colIndex) > 1) {
    key <- apply(key, 1, paste0, collapse=sep)
  }
  key <- gsub("[[:space:]]", "", key)
  return(key)
}
