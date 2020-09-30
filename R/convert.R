#' Convert dates of relevant columns by reference, so as not to copy big-as
#' tables (i.e. doesn't return anything, modifies the input.)
#' Should be able to detect automatically if date.cos is false
#'
#' @param input.dt The DAOH data with date columns
#' @param date.col.names The columns to convert.
#'
#' @export
convert.date.cols = function(input.dt, date.col.names, fmat = NULL) {

  for (date.col.name in date.col.names) {
    blanks = input.dt[, get(date.col.name) == ""]
    if (any(blanks)) {
      set(x = input.dt,
          i = which(blanks),
          j = date.col.name,
          value = NA)
    }
    set(x = input.dt,
        j = date.col.name,
        value = as.Date(input.dt[, get(date.col.name)], 
                        tryFormats = c(fmat, "%Y-%m-%d", "%d/%m/%Y")))
  }
}

#' Convert dates of relevant columns by reference, so as not to copy big-as
#' tables (i.e. doesn't return anything, modifies the input.) This one uses
#' fasttime, which is a lot faster (...) if you've got big big data. However,
#' times need to be in a specific format.
#'
#' @param input.dt The DAOH data with date columns
#' @param date.col.names The columns to convert.
#'
#' @export
convert.date.cols.fasttime = function(input.dt, date.col.names) {
  for (date.col in date.cols) {
    set(input.dt, j = date.col, value = (fastPOSIXct(input.dt[,..date.col][[1]])))
  }
}


#' Tries to detect date columns by looking at the first entry. Doesn't work as such.
#' @param input.dt The DAOH data with date columns
#' @param date.col.names The columns to convert.
#'
#' @export
convert.date.cols.auto = function(input.dt, date.col.names = NULL) {
  # if (is.null(date.col.names)) {
  #   date.col.names = names(input.dt)[!is.na(sapply(as.list(input.dt[1]),
  #                                                  function(x)
  #                                                    if (is.character(x)) {
  #                                                      as.Date(x,
  #                                                              tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
  #                                                    }))]
  #   for
  #   message(paste0('Date columns detected: ', paste(date.col.names, collapse = ', ')))
  # }
  # for (date.col.name in date.col.names) {
  #   set(x = input.dt,
  #       j = date.col.name,
  #       value = as.Date(unlist(input.dt[, date.col.name, with = F]),
  #                       tryFormats = c("%Y-%m-%d", "%d/%m/%Y")))
  # }
  NULL
}
