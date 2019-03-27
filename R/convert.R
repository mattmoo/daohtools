#' Convert dates of relevant columns by reference, so as not to copy big-as
#' tables (i.e. doesn't return anything, modifies the input.)
#'
#' @param input.dt The DAOH data with date columns
#' @param date.cols The columns to convert.
#'
#' @export
convert.date.cols = function(input.dt, date.cols, fmat = "%Y-%m-%d") {
  for (date.col in date.cols) {
    set(input.dt, j = date.col, value = as.IDate(strptime(as.list(input.dt[,..date.col])[[1]], fmat)))
  }
}

#' Convert dates of relevant columns by reference, so as not to copy big-as
#' tables (i.e. doesn't return anything, modifies the input.) This one uses
#' fasttime, which is a lot faster (...) if you've got big big data. However,
#' times need to be in a specific format.
#'
#' @param input.dt The DAOH data with date columns
#' @param date.cols The columns to convert.
#'
#' @export
convert.date.cols.fasttime = function(input.dt, date.cols) {
  for (date.col in date.cols) {
    set(input.dt, j = date.col, value = (fastPOSIXct(input.dt[,..date.col][[1]])))
  }
}
