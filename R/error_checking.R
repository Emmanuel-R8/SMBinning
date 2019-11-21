
# Check type and format of parameters which are broadly common to `smbinning` and
# `smbinning.factor`
haveParametersError <- function(df, x, y, xIsFactor = FALSE, ...) {

  returnMsg <- ""

  # Check data frame and formats
  if (!is.data.frame(df) | !is_tibble(df)) {
    ReturnMsg <- "Data df is not a data.frame (tibble not allowed)"
  } else if (!is.string(x)) {
    ReturnMsg <- "Variable x must be a string."
  } else if (!is.string(y)) {
    ReturnMsg <- "Variable y must be a string."
  } else if (str_detect(x, "\\.") == TRUE) {
    ReturnMsg <- "Variable x cannot contain a dot [.]."
  } else if (str_detect(y, "\\.") == TRUE) {
    ReturnMsg <- "Variable y cannot contain a dot [.]."
  } else {
    col_x <- which(names(df) == x)
    col_y <- which(names(df) == y)

    if (col_x == 0) {
      ReturnMsg <- "Variable x not found"
    } else if (xIsFactor == FALSE & !is.numeric(df[, col_x])) {
      ReturnMsg <- "Variable x is not numeric"
    } else if (xIsFactor == TRUE & !is.factor(df[, col_x])) {
      ReturnMsg <- "Variable x is not categorical (factor)"
    } else if (xIsFactor == TRUE & !between(length(levels(df[, col_x])), 2, maxcat)) {
      ReturnMsg <- "Variable x contains less than 2 or more than maxcat levels"
    } else if (xIsFactor == TRUE & !between(length(levels(df[, col_x])), 2, maxcat)) {
      ReturnMsg <- "Variable x contains less than 2 or more than maxcat levels"
    } else if (xIsFactor == TRUE & any(str_detect(df[, col_x], "\\,"))) {
      ReturnMsg <- "Values in variable x cannot contain commas"
    } else if (col_y == 0) {
      ReturnMsg <- "Variable y not found "
    } else if (!is.numeric(df[, col_y])) {
      ReturnMsg <- "Variable y is not numeric"
    } else if (max(df[, col_y], na.rm = TRUE) != 1) {
      ReturnMsg <- "Maximum of Variable y is not 1"
    } else if (tolower(y) == "default") {
      ReturnMsg <- "Variable name y cannot be named 'default'"
    } else if (sum(is.infinite(df[, x_col])) > 0) {
      ReturnMsg <- "Variable x contains positive or negative infinite values. Replace by NA"
    } else if (min(df[, col_y], na.rm = TRUE) != 0) {
      ReturnMsg <- "Minimum not 0"
    } else if (!is.numeric(df[, col_x])) {
      ReturnMsg <- "Characteristic (x) not found or it is not a number"
    } else if (length(unique(df[, col_x])) < 5) {
      ReturnMsg <- "Uniques values < 5"
    } else {
      returnMsg <- ""
    }
  }

  return(returnMsg)
}
