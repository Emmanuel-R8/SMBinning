#' Ensure that the content of df[,y] is boolean
#'
#' This function is not exported and used in WoE table creation
#'
#' @param df
#' @param y
#'
#' @return Reformatted data frame
#' @examplesm [TODO]
#'
ensureLogical <- function(df, y, verbose = FALSE) {
  #
  assertDataFrame(df)
  assertString(y)

  if (verbose == TRUE) {
    cat("df is a dataframe with variables: ", names(df), "\n")
    cat("Conversion of y (", y, ") to logical.\n \n")
    cat("Starting values of y: ", as.character(head(df[, y])), "\n")
  }

  # Make symbol out of name strings
  ySym <- sym(y)

  #
  # Check type of y
  #
  yClass <- class(df[[1, y]])

  # If class is already logical, all OK
  if (yClass == "logical") {
    if (verbose == TRUE) {
      cat("y is logical: OK")
    }
  } else {
    # Otherwise, first transforms y to factors
    if (verbose == TRUE) {
      cat("y is not a logical variable. \n")
    }

    df <- df %>%
      mutate(!!ySym := as_factor(!!ySym))

    # For some reason, levels doesn't work on tibles
    dfLevels <- levels(as.data.frame(df)[, y])

    if (verbose == TRUE) {
      cat("After conversion to factors, factors are: ",
          dfLevels, " --- and starting factors have become:",
          as.character(head(df[, y])),
          "\n")
    }


    # Check only 2 factors
    numberLevelsy <- length(dfLevels)
    if (verbose == TRUE) {
      cat(
        "After conversion to factors, y has ",
        numberLevelsy,
        " levels. (Error will be thrown if not 2)\n"
      )
    }
    assert(numberLevelsy == 2)

    if (verbose == TRUE) {
      cat("Factors are: ", dfLevels, "\n")
    }

    # and transforms the factors into booleans (should reflect alphabetical order which is good since 0/1 and FALSE/TRUE work like that)
    df[, y] <- ifelse(df[, y] == tmp[1], FALSE, TRUE)
    if (verbose == TRUE) {
      cat(tmp[1],
          " recoded as logical FALSE; ",
          tmp[2],
          " recoded as logical TRUE.\n")
    }
  }

  return(df)

}