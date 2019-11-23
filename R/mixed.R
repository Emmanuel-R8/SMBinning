#' @include common.R
#' @include continuous.R
#' @include categorical.R
#'
#' Weight of Evidence and Information Value for a single variable
#'
#' This function calculates the Weight of Evidence and Infomation value for a particular response. The variable tested
#' is either continuous or categorical. The response is assumed to be logical. If not, if the response takes only 2
#' values (either factor or numerical), it will be transformed into logical values. `verbose = TRUE` shows the
#' conversion. `p` and `maxCategories` parameters for continuous and categorical variables can be passed.

#' @param df
#' @param x
#' @param y
#' @param p
#' @param verbose
#'
#' @import checkmate
#' @import tidyverse
#' @import partykit
#'
#' @export
#'
#' @examples [TODO]
#'
WoETable <-
  function(df,
           x,
           y,
           verbose = FALSE,
           p = -1,
           maxCategories = -1) {
    #
    if (verbose == TRUE) {
      cat("Variable ", x)
    }

    assertDataFrame(df)
    assertString(x)
    assertString(y)
    assertFlag(verbose)
    assertNumber(p)
    assertNumber(maxCategories)

    # Format df as tibble
    df <- as_tibble(df)

    # Ensure reasonable defaults for parameters
    if (p == -1) {
      p <- 0.05
    }
    if (maxCategories == -1) {
      maxCategories <-  5
    }


    # Dispacth on the variable type of column x
    if (class(as.data.frame(df)[[1, x]]) == "factor") {
      if (verbose == TRUE) {
        cat(" is categorical \n")
      }

      # Make sure that the content of df[,y] is boolean
      df <- ensureLogical(df, y, verbose = verbose)

      result <- WoETableCategorical(
        df = df,
        x = x,
        y = y,
        maxCategories = maxCategories,
        verbose = verbose
      )
    } else if (class(as.data.frame(df)[[1, x]]) == "numeric") {
      if (verbose == TRUE) {
        cat(" is numeric. \n")
      }
      result <- WoETableContinuous(
        df = df,
        x = x,
        y = y,
        p = p,
        verbose = verbose
      )
    } else{
      result = NA
      stop("Variable x is neither categorical nor numeric.")

    }

    return(result)
  }
