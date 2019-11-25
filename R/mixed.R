


#' @include common.R
#'
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
#' @param verbose Boolean to add additional information. Default is \code{FALSE}
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



#' Create a table that categorises a variable into the bins described by a WoE table
#'
#' This function takes a WoE table of a particular variable describing its bins, and takes the observations of the
#' variable. It creates a tibble of variables (one per bin) containing 0 or 1 whether the original observations
#' fall into the relevant bin.
#'
#' @param df Dataframe containing the at least one column named `varName`
#' @param varName Name of the variable (string format) which will be binned follwoing its Weight of Evidence
#' @param woeTable Tibble containing the Weight of Evidence table created by WoETable for `varName`
#' @param verbose Boolean to add additional information. Default is \code{FALSE}
#' @return Tibble of categories containing 0/1
#' @import checkmate
#' @import tidyverse
#' @export
#'
#' @examples
categoriseFromWoE <- function(df,
                              varName,
                              woeTable,
                              verbose = FALSE) {
  assertDataFrame(df)
  assertTibble(woeTable)
  assertString(varName)

  # Check if there is a variable with that name in df
  index <- which(names(df) == varName)
  if (index == 0) {
    stop("df does not contain a variable named varName")
  }

  if (verbose == TRUE) {
    cat("categoriseFromWoE with variable ", varName, "\n")
    cat("All asserts are OK \n")
    cat("Variable has index ", index, " in dataframe \n")
  }


  vSym <- sym(varName)
  vCleanName <- str_remove(varName, " ")
  if (verbose == TRUE) {
    cat("Clean variable name: \"", vCleanName, "\"\n")
  }

  if (names(woeTable)[1] == "CutNumber") {
    vType <- "numeric"
  } else{
    vType <- "categorical"
  }
  if (verbose == TRUE) {
    cat("Variable type is", vType, "\n")
  }


  binned <- df[, varName]

  if (vType == "categorical") {
    if (verbose == TRUE) {
      cat("Type is categorical\n")
    }

    # Go though each bin
    for (b in 1:nrow(woeTable)) {
      # Current bin factor name
      vFactor <- woeTable[[b, varName]]
      if (verbose == TRUE) {
        cat("Current factor is:", as.character(vFactor), "\n")
      }

      if (is.na(vFactor)) {
        vColumn <- paste0(vCleanName, "=NA")
        if (verbose == TRUE) {
          cat("Creating column ", vColumn, "\n")
        }
        vColumn <- sym(vColumn)

        # Create a new column
        binned <- binned %>%

          # Create a new column containing whether the variable is in the bin
          mutate(!!vColumn := if_else(is.na(!!vSym), 1, 0))

      } else {
        # create bin name
        vColumn <- paste0(vCleanName, "=", as.character(vFactor))
        if (verbose == TRUE) {
          cat("Creating column ", vColumn, "\n")
        }
        vColumn <- sym(vColumn)

        # Create a new column
        binned <- binned %>%

          # Create a new column containing whether the variable is in the bin
          mutate(!!vColumn := if_else(is.na(!!vSym),
                                      0,
                                      if_else(!!vSym ==  vFactor, 1, 0)))
      }
    }


    # Else is variable type is numeric
  } else {
    if (verbose == TRUE) {
      cat("Type is numeric\n")
    }

    # Go though each bin
    for (b in 1:nrow(woeTable)) {
      # Min and Max of current bin
      vMin <- woeTable[[b, "Min"]]
      vMax <- woeTable[[b, "Max"]]
      if (verbose == TRUE) {
        cat(
          "Current bin is: Min = ",
          as.character(vMin),
          " and Max = ",
          as.character(vMax),
          "\n"
        )
      }


      if (is.na(vMin)) {
        vColumn <- paste0(vCleanName, "=NA")
        if (verbose == TRUE) {
          cat("Creating column ", vColumn, "\n")
        }
        vColumn <- sym(vColumn)

        # Create a new column
        binned <- binned %>%

          # Create a new column containing whether the variable is in the bin
          mutate(!!vColumn := if_else(is.na(!!vSym), 1, 0))

      } else {
        # create bin name
        vColumn <-
          paste0(as.character(round(vMin, digits = 3)),
                 "<",
                 vCleanName,
                 "<=",
                 as.character(round(vMax, digits = 3)))
        if (verbose == TRUE) {
          cat("Creating column ", vColumn, "\n")
        }
        vColumn <- sym(vColumn)

        # Create a new column
        binned <- binned %>%

          # Create a new column containing whether the variable is in the bin
          dplyr::mutate(!!vColumn := dplyr::if_else(
            is.na(!!vSym),
            0,
            dplyr::if_else((!!vSym !=  vMin) &
                             dplyr::between(!!vSym, vMin, vMax),
                           1,
                           0
            )
          ))
      }
    }

  }

  # Remove the original variable to only keep the bins
  #binned <- binned %>% select(-!!vSym)

  return(binned[, 2:length(names(binned))])

}


