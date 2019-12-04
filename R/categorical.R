#' @include common.R
#'
#' Weight of Evidence and Information Value for a categorical variable
#'
#' This function calculates the Weight of Evidence and Infomation value for a particular response.
#' The variable tested is categorical (factors). The response is assumed to be logical. If not, if
#' the response takes only 2 values (either factor or numerical), it will be transformed into
#' logical values. `verbose = TRUE` shows the conversion.
#'
#' @param df Dataframe containing the two columns of data
#' @param x  Name of the categorical variable as a string
#' @param y  Name of the logical response variable as a string.
#' @param maxCategories  maximum number of accepted categories. Default is 5.
#' @param verbose Boolean to add additional information. Default is \code{FALSE}
#'
#' @return A list with the total Information Value, types of bins, and a tibble containing the bins with detailed
#'  information.
#'
#' @import checkmate
#' @import tidyverse
#'
#' @export
#'
#' @examples [TODO]
WoETableCategorical <- function(df,
                                x,
                                y,
                                maxCategories = 5,
                                verbose = FALSE) {
  checkmate::assertDataFrame(df)
  checkmate::assertString(x)
  checkmate::assertString(y)
  checkmate::assertFlag(verbose)


  # Make symbol out of name strings
  xSym <- dplyr::sym(x)
  ySym <- dplyr::sym(y)

  # Enforce x to be a factor
  df <- df %>%
    dplyr::mutate(!!xSym := as_factor(!!xSym))



  # Check number of categories
  nFactor <- df %>% dplyr::select(!!xSym) %>% dplyr::n_distinct()
  if (verbose == TRUE) {
    cat("Number of categories: ", nFactor, "\n")
    cat("Categories: ", levels(df[, x, drop = TRUE][[1]]), "\n")
  }

  checkmate::assertNumber(maxCategories)
  checkmate::assertNumber(nFactor, upper = maxCategories)

  # Make sure that the content of df[,y] is boolean
  df <- ensureLogical(df, y, verbose = verbose)
  if (verbose == TRUE) {
    cat("DOUBLE CHECK: dataframe dimension after ensureLogical: ", dim(df), "\n")
    cat("Selecting only two columns to work with: ", x, "and ", y, "\n\n")
  }

  df <- df %>% dplyr::select(!!xSym, !!ySym)
  factorNames <- levels(df[, x][[1]])

  if (verbose == TRUE) {
    cat("DOUBLE CHECK: Number of categories: ", nFactor, "  ---  ",
        "Categories: ", as.character(factorNames), "\n")
    cat("DOUBLE CHECK: dataframe dimension: ", dim(df), "\n")
  }

  # Prepare a result tibble
  result <-
    dplyr::tibble(!!xSym := factorNames) %>%
    dplyr::mutate(
      Count       = 0.0,
      nGood       = 0.0,
      nBad        = 0.0,
      cumCount    = 0.0,
      cumGood     = 0.0,
      cumBad      = 0.0,
      pctCount    = 0.0,
      pctGood     = 0.0,
      pctBad      = 0.0,
      OddsInBin   = 0.0,
      LnOddsInBin = 0.0,
      WoEInBin    = 0.0,
      IVInBin     = 0.0,
      WoE         = 0.0,
      IV          = 0.0
    )

  # Calculate total Goods and Bads
  totalGood  <- (df %>% dplyr::filter(!!ySym == TRUE)  %>% nrow())
  totalBad   <- (df %>% dplyr::filter(!!ySym == FALSE) %>% nrow())
  totalCount <- totalGood + totalBad

  # For each factor/bin, fill in the number of Goods and Bads
  for (currentBin in 1:nFactor) {

    # Name of the factor being considered
    factorName <- factorNames[currentBin]

    if (verbose == TRUE) {
      cat(" Bin row number: ", currentBin,
          ", name = ", factorName, "\n")
    }

    # Select the data with that factor
    xBand <- df %>% dplyr::filter(!!xSym == factorName)

    result[currentBin, "nGood"] <- nrow(xBand %>% dplyr::filter(!!ySym == TRUE))
    result[currentBin, "nBad"]  <- nrow(xBand %>% dplyr::filter(!!ySym == FALSE))
  }


  # Remove any NA's in case some categories didn't have any true or false
  # otherwise this immediately leads to infinite Information Values
  result[is.na(result)] <- 0


  # Fill the rest of the columns
  result <-
    result %>%
    dplyr::mutate(
      Count       = nGood + nBad,
      cumCount    = cumsum(Count),
      cumGood     = cumsum(nGood),
      cumBad      = cumsum(nBad),

      pctCount    = Count / totalCount,
      pctGood     = nGood / totalGood,
      pctBad      = nBad  / totalBad,
      pctGoodBin  = nGood / Count,
      pctBadBin   = nBad  / Count,

      OddsInBin   = pctGoodBin / (1 - pctGoodBin),
      LnOddsInBin = log(OddsInBin),

      WoEInBin    = log(pctGoodBin) - log(pctBadBin),
      IVInBin     = (pctGoodBin - pctBadBin) * WoE,

      WoE         = log(pctGood) - log(pctBad),
      IV          = (pctGood - pctBad) * WoE
    )

  if (verbose == TRUE) {
    cat("Columns in the result dataframe before adding the WoE and IV: ", names(result), "\n")
    cat("Categories: ", levels(df[, x, drop = TRUE]), "\n")
  }


  return(list(
    IV = sum(result$IV[!is.infinite(result$IV)]),
    type = "categorical",
    table = result
  ))
}
