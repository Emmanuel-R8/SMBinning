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
  assertDataFrame(df)
  assertString(x)
  assertString(y)
  assertFlag(verbose)


  # Make symbol out of name strings
  xSym <- sym(x)
  ySym <- sym(y)

  # Enforce x to be a factor
  df <- df %>%
    mutate(!!xSym := as_factor(!!xSym))



  # Check number of categories
  nCat <- df %>% select(!!xSym) %>% n_distinct()
  if (verbose == TRUE) {
    cat("Number of categories: ", nCat, "\n")
    cat("Categories: ", levels(df[, x, drop = TRUE]), "\n")
  }

  assertNumber(maxCategories)
  assertNumber(nCat, upper = maxCategories)

  # Make sure that the content of df[,y] is boolean
  df <- ensureLogical(df, y, verbose = verbose)


  # Calculate total Goods and Bads
  totalGood  <- df %>% filter(!!ySym == TRUE)  %>% nrow()
  totalBad   <- df %>% filter(!!ySym == FALSE) %>% nrow()
  totalCount <- totalGood + totalBad

  result <- df %>% select(!!xSym, !!ySym)

  result <- result %>%
    # Goods and Bads in each category
    group_by(!!xSym, !!ySym) %>%
    mutate(Count = n()) %>%
    ungroup() %>%

    # All counts are identical for each unique pair (x, y)
    distinct(!!xSym, !!ySym, .keep_all = TRUE) %>%

  # Place the Good/Bad counts where they should be
    pivot_wider(names_from = !!ySym, values_from = Count)

  # Remove any NA's in case some categories didn't have any true or false
  result[is.na(result)] <- 0

  result <- result %>%

    # Rename to sensible names
    rename(nGood = "TRUE", nBad = "FALSE") %>%

    # Add all the other columns
    mutate(Count = nGood + nBad,
           cumCount = cumsum(Count),
           cumGood  = cumsum(nGood),
           cumBad   = cumsum(nBad),
           pctCount = Count / totalCount,
           pctGood  = nGood / totalGood,
           pctBad   = nBad  / totalBad,
           Odds     = pctGood / (1 - pctGood),
           LnOdds   = log(Odds),
           WoE      = log(pctGood) - log(pctBad),
           IV       = (pctGood - pctBad) * WoE)

  return(list(IV = sum(result$IV[!is.infinite(result$IV)]),
              type = "categorical",
              table = result))
}
