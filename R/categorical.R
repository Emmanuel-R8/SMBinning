
#' Weight of Evidence and Information Value for a categorical variable
#'
#' @param df Dataframe containing the two columns of data
#' @param x  Name of the categorical variable as a string
#' @param y  Name of the boolean response as a string
#' @param maxCategories  maximum number of accepted categories. Default is 5.
#' @param verbose Boolean to add additional information. Default is \code{FALSE}
#'
#' @return A list with the total Information Value, and a tibble containing the bins with detailed
#'  information.
#'
#' @import checkmate
#' @import tidyverse
#'
#' @export
#'
#' @examples [TODO]
binTableCategorical <- function(df,
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

  # Calculate total Goods and Bads
  totalGood  <- (df %>% filter(!!ySym == TRUE)  %>% nrow())
  totalBad   <- (df %>% filter(!!ySym == FALSE) %>% nrow())
  totalTotal <- totalGood + totalBad

  result <- df %>% select(!!xSym, !!ySym)

  result <- result %>%
    # Goods and Bads in each category
    group_by(!!xSym, !!ySym) %>%
    mutate(Count = n()) %>%
    ungroup() %>%

    # All counts are identical for each unique pair (x, y)
    distinct(!!xSym, !!ySym, .keep_all = TRUE) %>%

    # Place the Good/Bad counts where they should be
    pivot_wider(names_from = !!ySym, values_from = Count) %>%

    # Rename to sensible names
    rename(nGood = "TRUE", nBad = "FALSE") %>%

    # Add all the other columns
    mutate(Count = nGood + nBad,
           cumCount = cumsum(Count),
           cumGood  = cumsum(nGood),
           cumBad   = cumsum(nBad),
           pctGood  = nGood / totalGood,
           pctBad   = nBad  / totalBad,
           Odds     = pctGood / (1 - pctGood),
           LnOdds   = log(Odds),
           WoE      = log(pctGood) - log(pctBad),
           IV       = (pctGood - pctBad) * WoE)

  return(list(IV = sum(result$IV), table = result))
}
