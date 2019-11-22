


#' Weight of Evidence and Information Value for a continuous variable
#'
#' This function calculates the Weight of Evidence and Infomation value for a particular response.
#' The variable tested is continuous. The response is assumed to be logical. If not, if
#' the response takes only 2 values (either factor or numerical), it will be transformed into
#' logical values. `verbose = TRUE` shows the conversion.
#'
#' @param df Dataframe containing the two columns of data
#' @param x  Name of the continuous variable as a string
#' @param y  Name of the boolean response as a string
#' @param p  Parameter for the Conditional Recuresive Tree used (see \code{partykit})
#' @param verbose Boolean to add additional information. Default is \code{FALSE}
#'
#' @return A list with the total Information Value, and a tibble containing the bins with detailed
#'  information.
#'
#' @import checkmate
#' @import tidyverse
#' @import partykit
#'
#' @export
#'
#' @examples [TODO]
binTableContinuous <-
  function(df, x, y, p = 0.05, verbose = FALSE) {
    assertDataFrame(df)
    assertString(x)
    assertString(y)
    assertNumber(p, lower = 0, upper = 0.5)
    assertFlag(verbose)

    # Check type of y
    if (is_logical(df[, y])) {
      if (verbose == TRUE) {
        cat("y is logical: OK")
      }
    } else {

      # Transforms y to factors
      df[, y] <- as_factor(df[, y])

      tmp <- nlevels(df[, y])
      if (verbose == TRUE) {
        cat("y is not a logical variable. After conversion to factors, y has ", tmp, " levels. (Error if not 2)\n")
      }
      assert(tmp == 2)

      tmp <- levels(df[, y])
      if (verbose == TRUE) {
        cat("Factors are: ", tmp, "\n")
      }

      df[, y] <- ifelse( df[, y] == tmp[1], FALSE, TRUE)
      if (verbose == TRUE) {
        cat(tmp[1], " recoded as logical FALSE; ", tmp[2], " recoded as logical TRUE.\n")
      }
    }


    if (verbose == TRUE) {
      cat("Start recursive partitioning.")
      startTime <- Sys.time()
    }

    # Conditional recursive partioning requires 0/1 variable
    treeFrame <- df[,c(x, y)]
    treeFrame[, y] <- ifelse(treeFrame[, y], 1, 0)

    treeControl <-
      ctree_control(minbucket = ceiling(round(p * nrow(df))))

    binTree <- ctree(
      formula(paste(y, "~", x)),
      data = treeFrame,
      na.action = na.exclude,
      control = treeControl
    )

    if (verbose == TRUE) {
      cat("  Elapsed: ", Sys.time() - startTime, "\n")
    }

    bins <- width(binTree)
    if (verbose == TRUE) {
      cat("Number of bins = ", bins, "\n")
    }
    if (!testNumber(bins, lower = 2)) {
      warning("Number of bins is not at least 2. Returning NA")
      return(NA)
    }

    # Number of nodes
    nNodes <- length(binTree)
    if (verbose == TRUE) {
      cat("Number of nodes = ", nNodes, "\n")
    }


    # Collect all the cutting points
    listCuts <- tibble(CutNumber = 1:nNodes, CutPoint = 0.0)
    for (i in 1:nNodes) {
      listCuts[i, 2] <-
        ifelse(is.numeric(binTree[i]$node$split$breaks),
               binTree[i]$node$split$breaks,
               NA)
    }

    # Make symbol out of name strings
    xSym <- sym(x)
    ySym <- sym(y)

    # Min and Max of x
    xMin <- min(df %>% select(!!x) %>% filter(!is.na(!!xSym)))
    xMax <- max(df %>% select(!!x) %>% filter(!is.na(!!xSym)))

    # Clean up the list of Cuts and add one for everything above the last cut
    listCuts <- listCuts %>%
      filter(!is.na(CutPoint)) %>%
      arrange(CutPoint) %>%
      add_row(CutPoint = -Inf, .before = 1) %>%
      add_row(CutPoint = xMax) %>%
      mutate(CutNumber = row_number() - 1)


    # Prepare a result tibble
    result <- listCuts %>%
      mutate(
        Min = 0.0,
        Max = 0.0,
        Count = 0.0,
        nGood = 0.0,
        nBad = 0.0,
        cumCount = 0.0,
        cumGood = 0.0,
        cumBad = 0.0,
        pctCount = 0.0,
        pctGood = 0.0,
        pctBad = 0.0,
        Odds = 0.0,
        LnOdds = 0.0,
        WoE = 0.0,
        IV = 0.0
      )

    # Calculate total Goods and Bads
    totalGood  <- (df %>% filter(!!ySym == TRUE)  %>% nrow())
    totalBad   <- (df %>% filter(!!ySym == FALSE) %>% nrow())
    totalCount <- totalGood + totalBad

    totalBins <- nrow(listCuts)

    for (currentCut in 2:totalBins) {
      minBinCut <- as.numeric(listCuts[currentCut - 1, "CutPoint"])
      maxBinCut <- as.numeric(listCuts[currentCut,     "CutPoint"])

      if (verbose == TRUE) {
        cat(" Cut row number: ",
            currentCut,
            ", min = ",
            minBinCut,
            ", max = ",
            maxBinCut,
            "\n")
      }

      xBand <- df %>%
        select(!!xSym, !!ySym) %>%
        filter(between(!!xSym, minBinCut, maxBinCut) &
                 (!!xSym) != minBinCut)

      result[currentCut, "Min"]      <- minBinCut
      result[currentCut, "Max"]      <- maxBinCut
      result[currentCut, "Count"]    <- nrow(xBand)
      result[currentCut, "nGood"]    <-
        nrow(xBand %>% filter(!!ySym == TRUE))
      result[currentCut, "nBad"]     <-
        nrow(xBand %>% filter(!!ySym == FALSE))
    }

    # Remove any NA's in case some categories didn't have any true or false
    # otherwise this immediately leads to infinite Information Values
    result[is.na(result)] <- 0


        # Add row for missing / NA values
    xBand <- df %>%
      select(!!xSym, !!ySym) %>%
      filter(is.na(!!xSym) & !is.na(!!ySym))

    cat("Number of missing/NA values: ", nrow(xBand), "\n")
    if (nrow(xBand) > 0) {
      result <- result %>% add_row(CutNumber = totalBins)


    result[totalBins + 1, "CutPoint"] <- NA
    result[totalBins + 1, "Min"]      <- NA
    result[totalBins + 1, "Max"]      <- NA
    result[totalBins + 1, "Count"]    <- nrow(xBand)
    result[totalBins + 1, "nGood"]    <-
      nrow(xBand %>% filter(!!ySym == TRUE))
    result[totalBins + 1, "nBad"]     <-
      nrow(xBand %>% filter(!!ySym == FALSE))
    }

    # Fill the rest of the columns
    result <-
      result %>%
      mutate(
        cumCount = cumsum(Count),
        cumGood  = cumsum(nGood),
        cumBad   = cumsum(nBad),
        pctCount = Count / totalCount,
        pctGood  = nGood / totalGood,
        pctBad   = nBad  / totalBad,
        Odds     = pctGood / (1 - pctGood),
        LnOdds   = log(Odds),
        WoE      = log(pctGood) - log(pctBad),
        IV       = (pctGood - pctBad) * WoE
      )

    # Remove the first -Infinity cut point
    result <- result %>% slice(2:n())

    return(list(IV = sum(result$IV[!is.infinite(result$IV)]), table = result))
  }
