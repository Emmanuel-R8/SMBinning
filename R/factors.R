#' @include error_checking.R

#' Binning on Factor Variables
#'
#' Generates a table with relevant metrics for all the categories of a given factor variable.
#' @param df A data frame.
#' @param y Binary response variable (0,1). Integer (\code{int}) is required.
#' Name of \code{y} must not have a dot.
#' @param x A factor variable with at least 2 different values. Labesl with commas are not allowed.
#' @param maxcat Specifies the maximum number of categories.  Default value is 10.
#' Name of \code{x} must not have a dot.
#' @return The command \code{smbinning.factor} generates and object containing the necessary info and utilities for binning.
#' The user should save the output result so it can be used
#' with \code{smbinning.plot}, \code{smbinning.sql}, and \code{smbinning.gen.factor}.
#' @examples
#' # Load library and its dataset
#' library(smbinning) # Load package and its data
#'
#' # Binning a factor variable
#' result <- smbinning.factor(smbsimdf1, x = "inc", y = "fgood", maxcat = 11)
#' result$ivtable
#' @export
smbinning.factor <- function(df, y, x, maxcat = 10) {
  requireNamespace("gsubfn")
  requireNamespace("sqldf")

  # Check data frame and formats
  msg <- haveParametersError(df, x, y, xIsFactor = TRUE, maxcat = maxcat)
  tryCatch(
    {
      assertthat::assert_that(msg == "")
    },
    error = function(e) {
      message(msg)
      return(NA)
    }
  )

  col_x <- which(names(df) == x) # Find Column for independant
  col_y <- which(names(df) == y) # Find Column for dependant


  # Append cutpoints in a table (Automated)
  # cutvct=data.frame(matrix(ncol=0,nrow=0))

  # Shell
  cutvct <- c()
  cuts <- fn$sqldf("select distinct $x from df where $x is not NULL")
  cuts <- as.vector(as.matrix(cuts))

  # Number of cutpoints
  n <- length(cuts)

  # At least 1 cutpoint
  if (n < 1) {
    return("No Bins")
  }

  for (row_i in 1:n) {
    cutvct <- rbind(cutvct, cuts[row_i])
  }

  # Sort / converts to a ordered vector (asc)
  cutvct <- cutvct[order(cutvct[, 1]), ]

  # Build Information Value Table
  # Counts per not missing cutpoint
  ivt <- data.frame(matrix(ncol = 0, nrow = 0)) # Shell

  # Number of cutpoits
  n <- length(cutvct)
  for (row_i in 1:n) {
    cutpoint <- cutvct[row_i]
    ivt <- rbind(
      ivt,
      gsubfn::fn$sqldf(
        "select '= ''$cutpoint''' as Cutpoint,
                  sum(case when $x = '$cutpoint' and $y in (1,0) then 1 else 0 end) as CntRec,
                  sum(case when $x = '$cutpoint' and $y=1 then 1 else 0 end) as CntGood,
                  sum(case when $x = '$cutpoint' and $y=0 then 1 else 0 end) as CntBad,
                  NULL as CntCumRec,
                  NULL as CntCumGood,
                  NULL as CntCumBad,
                  NULL as PctRec,
                  NULL as GoodRate,
                  NULL as BadRate,
                  NULL as Odds,
                  NULL as LnOdds,
                  NULL as WoE,
                  NULL as IV
                  from df where $x is not NULL and $y is not NULL"
      )
    )
  }
  # Missing Data
  x.na <- sum(is.na(df[, x]))
  y.na <- sum(is.na(df[, y]))

  if (x.na > 0) {
    ivt <- rbind(
      ivt,
      gsubfn::fn$sqldf(
        "select 'Missing' as Cutpoint,
                  sum(case when $x is NULL and $y in (1,0) then 1 else 0 end) as CntRec,
                  sum(case when $x is NULL and $y=1 then 1 else 0 end) as CntGood,
                  sum(case when $x is NULL and $y=0 then 1 else 0 end) as CntBad,
                  NULL as CntCumRec,
                  NULL as CntCumGood,
                  NULL as CntCumBad,
                  NULL as PctRec,
                  NULL as GoodRate,
                  NULL as BadRate,
                  NULL as Odds,
                  NULL as LnOdds,
                  NULL as WoE,
                  NULL as IV
                  from df where $y is not NULL"
      )
    )
  } else {
    ivt <- rbind(
      ivt,
      c("Missing", 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    )
  }

  # Total
  ivt <- rbind(
    ivt,
    gsubfn::fn$sqldf(
      "select 'Total' as Cutpoint,
                count(*) as CntRec,
                sum(case when $y=1 then 1 else 0 end) as CntGood,
                sum(case when $y=0 then 1 else 0 end) as CntBad,
                NULL as CntCumRec,
                NULL as CntCumGood,
                NULL as CntCumBad,
                NULL as PctRec,
                NULL as GoodRate,
                NULL as BadRate,
                NULL as Odds,
                NULL as LnOdds,
                NULL as WoE,
                NULL as IV
                from df where $y is not NULL"
    )
  )

  # Covert table to numeric
  options(warn = -1)
  ncol <- ncol(ivt)
  for (col_i in 2:ncol) {
    ivt[, col_i] <- as.numeric(ivt[, col_i])
  }
  options(warn = 0)

  # Complete Table: 1st row
  ivt[1, 5] <- ivt[1, 2] # Nbr Cum. Records
  ivt[1, 6] <- ivt[1, 3] # Nbr Cum. Goods
  ivt[1, 7] <- ivt[1, 4] # Nbr Cum. Bads

  # From 2nd row
  n <- nrow(ivt) - 2
  for (row_i in 2:n) {
    ivt[row_i, 5] <- ivt[row_i, 2] + ivt[row_i - 1, 5]
    ivt[row_i, 6] <- ivt[row_i, 3] + ivt[row_i - 1, 6]
    ivt[row_i, 7] <- ivt[row_i, 4] + ivt[row_i - 1, 7]
  }

  ivt[2, 5] <- ivt[2, 2] + ivt[1, 5]
  ivt[2, 6] <- ivt[2, 3] + ivt[1, 6]
  ivt[2, 7] <- ivt[2, 4] + ivt[1, 7]

  # Missing row
  ivt[col_y + 1, 5] <- ivt[col_y, 5] + ivt[col_y + 1, 2]
  ivt[col_y + 1, 6] <- ivt[col_y, 6] + ivt[col_y + 1, 3]
  ivt[col_y + 1, 7] <- ivt[col_y, 7] + ivt[col_y + 1, 4]

  # Calculating metrics

  # Remove Scientific Notation
  options(scipen = 999)
  ivt[, 8] <- round(ivt[, 2] / ivt[col_y + 2, 2], 4) # PctRec
  ivt[, 9] <- round(ivt[, 3] / ivt[, 2], 4) # GoodRate
  ivt[, 10] <- round(ivt[, 4] / ivt[, 2], 4) # BadRate
  ivt[, 11] <- round(ivt[, 3] / ivt[, 4], 4) # Odds
  ivt[, 12] <- round(log(ivt[, 3] / ivt[, 4]), 4) # LnOdds

  G <- ivt[col_y + 2, 3]
  B <- ivt[col_y + 2, 4]
  LnGB <- log(G / B) # IV Part 1

  # WoE
  ivt[, 13] <- round(log(ivt[, 3] / ivt[, 4]) - LnGB, 4)

  # Mg IV
  ivt[, 14] <- round(ivt[, 13] * (ivt[, 3] / G - ivt[, 4] / B), 4)

  # Calculates Information Value even with undefined numbers
  ivt[col_y + 2, 14] <- 0.0
  for (k in 1:(nrow(ivt) - 1))
  {
    if (is.finite(ivt[k, 14])) {
      mgiv <- ivt[k, 14]
    } else {
      mgiv <- 0.0000
    }
    ivt[col_y + 2, 14] <- ivt[col_y + 2, 14] + mgiv
  }
  iv <- ivt[col_y + 2, 14]
  # End Inf. Value Table

  list(
    ivtable = ivt,
    iv = iv,
    x = x,
    col_id = col_x,
    cuts = cutvct
  )
}
# End Binning Factors




# Begin Custom Binning Factors
#
#' Customized Binning on Factor Variables
#'
#' Gives the user the ability to combine categories and create new attributes for a given characteristic.
#' Once these new attribues are created in a list (called \code{groups}), the funtion generates a table for
#' the uniques values of a given factor variable.
#' @param df A data frame.
#' @param y Binary response variable (0,1). Integer (\code{int}) is required.
#' Name of \code{y} must not have a dot.
#' @param x A factor variable with at least 2 different values. Value \code{Inf} is not allowed.
#' @param groups Specifies customized groups created by the user.
#' Name of \code{x} must not have a dot.
#' @return The command \code{smbinning.factor.custom} generates an object containing the necessary information
#' and utilities for binning.
#' The user should save the output result so it can be used
#' with \code{smbinning.plot}, \code{smbinning.sql}, and \code{smbinning.gen.factor}.
#' @examples
#' # Load library and its dataset
#' library(smbinning) # Load package and its data
#'
#' # Example: Customized binning for a factor variable
#' # Notation: Groups between double quotes
#' result <- smbinning.factor.custom(
#'   smbsimdf1,
#'   x = "inc",
#'   y = "fgood",
#'   c(
#'     "'W01','W02'", # Group 1
#'     "'W03','W04','W05'", # Group 2
#'     "'W06','W07'", # Group 3
#'     "'W08','W09','W10'"
#'   )
#' ) # Group 4
#' result$ivtable
#' @export
smbinning.factor.custom <- function(df, y, x, groups) {
  # Check data frame and formats
  msg <- haveParametersError(df, x, y, xIsFactor = TRUE)
  tryCatch(
    {
      assertthat::assert_that(msg == "")
    },
    error = function(e) {
      message(msg)
      return(NA)
    }
  )

  col_x <- which(names(df) == x) # Find Column for independant
  col_y <- which(names(df) == y) # Find Column for dependant

  ivt <- data.frame(matrix(ncol = 0, nrow = 0)) # Shell
  n <- length(groups) # Number of cutpoits
  for (row_i in 1:n) {
    cutpoint <- groups[row_i]
    statement <- paste(
      "select '",
      gsub(",", "/", gsub("'", "", cutpoint)),
      "' as Cutpoint,
                      sum(case when $x in ($cutpoint) and $y in (1,0) then 1 else 0 end) as CntRec,
                      sum(case when $x in ($cutpoint) and $y=1 then 1 else 0 end) as CntGood,
                      sum(case when $x in ($cutpoint) and $y=0 then 1 else 0 end) as CntBad,
                      NULL as CntCumRec,
                      NULL as CntCumGood,
                      NULL as CntCumBad,
                      NULL as PctRec,
                      NULL as GoodRate,
                      NULL as BadRate,
                      NULL as Odds,
                      NULL as LnOdds,
                      NULL as WoE,
                      NULL as IV
                      from df where $x is not NULL and $y is not NULL",
      sep = ""
    )
    ivt <- rbind(ivt, fn$sqldf(statement))
  }

  # Missing Data
  x.na <- sum(is.na(df[, x]))
  y.na <- sum(is.na(df[, y]))

  if (x.na > 0) {
    ivt <- rbind(
      ivt,
      gsubfn::fn$sqldf(
        "select 'Missing' as Cutpoint,
                  sum(case when $x is NULL and $y in (1,0) then 1 else 0 end) as CntRec,
                  sum(case when $x is NULL and $y=1 then 1 else 0 end) as CntGood,
                  sum(case when $x is NULL and $y=0 then 1 else 0 end) as CntBad,
                  NULL as CntCumRec,
                  NULL as CntCumGood,
                  NULL as CntCumBad,
                  NULL as PctRec,
                  NULL as GoodRate,
                  NULL as BadRate,
                  NULL as Odds,
                  NULL as LnOdds,
                  NULL as WoE,
                  NULL as IV
                  from df where $y is not NULL"
      )
    )
  } else {
    ivt <- rbind(
      ivt,
      c("Missing", 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    )
  }
  # Total
  ivt <- rbind(
    ivt,
    gsubfn::fn$sqldf(
      "select 'Total' as Cutpoint,
                count(*) as CntRec,
                sum(case when $y=1 then 1 else 0 end) as CntGood,
                sum(case when $y=0 then 1 else 0 end) as CntBad,
                NULL as CntCumRec,
                NULL as CntCumGood,
                NULL as CntCumBad,
                NULL as PctRec,
                NULL as GoodRate,
                NULL as BadRate,
                NULL as Odds,
                NULL as LnOdds,
                NULL as WoE,
                NULL as IV
                from df where $y is not NULL"
    )
  )

  # Covert table to numeric
  options(warn = -1)
  ncol <- ncol(ivt)
  for (col_i in 2:ncol) {
    ivt[, col_i] <- as.numeric(ivt[, col_i])
  }
  options(warn = 0)

  # Complete Table: 1st row
  ivt[1, 5] <- ivt[1, 2] # Nbr Cum. Records
  ivt[1, 6] <- ivt[1, 3] # Nbr Cum. Goods
  ivt[1, 7] <- ivt[1, 4] # Nbr Cum. Bads

  # From 2nd row
  n <- nrow(ivt) - 2
  for (row_i in 2:n) {
    ivt[row_i, 5] <- ivt[row_i, 2] + ivt[row_i - 1, 5]
    ivt[row_i, 6] <- ivt[row_i, 3] + ivt[row_i - 1, 6]
    ivt[row_i, 7] <- ivt[row_i, 4] + ivt[row_i - 1, 7]
  }

  ivt[2, 5] <- ivt[2, 2] + ivt[1, 5]
  ivt[2, 6] <- ivt[2, 3] + ivt[1, 6]
  ivt[2, 7] <- ivt[2, 4] + ivt[1, 7]

  # Missing row
  ivt[col_y + 1, 5] <- ivt[col_y, 5] + ivt[col_y + 1, 2]
  ivt[col_y + 1, 6] <- ivt[col_y, 6] + ivt[col_y + 1, 3]
  ivt[col_y + 1, 7] <- ivt[col_y, 7] + ivt[col_y + 1, 4]

  # Calculating metrics
  options(scipen = 999) # Remove Scientific Notation
  ivt[, 8] <- round(ivt[, 2] / ivt[col_y + 2, 2], 4) # PctRec
  ivt[, 9] <- round(ivt[, 3] / ivt[, 2], 4) # GoodRate
  ivt[, 10] <- round(ivt[, 4] / ivt[, 2], 4) # BadRate
  ivt[, 11] <- round(ivt[, 3] / ivt[, 4], 4) # Odds
  ivt[, 12] <- round(log(ivt[, 3] / ivt[, 4]), 4) # LnOdds

  G <- ivt[col_y + 2, 3]
  B <- ivt[col_y + 2, 4]
  LnGB <- log(G / B) # IV Part 1

  ivt[, 13] <- round(log(ivt[, 3] / ivt[, 4]) - LnGB, 4) # WoE
  ivt[, 14] <-
    round(ivt[, 13] * (ivt[, 3] / G - ivt[, 4] / B), 4) # Mg IV
  # ivt[i+2,14]=round(sum(ivt[,13]*(ivt[,3]/G-ivt[,4]/B),na.rm=T),4) -- Old Calculation
  # Calculates Information Value even with undefined numbers
  ivt[col_y + 2, 14] <- 0.0000
  for (k in 1:(nrow(ivt) - 1)) {
    if (is.finite(ivt[k, 14])) {
      mgiv <- ivt[k, 14]
    } else {
      mgiv <- 0.0000
    }
    ivt[col_y + 2, 14] <- ivt[col_y + 2, 14] + mgiv
  }
  iv <- ivt[col_y + 2, 14]
  # End Inf. Value Table

  list(
    ivtable = ivt,
    iv = iv,
    x = x,
    col_id = col_x,
    groups = groups
  )
}
# End Custom Binning Factors



# Begin Gen Characteristic for factor variables
#
#' Utility function to generate a new characteristic from a factor variable
#'
#' It generates a data frame with a new predictive characteristic from a factor variable after applying
#' \code{smbinning.factor} or \code{smbinning.factor.custom}.
#' @param df Dataset to be updated with the new characteristic.
#' @param ivout An object generated after \code{smbinning.factor} or \code{smbinning.factor.custom}.
#' @param chrname Name of the new characteristic.
#' @return A data frame with the binned version of the original characteristic.
#' @examples
#' # Load library and its dataset
#' library(smbinning) # Load package and its data
#' pop <- smbsimdf1 # Set population
#' train <- subset(pop, rnd <= 0.7) # Training sample
#'
#' # Binning a factor variable on training data
#' result <- smbinning.factor(train, x = "home", y = "fgood")
#'
#' # Example: Append new binned characteristic to population
#' pop <- smbinning.factor.gen(pop, result, "g1home")
#'
#' # Split training
#' train <- subset(pop, rnd <= 0.7) # Training sample
#'
#' # Check new field counts
#' table(train$g1home)
#' table(pop$g1home)
#' # Updated 20170910
#' @export
smbinning.factor.gen <- function(df, ivout, chrname = "NewChar") {
  df <- cbind(df, tmpname = NA)
  ncol <- ncol(df)
  col_id <- ivout$col_id
  df[, ncol][is.na(df[, col_id])] <- 0 # Missing

  # Loop through all factor values
  # If smbinning.factor
  if (is.null(ivout$groups)) {
    b <- ivout$cuts
    for (i in seq_len(b)) {
      df[, ncol][df[, col_id] == b[i]] <- i
    }
  }
  # If smbinning.factor.custom
  else {
    for (i in seq_len(ivout$groups)) {
      gelements <- as.list(strsplit(as.character(gsub(
        "'", "", ivout$groups[i]
      )), ",")[[1]])
      df[, ncol][df[, col_id] %in% gelements] <- i
    }
  }

  # Convert to factor for modeling
  df[, ncol] <- as.factor(df[, ncol])

  # Labeling: If smbinning.factor
  if (is.null(ivout$groups)) {
    blab <- c(paste("01 =  '", b[1], "'"))
    for (i in 2:length(b)) {
      blab <- c(blab, paste(sprintf("%02d", i), "=  '", b[i], "'"))
    }
  }
  # Labeling: If smbinning.factor.custom
  else {
    blab <- c(paste("01 in (", ivout$groups[1], ")"))
    for (i in 2:length(ivout$groups)) {
      blab <-
        c(blab, paste(sprintf("%02d", i), "in (", ivout$groups[i], ")"))
    }
  }

  # If for any reason #bins in test sample are different, error
  if (any(is.na(df[, col_id])) == F &
    length(blab) > length(unique(df[, ncol]))) {
    stop(
      "Number of bins in dataset different from original result.\n  Likely due to splitting population in training/testing sample."
    )
  }

  if (any(is.na(df[, col_id])) == TRUE &
    length(blab) >= length(unique(df[, ncol]))) {
    stop(
      "Number of bins in dataset different from original result.\n  Likely due to splitting population in training/testing sample."
    )
  }

  # Are there ANY missing values
  # any(is.na(df[,col_id]))

  if (any(is.na(df[, col_id]))) {
    blab <- c("00 Miss", blab)
  }

  # Some Make Up
  blab <- gsub(" '", "'", blab)
  blab <- gsub("' ", "'", blab)

  df[, ncol] <-
    factor(df[, ncol], labels = blab) # Here is the error

  names(df)[names(df) == "tmpname"] <- chrname
  return(df)
}
