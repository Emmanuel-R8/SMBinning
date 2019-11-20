#' @include error_checking.R


#' Optimal Binning for Scoring Modeling
#'
#' \strong{Optimal Binning} categorizes a numeric characteristic into bins for ulterior usage in scoring modeling.
#' This process, also known as \emph{supervised discretization},
#' utilizes \href{https://cran.r-project.org/package=partykit}{Recursive Partitioning} to categorize
#' the numeric characteristic.\cr
#' The especific algorithm is Conditional Inference Trees
#' which initially excludes missing values (\code{NA}) to compute the cutpoints, adding them back later in the
#' process for the calculation of the \emph{Information Value}.
#' @param df A data frame.
#' @param y Binary response variable (0,1). Integer (\code{int}) is required.
#' Name of \code{y} must not have a dot. Name "default" is not allowed.
#' @param x Continuous characteristic. At least 5 different values. Value \code{Inf} is not allowed.
#' Name of \code{x} must not have a dot.
#' @param p Percentage of records per bin. Default 5\% (0.05).
#' This parameter only accepts values greater that 0.00 (0\%) and lower than 0.50 (50\%).
#' @return The command \code{smbinning} generates and object containing the necessary info and utilities for binning.
#' The user should save the output result so it can be used
#' with \code{smbinning.plot}, \code{smbinning.sql}, and \code{smbinning.gen}.
#' @examples
#' # Load library and its dataset
#' library(smbinning) # Load package and its data
#'
#' # Example: Optimal binning
#' result=smbinning(df=smbsimdf1,y="fgood",x="cbs1") # Run and save result
#' result$ivtable # Tabulation and Information Value
#' result$iv # Information value
#' result$bands # Bins or bands
#' result$ctree # Decision tree
#' @export
smbinning <- function(df, y, x, p = 0.05) {

  require(assertthat)
  require(gsubfn)
  require(partykit)
  require(sqldf)

  #
  # Check data frame and formats
  msg <- haveParametersError(df, x, y, xIsFactor = FALSE, ...)

  tryCatch({
    msg == ""
  },
  error = function(e) {
    message(msg)
    return(NA)
  })

  tryCatch({
    assert_that(between(p, 0.0, 0.5))
  },
  error = function(e) {
    message("p must be greater than 0 and lower than 0.5 (50%).")
    return(NA)
  })


  ctree <- ctree(
    formula(paste(y, "~", x)),
    data = df,
    na.action = na.exclude,
    control = ctree_control(minbucket = ceiling(round(p * nrow(
      df
    ))))
  )

  bins <- width(ctree)
  tryCatch({
    assert_that(bins >= 2)
  },
  warning = function(e) {
    message("No significant splits.")
    return(NA)
  })

  col_x <- which(names(df) == x)
  col_y <- which(names(df) == y)

  # Append cutpoinstop()ts in a table (Automated)
  # Shell
  cutvct <- data.frame(matrix(ncol = 0, nrow = 0))
  n <- length(ctree) # Number of nodes

  for (index_i in 1:n) {
    cutvct <-  rbind(cutvct, ctree[index_i]$node$split$breaks)
  }

  # Sort / converts to a ordered vector (asc)
  cutvct <- cutvct[order(cutvct[, 1]), ]

  # Round to 4 dec. to avoid borderline cases
  cutvct <-  ifelse(cutvct < 0,
                    trunc(10000 * cutvct) / 10000,
                    ceiling(10000 * cutvct) / 10000)

  # Build Information Value Table
  # Counts per not missing cutpoint
  # Empty table
  IVTable <- data.frame(matrix(ncol = 0, nrow = 0))

  # Number of cutpoints
  n <- length(cutvct)

  for (col_i in 1:n) {
    cutpoint  <-  cutvct[col_i]
    IVTable <-  rbind(
      IVTable,
      fn$sqldf(
        "select '<= $cutpoint' as Cutpoint,
                  NULL as CntRec,
                  NULL as CntGood,
                  NULL as CntBad,
                  sum(case when $x <= $cutpoint and $y in (1,0) then 1 else 0 end) as CntCumRec,
                  sum(case when $x <= $cutpoint and $y=1 then 1 else 0 end) as CntCumGood,
                  sum(case when $x <= $cutpoint and $y=0 then 1 else 0 end) as CntCumBad,
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

  # Calculate Max without Missing

  # Round to 4 dec. to avoid borderline cases
  cutpoint <- max(df[, col_x], na.rm = TRUE)
  cutpoint <- ifelse(cutpoint < 0,
                     trunc(10000 * cutpoint) / 10000,
                     ceiling(10000 * cutpoint) / 10000)

  # Calculte Max cut point
  maxcutpoint <- max(cutvct)

  # Calculte Min without Missing for later usage
  mincutpoint <- min(df[, col_x], na.rm = TRUE)

  # Round to 4 dec. to avoid borderline cases
  mincutpoint <- ifelse(
    mincutpoint < 0,
    trunc(10000 * mincutpoint) / 10000,
    ceiling(10000 * mincutpoint) / 10000
  )

  IVTable  <-  rbind(
    IVTable,
    fn$sqldf(
      "select '> $maxcutpoint' as Cutpoint,
                NULL as CntRec,
                NULL as CntGood,
                NULL as CntBad,
                sum(case when $x <= $cutpoint and $y in (1,0) then 1 else 0 end) as CntCumRec,
                sum(case when $x <= $cutpoint and $y=1 then 1 else 0 end) as CntCumGood,
                sum(case when $x <= $cutpoint and $y=0 then 1 else 0 end) as CntCumBad,
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
  # Missing Data
  x.na <- sum(is.na(df[, x]))
  y.na <- sum(is.na(df[, y]))

  if (x.na > 0) {
    IVTable <-  rbind(
      IVTable,
      fn$sqldf(
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
  }

  else {
    IVTable <-  rbind(IVTable,
                      c("Missing", 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  }

  # Total
  IVTable <- rbind(
    IVTable,
    fn$sqldf(
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

  # Covert to table numeric
  options(warn = -1)

  ncol  <-  ncol(IVTable)
  for (col_j in 2:ncol) {
    IVTable[, col_j] <- as.numeric(IVTable[col_j])
  }

  options(warn = 0)

  # Complete Table
  IVTable[1, 2] <- IVTable[1, 5] # Nbr Records
  IVTable[1, 3] <- IVTable[1, 6] # Nbr Goods
  IVTable[1, 4] <- IVTable[1, 7] # Nbr Bads

  # From 2nd row
  n <- nrow(IVTable) - 2
  for (row_i in 2:n) {
    IVTable[row_i, 2] <- IVTable[row_i, 5] - IVTable[row_i - 1, 5]
    IVTable[row_i, 3] <- IVTable[row_i, 6] - IVTable[row_i - 1, 6]
    IVTable[row_i, 4] <- IVTable[row_i, 7] - IVTable[row_i - 1, 7]
  }

  IVTable[2, 2] <- IVTable[2, 5] - IVTable[1, 5]
  IVTable[2, 3] <- IVTable[2, 6] - IVTable[1, 6]
  IVTable[2, 4] <- IVTable[2, 7] - IVTable[1, 7]

  # Missing row.  Update: Added "if" statement
  IVTable[col_y + 1, 5] <- IVTable[col_y, 5] + IVTable[col_y + 1, 2]
  IVTable[col_y + 1, 6] <- IVTable[col_y, 6] + IVTable[col_y + 1, 3]
  IVTable[col_y + 1, 7] <- IVTable[col_y, 7] + IVTable[col_y + 1, 4]

  # Calculating metrics
  # Remove Scientific Notation
  options(scipen = 999)

  # PctRec
  IVTable[, 8] <- round(IVTable[, 2] / IVTable[col_y + 2, 2], 4)

  # GoodRate
  IVTable[, 9] <- round(IVTable[, 3] / IVTable[, 2], 4)

  # BadRate
  IVTable[, 10] <- round(IVTable[, 4] / IVTable[, 2], 4)

  # Odds
  IVTable[, 11] <- round(IVTable[, 3] / IVTable[, 4], 4)

  # LnOdds
  IVTable[, 12] <- round(log(IVTable[, 3] / IVTable[, 4]), 4)
  G <- IVTable[col_y + 2, 3]
  B <- IVTable[col_y + 2, 4]

  # IV Part 1
  LnGB <- log(G / B)

  # WoE
  IVTable[, 13] <- round(log(IVTable[, 3] / IVTable[, 4]) - LnGB, 4)

  # Mg IV
  IVTable[, 14] <-
    round(IVTable[, 13] * (IVTable[, 3] / G - IVTable[, 4] / B), 4)
  # ivt[i+2,14]=round(sum(ivt[,13]*(ivt[,3]/G-ivt[,4]/B),na.rm=T),4) -- Old Calculation

  # Calculates Information Value even with undefined numbers
  IVTable[col_y + 2, 14] <- 0.0000
  for (k in 1:(nrow(IVTable) - 1)) {
    if (is.finite(IVTable[k, 14])) {
      mgiv <- IVTable[k, 14]
    } else {
      mgiv <- 0.0000
    }
    IVTable[col_y + 2, 14] <- IVTable[col_y + 2, 14] + mgiv
  }
  iv <- IVTable[col_y + 2, 14]
  # End Inf. Value Table

  bands <- append(mincutpoint, cutvct)
  bands <- append(bands, cutpoint)
  list(
    ivtable = IVTable,
    iv = iv,
    ctree = ctree,
    bands = bands,
    x = x,
    col_id = col_x,
    cuts = cutvct
  )
}
# End smbinning



# Begin Custom Cutpoints
#' Customized Binning
#'
#' It gives the user the ability to create customized cutpoints.
#' @param df A data frame.
#' @param y Binary response variable (0,1). Integer (\code{int}) is required.
#' Name of \code{y} must not have a dot. Name "default" is not allowed.
#' @param x Continuous characteristic. At least 5 different values. Value \code{Inf} is not allowed.
#' Name of \code{x} must not have a dot.
#' @param cuts Vector with the cutpoints selected by the user. It does not have a default so user must define it.
#' @return The command \code{smbinning.custom} generates and object containing the necessary info and utilities for binning.
#' The user should save the output result so it can be used
#' with \code{smbinning.plot}, \code{smbinning.sql}, and \code{smbinning.gen}.
#' @examples
#' # Load library and its dataset
#' library(smbinning) # Load package and its data
#'
#' # Custom cutpoints using percentiles (20% each)
#' cbs1cuts=as.vector(quantile(smbsimdf1$cbs1, probs=seq(0,1,0.2), na.rm=TRUE)) # Quantiles
#' cbs1cuts=cbs1cuts[2:(length(cbs1cuts)-1)] # Remove first (min) and last (max) values
#'
#' # Example: Customized binning
#' result=smbinning.custom(df=smbsimdf1,y="fgood",x="cbs1",cuts=cbs1cuts) # Run and save
#' result$ivtable # Tabulation and Information Value
#' @export
smbinning.custom <- function(df, y, x, cuts) {
  # Check data frame and formats
  msg <- haveParametersError(df, x, y, xIsFactor = FALSE)
  tryCatch({
    assertthat::msg == ""
  },
  error = function(e) {
    message(msg)
    return(NA)
  })

  i <- which(names(df) == y) # Find Column for dependant
  j <- which(names(df) == x) # Find Column for independant

  # Append cutpoints in a table (Automated)
  cutvct <- data.frame(matrix(ncol = 0, nrow = 0)) # Shell
  n <- length(cuts) # Number of cutpoints
  if (n < 1) {
    return("No Bins")
  } # At least 1 cutpoint
  for (i in 1:n) {
    cutvct <- rbind(cutvct, cuts[i])
  }
  cutvct <-
    cutvct[order(cutvct[, 1]),] # Sort / converts to a ordered vector (asc)
  cutvct <- ifelse(cutvct < 0,
                   trunc(10000 * cutvct) / 10000,
                   ceiling(10000 * cutvct) / 10000) # Round to 4 dec. to avoid borderline cases
  # Build Information Value Table
  # Counts per not missing cutpoint
  ivt <- data.frame(matrix(ncol = 0, nrow = 0)) # Shell
  n <- length(cutvct) # Number of cutpoits
  for (i in 1:n) {
    cutpoint <- cutvct[i]
    ivt <- rbind(
      ivt,
      fn$sqldf(
        "select '<= $cutpoint' as Cutpoint,
                  NULL as CntRec,
                  NULL as CntGood,
                  NULL as CntBad,
                  sum(case when $x <= $cutpoint and $y in (1,0) then 1 else 0 end) as CntCumRec,
                  sum(case when $x <= $cutpoint and $y=1 then 1 else 0 end) as CntCumGood,
                  sum(case when $x <= $cutpoint and $y=0 then 1 else 0 end) as CntCumBad,
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
  cutpoint <-
    max(df[, j], na.rm = TRUE) # Calculte Max without Missing
  cutpoint <- ifelse(cutpoint < 0,
                     trunc(10000 * cutpoint) / 10000,
                     ceiling(10000 * cutpoint) / 10000) # Round to 4 dec. to avoid borderline cases
  maxcutpoint <- max(cutvct) # Calculte Max cut point
  mincutpoint <-
    min(df[, j], na.rm = TRUE) # Calculte Min without Missing for later usage
  mincutpoint <- ifelse(
    mincutpoint < 0,
    trunc(10000 * mincutpoint) / 10000,
    ceiling(10000 * mincutpoint) / 10000
  ) # Round to 4 dec. to avoid borderline cases
  ivt <- rbind(
    ivt,
    fn$sqldf(
      "select '> $maxcutpoint' as Cutpoint,
                NULL as CntRec,
                NULL as CntGood,
                NULL as CntBad,
                sum(case when $x <= $cutpoint and $y in (1,0) then 1 else 0 end) as CntCumRec,
                sum(case when $x <= $cutpoint and $y=1 then 1 else 0 end) as CntCumGood,
                sum(case when $x <= $cutpoint and $y=0 then 1 else 0 end) as CntCumBad,
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
  # Missing Data
  x.na <- fn$sqldf("select count(*) from df where $x is null")
  y.na <- fn$sqldf("select count(*) from df where $y is null")
  if (x.na > 0) {
    ivt <- rbind(
      ivt,
      fn$sqldf(
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
    ivt <- rbind(ivt,
                 c("Missing", 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  }
  # Total
  ivt <- rbind(
    ivt,
    fn$sqldf(
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

  # Covert to table numeric
  options(warn = -1)
  ncol <- ncol(ivt)
  for (i in 2:ncol) {
    ivt[, i] <- as.numeric(ivt[, i])
  }
  options(warn = 0)

  # Complete Table
  ivt[1, 2] <- ivt[1, 5] # Nbr Records
  ivt[1, 3] <- ivt[1, 6] # Nbr Goods
  ivt[1, 4] <- ivt[1, 7] # Nbr Bads

  # From 2nd row
  n <- nrow(ivt) - 2
  for (i in 2:n) {
    ivt[i, 2] <- ivt[i, 5] - ivt[i - 1, 5]
    ivt[i, 3] <- ivt[i, 6] - ivt[i - 1, 6]
    ivt[i, 4] <- ivt[i, 7] - ivt[i - 1, 7]
  }

  ivt[2, 2] <- ivt[2, 5] - ivt[1, 5]
  ivt[2, 3] <- ivt[2, 6] - ivt[1, 6]
  ivt[2, 4] <- ivt[2, 7] - ivt[1, 7]

  # Missing row
  ivt[i + 1, 5] <- ivt[i, 5] + ivt[i + 1, 2]
  ivt[i + 1, 6] <- ivt[i, 6] + ivt[i + 1, 3]
  ivt[i + 1, 7] <- ivt[i, 7] + ivt[i + 1, 4]

  # Calculating metrics
  options(scipen = 999) # Remove Scientific Notation
  ivt[, 8] <- round(ivt[, 2] / ivt[i + 2, 2], 4) # PctRec
  ivt[, 9] <- round(ivt[, 3] / ivt[, 2], 4) # GoodRate
  ivt[, 10] <- round(ivt[, 4] / ivt[, 2], 4) # BadRate
  ivt[, 11] <- round(ivt[, 3] / ivt[, 4], 4) # Odds
  ivt[, 12] <- round(log(ivt[, 3] / ivt[, 4]), 4) # LnOdds
  G <- ivt[i + 2, 3]
  B <- ivt[i + 2, 4]
  LnGB <- log(G / B) # IV Part 1
  ivt[, 13] <- round(log(ivt[, 3] / ivt[, 4]) - LnGB, 4) # WoE
  ivt[, 14] <-
    round(ivt[, 13] * (ivt[, 3] / G - ivt[, 4] / B), 4) # Mg IV
  # ivt[i+2,14]=round(sum(ivt[,13]*(ivt[,3]/G-ivt[,4]/B),na.rm=T),4) -- Old Calculation
  # Calculates Information Value even with undefined numbers
  ivt[i + 2, 14] <- 0.0000
  for (k in 1:(nrow(ivt) - 1))
  {
    if (is.finite(ivt[k, 14])) {
      mgiv <- ivt[k, 14]
    } else {
      mgiv <- 0.0000
    }
    ivt[i + 2, 14] <- ivt[i + 2, 14] + mgiv
  }
  iv <- ivt[i + 2, 14]
  # End Inf. Value Table

  bands <- append(mincutpoint, cutvct)
  bands <- append(bands, cutpoint)
  list(
    ivtable = ivt,
    iv = iv,
    bands = bands,
    x = x,
    col_id = j,
    cuts = cutvct
  )
}
# End Custom Cutpoints




# Begin Gen Characteristic
#' Utility to generate a new characteristic from a numeric variable
#'
#' It generates a data frame with a new predictive characteristic after applying
#' \code{smbinning} or \code{smbinning.custom}.
#' @param df Dataset to be updated with the new characteristic.
#' @param ivout An object generated after \code{smbinning} or \code{smbinning.custom}.
#' @param chrname Name of the new characteristic.
#' @return A data frame with the binned version of the original characteristic.
#' @examples
#' # Load library and its dataset
#' library(smbinning) # Load package and its data
#' pop=smbsimdf1 # Set population
#' train=subset(pop,rnd<=0.7) # Training sample
#'
#' # Binning application for a numeric variable
#' result=smbinning(df=train,y="fgood",x="dep") # Run and save result
#'
#' # Generate a dataset with binned characteristic
#' pop=smbinning.gen(pop,result,"g1dep")
#'
#' # Check new field counts
#' table(pop$g1dep)
#' @export
smbinning.gen <- function(df, ivout, chrname = "NewChar") {
  df <- cbind(df, tmpname = NA)
  ncol <- ncol(df)
  col_id <- ivout$col_id

  # Updated 20160130
  b <- ivout$bands
  df[, ncol][is.na(df[, col_id])] <- 0 # Missing
  df[, ncol][df[, col_id] <= b[2]] <- 1 # First valid

  # Loop goes from 2 to length(b)-2 if more than 1 cutpoint
  if (length(b) > 3) {
    for (i in 2:(length(b) - 2)) {
      df[, ncol][df[, col_id] > b[i] & df[, col_id] <= b[i + 1]] <- i
    }
  }
  df[, ncol][df[, col_id] > b[length(b) - 1]] <-
    length(b) - 1 # Last
  df[, ncol] <-
    as.factor(df[, ncol]) # Convert to factor for modeling
  blab <- c(paste("01 <=", b[2]))
  if (length(b) > 3) {
    for (i in 3:(length(b) - 1)) {
      blab <- c(blab, paste(sprintf("%02d", i - 1), "<=", b[i]))
    }
  } else {
    i <- 2
  }

  # Labels computed with training sample
  blab <- c(blab, paste(sprintf("%02d", i), ">", b[length(b) - 1]))

  # If for any reason #bins in test sample are different, error
  if (any(is.na(df[, col_id])) == F &
      length(blab) > length(unique(df[, ncol])))
  {
    stop(
      "Number of bins in dataset different from original result.\n  Likely due to splitting population in training/testing sample."
    )
  }

  if (any(is.na(df[, col_id])) == TRUE &
      length(blab) >= length(unique(df[, ncol])))
  {
    stop(
      "Number of bins in dataset different from original result.\n  Likely due to splitting population in training/testing sample."
    )
  }

  # Are there ANY missing values
  # any(is.na(df[,col_id]))

  if (any(is.na(df[, col_id]))) {
    blab <- c("00 Miss", blab)
  }
  df[, ncol] <- factor(df[, ncol], labels = blab)

  names(df)[names(df) == "tmpname"] <- chrname
  return(df)
}
# End Gen Characteristic