#' @include error_checking.R

# Ini Monotonic Binning
#' Monotonic Binning
#'
#' It gives the user the ability to impose a monotonic trend for good/bad rates per bin.
#' @param df A data frame.
#' @param y Binary response variable (0,1). Integer (\code{int}) is required.
#' Name of \code{y} must not have a dot. Name "default" is not allowed.
#' @param x Continuous characteristic. At least 5 different values. Value \code{Inf} is not allowed.
#' Name of \code{x} must not have a dot.
#' @param p Percentage of records per bin. Default 5\% (0.05).
#' @return The command \code{smbinning.monotonic} generates and object containing the necessary info and utilities for binning.
#' The user should save the output result so it can be used
#' with \code{smbinning.plot}, \code{smbinning.sql}, and \code{smbinning.gen}.
#' @examples
#' # Load library and its dataset
#' library(smbinning) # Load package and its data
#'
#' # Example 1: Monotonic Binning (Increasing Good Rate per Bin)
#' smbinning(df=smbsimdf2,y="fgood2",x="chr2",p=0.05)$ivtable # Run regular binning
#' smbinning.monotonic(df=smbsimdf2,y="fgood2",x="chr2",p=0.05)$ivtable # Run monotonic binning
#'
#' # Example 2: Monotonic Binning (Decreasing Good Rate per Bin)
#' smbinning(df=smbsimdf2,y="fgood3",x="chr3",p=0.05)$ivtable # Run regular binning
#' smbinning.monotonic(df=smbsimdf2,y="fgood3",x="chr3",p=0.05)$ivtable # Run monotonic binning
#' @export
smbinning.monotonic <- function(df, y, x, p = 0.05) {
  # Ini function monotonic

  i <- which(names(df) == y) # Column for y
  j <- which(names(df) == x) # Column for x

  result <- smbinning(df, y, x, p) # Save result from usual binning
  c <-
    cor(df[, i], df[, j], use = "complete.obs", method = c("pearson"))
  col <- if (c > 0) {
    9
  } else {
    10
  } # Increasing (Column 9 Good Rate) or Decreasing (Column 10 Bad Rate)?

  if (result$iv < 0.1) {
    return("Not Meaningful (IV<0.1)")
  }

  else {
    # Ini condition

    # Get relevant data
    ivtable <- result$ivtable
    ratevalues <-
      ivtable[, col] # Column 9 for increasing (Good Rate), 10 for decreasing (Bad Rate)
    ratevalues <-
      ratevalues[1:(length(ratevalues) - 2)] # Excludes total and missing
    ratecuts <- result$cuts

    # Start WHILE Loop
    count <- 0
    iter <- 0

    while (count < 1) {
      # If last bin not follow trend, change it the previous bin
      i <- length(ratevalues) - 1
      ratecuts[i] <-
        ifelse(ratevalues[i + 1] < ratevalues[i], ratecuts[i -
                                                             1], ratecuts[i])
      # If next bin (i+1) lower than previous (i) then merge
      for (i in 1:length(ratevalues) - 1) {
        ratecuts[i] <-
          ifelse(ratevalues[i + 1] < ratevalues[i], ratecuts[i + 1], ratecuts[i])
      }

      # Make unique bin values
      ratecuts <- unique(ratecuts)
      ratecuts <- ratecuts[!is.na(ratecuts)]

      result <- smbinning.custom(df, y, x, ratecuts)
      ivtable <- result$ivtable
      ratevalues <- ivtable[, col]
      ratevalues <-
        ratevalues[1:(length(ratevalues) - 2)] # Excludes total and missing

      iter <- iter + 1 # Number of iterations

      count <- if (all(ratevalues == cummax(ratevalues)) == FALSE) {
        0
      } else{
        1
      }

    }
  } # End condition

  return(result)

} # End function monotonic
# End Monotonic Binning


# Ini: Simulated Credit Data
#' Simulated Credit Data
#'
#' A simulated dataset where the target variable is fgood,
#' which represents the binary status of default (0) and not default (1).
#'
#' \itemize{
#'   \item fgood: Default (0), Not Default (1).
#'   \item cbs1: Credit quality index (1-100).
#'   \item cbs2: Profitability index (1-100).
#'   \item cbinq: Number of inquiries.
#'   \item cbline: Number of credit lines.
#'   \item cbterm: Number of term loans.
#'   \item cblineut: Line utilization (0-100).
#'   \item cbtob: Number of years on file.
#'   \item cbdpd: Indicator of days past due on bureau (Yes, No).
#'   \item cbnew: Number of new loans.
#'   \item pmt: Type of payment (M: Manual, A: Autopay, P: Payroll).
#'   \item tob: Time on books (Years).
#'   \item dpd: Level of delinquency (No, Low, High).
#'   \item dep: Amount of deposits.
#'   \item dc: Number of transactions.
#'   \item od: Number of overdrafts.
#'   \item home: Home ownership indicator (Yes, No).
#'   \item inc: Level of income.
#'   \item dd: Number of electronic transfers.
#'   \item online: Indicator of online activity (Yes, No).
#'   \item rnd: Random number to select testing and training samples.
#'   \item period: Factor that indicates the year/month of the data (Based on rnd).
#'   }
#'
#' @format Data frame with 2,500 rows and 22 columns with 500 defaults.
#' @name smbsimdf1
NULL
# End: Simulated Credit Data


# Begin: Monotonic Sample Data
#' Monotonic Binning Sample Data
#'
#' A simulated dataset used to illustrate the application of monotonic binning.
#'
#' \itemize{
#'   \item fgood1: Default (0), Not Default (1) for Numeric Variable 1.
#'   \item chr1: Numeric variable 1.
#'   \item fgood2: Default (0), Not Default (1) for Numeric Variable 2.
#'   \item chr2: Numeric variable 2.
#'   \item fgood3: Default (0), Not Default (1) for Numeric Variable 3.
#'   \item chr3: Numeric variable 3.
#'   }
#'
#' @format Data frame with 2,500 rows and 6 columns.
#' @name smbsimdf2
NULL
# End: Monotonic Sample Data



# Begin: Model Ranking Sample Data
#' Monotonic Binning Sample Data
#'
#' A simulated dataset used to illustrate the application of model ranking.
#'
#' \itemize{
#'   \item fgood1: Default (0), Not Default (1) for Numeric Variable 1.
#'   \item chr1: Numeric variable 1.
#'   \item chr2: Numeric variable 2.
#'   \item chr3: Numeric variable 3.
#'   }
#'
#' @format Data frame with 1,000 rows and 4 columns.
#' @name smbsimdf3
NULL
# End: Model Ranking Sample Data
