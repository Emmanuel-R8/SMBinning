#' @include error_checking.R


#' Information Value Summary
#'
#' `smbinning.sumiv` gives the user the ability to calculate, in one step, the IV for each characteristic of the dataset.
#' This function also shows a progress bar so the user can see the status of the process.
#' @param df A data frame.
#' @param y Binary response variable (0,1). Integer (\code{int}) is required.
#' Name of \code{y} must not have a dot. Name "default" is not allowed.
#' @return The command \code{smbinning.sumiv} generates a table that lists each characteristic
#' with its corresponding IV for those where the calculation is possible, otherwise it will generate a
#' missing value (\code{NA}).
#' @examples
#' # Load library and its dataset
#' library(smbinning)
#' #
#' # Test sample
#' test <- subset(smbsimdf1, rnd > 0.9) # Training sample
#' test$rnd <- NULL
#'
#' # Example: Information Value Summary
#' testiv <- smbinning.sumiv(test, y = "fgood")
#' testiv
#'
#' # Example: Plot of Information Value Summary
#' smbinning.sumiv.plot(testiv)
#' @export
smbinning.sumiv <- function(df, y) {
  # Check data frame and formats
  tryCatch(
    {
      assertthat::assert_that(is.data.frame(df))
    },
    error = function(e) {
      message("Data df not a dataframe.")
      return(NA)
    }
  )

  ncol <- ncol(df)

  # Empty table
  sumivt <- data.frame(matrix(ncol = 0, nrow = 0))

  # Turn off warnings
  options(warn = -1)
  cat("", "\n")
  pb <- txtProgressBar(
    min = 0,
    max = 1,
    initial = 0,
    style = 3,
    char = "-",
    width = 50
  )

  for (i in 1:ncol) {
    smbnum <- smbinning(df, y, colnames(df[i]))
    smbfac <- smbinning.factor(df, y, colnames(df[i]))
    if (colnames(df[i]) != y) {
      if (is.numeric(df[, i]) &
        is.list(smbnum)) {
        sumivt <- rbind(
          sumivt,
          data.frame(
            Char = colnames(df[i]),
            IV = smbnum$iv,
            Process = "Numeric binning OK"
          )
        )
      }
      else if (is.numeric(df[, i]) & !is.list(smbnum)) {
        sumivt <- rbind(
          sumivt,
          data.frame(
            Char = colnames(df[i]),
            IV = NA,
            Process = smbnum
          )
        )
      }
      else if (is.factor(df[, i]) & is.list(smbfac)) {
        sumivt <- rbind(
          sumivt,
          data.frame(
            Char = colnames(df[i]),
            IV = smbfac$iv,
            Process = "Factor binning OK"
          )
        )
      }
      else if (is.factor(df[, i]) & !is.list(smbfac)) {
        sumivt <- rbind(
          sumivt,
          data.frame(
            Char = colnames(df[i]),
            IV = NA,
            Process = smbfac
          )
        )
      }
      else {
        sumivt <- rbind(
          sumivt,
          data.frame(
            Char = colnames(df[i]),
            IV = NA,
            Process = "Not numeric nor factor"
          )
        )
      }
    }
    setTxtProgressBar(pb, i / ncol)
  }
  close(pb)

  # Turn back on warnings
  options(warn = 0)
  sumivt <- sumivt[with(sumivt, order(-IV)), ]
  cat("", "\n")
  return(sumivt)
}


