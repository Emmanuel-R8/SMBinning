#' @import assertthat
#'
#' @include error_checking.R
#'
# Begin Summary IV
#' Information Value Summary
#'
#' It gives the user the ability to calculate, in one step, the IV for each characteristic of the dataset.
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
#
#' # Test sample
#' test=subset(smbsimdf1,rnd>0.9) # Training sample
#' test$rnd=NULL
#'
#' # Example: Information Value Summary
#' testiv=smbinning.sumiv(test,y="fgood")
#' testiv
#'
#' # Example: Plot of Information Value Summary
#' smbinning.sumiv.plot(testiv)
#' @export
smbinning.sumiv <- function(df, y) {
  # Check data frame and formats
  tryCatch({
    assert_that(is.data.frame(df))
  },
  error = function(e) {
    message("Data df not a dataframe.")
    return(NA)
  })

  ncol <- ncol(df)
  sumivt <- data.frame(matrix(ncol = 0, nrow = 0)) # Empty table
  options(warn = -1) # Turn off warnings
  cat("", "\n")
  pb <- txtProgressBar(
    min = 0,
    max = 1,
    initial = 0,
    style = 3,
    char = "-",
    width = 50
  )
  # t0=Sys.time()
  # t1=0
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
      else if (is.numeric(df[, i]) &
               !is.list(smbnum)) {
        sumivt <- rbind(sumivt,
                        data.frame(
                          Char = colnames(df[i]),
                          IV = NA,
                          Process = smbnum
                        ))
      }
      else if (is.factor(df[, i]) &
               is.list(smbfac)) {
        sumivt <- rbind(
          sumivt,
          data.frame(
            Char = colnames(df[i]),
            IV = smbfac$iv,
            Process = "Factor binning OK"
          )
        )
      }
      else if (is.factor(df[, i]) &
               !is.list(smbfac)) {
        sumivt <- rbind(sumivt,
                        data.frame(
                          Char = colnames(df[i]),
                          IV = NA,
                          Process = smbfac
                        ))
      }
      else {
        sumivt <- rbind(sumivt,
                        data.frame(
                          Char = colnames(df[i]),
                          IV = NA,
                          Process = "Not numeric nor factor"
                        ))
      }
      # t1=Sys.time() # Recall system time
      # t1=round(t1-t0,2) # Compares against starting time t0
    }
    setTxtProgressBar(pb, i / ncol)
    # if(i<ncol)
    # {cat(" | Time:",t1,"| Binning",substring(colnames(df[i]),1,10),"...")}
    # else {cat(" | Time:",t1,"| Binning Done           ")}
  }
  close(pb)
  options(warn = 0) # Turn back on warnings
  sumivt <- sumivt[with(sumivt, order(-IV)), ]
  cat("", "\n")
  return(sumivt)
}
# End Summary IV


