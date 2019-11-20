#' @include error_checking.R

# Begin Exploratory Data Analysis
#' Exploratory Data Analysis (EDA)
#'
#' It shows basic statistics for each characteristic in a data frame.
#' The report includes:
#' \itemize{
#'   \item Field: Field name.
#'   \item Type: Factor, numeric, integer, other.
#'   \item Recs: Number of records.
#'   \item Miss: Number of missing records.
#'   \item Min: Minimum value.
#'   \item Q25: First quartile. It splits off the lowest 25\% of data from the highest 75\%.
#'   \item Q50: Median or second quartile. It cuts data set in half.
#'   \item Avg: Average value.
#'   \item Q75: Third quartile. It splits off the lowest 75\% of data from the highest 25\%.
#'   \item Max: Maximum value.
#'   \item StDv: Standard deviation of a sample.
#'   \item Neg: Number of negative values.
#'   \item Pos: Number of positive values.
#'   \item OutLo: Number of outliers. Records below \code{Q25-1.5*IQR}, where \code{IQR=Q75-Q25}.
#'   \item OutHi: Number of outliers. Records above \code{Q75+1.5*IQR}, where \code{IQR=Q75-Q25}.
#'   }
#' @param df A data frame.
#' @param rounding Optional parameter to define the decimal points shown in the output table. Default is 3.
#' @param pbar Optional parameter that turns on or off a progress bar. Default value is 1.
#' @return The command \code{smbinning.eda} generates two data frames that list each characteristic
#' with basic statistics such as extreme values and quartiles;
#' and also percentages of missing values and outliers, among others.
#' @examples
#' # Load library and its dataset
#' library(smbinning) # Load package and its data
#'
#' # Example: Exploratory data analysis of dataset
#'
#' # Table with basic statistics
#' smbinning.eda(smbsimdf1,
#'               rounding = 3)$eda
#'
#' # Table with basic percentages
#' smbinning.eda(smbsimdf1,
#'               rounding = 3)$edapct
#' @export
smbinning.eda <- function(df, rounding = 3, pbar = 1) {
  # Check data frame and formats
  tryCatch({
    assert_that(is.data.frame(df) & !is_tibble(df))
  },
  error = function(e) {
    message("Data df not a dataframe (tibble not supported)")
    return(NA)
  })

  ncol <- ncol(df)
  nrow <- nrow(df)
  r <- rounding
  eda <- data.frame(matrix(ncol = 0, nrow = 0)) # Empty table
  options(scipen = 999) # No scientific notation
  options(warn = -1) # Turn off warnings
  if (pbar == 1) {
    cat("", "\n")
    pb <- txtProgressBar(
      min = 0,
      max = 1,
      initial = 0,
      style = 3,
      char = "-",
      width = 50
    )
  }
  for (i in 1:ncol) {
    # t1=round(Sys.time()-t0,2)
    Miss <- sum(is.na(df[, i]))
    if (is.numeric(df[, i]) | is.integer(df[, i])) {
      q <- unname(quantile(df[, i], na.rm = TRUE))
      iqr <- q[4] - q[2]
      iqrlow <- q[2] - 1.5 * iqr
      iqrupp <- q[4] + 1.5 * iqr
      Avg <- mean(df[, i], na.rm = TRUE)
      eda <- rbind(
        eda,
        data.frame(
          Field = colnames(df[i]),
          Type = "Num/Int",
          Recs = nrow,
          Miss = Miss,
          Unique = length(unique((df[, i][!is.na(df[, i])]))),
          Min = round(q[1], r),
          Q25 = round(q[2], r),
          Q50 = round(q[3], r),
          Avg = round(Avg, r),
          Q75 = round(q[4], r),
          Max = round(q[5], r),
          StDv = round(sd(df[, i], na.rm = TRUE), r),
          Neg = nrow(subset(df, df[, i] < 0 &
                              !is.na(df[, i]))),
          Zero = nrow(subset(df, df[, i] == 0 &
                               !is.na(df[, i]))),
          Pos = nrow(subset(df, df[, i] > 0 &
                              !is.na(df[, i]))),
          OutLo = nrow(subset(df, df[, i] < iqrlow)),
          OutHi = nrow(subset(df, df[, i] > iqrupp))
        )
      )
    }
    else if (is.factor(df[, i])) {
      eda <- rbind(
        eda,
        data.frame(
          Field = colnames(df[i]),
          Type = "Factor",
          Recs = nrow,
          Miss = Miss,
          Unique = length(unique((df[, i][!is.na(df[, i])]))),
          Min = NA,
          Q25 = NA,
          Q50 = NA,
          Avg = NA,
          Q75 = NA,
          Max = NA,
          StDv = NA,
          Neg = NA,
          Zero = NA,
          Pos = NA,
          OutLo = NA,
          OutHi = NA
        )
      )
    }
    else {
      eda <- rbind(
        eda,
        data.frame(
          Field = colnames(df[i]),
          Type = "Other",
          Recs = nrow,
          Miss = Miss,
          Unique = length(unique((df[, i][!is.na(df[, i])]))),
          Min = NA,
          Q25 = NA,
          Q50 = NA,
          Avg = NA,
          Q75 = NA,
          Max = NA,
          StDv = NA,
          Neg = NA,
          Zero = NA,
          Pos = NA,
          OutLo = NA,
          OutHi = NA
        )
      )
    }
    if (pbar == 1) {
      setTxtProgressBar(pb, i / ncol)
    }
  }

  if (pbar == 1) {
    close(pb)
  }

  # Table with percentages (edapct)
  edapct <- cbind(eda[, 1:4], eda[, 13:17])
  edapct[, 4] <- round(edapct[, 4] / edapct[, 3], r)
  edapct[, 5] <- round(edapct[, 5] / edapct[, 3], r)
  edapct[, 6] <- round(edapct[, 6] / edapct[, 3], r)
  edapct[, 7] <- round(edapct[, 7] / edapct[, 3], r)
  edapct[, 8] <- round(edapct[, 8] / edapct[, 3], r)
  edapct[, 9] <- round(edapct[, 9] / edapct[, 3], r)

  options(warn = 0) # Turn back on warnings
  list(eda = eda, edapct = edapct)
}
# End Exploratory Data Analysis




# Ini Logit Rank
#' Logistic Regression Ranking
#'
#' It runs all the possible logistic models for a given set of characteristics (\code{chr}) and then rank them
#' from highest to lowest performance based on AIC.
#' Important Note: This function may take time depending on the datset size and number of variables used in it.
#' The user should run it at the end of the modeling process once variables have been pre-selected in previous steps.
#' @param df Data frame.
#' @param y Binary dependent variable.
#' @param chr Vector with the characteristics (independent variables).
#' @return The command \code{smbinning.logitrank} returns a table with the combination of characteristics
#' and their corresponding AIC and deviance. The table is ordered by AIC from lowest (best) to highest.
#' @examples
#' # Load library and its dataset
#' library(smbinning)
#'
#' # Example: Best combination of characteristics
#' smbinning.logitrank(y = "fgood",
#'                     chr = c("chr1","chr2","chr3"),
#'                     df = smbsimdf3)
#'
#' @export
smbinning.logitrank <- function(y, chr, df) {
  f <- c() # Initialize empty list of formulas
  att <-
    c() # Initialize empty list of characteristics in each formula
  for (k in 1:length(chr)) {
    v <- t(combn(chr, k))
    nrow <- nrow(v)
    ncol <- ncol(v)

    # Empty list for 1 set of combinations
    fnext <- c()

    # Empty list for 1 set of characteristics
    attnext <- c()
    for (j in 1:nrow) {
      ftmp <- paste0(y, " ~ ", v[j, 1])
      atttmp <- v[j, 1]

      # If more than one column
      if (ncol > 1) {
        for (i in 2:ncol) {
          ftmp <- paste0(ftmp, paste0("+", c(v[j,])[i]))
          atttmp <- paste0(atttmp, paste0("+", c(v[j,])[i]))
        } # End columns
      }

      fnext <- c(ftmp, fnext)
      attnext <- c(atttmp, attnext)
    } # End rows
    f <- c(f, fnext)
    att <- c(att, attnext)
  }

  # List attributes
  chrsum <- data.frame(character(0), numeric(0), numeric(0))

  # Intercept Only
  model <- glm(paste0(y, " ~ 1"),
               family = binomial(link = 'logit'),
               data = df)
  chrsum <-
    rbind(chrsum, cbind(c("Intercept Only"), c(model$aic), c(model$deviance)))

  for (i in 1:length(f)) {
    model <- glm(f[i], family = binomial(link = 'logit'), data = df)
    chrsum <-
      rbind(chrsum, cbind(c(att[i]), c(model$aic), c(model$deviance)))
  }

  colnames(chrsum) <- c("Characteristics", "AIC", "Deviance")
  chrsum$AIC <- as.numeric(as.character(chrsum$AIC))
  chrsum$Deviance <- as.numeric(as.character(chrsum$Deviance))
  chrsum <- chrsum[order(chrsum$AIC),]

  return(chrsum)
}
# Ini Logit Rank




# Begin Model Scaling
#' Scaling
#'
#' It transforms the coefficients of a logistic regression into scaled points
#' based on the following three parameters pre-selected by the analyst: PDO, Score, and Odds.
#' @param logitraw Logistic regression (glm) that must have specified \code{family=binomial} and
#' whose variables have been generated with \code{smbinning.gen} or \code{smbinning.factor.gen}.
#' @param pdo Points to double the oods.
#' @param odds Desired \code{odds} at the selected \code{score}.
#' @param score Score at which the desire \code{odds} occur.
#' @return A scaled model from a logistic regression built with binned variables, the parameters
#' used in the scaling process, the expected minimum and maximum score, and the original logistic model.
#' @examples
#' # Load library and its dataset
#' library(smbinning)
#'
#' # Sampling
#' # Population
#' pop <- smbsimdf1
#'
#' # Training sample
#' train <- subset(pop,rnd <= 0.7)
#'
#' # Generate binning object to generate variables
#' smbcbs1 <- smbinning(train,
#'                      x = "cbs1",
#'                      y = "fgood")
#'
#' smbcbinq <- smbinning.factor(train,
#'                              x = "cbinq",
#'                              y = "fgood")
#'
#' smbcblineut <- smbinning.custom(train,
#'                                 x = "cblineut",
#'                                 y = "fgood",
#'                                 cuts = c(30,40,50))
#'
#' smbpmt <- smbinning.factor(train,
#'                            x = "pmt",
#'                            y = "fgood")
#'
#' smbtob <- smbinning.custom(train,
#'                            x = "tob",
#'                            y = "fgood",
#'                            cuts = c(1,2,3))
#'
#' smbdpd <- smbinning.factor(train,
#'                            x = "dpd",
#'                            y = "fgood")
#'
#' smbdep <- smbinning.custom(train,
#'                            x = "dep",
#'                            y = "fgood",
#'                            cuts = c(10000,12000,15000))
#'
#' smbod <- smbinning.factor(train,
#'                           x = "od",
#'                           y = "fgood")
#'
#' smbhome <- smbinning.factor(train,
#'                             x = "home",
#'                             y = "fgood")
#'
#' smbinc <- smbinning.factor.custom(train,
#'                                   x = "inc",
#'                                   y = "fgood",
#'                                   c("'W01','W02'","'W03','W04','W05'",
#'                                   "'W06','W07'","'W08','W09','W10'"))
#'
# Generate new characteristics and update population dataset
#' pop <- smbinning.gen(pop,smbcbs1, "g1cbs1")
#' pop <- smbinning.factor.gen(pop, smbcbinq, "g1cbinq")
#' pop <- smbinning.gen(pop, smbcblineut, "g1cblineut")
#' pop <- smbinning.factor.gen(pop, smbpmt, "g1pmt")
#' pop <- smbinning.gen(pop, smbtob, "g1tob")
#' pop <- smbinning.factor.gen(pop, smbdpd, "g1dpd")
#' pop <- smbinning.gen(pop, smbdep, "g1dep")
#' pop <- smbinning.factor.gen(pop, smbod, "g1od")
#' pop <- smbinning.factor.gen(pop, smbhome, "g1home")
#' pop <- smbinning.factor.gen(pop, smbinc, "g1inc")
#'
#' # Resample
#' # Training sample
#' train <- subset(pop, rnd <= 0.7)
#'
#' # Testing sample
#' test <- subset(pop,rnd > 0.7)
#'
#' # Run logistic regression
#' f <- fgood ~ g1cbs1 + g1cbinq + g1cblineut + g1pmt + g1tob + g1dpd + g1dep + g1od + g1home + g1inc
#'
#' modlogisticsmb <- glm(f,
#'                       data  =  train,
#'                       family  =  binomial())
#' summary(modlogisticsmb)
#'
#' # Example: Scaling from logistic parameters to points
#' smbscaled <- smbinning.scaling(modlogisticsmb,
#'                                pdo = 20,
#'                                score = 720,
#'                                odds = 99)
#'
#' # Scaled model
#' smbscaled$logitscaled
#'
#' # Expected minimum and maximum Score
#' smbscaled$minmaxscore
#'
#' # Parameters used for scaling
#' smbscaled$parameters
#'
#' # Extract of original logistic regression
#' summary(smbscaled$logitraw)
#'
#' # Example: Generate score from scaled model
#' pop1 <- smbinning.scoring.gen(smbscaled = smbscaled, dataset = pop)
#'
#' # Example Generate SQL code from scaled model
#' smbinning.scoring.sql(smbscaled)
#'
#' @export
smbinning.scaling <- function(logitraw,
                              pdo = 20,
                              score = 720,
                              odds = 99) {
  if (missing(logitraw)) {
    return(stop("Logistic model missing."))
  }
  else if (as.character(summary(logitraw)[1]$call)[1] != "glm") {
    return(stop("Logistic model must be glm"))
  }
  else if (as.character(logitraw$family)[1] != "binomial") {
    return(stop("Logistic model must be a binomial family"))
  }
  else if (names(logitraw$coefficients[1]) != "(Intercept)") {
    return(stop("Logistic model must have a constant"))
  }
  else if (!is.numeric(pdo)) {
    return(stop("PDO must be numeric"))
  }
  else if (!is.numeric(score)) {
    return(stop("Score must be numeric"))
  }
  else if (!is.numeric(odds)) {
    return(stop("Odds must be numeric"))
  }
  else if (all.equal(pdo, as.integer(pdo)) != TRUE | pdo < 0) {
    return(stop("PDO must be positive integer"))
  }
  else if (all.equal(score, as.integer(score)) != TRUE |
           score < 0) {
    return(stop("score must be positive integer"))
  }
  else if (all.equal(odds, as.integer(odds)) != TRUE | odds < 0) {
    return(stop("Odds must be positive integer"))
  }
  else{
    # Number of characteristics
    nchr <- length(logitraw$xlevels)

    # Characteristics and bin names #
    chrname <- list() # Generate empty list
    chrbinname <- list() # Generate empty list
    chrbinlist <- list() # Generate empty list
    for (i in 1:nchr) {
      chrname[i] <-
        names(data.frame(logitraw$xlevels[i])) # Name of the characteristics
      chrbin <-
        data.frame(names(table(logitraw$xlevels[i]))) # Characteristic's bins
      n <- as.character(chrname[i])
      binappend <- list()
      for (j in 1:dim(chrbin)[1]) {
        bincut <- as.character(chrbin[j, 1]) # For example "00 Miss"
        chrbinname <-
          c(chrbinname, paste(chrname[i], bincut, sep = "")) # For example "binnedage 01 <= 25"
        binappend <- c(binappend, bincut)
      }
      chrbinlist[n] <- list(binappend)
    }

    # Bins and coefficients
    bincoeffname <-
      rownames(as.data.frame(logitraw$coefficients)) # Getting bin names
    bincoeff <-
      as.list(matrix(logitraw$coefficients)) # Getting bin coefficients
    # Creating a list of bins and their coefs
    names(bincoeff) <- bincoeffname
    # Updating bins, their coefficients and adding base bins and their coefficients
    for (i in 1:length(chrbinname)) {
      # if not a base bin then next
      if (exists(as.character(chrbinname[i]), where = bincoeff)) {
        next
      }
      # if  a base bin then add it to bincoeff
      else {
        a <- as.character(chrbinname[i])
        bincoeff[a] <- 0.0000
      }
    }

    # Naming
    bincoeffname <- names(bincoeff)

    # Scaling parameters
    factor <- pdo / (log(2))
    offset <- (score) - ((factor) * (log(odds)))
    intercept <- unname(logitraw$coefficients["(Intercept)"])
    interceptweight <- (intercept) * (factor)
    interceptbychr <- (interceptweight) / (nchr)
    offsetbychr <- (offset) / (nchr)
    scorebychr <- (interceptbychr) + (offsetbychr)

    # Updating the bincoeff into a dataframe
    bincoeff <- data.frame(sapply(bincoeff, function(x)
      x[[1]]))
    colnames(bincoeff) <- "Coefficient"
    bincoeff <- cbind(rownames(bincoeff), bincoeff)
    bincoeff$Weight <- (bincoeff$Coefficient) * (factor)
    bincoeff$WeightScaled <- (bincoeff$Weight) + (scorebychr)
    bincoeff[1, 4] <- 0.000 #Make scaled constant equal to zero
    bincoeff$Points <- round(bincoeff$WeightScaled, 0)
    colnames(bincoeff) <- c("FullName",
                            "Coefficient",
                            "Weight",
                            "WeightScaled",
                            "Points")
    # Sorting bincoeff
    FullName <- unlist(chrbinname)
    FullName <- c("(Intercept)", FullName)
    bincoeff$FullName <-
      factor(bincoeff$FullName, levels = FullName)
    bincoeff <- bincoeff[order(bincoeff$FullName),]
    #bincoeff=within(bincoeff, WeightScaled[FullName=='(Intercept)']==0)
    rownames(bincoeff) <- 1:dim(bincoeff)[1]
    # Create attributes
    Attribute <- unlist(chrbinlist)
    # Add value for intercept to attributes
    Attribute <- c("", Attribute)
    Attribute <- unname(data.frame(Attribute))
    rownames(Attribute) <- NULL
    # Creating Characteristic
    Characteristic = list()
    for (i in 1:length(FullName)) {
      # Characteristic=c(Characteristic, gsub(gsub("[[:punct:]]","",as.character(Attribute[[i,1]])),"",gsub("[[:punct:]]","",FullName[i])))
      Characteristic <-
        c(Characteristic, gsub("[0-9][0-9] .*$", "",  FullName[i]))
    }
    Characteristic <- unlist(Characteristic)
    Characteristic <- unname(data.frame(Characteristic))
    # Removing column FullName from the bincoeff
    drops <- c("FullName")
    bincoeff <- bincoeff[, !(names(bincoeff) %in% drops)]
    # Adding attributes to bincoeff
    bincoeff <- cbind(Attribute, bincoeff)
    # Adding characteristic to bincoeff
    bincoeff <- cbind(Characteristic, bincoeff)

    # Get Min/Max Score
    chrpts <- bincoeff
    chrpts <- chrpts[, c("Characteristic", "Points")]
    chrpts <- chrpts[-1,] # Remove (intercept)
    minmaxscore <-
      c(sum(aggregate(Points ~ Characteristic, chrpts, min)$Points),
        sum(aggregate(Points ~ Characteristic, chrpts, max)$Points))

    # Converting bincoeff into a list
    bincoeff <- list(bincoeff)

    # Creating a list for all the parameters
    parameters <- list()
    parameters$pdo <- pdo
    parameters$odds <- odds
    parameters$factor <- factor
    parameters$score <- score
    parameters$offset <- offset
    parameters$intercept <- intercept
    parameters$interceptweight <- interceptweight
    parameters$nchr <- nchr
    parameters$interceptbychr <- interceptbychr
    parameters$offsetbychr <- offsetbychr
    parameters$scorebychr <- scorebychr

    # Final output
    modelscaled <- list()
    modelscaled$logitscaled <- bincoeff
    modelscaled$parameters <- parameters
    modelscaled$logitraw <- logitraw
    modelscaled$minmaxscore <- minmaxscore

    # Return
    return(modelscaled)
  }
}
# End Model Scaling



# Begin Add Points and Score
#' Generation of Score and Its Weights
#'
#' After applying \code{smbinning.scaling} to the model, \code{smbinning.scoring} generates a data frame
#' with the final Score and additional fields with the points assigned to each characteristic so the user
#' can see how the final score is calculated. Example shown on \code{smbinning.scaling} section.
#' @param smbscaled Object generated using \code{smbinning.scaling}.
#' @param dataset A data frame.
#' @return The command \code{smbinning.scoring} generates a data frame with the final scaled Score and its
#' corresponding scaled weights per characteristic.
#' @export
smbinning.scoring.gen <- function(smbscaled, dataset) {
  if (missing(smbscaled)) {
    # Check if logitscaled is missing or not
    return(stop("Missing argument: smbscaled"))
  }
  else if (missing(dataset)) {
    # Check if dataset is missing or not
    return(stop("Missing argument: dataset"))
  }
  else if (!is.data.frame(dataset)) {
    # Check if data.frame
    return(stop("Not a data frame"))
  }
  else if (names(smbscaled)[1] != "logitscaled") {
    return(stop("Not from 'smbinning.scaling'"))
  } else {
    df <- dataset
    logitraw <- smbscaled$logitraw
    logitscaled <- smbscaled$logitscaled
    chrattpts <- as.data.frame(logitscaled)
    chrattpts <-
      chrattpts[, c("Characteristic", "Attribute", "Points")]
    for (i in 1:length(logitraw$xlevels)) {
      chrname <- names(logitraw$xlevels[i])
      chrattptstmp <-
        subset(chrattpts, chrattpts$Characteristic == chrname) # Tmp df for char [i] with attr and points
      df <- cbind(df, chrtmp = NA)
      colidx <-
        which(names(df) == chrname) # Where is original characteristic
      # df=cbind(df,chrtmporiginal=NA)
      # df$chrtmporiginal=df[,colidx] # Populate temporary original characteristic
      for (j in 1:nrow(chrattptstmp)) {
        df <- within(df, chrtmp[df[, colidx] == logitraw$xlevels[[i]][j]] <-
                       chrattptstmp[j,][3])
      }
      # df$chrtmporiginal=NULL
      df$chrtmp <- as.numeric(df$chrtmp)

      if (paste0(names(logitraw$xlevels[i]), "Points") %in% colnames(df))
      {
        stop("Column '",
             paste0(
               names(logitraw$xlevels[i]),
               "Points' already exists. Drop it or rename it."
             ))
      }

      names(df)[names(df) == "chrtmp"] = paste(chrname, "Points", sep = "") # Rename tmp column name to Points.
    }

    # Create final score
    if ("Score" %in% colnames(df)) {
      stop("Column 'Score' already exists. Drop it or rename it.")
    } # Stop process if column Score already exists.
    df <- cbind(df, Score = 0) # Add score column
    scorecolid <- ncol(df) # Score column id

    # All characteristics
    crhpts <- list()
    for (i in 1:length(logitraw$xlevels)) {
      crhpts <-
        cbind(crhpts, paste(names(logitraw$xlevels[i]), "Points", sep =
                              ""))
    }
    nbrchar <- length(crhpts) # Number of new generated chars
    colini <-
      which(names(df) == crhpts[1]) # Where is the new characteristic
    colend <- colini + nbrchar - 1
    df[, scorecolid] <-
      rowSums(df[, colini:colend]) # Sum rows to get Score
  }
  return(df)
}
# End Add Points and Score
