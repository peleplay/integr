#' Calculates Shannon's entropy
#'
#' Formula: \eqn{H(S) = -P_i * \sum log_2 * P_i}, where \eqn{P_i} is the
#' probability of the corresponding \eqn{i}-th class
#'
#' @param df A (discrete) data.frame
#' @param classAtt A class column of the df (string)
#' @return The Shannon's entropy of \code{df}, based on the \code{classAtt}
#'   attribute

entropy <- function(df, classAtt) {
  freq <- table(df[classAtt]) #vector of class frequencies
  prob <- round(freq / sum(freq), 3) #vector of class probabilities
  logs <- log2(prob) * (-1) #vector of logarithms of class probabilities
  entr <- sum(prob * logs) #overall entropy
  return(entr)
}

#' Calculates Information Gain (2-way Interaction Gain) of a discrete dataframe
#'
#' \eqn{InfoGAIN = H(S) - H(S|X)}, where \eqn{H(S)} is the difference in the
#' Shannon's entropy of the system \eqn{S} before a new attribute \eqn{X} is
#' introduced, and \eqn{H(S|X)} is the entropy afterwards.
#'
#' @param df A discrete dataframe
#' @param inAtt An input column of the dataframe (string)
#' @param classAtt A class column of the dataframe (string)
#' @return The Information Gain of \code{df} on the class attribute
#'   \code{classAtt}

infoGain <- function(df, inAtt, classAtt) {
  apr <- entropy(df, classAtt) #a-priori entropy of the df

  #Frequency table for the new attribute
  freqA <- df %>%
           dplyr::group_by(!!as.symbol(inAtt)) %>%
           dplyr::count(!!as.symbol(classAtt))

  #Frequency table for the class attribute
  freqB <- freqA %>%
           dplyr::group_by(!!as.symbol(inAtt)) %>%
           dplyr::summarise(classCount = sum(n))

  #Overall frequency table
  freqC <- dplyr::inner_join(freqA, freqB, by = inAtt) #'by' argument is char!
  freqC$probs <- round(freqC$n/freqC$classCount, 3) #Calculate probabilities
  freqC$logs <- freqC$probs * log2(freqC$probs) * (-1) #Calculate logarithms

  #Upper level entropy components
  freqB$entrs <- freqC %>%
                 dplyr::group_by(!!as.symbol(inAtt)) %>%
                 dplyr::summarise(entrs = sum(logs))
  freqB$weight <- round(freqB$classCount / sum(freqB$classCount),3)

  #Overall entropy
  entr <- sum(freqB$weight * freqB$entrs$entrs)

  #Information gain
  infgn <- apr - entr
  return(infgn)
}

#' Tests if data.frame is discrete (i.e. all of its columns are factors)
#'
#' @param df A data frame
#' @return Boolean: TRUE if all columns of the data frame \code{df}
#' are factors, FALSE otherwise; If the provided \code{df} object is of other
#' type than data.frame, the function throws an error.

isDiscreteDataFrame <- function(df) {

  #Check if df is data.frame
  if (is.data.frame(df)) {

    #Check if there is a non-discrete column; If there is, return FALSE
    if (FALSE %in% sapply(df, is.factor)) {
      return(FALSE)
    }

    #df is data.frame & discrete, return TRUE
    else return(TRUE)

  }

  #not a data.frame, stop!
  else {
    stop("The provided object is not a data.frame!")
  }
}
