#Entropy function ----

#' Calculates Shannon's entropy
#'
#' @param df A dataframe (discrete!) (name, string)
#' @param classAtt A class column of the df (name, string)
#' @return the Shannon's entropy of \code{df} based on \code{classAtt}
#' @example
#' \dontrun{
#' Entropy(myDataFrame, myClassAttribute)
#' }

Entropy <- function(df, classAtt){
  freq <- table(df[classAtt]) #vector of class frequencies
  prob <- round(freq / sum(freq), 3) #vector of class probabilities
  logs <- log2(prob) * (-1) #vector of logarithms of class probabilities
  entr <- sum(prob*logs) #overall entropy
  return(entr)
}

#Information gain (2-way interaction) function ----

#' Calculates Information gain
#'
#' @param df A dataframe (discrete!)
#' @param inAtt An input column of the dataframe
#' @param classAtt A class column of the dataframe
#' @return the Shannon's entropy of \code{df} based on \code{classAtt}
#' @example
#' \dontrun{
#' InfoGain(myDataFrame, myInputAttribute, myClassAttribute)
#' }

InfoGain <- function(df, inAtt, classAtt){
  apr <- Entropy(df, classAtt) #a-priori entropy of the df

  #TODO: use dplyr to calculate frequencies, and regular...
  freqA <- df %>% dplyr::group_by(inAtt) %>% dplyr::count(classAtt)
  freqB <- freqA %>% dplyr::group_by(inAtt) %>%
                  dplyr::summarise(classCount = sum(n))
  freqC <- dplyr::inner_join(freqA, freqB, by = inAtt)
  freqC$probs <- round(freqC$n/freqC$classCount, 3)
  freqC$logs <- freqC$probs * log2(freqC$probs) * (-1)

  freqB$entrs <- freqC %>% dplyr::group_by(inAtt)
                       %>% summarise(entrs = sum(log))

  freqB$weight <- round(freqB$classCount / sum(freqB$classCount),3)

  entr <- sum(freqB$weight*freqB$entrs$entrs)

  infgn <- apr - entr
  return(infgn)
}

#Test for discrete data frame ----

#' Tests if all columns of the data.frame are discrete (i.e. factors)
#'
#' @param df A data frame
#' @return Boolean: TRUE if \code{obj} all columns of the data frame \code{df}
#' are factors, FALSE otherwise
#' @example
#' \dontrun{
#' IsDiscreteDataFrame(myDataFrame)
#' }

IsDiscreteDataFrame <- function(df){

  #Check if df is data.frame
  if(is.data.frame(df)){

    #Check if there is a non-discrete column
    if(FALSE %in% sapply(df, is.factor)){
      return(FALSE)
    }

    #df is data.frame & discrete
    else return(TRUE)

  }

  #not a data.frame
  else{
    return(FALSE)
  }
}
