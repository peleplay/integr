#TODO: calculate interactions ----

#' Calculates 3-Way Interactions
#'
#' Formula: \eqn{}, where \eqn{} is
#'
#' @param df A (discrete) data.frame
#' @param classAtt A class column of the df (string)
#' @param intNo A desired number of interactions (integer); In range: [2,20] in
#'   steps of 2; Default value is 16 (8 positive and 8 negative interactions).
#'   If the provided value is odd, it will be rounded to the closest largest
#'   value; if new valu is outside the range, it will be scaled to fit (e.g.
#'   1->2, 22->20).
#' @return A data-frame with 3-way interactions

interactions3Way <- function(df, classAtt, intNo = 16){

  #check if df is discrete
  if (integr::isDiscreteDataFrame(df)) { #df is discrete, proceed
    #check if intNo is not a number
    if(typeof(intNo) != "numeric"){
      stop("The number of interactions to calculate must be an integer in range
           [2,20]!")
    }

    #Check numerical correctness of intNo
    if (intNo < 2) { #intNo is smaller than 2, make it minimal (i.e. 2)
      intNo <- 2
      warning("Parameter intNo was smaller than the minimum. It is scaled to fit
              the range [2,20], and now it is 2")
    }

    if (intNo %% 2 != 0) { #intNo is odd, round it to the first closest largest
      intNo <- intNo + 1
      warning(paste0("Parameter intNo was odd. It is converted to even number,
                     and now is ", intNo))
    }

    if (intNo > 20) { #intNo is larger than 20, make it maximal (i.e. 20)
      intNo <- 20
      warning("Parameter intNo was larger than the maximum. It is scaled to fit
              the range [2,20], and now it is 20")
    }

    #Check if classAtt is not a string, and throw an error if true.
    if (typeof(classAtt) != "character") {
      stop("Class attribute name needs to be provided as a string!")
    }

    #TODO: implement interaction gain

    #list of input attributes
    inputAttribs <- base::setdiff(colnames(df), classAtt)

    #Calculate 2-way interaction gains (information gain)
    lst2WayIntGain <- vector() #define empty list of 2-way interaction gains
    for (inputAtt in inputAttribs) {
      lst2WayIntGain <- c(lst2WayIntGain, infoGain(df, inputAtt, classAtt))
      names(lst2WayIntGain) <- inputAtt #add current attribute name
      lst2WayIntGain <- as.list(lst2WayIntGain) #update list
    }

    #Pruning: Remove attributes with information gain equal to zero (4 decimals
    #precision) to speedup further computations
    lst2WayIntGain <- lst2WayIntGain[lapply(lst2WayIntGain, function(x)
      length(grep("0.0000", x, value=FALSE))) == 0]


    #Calculate 3-way interaction gains
    pairs <- combn(names(lst2WayIntGain), 2, simplify = FALSE) #define att pairs

    for(pair in 1:length(pairs)){
      e1 <- pairs[[pair]][1]
      e2 <- pairs[[pair]][2]
      df$new <- as.factor(apply( df[ , c(e1, e2) ] , 1 , paste , collapse = "_" ))
      c <- infoGain(df, "new", classAtt)
      #3-way infogain
      pairs[[pair]][3] <- c
      #3-way interaction gain
      s1 <- lst2WayIntGain[[e1]]
      s2 <- lst2WayIntGain[[e2]]
      pairs[[pair]][4] <- c - s1 - s2
    }
    df$new <- NULL

    #data frame for sorting interaction gains
    df3WayIntGain <- data.frame(x=character(), y=character(),
                                z=numeric(), u=numeric())

    for(e in 1:length(pairs)){
      tmpDf <- as.data.frame(as.list(pairs[[e]]))
      names(tmpDf) <- c("x", "y", "z", "u")
      df3WayIntGain <- rbind(df3WayIntGain, tmpDf)
    }

    return()


  }

  #not a discrete df, throw an error
  else {
    stop("The provided data.frame is not discrete (i.e. one of its columns
         is not factor)!")
  }

}

#TODO: create graph ----
