#TODO: calculate interactions ----

#' Calculates 3-Way Interactions
#'
#' Formula: \eqn{}, where \eqn{} is
#'
#' @param df A (discrete) data.frame
#' @param classAtt A class column of the df (string)
#' @param intNo A desired number of interactions (integer); In range: [2,20] in
#'   steps of 2; Default value is 16 (8 positive and 8 negative interactions).
#'   If a provided value is odd, it will be rounded to the closest larger value,
#'   if it is outside the range, it will be scaled to fit (e.g. 1->2, 22->20).
#' @return A data-frame with 3-way interactions

Interactions3Way <- function(df, classAtt, intNo=16){

  #check if df is discrete
  if(integr::IsDiscreteDataFrame(df)){ #everything OK, proceed
    #TODO: check if intNo is not an integer

    #Check numerical correctness of intNo, and repair + warning
    if(intNo < 2){ #case: intNo is smaller than 2
      intNo <- 2
      warning("Parameter intNo was smaller than the minimum. It's scaled to fit
              the range [2,20], and now it is 2")
    }

    if(intNo %% 2 != 0){ #intNo is odd, round it to the first larger number
      intNo <- intNo + 1
      warning(paste0("Parameter intNo was odd. It is converted to even number,
                     and now is ", intNo))
    }

    if(intNo > 20){ #case: intNo is larger than 20
      intNo <- 20
      warning("Parameter intNo was larger than the maximum. It's scaled to fit
              the range [2,20], and now it is 20")
    }

    #Check if classAtt is not a string and throw error if true.
    if(typeof(classAtt) != "character"){
      stop("Class attribute name needs to be provided as a string!")
    }

    #TODO: implement interaction gain

    }

  #not a discrete df, throw an error
  else{
    stop("The provided data.frame is not discrete (i.e. one of its columns
         is not factor)!")
  }



}

#TODO: create graph ----
