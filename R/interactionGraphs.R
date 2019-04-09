#Define Interaction graph nodes S3 class, constructor & generic methods ----

#' Constructs Interaction Graph Nodes (S3 class)
#'
#' @param n igNode.name \code{(character)}
#' @param v igNode.value \code{(double)} (i.e. 2-way Interaction Gain)
#' @return An instance of the \code{igNode} class

igNode <- function(n, v){

  #Check validity of the input
  if (typeof(n) != "character") { #igNode name is not string
    stop("Interaction graph Node.name must be of type character")
  }

  if (typeof(v) != "double"){ #igNode value is not double
    stop("Interaction graph Node.value must be of type double")
  }

  #Define class
  val <- list(name = n, value = v)

  #Set class name
  attr(val, "class") <- "igNode"

  #Create object
  return(val)

}

#Print.igNode() generic
print.igNode <- function(node) {
  cat(paste0("\t", node$name, " ", "[label='", node$name, " \\n ",
             node$value, "%'] ; \n"))
}

#TODO: add paste generic method


#Define Interaction graph edges s3 class, constructor & generic methods ----

#' Constructs Interaction Graph Edges (S3 class)
#'
#' @param n1 igEdge.node1 \code{(character)}
#' @param n2 igEdge.node2 \code{(character)}
#' @param w igEdge.weight \code{(double)} (i.e. 3-way Interaction Gain)
#' @return An instance of the \code{igEdge} class

igEdge <- function(n1, n2, w) {

  #Check validity of the input
  if (typeof(n1) != "character") { #igEdge.node1 name is not string
    stop("Interaction graph Edge.Node1 name must be of type character")
  }

  if (typeof(n2) != "character") { #igEdge.node2 name is not string
    stop("Interaction graph Edge.Node2 name must be of type character")
  }

  if (typeof(w) != "double") { #igNode weight is not double
    stop("Interaction graph Edge.weight must be of type double")
  }

  #Define class
  val <- list(node1 = n1, node2 = n2, weight = w)

  #Set class name
  attr(val, "class") <- "igEdge"

  #Create object
  return(val)
}

#Print.igEdge() generic
print.igEdge <- function(edge) {

  if (edge$weight < 0) { #print string for negative edges
    cat(paste0("\t", egde$node1, " -> " , edge$node2,
              " [labeldistance=2.5, labelangle=-45, label='", edge$weight,
              "%', color='red', dir='none'] ; \n"))
  }

  else { #print string for positive edges
    cat(paste0("\t", egde$node1, " -> " , egde$node2,
              " [labeldistance=2.5, labelangle=-45, label='", edge$weight,
              "%', color='green', dir='both'] ; \n"))
  }
}

#TODO: add paste generic method

#Define Interaction graph s3 class, constructor & generic methods ----

#' Constructs Interaction Graph (S3 class)
#'
#' @param n ig.nodes \code{(a list of igNode-s)}
#' @param ne ig.negEdges \code{(a list of negative igEdge-s)}
#' @param pe ig.posEdges \code{(a list of negative igEdge-s)}
#' @return An instance of the \code{ig} class

ig <- function(n, ne, pe){

  #Check validity of the input
  if (class(n) != "igNode") { #not an int. graph node
    stop("Not an Interaction graph node!")
  }

  if (class(ne) != "igEdge" || class(pe) != "igEdge") { #not an int. graph edge
    stop("Not an Interaction graph edge!")
  }

  #Define class
  val <- list(head = "digraph InteractionGraph { \n
              graph [overlap = scale, fontsize = 10, fontname = 'helvetica']; \n
              node [shape=box, margin=0.1, fontname = 'helvetica'] ; \n
              edge [fontname = 'helvetica'] ; \n", #header
              nodes = n,
              negEdges = ne,
              posEdges = pe,
              foot = "\n } \n") #footer

  #Set class name
  attr(val, "class") <- "ig"

  #Create object
  return(value)
}

#Print.ig() generic
print.ig <- function(intGraph){
  cat(intGraph$head) #print header of the graph
  for (n in 1:length(intGraph$nodes)) { #print all nodes
    print(intGraph$nodes[n])
  }
  for (ne in 1:length(intGraph$negEdges)) { #print negative edges
    print(intGraph$nedEdges[ne])
  }
  for (pe in 1:length(intGraph$posEdges)) { #print positive edges
    print(intGraph$posEdges[pe])
  }
  cat(intGraph$foot)
}

#TODO: add paste generic method

#Calculate interactions ----

#' Calculates 3-Way Interactions
#'
#' Formula: \eqn{}, where \eqn{} is
#'
#' @param df A (discrete) data.frame
#' @param classAtt A class column of the df (string)
#' @param speedUp A (boolean) parameter, if TRUE, indicating whether pairs of
#'   attributes with information gain equal to zero (on the 4th decimal) should
#'   be pruned. This speeds up calculations for larger datasets. By default it
#'   is turned off (i.e. set to FALSE).
#' @return A data-frame with 3-way interactions
#' @export

interactions3Way <- function(df, classAtt, speedUp = FALSE){

  #check if df is discrete
  if (isDiscreteDataFrame(df)) { #df is discrete, proceed

    #Check if classAtt is not a string, and throw an error if true.
    if (typeof(classAtt) != "character") {
      stop("Class attribute name needs to be provided as a string!")
    }

    #Check if speedUp is not a boolean, and throw an error if true
    if (typeof(speedUp) != "logical") {
      stop("'speedUp' parameter needs to be of logical (boolean) type!")
    }

    #list of input attributes
    inputAttribs <- base::setdiff(colnames(df), classAtt)

    #Calculate 2-way interaction gains (information gain) ----

    lst2WayIntGain <- vector() #define empty list of 2-way interaction gains
    for (ia in 1:length(inputAttribs)) {
      currAtt <- inputAttribs[ia] #name of the current attribute
      lst2WayIntGain <- c(lst2WayIntGain, infoGain(df, currAtt, classAtt))
      names(lst2WayIntGain)[ia] <- currAtt #update the element's attribute name
      lst2WayIntGain <- as.list(lst2WayIntGain) #keep the list structure
    }


    #Pruning ----

    #Remove attributes with Information gain equal to zero (4 decimals
    #precision) to speedup further computations (useful for large data.frames)
    if (speedUp == TRUE) {
      lst2WayIntGain <- lst2WayIntGain[lapply(lst2WayIntGain, function(x)
        length(grep("0.0000", x, value=FALSE))) == 0]
    }

    #Calculate 3-way interaction gains ----

    #define att pairs
    lst3WayIntGain <- combn(names(lst2WayIntGain), 2, simplify = FALSE)

    for (row in 1:length(lst3WayIntGain)) { #For each "row" of the list
      e1 <- lst3WayIntGain[[row]][1] #create current element 1
      e2 <- lst3WayIntGain[[row]][2] #create current element 2

      #add new temp column to df - a concatenation of columns represented with
      #elements 1 and 2 (i.e. - (e1, e2)):
      df$new <- as.factor(apply(df[ , c(e1, e2)], 1, paste, collapse = "_" ))

      c <- infoGain(df, "new", classAtt) #calculate infoGain for the new column
      lst3WayIntGain[[row]][3] <- c #add Infogain for the new attr. to the list

      #Calculate 3-way interaction gain:
      s1 <- lst2WayIntGain[[e1]] #select current element1
      s2 <- lst2WayIntGain[[e2]] #select current element2
      lst3WayIntGain[[row]][4] <- c - s1 - s2 #3-way interact. gain to list
    }

    df$new <- NULL

    #Convert list to data.frame
    df3WayIntGain <- data.frame(x = character(), y = character(),
                                z = numeric(), u = numeric())

    for (el in 1:length(lst3WayIntGain)) {
      tmpDf <- as.data.frame(as.list(lst3WayIntGain[[el]]))
      names(tmpDf) <- c("Col1", "Col2", "InformationGain", "InteractionGain")
      df3WayIntGain <- rbind(df3WayIntGain, tmpDf)
    }

    df3WayIntGain$InformationGain <- NULL #Drop info gain of the concatenation

    #Return 3-way interaction gains data.frame
    return(df3WayIntGain)
  }

  #not a discrete df, throw an error
  else {
    stop("The provided data.frame is not discrete (i.e. one of its columns
         is not factor)!")
  }

}

#TODO: create graph ----

#' Creates Interaction graph
#'
#' Descirpiton...
#'
#' @param df A (discrete) data.frame
#' @param classAtt A class column of the df (string)
#' @param intNo A desired number of interactions (integer) to show. In range:
#'   [2,20] in steps of 2; Default value is 16 (8 positive and 8 negative
#'   interactions). If the provided value is odd, it will be rounded to the
#'   closest largest value; if new valu is outside the range, it will be scaled
#'   to fit (e.g. 1->2, 22->20).
#' @param precomputed A (boolean) indication if the \code{df} is pointing to a
#'   data.frame with precomputed interaction gains (generated by
#'   \code{interactions3Way} function) or to the original dataset. By default a
#'   data.frame containing the original data set is expected
#'   (\code{precomputed=FALSE}.
#' @return An interaction graph object
#' @export

interactionGraph <- function(df, classAtt, intNo = 16, precomputed = FALSE){

    #Check if classAtt is not a string, and throw an error if true.
    if (typeof(classAtt) != "character") {
      stop("Class attribute name needs to be provided as a string!")
    }

    #check if intNo is not a number
    if(typeof(intNo) != "double"){
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

    #TODO: define interaction graph


}
