#Interaction graph nodes S3 class, constructor & generic methods ----

#' Constructs Interaction Graph Nodes (S3 class)
#'
#' @param n igNode.name \code{(character)}
#' @param v igNode.value \code{(double)} (i.e. 2-way Interaction Gain)
#' @return An instance of the \code{igNode} class

igNode <- function(n, v){

  #Check validity of the input

  if (missing(n) || missing(v)) { #a node.name or a node.value is missing
    stop("Please provide igNode.name and/or igNode.value to the function")
  }

  if (typeof(n) != "character") { #igNode name is not string
    stop("Interaction graph Node.name must be of type character")
  }

  if (typeof(v) != "double") { #igNode value is not double
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

#' Print generic method for Interaction Graph Nodes (S3 class)
#'
#' @param node An \code{(igNode)} object
#' @return Print \code{(igNode)} object

print.igNode <- function(node) {

  #Check input validity
  if (missing(node)) { #a node is missing
    stop("Please provide igNode to the function")
  }

  cat(paste0("\t ", node$name, " ", "[label='", node$name, " \\n ",
             node$value, "%']; \n"))
}

#toString.igNode() generic method

#' toString() generic method for Interaction Graph Nodes (S3 class)
#'
#' @param node An \code{(igNode)} object
#' @return \code{(character)} object made of the provided \code{(igNode)} object

toString.igNode <- function(node) {

  if (missing(node)) { #a node is missing
    stop("Please provide igNode to the function")
  }

  return(paste0("\t ", node$name, " ", "[label='", node$name, " \\n ",
         node$value, "%']; \n"))
}


#Interaction graph edges s3 class, constructor & generic methods ----

#' Constructs Interaction Graph Edges (S3 class)
#'
#' @param n1 igEdge.node1 \code{(character)}
#' @param n2 igEdge.node2 \code{(character)}
#' @param w igEdge.weight (i.e. 3-way Interaction Gain) \code{(double)}
#' @return An instance of the \code{igEdge} class

igEdge <- function(n1, n2, w) {

  #Check validity of the input

  if (missing(n1) || missing(n2) || missing(w)) { #input parameters missing
    stop("Please provide igEdge.node1 and/or igEdge.node2 and/or igEdge.weight
         to the function")
  }

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

#' Print generic method for Interaction Graph Edges (S3 class)
#'
#' @param edge An \code{(igEdge)} object
#' @return Print \code{(igEdge)} object

print.igEdge <- function(edge) {

  #Check input validity
  if (missing(edge)) { #ig.Edge is missing
    stop("Please provide igEdge to the function")
  }

  if (edge$weight < 0) { #print string for negative edges
    cat(paste0("\t ", edge$node1, " -> " , edge$node2,
              " [labeldistance=2.5, labelangle=-45, label='", edge$weight,
              "%', color='red', dir='none']; \n"))
  }

  else { #print string for positive edges
    cat(paste0("\t ", edge$node1, " -> " , edge$node2,
              " [labeldistance=2.5, labelangle=-45, label='", edge$weight,
              "%', color='green', dir='both']; \n"))
  }
}

#toString.igEdge() generic method

#' toString() generic method for Interaction Graph Edges (S3 class)
#'
#' @param edge An \code{(igEdge)} object
#' @return \code{(character)} object made of the provided \code{(igEdge)} object

toString.igEdge <- function(edge) {

  #Check input validity
  if (missing(edge)) { #ig.Edge is missing
    stop("Please provide igEdge to the function")
  }

  if (edge$weight < 0) { #return string for negative edges
    return(paste0("\t ", edge$node1, " -> " , edge$node2,
               " [labeldistance=2.5, labelangle=-45, label='", edge$weight,
               "%', color='red', dir='none']; \n"))
  }

  else { #return string for positive edges
    return(paste0("\t ", edge$node1, " -> " , edge$node2,
               " [labeldistance=2.5, labelangle=-45, label='", edge$weight,
               "%', color='green', dir='both']; \n"))
  }
}

#Define Interaction graph s3 class, constructor & generic methods ----

#' Constructs Interaction Graph (S3 class)
#'
#' @param n ig.nodes (\code{a list of igNode objects})
#' @param e ig.edges (\code{a list of igEdge objects})
#' @return An instance of the \code{ig} class

ig <- function(n, e){

  #Check input validity
  if (missing(n) || missing(e)) { #list of nodes or edges is missing
    stop("Please provide lists of igNodes and igEdges to the function")
  }

  #Define class
  val <- list(head = paste0("digraph InteractionGraph { \n ",
        "graph [overlap = scale, fontsize = 10, fontname = 'helvetica']; \n ",
        "node [shape = box, margin = 0.1, fontname = 'helvetica']; \n ",
        "edge [fontname = 'helvetica']; \n"), #header
              nodes = n,
              edges = e,
              foot = "} \n") #footer

  #Set class name
  attr(val, "class") <- "ig"

  #Create object
  return(val)
}

#Print.ig() generic

#' Print generic method for Interaction Graph (S3 class)
#'
#' @param intGraph An \code{(ig)} object
#' @return Print \code{(ig)} object

print.ig <- function(intGraph){

  #Check input validity
  if (missing(intGraph)) { #ig is missing
    stop("Please provide interaction graph to the function")
  }

  cat(intGraph$head) #print header of the graph
  for (n in 1:length(intGraph$nodes)) { #print nodes
    print(intGraph$nodes[[n]])
  }
  for (e in 1:length(intGraph$edges)) { #print edges
    print(intGraph$edges[[e]])
  }
  cat(intGraph$foot)
}

#toString.ig() generic method

#' toString() generic method for Interaction Graph (S3 class)
#'
#' @param intGraph An \code{ig} object
#' @return A \code{character} object made of the provided \code{ig} object

toString.ig <- function(intGraph) {

  #Check input validity
  if (missing(intGraph)) { #ig is missing
    stop("Please provide interaction graph to the function")
  }

  #init strings
  nodes <- ""
  edges <- ""

  for (n in 1:length(intGraph$nodes)) { #convert to string all nodes
    nodes <- c(nodes, toString(intGraph$nodes[[n]]))
  }
  nodes <- paste(nodes, collapse = "") #paste all strings to a single string

  for (e in 1:length(intGraph$edges)) { #convert to string all neg. edges
    edges <- c(edges, toString(intGraph$edges[[e]]))
  }
  edges <- paste(edges, collapse = "") #paste strings to a single string

  #Final graph string
  return(paste0(intGraph$head,
                nodes,
                edges,
                intGraph$foot))

}

#Calculate interactions ----

#' Calculates 3-Way Interactions
#'
#' Formula: \eqn{I(X;Y;C) = I(X,Y;C) - IG(X;C) - IG(Y;C)}, where \eqn{I(X;Y;C)}
#' is 3-way Interaction gain of the attributes \eqn{X} and \eqn{Y}, given the
#' context (i.e. class) attribute \eqn{C}. Hence, \eqn{I(X,Y;C)} is a joint
#' 2-way interaction gain (i.e. Information Gain) of the attributes \eqn{X} and
#' \eqn{Y}, and \eqn{I(X;C)} and \eqn{I(Y;C)} are 2-way Interaction gains
#' (i.e. Information Gains) of the attributes \eqn{X} and \eqn{Y}, respectively.
#'
#' @param df A discrete \code{data.frame}
#' @param classAtt A class column of the df (\code{string})
#' @param speedUp A (\code{boolean}) parameter. If \code{TRUE}, indicates
#'   whether the pairs of attributes with Information Gain equal to zero (on the
#'   4th decimal) should be pruned. This speeds up calculations for larger
#'   datasets. By default it is turned off (i.e. set to \code{FALSE}).
#' @return A list with a: 1) data-frame with 3-way interactions, 2)list of 2-way
#'   interactions of the input attributes
#' @export
#' @examples
#' \dontrun{interactions3Way(golf, "Play")}
#' \dontrun{interactions3Way(golf, "Play", speedUp = TRUE)}
#' \dontrun{interactions3Way(golf, "Play", speedUp = FALSE)}
#' @import dplyr gtools utils

interactions3Way <- function(df, classAtt, speedUp = FALSE){

  #Check input validity
  if (missing(df)) { #df is missing
    stop("Please provide a data.frame to the function")
  }

  if (missing(classAtt)) { #class attribute is missing
    stop("Please provide class attribute to the function")
  }

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
    lst3WayIntGain <- utils::combn(names(lst2WayIntGain), 2, simplify = FALSE)

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
    output <- list(df = df3WayIntGain, twoWayIG = lst2WayIntGain)
    return(output)
  }

  #not a discrete df, throw an error
  else {
    stop("The provided data.frame is not discrete (i.e. one of its columns
         is not factor)!")
  }

}

#Create interaction graph ----

#' Creates Interaction graph
#'
#' @param df A \code{discrete} \code{data.frame}
#' @param classAtt A class column of the df (\code{string})
#' @param intNo A desired number of interactions to show, i.e. an
#'   (\code{integer}) in range: [2,20]; Default value is 16.
#' @param speedUp A (\code{boolean}) parameter. If \code{TRUE}, indicates
#'   whether the pairs of attributes with Information Gain equal to zero (on the
#'   4th decimal) should be pruned. This speeds up calculations for larger
#'   datasets. By default it is turned off (i.e. set to \code{FALSE}).
#' @return An interaction graph object (\code{string})
#' @export
#' @examples
#' \dontrun{interactionGraph(golf, "Play")}
#' \dontrun{interactionGraph(golf, "Play", intNo = 10, speedUp = TRUE)}
#' @import dplyr

interactionGraph <- function(df, classAtt, intNo = 16, speedUp = FALSE) {

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

  if (intNo > 20) { #intNo is larger than 20, make it maximal (i.e. 20)
    intNo <- 20
    warning("Parameter intNo was larger than the maximum. It is scaled to fit
              the range [2,20], and now it is 20")
  }

  #Define interaction graph ----

  #Get interactions
  tmp <- interactions3Way(df, classAtt, speedUp) #Tuple (df, lst2WayIntGain)
  df <- tmp[[1]] #Unpack df
  lst2WayIntGain <- tmp[[2]] #Unpack lst2WayIntGain
  rm(tmp) #clean-up

  #Sort interactions df w.r.t. absolute interaction gain
  df$InteractionGain <- as.double(as.character(df$InteractionGain))
  df$intGainAbs <- abs(df$InteractionGain)
  df <- df[order(-df$intGainAbs),] #order df descending w.r.t. abs inter. gain.

  #Compute max possible number of interactions
  #Get number of input attributes
  noInAtts = length(unique(c(as.character(df[, 1]), as.character(df[, 2]))))
  combNo = (noInAtts * (noInAtts-1))/2 #no. of possible interactions for the df
  maxIntNo = min(combNo, intNo) #maximum number of interactions for the df

  #Check if requested number of interactions exceeds theoretical maximum, and if
  #true, warn the user that the intNo has changed
  if(intNo > maxIntNo) {
    warning(paste0("The requested number of interactions exceeds maximum
                   possible number of interactions for the given dataset.
                   IntNo parameter has been reduced to ", maxIntNo, "."))
  }

  #Select top k-strongest interactions
  iTotal <- df[1:maxIntNo,]

  #Create nodes
  lstIgNodes <- list() #intiialize empty list of nodes
  nodes <- unique(c(as.character(iTotal[, 1]), as.character(iTotal[, 2])))
  for (n in 1:length(nodes)) { #foreach node add corresp. igNode obj. to the lst
    lstIgNodes[[n]] <- igNode(nodes[n],
                            round(lst2WayIntGain[[nodes[n]]] * 100, 2))
  }

  #Create edges
  lstEdges <- list() #intiialize empty list of edges
  for (e in 1:nrow(iTotal)) { #foreach edge add corresp. igEdge obj. to the lst
    lstEdges[[e]] <- igEdge(as.character(iTotal[e, 1]),
                                as.character(iTotal[e, 2]),
                                round(iTotal[e, 3] * 100, 2))
  }

  #Create interaction graph object
  interGraph <- ig(lstIgNodes, lstEdges)

  #Convert interaction graph object to string & return it
  interGraph <- utils::capture.output(print(interGraph), file = NULL)
  interGraph <- paste(interGraph,collapse="\n")

  return(interGraph)

}
