#Plot graphviz ----

#' Plots Interaction graph
#'
#' @param ig Interaction graph
#' @return Plots the \code{ig}
#' @export
#' @examples
#' plotIntGraph(interactionGraph(golf, "Play", intNo = 10))
#' @import DiagrammeR DiagrammeRsvg rsvg

plotIntGraph <- function(ig) {

  #Check input validity

  if (missing(ig)) { #interaction graph is missing
    stop("Please provide an interaction graph object to the function")
  }

  if (typeof(ig) != "character" || grepl("InteractionGraph", ig) != TRUE) {
    #the ig parameter does not point to InteractionGraph definition string
    stop("Please provide Interaction graphs definition as the 'ig' parameter")
  }

  DiagrammeR::grViz(ig)
}

#Export graphviz ----

#' Exports Interaction graph to a GraphViz file
#'
#' @param ig Interaction graph
#' @param path The folder in which to write the GraphViz file;
#' @param fName The name of the file to be created; "InteractionGraph" by
#'   default
#' @return Writes the \code{ig} interaction graph to a GraphViz \code{.gv} file
#'   to the folder specified in the \code{path}
#' @export
#' @examples
#' #create temp dir path with slashes
#' myDir <- gsub("\\\\", "/", tempdir())
#'
#' #create interaction graph
#' g <- interactionGraph(golf, "Play", intNo = 10)
#'
#' #write to 'graphviz' file
#' igToGrViz(g, path = myDir, fName = "MyGraph")

igToGrViz <- function(ig, path = "", fName = "InteractionGraph") {

  #Check input validity

  if (missing(ig)) { #Interaction graph is missing
    stop("Please provide an interaction graph object to the function")
  }

  if (typeof(ig) != "character" || grepl("InteractionGraph", ig) != TRUE) {
    #the ig parameter does not point to InteractionGraph definition string
    stop("Please provide Interaction graphs definition as the 'ig' parameter")
  }

  if (path == "") { #The path parameter is missing
    stop("Please provide the 'path' parameter")
  }

  if (typeof(path) != "character" || grepl("\\\\", path) == TRUE) {
    #the path parameter is not defined properly
    stop("The 'path' parameter is not properly set. It needs to be string and
         without \\ symbol in the path")
  }

  if (typeof(fName) != "character" || grepl(" ", fName) == TRUE) {
    #the fName parameter is not defined properly
    stop("The 'fName' parameter is not a proper name for the interaction graph.
         It needs to be string and without empty spaces and extension")
  }

  #Write ig to .gv binary
  #Add trailing backslash if current path does not contains it
  if (substr(path, nchar(path), nchar(path)) != "/") {
    path <- paste0(path, "/")
  }

  file <- paste0(path, fName, ".gv") #set file name
  sink(file) #open connection
  cat(ig) #write graphviz contents
  sink() #close connection
}

#Export image (svg) ----

#' Exports Interaction graph to a SVG file
#'
#' @param ig Interaction graph
#' @param path The folder in which to write the SVG file;
#' @param fName The name of the file to be created; "InteractionGraph" by
#'   default
#' @param h Desired height of the image in pixels; 2000px by default
#' @return Writes the \code{ig} interaction graph to a SVG (\code{.svg}) file
#'   to the folder specified in the \code{path}
#' @export
#' @examples
#' #create temp dir path with slashes
#' myDir <- gsub("\\\\", "/", tempdir())
#'
#' #create interaction graph
#' g <- interactionGraph(golf, "Play", intNo = 10)
#'
#' #write to SVG
#' igToSVG(g, path = myDir, fName = "MyGraph", h = 2000)
#' @import DiagrammeR DiagrammeRsvg rsvg

igToSVG <- function(ig,
                    path = "",
                    fName = "InteractionGraph",
                    h = 2000) {

  #Check input validity

  if (missing(ig)) { #Interaction graph is missing
    stop("Please provide an interaction graph object to the function")
  }

  if (typeof(ig) != "character" || grepl("InteractionGraph", ig) != TRUE) {
    #the ig parameter does not point to InteractionGraph definition string
    stop("Please provide Interaction graphs definition as the 'ig' parameter")
  }

  if (path == "") { #The path parameter is missing
    stop("Please provide the 'path' parameter")
  }

  if (typeof(path) != "character" || grepl("\\\\", path) == TRUE) {
    #the path parameter is not defined properly
    stop("The 'path' parameter is not properly set. It needs to be string and
         without \\ symbol in the path")
  }

  if (typeof(fName) != "character" || grepl(" ", fName) == TRUE) {
    #the fName parameter is not defined properly
    stop("The 'fName' parameter is not a proper name for the interaction graph.
         It needs to be string and without empty spaces")
  }

  if (typeof(h) != "double") {
    #the height parameter is not defined properly
    stop("The 'height' parameter needs to be integer")
  }

  #Export ig to svg

  #Add trailing backslash if current path does not contains it
  if (substr(path, nchar(path), nchar(path)) != "/") {
    path <- paste0(path, "/")
  }

  DiagrammeRsvg::export_svg(grViz(ig)) %>%
    charToRaw %>% rsvg::rsvg_svg(file=paste0(path, fName, ".svg"),
                                 height = as.integer(h))
}

#Export image (png) ----

#' Exports Interaction graph to a PNG file
#'
#' @param ig Interaction graph
#' @param path The folder in which to write the PNG file;
#' @param fName The name of the file to be created; "InteractionGraph" by
#'   default
#' @param h Desired height of the image in pixels; 2000px by default
#' @return Writes the \code{ig} interaction graph to a PNG (\code{.png}) file
#'   to the folder specified in the \code{path}
#' @export
#' @examples
#' #create temp dir path with slashes
#' myDir <- gsub("\\\\", "/", tempdir())
#'
#' #create interaction graph
#' g <- interactionGraph(golf, "Play", intNo = 10)
#'
#' #write to PNG
#' igToPNG(g, path = myDir, fName = "MyGraph", h = 2000)
#' @import DiagrammeR DiagrammeRsvg rsvg

igToPNG <- function(ig,
                    path = "",
                    fName = "InteractionGraph",
                    h = 2000) {

  #Check input validity

  if (missing(ig)) { #Interaction graph is missing
    stop("Please provide an interaction graph object to the function")
  }

  if (typeof(ig) != "character" || grepl("InteractionGraph", ig) != TRUE) {
    #the ig parameter does not point to InteractionGraph definition string
    stop("Please provide Interaction graphs definition as the 'ig' parameter")
  }

  if (path == "") { #The path parameter is missing
    stop("Please provide the 'path' parameter")
  }

  if (typeof(path) != "character" || grepl("\\\\", path) == TRUE) {
    #the path parameter is not defined properly
    stop("The 'path' parameter is not properly set. It needs to be string and
         without \\ symbol in the path")
  }

  if (typeof(fName) != "character" || grepl(" ", fName) == TRUE) {
    #the fName parameter is not defined properly
    stop("The 'fName' parameter is not a proper name for the interaction graph.
         It needs to be string and without empty spaces")
  }

  if (typeof(h) != "double") {
    #the height parameter is not defined properly
    stop("The 'height' parameter needs to be integer")
  }


  #Export ig to png

  #Add trailing backslash if current path does not contains it
  if (substr(path, nchar(path), nchar(path)) != "/") {
    path <- paste0(path, "/")
  }

  DiagrammeRsvg::export_svg(grViz(ig)) %>%
    charToRaw %>% rsvg::rsvg_png(file=paste0(path, fName, ".png"),
                                 height = as.integer(h))
}

#Export image (pdf) ----

#' Exports Interaction graph to a PDF file
#'
#' @param ig Interaction graph
#' @param path The folder in which to write the PDF file;
#' @param fName The name of the file to be created; "InteractionGraph" by
#'   default
#' @param h Desired height of the image in pixels; 2000px by default
#' @return Writes the \code{ig} interaction graph to a PDF (\code{.pdf}) file
#'   to the folder specified in the \code{path}
#' @export
#' @examples
#' #create temp dir path with slashes
#' myDir <- gsub("\\\\", "/", tempdir())
#'
#' #create interaction graph
#' g <- interactionGraph(golf, "Play", intNo = 10)
#'
#' #write to PDF
#' igToPDF(g, path = myDir, fName = "MyGraph", h = 2000)
#' @import DiagrammeR DiagrammeRsvg rsvg

igToPDF <- function(ig,
                    path = "",
                    fName = "InteractionGraph",
                    h = 2000) {

  #Check validity of input

  if (missing(ig)) { #Interaction graph is missing
    stop("Please provide an interaction graph object to the function")
  }

  if (typeof(ig) != "character" || grepl("InteractionGraph", ig) != TRUE) {
    #the ig parameter does not point to InteractionGraph definition string
    stop("Please provide Interaction graphs definition as the 'ig' parameter")
  }

  if (path == "") { #The path parameter is missing
    stop("Please provide the 'path' parameter")
  }

  if (typeof(path) != "character" || grepl("\\\\", path) == TRUE) {
    #the path parameter is not defined properly
    stop("The 'path' parameter is not properly set. It needs to be string and
         without \\ symbol in the path")
  }

  if (typeof(fName) != "character" || grepl(" ", fName) == TRUE) {
    #the fName parameter is not defined properly
    stop("The 'fName' parameter is not a proper name for the interaction graph.
         It needs to be string and without empty spaces")
  }

  if (typeof(h) != "double") {
    #the height parameter is not defined properly
    stop("The 'height' parameter needs to be integer")
  }

  #Export ig to pdf

  #Add trailing backslash if current path does not contains it
  if (substr(path, nchar(path), nchar(path)) != "/") {
    path <- paste0(path, "/")
  }

  DiagrammeRsvg::export_svg(grViz(ig)) %>%
    charToRaw %>% rsvg::rsvg_pdf(file=paste0(path, fName, ".pdf"),
                                 height = as.integer(h))

}

#Export image (ps) ----

#' Exports Interaction graph to a PS (PostScript) file
#'
#' @param ig Interaction graph
#' @param path The folder in which to write the PS file;
#' @param fName The name of the file to be created; "InteractionGraph" by
#'   default
#' @param h Desired height of the image in pixels; 2000px by default
#' @return Writes the \code{ig} interaction graph to a PostScript (\code{.ps})
#'   file to the folder specified in the \code{path}
#' @export
#' @examples
#' \donttest{
#' #create temp dir path with slashes
#' myDir <- gsub("\\\\", "/", tempdir())
#'
#' #create interaction graph
#' g <- interactionGraph(golf, "Play", intNo = 10)
#'
#' #write to PS
#' igToPS(g, path = myDir, fName = "MyGraph", h = 2000) }
#' @import DiagrammeR DiagrammeRsvg rsvg

igToPS <- function(ig,
                   path = "",
                   fName = "InteractionGraph",
                   h = 2000) {

  #Check validity of input

  if (missing(ig)) { #Interaction graph is missing
    stop("Please provide an interaction graph object to the function")
  }

  if (typeof(ig) != "character" || grepl("InteractionGraph", ig) != TRUE) {
    #the ig parameter does not point to InteractionGraph definition string
    stop("Please provide Interaction graphs definition as the 'ig' parameter")
  }

  if (path == "") { #The path parameter is missing
    stop("Please provide the 'path' parameter")
  }

  if (typeof(path) != "character" || grepl("\\\\", path) == TRUE) {
    #the path parameter is not defined properly
    stop("The 'path' parameter is not properly set. It needs to be string and
         without \\ symbol in the path")
  }

  if (typeof(fName) != "character" || grepl(" ", fName) == TRUE) {
    #the fName parameter is not defined properly
    stop("The 'fName' parameter is not a proper name for the interaction graph.
         It needs to be string and without empty spaces")
  }

  if (typeof(h) != "double") {
    #the height parameter is not defined properly
    stop("The 'height' parameter needs to be integer")
  }

  #Export ig to ps

  #Add trailing backslash if current path does not contains it
  if (substr(path, nchar(path), nchar(path)) != "/") {
    path <- paste0(path, "/")
  }

  DiagrammeRsvg::export_svg(grViz(ig)) %>%
    charToRaw %>% rsvg::rsvg_ps(file=paste0(path, fName, ".ps"),
                                 height = as.integer(h))

}
