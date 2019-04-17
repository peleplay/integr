#Plot graphviz
#' Plots Interaction graph
#'
#' @param ig Interaction graph
#' @return Plots the \code{ig}
#' @export
plotIntGraph <- function(ig) {
  DiagrammeR::grViz(ig)
}

#Export graphviz ----

#' Exports Interaction graph to a GraphViz file
#'
#' @param ig Interaction graph
#' @param path The folder in which to write the GraphViz file; by default, the
#'   working directory
#' @param fName The name of the file to be created; "InteractionGraph" by
#'   default
#' @return Writes the \code{ig} interaction graph to a GraphViz \code{.gv} file
#'   to the specified folder in \code{path}
#' @export

igToGrViz <- function(ig, path = getwd(), fName = "InteractionGraph") {

  #Check input validity : Depreciated
  # if (typeof(ig) != "character" || grepl("InteractionGraph", ig) != TRUE) {
  #   #the ig parameter does not point to InteractionGraph definition string
  #   stop("Please provide Interaction graphs definition as the 'ig' parameter")
  # }

  if (typeof(ig) != "character") {
    #the ig parameter does not point to InteractionGraph definition string
    stop("Please provide Interaction graphs definition as the 'ig' parameter")
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

  #Write ig to .gv binary
  if(path == getwd()){ #Add trailing backslash if current wd is used
    path <- paste0(path, "/")
  }

  currGraph <- paste0(path, fName, ".gv") #set file name
  sink(currGraph) #open connection
  cat(ig) #write graphviz contents
  sink() #close connection

}

#Export image (svg) ----

#' Exports Interaction graph to a SVG file
#'
#' @param ig Interaction graph
#' @param path The folder in which to write the SVG file; by default,
#'   the working directory
#' @param fName The name of the file to be created; "InteractionGraph" by
#'   default
#' @param height Desired height of the image in pixels; 2000px by default
#' @return Writes the \code{ig} interaction graph to a SVG (\code{.svg}) file
#'   to the specified folder in \code{path}
#' @export

igToSVG <- function(ig,
                    path = getwd(),
                    fName = "InteractionGraph",
                    height = 2000) {

  #Check validity of input
  #Check input validity : Depreciated
  # if (typeof(ig) != "character" || grepl("InteractionGraph", ig) != TRUE) {
  #   #the ig parameter does not point to InteractionGraph definition string
  #   stop("Please provide Interaction graphs definition as the 'ig' parameter")
  # }

  if (typeof(ig) != "character") {
    #the ig parameter does not point to InteractionGraph definition string
    stop("Please provide Interaction graphs definition as the 'ig' parameter")
  }

  if (typeof(path) != "character" || grepl("\\", path) == TRUE) {
    #the path parameter is not defined properly
    stop("The 'path' parameter is not properly set. It needs to be string and
         without \\ symbol in the path")
  }

  if (typeof(fName) != "character" || grepl(" ", fName) == TRUE) {
    #the fName parameter is not defined properly
    stop("The 'fName' parameter is not a proper name for the interaction graph.
         It needs to be string and without empty spaces")
  }

  if (typeof(height) != "double" || typeof(height) != "integer") {
    #the height parameter is not defined properly
    stop("The 'height' parameter needs to be integer")
  }


  #Export ig to svg
  ig %>% DiagrammeRsvg::export_svg(height = height,
                                   file = paste0(path, fName, ".svg"))

}

#Export image (png) ----

#' Exports Interaction graph to a PNG file
#'
#' @param ig Interaction graph
#' @param path The folder in which to write the PNG file; by default,
#'   the working directory
#' @param fName The name of the file to be created; "InteractionGraph" by
#'   default
#' @param height Desired height of the image in pixels; 2000px by default
#' @param dpi Desired resolution of the image in dpi; 300dpi by default
#' @return Writes the \code{ig} interaction graph to a PNG (\code{.png}) file
#'   to the specified folder in \code{path}
#' @export

igToPNG <- function(ig,
                    fName = "InteractionGraph",
                    path = getwd(),
                    height = 2000,
                    dpi = 300) {

  #Check validity of input
  #Check input validity : Depreciated
  # if (typeof(ig) != "character" || grepl("InteractionGraph", ig) != TRUE) {
  #   #the ig parameter does not point to InteractionGraph definition string
  #   stop("Please provide Interaction graphs definition as the 'ig' parameter")
  # }

  if (typeof(ig) != "character") {
    #the ig parameter does not point to InteractionGraph definition string
    stop("Please provide Interaction graphs definition as the 'ig' parameter")
  }

  if (typeof(path) != "character" || grepl("\\", path) == TRUE) {
    #the path parameter is not defined properly
    stop("The 'path' parameter is not properly set. It needs to be string and
         without \\ symbol in the path")
  }

  if (typeof(fName) != "character" || grepl(" ", fName) == TRUE) {
    #the fName parameter is not defined properly
    stop("The 'fName' parameter is not a proper name for the interaction graph.
         It needs to be string and without empty spaces")
  }

  if (typeof(height) != "double" || typeof(height) != "integer") {
    #the height parameter is not defined properly
    stop("The 'height' parameter needs to be integer")
  }

  if (typeof(dpi) != "double" || typeof(dpi) != "integer") {
    #the dpi parameter is not defined properly
    stop("The 'dpi' parameter needs to be integer")
  }

  #Export ig to png
  ig %>%
    DiagrammeRsvg::export_svg %>%
    charToRaw %>%
    rsvgL::rsvg(height = height) %>%
    png::writePNG(paste0(path, fName, ".png"), dpi = dpi)

}

#Export image (pdf) ----

#' Exports Interaction graph to a PDF file
#'
#' @param ig Interaction graph
#' @param path The folder in which to write the PDF file; by default,
#'   the working directory
#' @param fName The name of the file to be created; "InteractionGraph" by
#'   default
#' @param height Desired height of the image in pixels; 2000px by default
#' @return Writes the \code{ig} interaction graph to a PDF (\code{.pdf}) file
#'   to the specified folder in \code{path}
#' @export

igToPDF <- function(ig,
                    fName = "InteractionGraph",
                    path = getwd(),
                    height = 2000) {

  #Check validity of input
  #Check input validity : Depreciated
  # if (typeof(ig) != "character" || grepl("InteractionGraph", ig) != TRUE) {
  #   #the ig parameter does not point to InteractionGraph definition string
  #   stop("Please provide Interaction graphs definition as the 'ig' parameter")
  # }

  if (typeof(ig) != "htmlwidget") {
    #the ig parameter does not point to InteractionGraph definition string
    stop("Please provide Interaction graphs definition as the 'ig' parameter")
  }

  if (typeof(path) != "character" || grepl("\\", path) == TRUE) {
    #the path parameter is not defined properly
    stop("The 'path' parameter is not properly set. It needs to be string and
         without \\ symbol in the path")
  }

  if (typeof(fName) != "character" || grepl(" ", fName) == TRUE) {
    #the fName parameter is not defined properly
    stop("The 'fName' parameter is not a proper name for the interaction graph.
         It needs to be string and without empty spaces")
  }

  if (typeof(height) != "double" || typeof(height) != "integer") {
    #the height parameter is not defined properly
    stop("The 'height' parameter needs to be integer")
  }

  #Export ig to pdf
  ig %>% DiagrammeRsvg::export_pdf(height = height,
                                   file = paste0(path, fName, ".pdf"))

}

#Export image (ps) ----

#' Exports Interaction graph to a PS (PostScript) file
#'
#' @param ig Interaction graph
#' @param path The folder in which to write the PS file; by default,
#'   the working directory
#' @param fName The name of the file to be created; "InteractionGraph" by
#'   default
#' @param height Desired height of the image in pixels; 2000px by default
#' @return Writes the \code{ig} interaction graph to a PDF (\code{.ps}) file
#'   to the specified folder in \code{path}
#' @export

igToPS <- function(ig,
                    fName = "InteractionGraph",
                    path = getwd(),
                    height = 2000) {

  #Check validity of input
  #Check input validity : Depreciated
  # if (typeof(ig) != "character" || grepl("InteractionGraph", ig) != TRUE) {
  #   #the ig parameter does not point to InteractionGraph definition string
  #   stop("Please provide Interaction graphs definition as the 'ig' parameter")
  # }

  if (typeof(ig) != "htmlwidget") {
    #the ig parameter does not point to InteractionGraph definition string
    stop("Please provide Interaction graphs definition as the 'ig' parameter")
  }

  if (typeof(path) != "character" || grepl("\\", path) == TRUE) {
    #the path parameter is not defined properly
    stop("The 'path' parameter is not properly set. It needs to be string and
         without \\ symbol in the path")
  }

  if (typeof(fName) != "character" || grepl(" ", fName) == TRUE) {
    #the fName parameter is not defined properly
    stop("The 'fName' parameter is not a proper name for the interaction graph.
         It needs to be string and without empty spaces")
  }

  if (typeof(height) != "double" || typeof(height) != "integer") {
    #the height parameter is not defined properly
    stop("The 'height' parameter needs to be integer")
  }

  #Export ig to ps
  ig %>% DiagrammeRsvg::export_ps(height = height,
                                   file = paste0(path, fName, ".ps"))

}
