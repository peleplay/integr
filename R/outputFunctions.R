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

igToGrViz <- function(ig, path, fName = "InteractionGraph") {

  #TODO: define

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
#' @param width Desired width of the image in pixels; 2000px by default
#' @param dpi Desired resolution of the image in dpi; 300dpi by default
#' @return Writes the \code{ig} interaction graph to a SVG (\code{.svg}) file
#'   to the specified folder in \code{path}
#' @export

igToSVG <- function(ig,
                    path = getwd(),
                    fName = "InteractionGraph",
                    height = 2000,
                    width = 2000,
                    dpi = 300) {

  #TODO: define

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
#' @param width Desired width of the image in pixels; 2000px by default
#' @param dpi Desired resolution of the image in dpi; 300dpi by default
#' @return Writes the \code{ig} interaction graph to a PNG (\code{.png}) file
#'   to the specified folder in \code{path}
#' @export

igToPNG <- function(ig,
                    fName = "InteractionGraph",
                    path = getwd(),
                    height = 2000,
                    width = 2000,
                    dpi = 300) {

  #TODO: define

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
#' @param width Desired width of the image in pixels; 2000px by default
#' @param dpi Desired resolution of the image in dpi; 300dpi by default
#' @return Writes the \code{ig} interaction graph to a PDF (\code{.pdf}) file
#'   to the specified folder in \code{path}
#' @export

igToPNG <- function(ig,
                    fName = "InteractionGraph",
                    path = getwd(),
                    height = 2000,
                    width = 2000,
                    dpi = 300) {

  #TODO: define

}
