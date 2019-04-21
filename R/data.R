#' Golf example dataset for Interaction graphs
#'
#' An example dataset containing the discrete data.frame (i.e. all columns are
#' factors) with variables used as an input for making a decision whether a
#' party of golf would be played, or not.
#'
#' @format A data.frame with 6 discrete variables (i.e. factors) and 14 rows
#'   (i.e. observations). 5 input variables and 1 class (i.e. context) variable:
#'   \describe{
#'     \item{Outlook}{Input attribute, values: Overcast, Rainy, Sunny}
#'     \item{Temperature}{Input attribute, values: Cool, Hot, Mild}
#'     \item{Humidity}{Input attribute, values: High, Normal}
#'     \item{Windy}{Input attribute, values: True, False}
#'     \item{Others}{Artifically added input attribute indicating whether the
#'     players on the other courts were playing the golf at the given time,
#'     values: Yes, No}
#'     \item{Play}{Class attribute, indicating whether the decision was to play
#'     or not to play a party of golf, values: Yes, No}
#'   }
#'   @source \url{https://gerardnico.com/data_mining/weather}
"golf"
