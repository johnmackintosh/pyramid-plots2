#' Applies specific options to chart title and caption
#'
#' Places legend to top, sets justification to 0 for title and caption, and
#' sets title font text to 14
#'
#' @param p existing ggplot2 object to be modified
#'
#' @return a modified ggplot2 object
#' @export
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' p <-  phicharts::phi_base_plot(mtcars,cyl,mpg) + geom_point()
#' p <- apply_chart_opts(p)
#'
#'
#' }
#'
apply_chart_opts <- function(p){

  # ensure is ggplot2 object
  checkmate::assert_class(p, c("gg", "ggplot"))


  p <- p +  ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0)) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0),
                   plot.caption.position = "plot")

  p <- removeGrid(p)
  p

}
