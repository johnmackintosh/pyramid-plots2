#' For pyramid plots or other plots where a split is required on the x axis
#' multiplies target value by -1
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
negater <- function(x, print = TRUE){

  x <- (x * -1)
  if (print) {x}
}



sex_as_factor <- function(x) {

  x <- factor(x,
              levels = c("Female", "Male", "persons"),
              labels = c("Female", "Male", "Persons"),
              ordered = TRUE)
  x

}


# the following functions are taken from ggExtra

#' Remove grid lines from ggplot2
#'
#' Remove grid lines from a ggplot2 plot, to have a cleaner and simpler
#' plot
#'
#' Minor grid lines are always removed.
#'
#' \code{removeGrid} removes the major grid lines from the x and/or y axis
#' (both by default).
#'
#' \code{removeGridX} is a shortcut for \code{removeGrid(x = TRUE, y = FALSE)}
#'
#' \code{removeGridY} is a shortcut for \code{removeGrid(x = FALSE, y = TRUE)}
#'
#'@param p a ggplot2 object to modify
#' @param x Whether to remove grid lines from the x axis.
#' @param y Whether to remove grid lines from the y axis.
#' @return A ggplot2 layer that can be added to an existing ggplot2 object.
#' @examples
#' df <- data.frame(x = 1:50, y = 1:50)
#' p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
#' p2 <- p
#' p3 <- p
#' p <-  removeGrid(p)
#' p2 <-  removeGridY(p2)
#' p3 <-  removeGridX(p3)
#' @name removeGrid
NULL

#' @export
#' @rdname removeGrid
removeGrid <- function(p,
                       x = TRUE,
                       y = TRUE) {

  p <- p  +  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  if (x) {
    p <- p +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  }
  if (y) {
    p <- p +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }

  p
}

#' @export
#' @rdname removeGrid
removeGridX <- function(p) {
  p <- removeGrid(p, x = TRUE, y = FALSE)
  p
}

#' @export
#' @rdname removeGrid
removeGridY <- function(p) {
  p <- removeGrid(p, x = FALSE, y = TRUE)
  p
}


#' Rotate ggplot x-axis labels
#'
#' @param p existing ggplot object
#' @param angle angle to rotate axis label text
#'
#' @return a ggplot object with rotated x-axis labels
#' @export
#'
#' @examples
#'
#' df <- data.frame(x = 1:50, y = 1:50)
#' p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
#' p <- rotateTextX(p)
#'
#'
#'
#'
rotateTextX <- function(p,
                        angle = 90){
  p <- p +  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle,
                                                               hjust = 1,
                                                               vjust = 0.5))
  p
}



#' child_age_bander
#'
#' assigns each element in a vector of ages to a specific age band
#' based on the child age bands commonly in use in Public Health
#'
#' @param df dataframe or tibble
#' @param x column to use as a base for age_band
#' @param factored convert age_band to ordered factor?
#'
#' @return
#' @export
#'
#' @examples
child_age_bander <- function(.df,
                             x,
                             factored = TRUE,
                             printout = FALSE){

  .df <- .df %>%
    dplyr::mutate(child_age_band =
                    dplyr::case_when(
                      {{x}} == 0 ~ "0",
                      {{x}} %in% c(1:4) ~ "01-04",
                      {{x}} %in% c(05:11) ~ "05-11",
                      {{x}} %in% c(12:17) ~ "12-17",
                      {{x}} %in% c(18:24) ~ "18-24",
                      {{x}} >= 25  ~ "25+",
                      TRUE ~ NA_character_))

  if (factored) {


    .df <- .df %>%
      dplyr::mutate(child_age_band = factor(child_age_band,
                                            levels = c("0",
                                                       "01-04",
                                                       "05-11",
                                                       "12-17",
                                                       "18-24",
                                                       "25+"),
                                            ordered = TRUE)
      )


  }




}









