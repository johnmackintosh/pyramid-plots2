#' Title
#'
#' @param sourcedata original source data
#' @param councilcol name of column containing council values
#' @param councilval desired council area
#' @param localitycol name of column containing locality values
#' @param localityval desired locality, if appropriate
#' @param xcol  column containing age band descriptions
#' @param ycol column containing age band totals or percentages
#' @param fill_col column that defines the correct sex for the male/female split
#' @param male_col colour for male column
#' @param female_col colour for female column
#' @param facet_cols number of columns to facet by
#' @param facet_scales should scales be `free` or `fixed`, passed to `facet_wrap`
#' @param nbreaks number of breaks on the ageband value axis
#' @param chart_title chart title
#' @param chart_subtitle chart subtitle
#' @param chart_caption chart caption
#' @param xlabs x axis labels
#' @param ylabs y axis labels
#' @param printed should the plot be printed
#' @param percentage are the y values percentages? If so, the axis values will
#' be formatted to display "%"
#' @param ... any additional values passed will be used to facet the final plot
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' facet_pyramid_percent(sourcedata = child_age_band_percent_tots,
#' councilcol = Council_area_name,
#' councilval = "Highland",
#' localitycol = CP_Name,
#' localityval = NULL,
#' xcol = child_age_band,
#' ycol = percent_of_tot,
#' fill_col = Sex,
#' male_col = '#0391BF',
#' female_col ="grey70",
#' facet_cols = 3,
#' facet_scales = "free",
#' nbreaks = 8,
#' chart_title = NULL,
#' chart_subtitle = NULL,
#' chart_caption = NULL,
#' xlabs = "Age band",
#' ylabs = "Population",
#' printed = TRUE,
#' CP_Name)
#'
#'
#'

phi_pop_pyramid_facet <- function(sourcedata = pyramid_percent_tots,
                                  councilcol,
                                  councilval = NULL,
                                  localitycol,
                                  localityval = NULL,
                                  xcol,
                                  ycol,
                                  fill_col = Sex,
                                  male_col = '#0391BF',
                                  female_col ="grey70",
                                  facet_cols = 2,
                                  facet_scales = "fixed",
                                  nbreaks = 8,
                                  chart_title = NULL,
                                  chart_subtitle = NULL,
                                  chart_caption = NULL,
                                  xlabs = "Age band",
                                  ylabs = "Population",
                                  printed = TRUE,
                                  percentage = FALSE,
                                  ...) {

# helper function to wrap by whatever is passed to the dots argument
  wrap_by <- function(...) {
    facet_wrap(vars(...),
               ncol = facet_cols,
               scales = facet_scales,
               labeller = label_value)
  }


  # filter for specific council first

  if (!is.null(councilval)) {
    sourcedata <- sourcedata %>%
      dplyr::filter( {{councilcol}} == councilval)
  }


  # then filter for locality

  if (!is.null(localityval)) {
    sourcedata <- sourcedata %>%
      dplyr::filter( {{localitycol}} == localityval)
  }


  sourcedata <- sourcedata %>%
    dplyr::mutate({{ycol}} := dplyr::case_when(
      {{fill_col}} == "Male" ~ {{ycol}} * -1,
      .default = {{ycol}}),
      col_breaks = {{ycol}}) %>%


  # tempdf <- sourcedata  %>%
    dplyr::mutate(., {{fill_col}} := sex_as_factor({{fill_col}}))


  female_data = sourcedata  %>%
    dplyr::filter(.,{{fill_col}} == "Female")

  male_data = sourcedata %>%
    dplyr::filter(., {{fill_col}} == "Male")

  p <- ggplot2::ggplot(data = NULL,
                       ggplot2::aes({{xcol}},
                                    {{ycol}},
                                    fill = {{fill_col}})) +
    ggplot2::geom_col(data = female_data) +
    ggplot2::geom_col(data = male_data)



  if (percentage) {

    p <- p +  ggplot2::scale_y_continuous(breaks = pretty(sourcedata$col_breaks,
                                                          n = nbreaks),
                                          labels = paste0(
                                            abs(
                                              pretty(sourcedata$col_breaks,
                                                     n = nbreaks)
                                            ),
                                            "%"))
  }


  if (!percentage) {

    p <- p +  ggplot2::scale_y_continuous(breaks = pretty(sourcedata$col_breaks,
                                                          n = nbreaks),
                                          labels =
                                            abs(
                                              pretty(sourcedata$col_breaks,
                                                     n = nbreaks)
                                            ))
  }

  p <- p +  ggplot2::coord_flip()

  p <- p + wrap_by(...)

  p <- apply_chart_opts(p)

  p <- p +  ggplot2::scale_fill_manual("",
                                       guide = "legend",
                                       values = c("Male" = male_col,
                                                  "Female" = female_col),
                                       labels = c("Female", "Male"))

  p <- p +  ggplot2::labs(title = chart_title,
                          subtitle = chart_subtitle,
                          caption = chart_caption,
                          x = xlabs,
                          y = ylabs)


  print(p)

}
