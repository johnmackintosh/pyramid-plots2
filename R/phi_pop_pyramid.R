# plot per HSCP

phi_pop_pyramid <- function(sourcedata = pyramid_tots,
                        hscpval,
                        filter_col = CP_Name,
                        xcol = pop_age_band,
                        ycol = pop_age_band_total,
                        fill_col = Sex,
                        male_val = "Male",
                        female_val = "Female",
                        male_col = '#0391BF',
                        female_col ="grey70",
                        chart_title = "",
                        chart_subtitle = "",
                        chart_caption = "",
                        xlabs = "",
                        ylabs = "",
                        nbreaks = 8,
                        printed = TRUE,
                        save_plot = FALSE,
                        extension = "png",
                        w = 4,
                        h = 5,
                       location = "./") {


# negate the male column values
# col_breaks so we have a known name for prettifying y axis breaks & labels

  tempdf <-  sourcedata |>
    dplyr::filter({{filter_col}} == hscpval) |>
    dplyr::mutate({{ycol}} := dplyr::case_when(
        {{fill_col}} == male_val ~ {{ycol}} * -1,
        .default = {{ycol}}),
        col_breaks = {{ycol}})

tempdf <- tempdf |>
  dplyr::mutate({{fill_col}} := sex_as_factor({{fill_col}}))

  female_data = tempdf |> dplyr::filter({{fill_col}} == female_val)
  male_data = tempdf |> dplyr::filter({{fill_col}} == male_val)

  p <- ggplot2::ggplot(data = NULL,
                       ggplot2::aes({{xcol}},
                                    {{ycol}},
                                    fill = {{fill_col}})) +
    ggplot2::geom_col(data = female_data) +
    ggplot2::geom_col(data = male_data)

 p <- p +  ggplot2::coord_flip()

p <- p + ggplot2::scale_fill_manual("",
                          guide = "legend",
                           values = c(male_val = male_col,
                                   female_val = female_col),
                           labels = c(female_val, male_val))



p <- p +  ggplot2::scale_y_continuous(breaks = pretty(tempdf$col_breaks,
                                                      n = nbreaks),
                                      labels = abs(pretty(tempdf$col_breaks,
                                                          n = nbreaks)))



p <- apply_chart_opts(p)

p <- p +  ggplot2::labs(title = chart_title,
         subtitle = chart_subtitle,
         caption = chart_caption,
         x = xlabs,
         y = ylabs)

  if (printed) {
    print(p)
  }


if (save_plot) {

  plotfilename <- paste0(hscpval, ".", extension)

  ggplot2::ggsave(filename = plotfilename,
                  width = w,
                  height = h,
                  path = location)
}

}





