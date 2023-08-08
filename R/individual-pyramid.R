# plot per HSCP

phi_pop_pyramid <- function(sourcedata = pyramid_tots,
                        hscpval,
                        xcol = pop_age_band,
                        ycol = pop_age_band_total,
                        fill_col = Sex,
                        male_col = '#0391BF',
                        female_col ="grey70",
                        chart_title = NULL,
                        chart_subtitle = NULL,
                        chart_caption = NULL,
                        xlabs = "Age band",
                        ylabs = "Population",
                        nbreaks = 8,
                        printed = TRUE,
                        save_plot = FALSE,
                        extension = "png",
                        w = 4,
                        h = 5,
                       location = "./") {


  tempdf <- collapse::fsubset(sourcedata,
                              SubHSCPName ==  hscpval) %>%
    collapse::fmutate(
      {{ycol}} := dplyr::case_when(
        Sex == "Male" ~ {{ycol}} * -1,
        .default = {{ycol}}))

  female_data = tempdf %>% filter({{fill_col}} == "Female")
  male_data = tempdf %>% filter({{fill_col}} == "Male")

  p <- ggplot2::ggplot(data = NULL,
                       ggplot2::aes({{xcol}},
                                    {{ycol}},
                                    fill = {{fill_col}})) +
    ggplot2::geom_col(data = female_data) +
    ggplot2::geom_col(data = male_data)

  p <- p + ggplot2::scale_fill_manual("",
                             values = c("Male" = male_col,
                                     'Female' = female_col),
                             labels = c("Male", "Female"))

p <- p +  ggplot2::scale_y_continuous(breaks = pretty(tempdf$pop_age_band_total,
                                                      n = nbreaks),
                       labels = abs(pretty(tempdf$pop_age_band_total,
                                           n = nbreaks)))

p <- p +  ggplot2::coord_flip()

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





