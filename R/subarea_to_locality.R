subarea_to_locality <- function(.df = pyramid_tots,
                                summary_var,
                                ...){



  # separate processing of intermediate dataframe depending on A&B or Highland
  #

    out <- .df %>%
      dplyr::mutate(SubHSCPName := dplyr::case_when(
        SubHSCPName == ("Mull, Iona, Coll, Tiree and Colonsay") ~ "Oban, Lorn and the Isles",
        SubHSCPName %in% c("Bute", "Cowal") ~ "Bute and Cowal",
        SubHSCPName == "Oban, Lorn and the Inner Isles" ~ "Oban, Lorn and the Isles",
        SubHSCPName == "Lorn and the Inner Isles" ~ "Oban, Lorn and the Isles",
        SubHSCPName == "Islay and Jura" ~ "Mid-Argyll, Kintyre and Islay",
        SubHSCPName %in% c("Kintyre", "Mid Argyll") ~ "Mid-Argyll, Kintyre and Islay",
        SubHSCPName == "Skye and Lochalsh" ~ "Skye, Lochalsh and West Ross",
        SubHSCPName == "Ross and Cromarty West" ~ "Skye, Lochalsh and West Ross",
        SubHSCPName == "Nairn" ~ "Nairn and Nairnshire",
        SubHSCPName == "Badenoch and Strathspey" ~ "Badenoch and Strathspey",
        .default = SubHSCPName))



  # No need to summarise here as we are only changing display names,
  # not reassigning smaller areas to larger ones
#
#   if (target_val == "Highland") {
#
#     out <- .df %>%
#       dplyr::filter(Council_area_name == target_val) %>%
#       dplyr::mutate(SubHSCPName := dplyr::case_when(
#         SubHSCPName == "Skye and Lochalsh" ~ "Skye, Lochalsh and West Ross",
#         SubHSCPName == "Ross and Cromarty West" ~ "Skye, Lochalsh and West Ross",
#         SubHSCPName == "Nairn" ~ "Nairn and Nairnshire",
#         SubHSCPName == "Badenoch and Strathspey" ~ "Badenoch and Strathspey",
#         SubHSCPName == "Caithness" ~ SubHSCPName,
#         SubHSCPName == "East Ross" ~  SubHSCPName,
#         SubHSCPName== "Inverness" ~ SubHSCPName,
#         SubHSCPName == "Lochaber" ~ SubHSCPName,
#         SubHSCPName == "Mid Ross" ~ SubHSCPName,
#         SubHSCPName == "Sutherland" ~ SubHSCPName,
#         .default = SubHSCPName))
#
#   }


  out <- out %>%
    dplyr::group_by(...) %>%
    #dplyr::group_by(Sex, pop_age_band, SubHSCPName, Council_area_name) %>%
    #dplyr::summarise(pop_age_band_total = sum(pop_age_band_total)) %>%
    dplyr::summarise({{summary_var}} := sum({{summary_var}})) %>%
    dplyr::ungroup()


  data.table::setDT(out)


  return(out)
}
