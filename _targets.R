library(targets)

tar_source("R")


tar_option_set(packages = c("collapse",
                            "data.table",
                            "forcats",
                            "dplyr",
                            "rio",
                            "tidyr"),
               format = "rds",
               error = NULL) # nolint

# End this file with a list of target objects.
list(
  tar_target(sape_source_male,
             command = "./inputs/sape-2021-males.xlsx",
             format = "file"),

  tar_target(sape_source_female,
             command = "./inputs/sape-2021-females.xlsx",
             format = "file"),

  tar_target(sape_source_persons,
             command = "./inputs/sape-2021-persons.xlsx",
             format = "file"),

  tar_target(sape_long_term_source,
             command = "./inputs/dz2011-pop-est_09092022.csv",
             format = "file"),

  tar_target(sape_male,
             command = rio::import(sape_source_male, which = 2, skip = 2)),

  tar_target(sape_female,
             command = rio::import(sape_source_female, which = 2, skip = 2)),

  tar_target(sape_persons,
             command = rio::import(sape_source_persons, which = 2, skip = 2)),

  tar_target(hscp_lookup,
             command = "./inputs/profiles_dz_iz_cp_lookup.csv",
             format = "file"),

  tar_target(sgurc_lookup,
             command = "./inputs/datazone2011_urban_rural_2020.csv",
             format = "file"),


  tar_target(sape_wide_male,
             import_and_join(sape_male,
                             hscp_lookup,
                             sgurc_lookup)),

  tar_target(sape_wide_male_scotland,
             import_and_join(sape_male, hscp_lookup, sgurc_lookup,
                             nhsh_only = FALSE)),

  tar_target(sape_wide_female,
             import_and_join(sape_female, hscp_lookup, sgurc_lookup)),

  tar_target(sape_wide_female_scotland,
             import_and_join(sape_female, hscp_lookup, sgurc_lookup,
                             nhsh_only = FALSE)),

  tar_target(sape_wide_persons,
             import_and_join(sape_persons, hscp_lookup, sgurc_lookup)),

  tar_target(sape_wide_persons_scotland,
             import_and_join(sape_persons, hscp_lookup, sgurc_lookup,
                             nhsh_only = FALSE)),

  tar_target(sape_tidy_male,
             transformDT(filename = sape_wide_male,
                         sex = "male")),

  tar_target(sape_tidy_female,
             transformDT(filename = sape_wide_female,
                         sex = "female")),

  tar_target(sape_tidy_persons,
             transformDT(filename = sape_wide_persons,
                         sex = "persons")),

  tar_target(sape_tidy_male_scotland,
             transformDT(filename = sape_wide_male_scotland,
                         sex = "male",
                         names_to_keep = c("DataZone",
                                           "UrbanRural2fold2020",
                                           "UrbanRural3fold2020",
                                           "UrbanRural6fold2020",
                                           "UrbanRural8fold2020",
                                           "Data_zone_code",
                                           "Data_zone_name",
                                           "Council_area_code",
                                           "Council_area_name",
                                           "Sex",
                                           "Total_population"))),

  tar_target(sape_tidy_female_scotland,
             transformDT(filename = sape_wide_female_scotland,
                         sex = "female",
                         names_to_keep = c("DataZone",
                                           "UrbanRural2fold2020",
                                           "UrbanRural3fold2020",
                                           "UrbanRural6fold2020",
                                           "UrbanRural8fold2020",
                                           "Data_zone_code",
                                           "Data_zone_name",
                                           "Council_area_code",
                                           "Council_area_name",
                                           "Sex",
                                           "Total_population"))),

  tar_target(sape_tidy_persons_scotland,
             transformDT(filename = sape_wide_persons_scotland,
                         sex = "persons",
                         names_to_keep = c("DataZone",
                                           "UrbanRural2fold2020",
                                           "UrbanRural3fold2020",
                                           "UrbanRural6fold2020",
                                           "UrbanRural8fold2020",
                                           "Data_zone_code",
                                           "Data_zone_name",
                                           "Council_area_code",
                                           "Council_area_name",
                                           "Sex",
                                           "Total_population"))),

  tar_target(nhsh_combined,
             merge_all_tidy(sape_tidy_male,
                            sape_tidy_female,
                            sape_tidy_persons)),

  tar_target(scotland_combined_age_band,
             merge_all_tidy(sape_tidy_male_scotland,
                            sape_tidy_female_scotland,
                            sape_tidy_persons_scotland,
                            sum_variable = "age_band",
                            new_var_name = "age_band_tots")),

  tar_target(scotland_combined_pop_age_band,
             merge_all_tidy(sape_tidy_male_scotland,
                            sape_tidy_female_scotland,
                            sape_tidy_persons_scotland,
                            sum_variable = "pop_age_band",
                            new_var_name = "pop_age_band_tots")),

  tar_target(scotland_combined_child_age_band,
             merge_all_tidy(sape_tidy_male_scotland,
                            sape_tidy_female_scotland,
                            sape_tidy_persons_scotland,
                            sum_variable = "child_age_band",
                            new_var_name = "child_age_band_tots")),

  tar_target(sape_long_term,
             transform_sape_long_term(sape_long_term_source,
                                      lookup = hscp_lookup)),


  #### Population pyramid totals ####


  # for each, create base data, then aggregate to 4 AB  & 9 Highland areas
  # with subarea to locality()

  # GRAND TOTAL Populations by CP Name
  tar_target(nhsh_high_level_populations, #  next step reduces to 4 A&B areas
             create_population_totals(nhsh_combined,
                                      new_var_name = "total_pop",
                                      grouping_cols = c("CP_Name",
                                                        "Council_area_name"),
                                      !is.na(age_band) & Sex == "persons")),


 # basic pyramid totals
  tar_target(pyramid_tots,
             create_population_totals(nhsh_combined,
                                      new_var_name = "pop_age_band_total",
                                      grouping_cols = c("Sex",
                                                        "CP_Name",
                                                        "pop_age_band",
                                                        "Council_area_name"),                                                                      !is.na(age_band))),




 tar_target(age_band_tots,
            create_population_totals(nhsh_combined,
                                     new_var_name = "age_band_total",
                                     grouping_cols = c("Sex",
                                                       "CP_Name",
                                                       "age_band",
                                                       "Council_area_name"),
                                     !is.na(age_band))),


 ## child_age_band_totals
 tar_target(child_age_band_tots,
            create_population_totals(nhsh_combined,
                                     new_var_name = "child_age_band_total",
                                     grouping_cols = c("Sex",
                                                       "CP_Name",
                                                       "child_age_band",
                                                       "Council_area_name"),
                                     !is.na(age_band) & child_age_band != "25+")),


 tar_target(child_age_band_tots_long_term,

            create_population_totals(.DT = sape_long_term,
                                     new_var_name = "child_age_band_total",
                                     grouping_cols = c("CAName",
                                                       "CP_Name",
                                                       "Year",
                                                       "Sex",
                                                       "child_age_band"),
                                     !is.na(age_band) & child_age_band != "25+")),

 # child age band totals by sgurc classifcation

 tar_target(child_age_band_sgurc_tots,
            create_population_totals(nhsh_combined,
                                     new_var_name = "child_age_band_sgurc_total",
                                     grouping_cols = c("Sex",
                                                       "CP_Name",
                                                       "child_age_band",
                                                       "Council_area_name",
                                                       "UrbanRural8fold2020"),
                                     !is.na(age_band) & child_age_band != "25+")),


 # the sex level totals by SUBHSCP - no age band grouping
 # might need these to recreate Figure 2 of CP Profile
 tar_target(sex_pop_totals,
            create_population_totals(nhsh_combined,
                                     new_var_name = "sex_total_pop",
                                     grouping_cols = c("CP_Name",
                                                       "Council_area_name",
                                                       "Sex"),
                                     !is.na(age_band))),



 tar_target(highland_pop_age_band,
            create_population_totals(nhsh_combined,
                                     new_var_name = "pop_age_band_total",
                                     grouping_cols = c("Sex",
                                                       "pop_age_band"),
                                     !is.na(age_band))),


 tar_target(highland_age_band,
            create_population_totals(nhsh_combined,
                                     new_var_name = "age_band_total",
                                     grouping_cols = c("Sex",
                                                       "age_band"),
                                     !is.na(age_band))),




 # create combined dataframes with percentage totals

 tar_target(pyramid_percent_tots,
            create_percentage_totals(.dt1 = pyramid_tots,
                                     .dt2 = nhsh_high_level_populations,
                                     joincols = c("CP_Name",
                                                  "Council_area_name"),
                                     new_var_name = "percent_of_tot",
                                     numerator = "pop_age_band_total",
                                     divisor = "total_pop")),

 # by age_band
 tar_target(age_band_percent_tots,
            create_percentage_totals(.dt1 = age_band_tots,
                                     .dt2 = nhsh_high_level_populations,
                                     joincols = c("CP_Name",
                                                  "Council_area_name"),
                                     new_var_name = "percent_of_tot",
                                     numerator = "age_band_total",
                                     divisor = "total_pop")),

 # by child_age_band
 tar_target(child_age_band_percent_tots,
            create_percentage_totals(.dt1 = child_age_band_tots,
                                     .dt2 = nhsh_high_level_populations,
                                     joincols = c("CP_Name",
                                                  "Council_area_name"),
                                     new_var_name = "percent_of_tot",
                                     numerator = "child_age_band_total",
                                     divisor = "total_pop")),

 # child age band totals by sgurc classifcation

 tar_target(child_age_band_sgurc_percent_tots,
            create_percentage_totals(.dt1 = child_age_band_sgurc_tots,
                                     .dt2 = nhsh_high_level_populations,
                                     joincols = c("CP_Name",
                                                  "Council_area_name"),
                                     new_var_name = "percent_of_tot",
                                     numerator = "child_age_band_sgurc_total",
                                     divisor = "total_pop")),


 # split by sex as well

 tar_target(pyramid_percent_sex_tots,
            create_percentage_totals(.dt1 = pyramid_tots,
                                     .dt2 = sex_pop_totals,
                                     joincols = c("CP_Name",
                                                  "Council_area_name",
                                                  "Sex"),
                                     new_var_name = "percent_of_sex_tot",
                                     numerator = "pop_age_band_total",
                                     divisor = "sex_total_pop")),

 tar_target(age_band_percent_sex_tots,
            create_percentage_totals(.dt1 = age_band_tots,
                                     .dt2 = sex_pop_totals,
                                     joincols = c("CP_Name",
                                                  "Council_area_name",
                                                  "Sex"),
                                     new_var_name = "percent_of_sex_tot",
                                     numerator = "age_band_total",
                                     divisor = "sex_total_pop",
                                     grouping_cols = c("Sex", "CP_Name")))


)
