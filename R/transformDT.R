
#' transformDT
#'
#'transform source data from wide to long, rename columns and create new summary variables
#'
#' wide to long with data.table::melt
#'
#' remove 'Age_' from column names
#' ensure variable is numeric and create age_band summary column
#'
#'
#' @param filename  exsting RDS file in wide format to be tidied
#' @param sex male/ female or persons
#' @param names_to_keep character vector of columns to be retained when
#' data is pivoted. This should not need any changes
#'
#' @return tidy RDS
#' @export
#'
#' @examples
#' transformDT(filename = sape_male_wide, sex = "male")
#'
#'
transformDT <- function(filename,
                        sex = "male",
                        names_to_keep = c("DataZone",
                                          "IntZone",
                                          "CP_Name",
                                          "CAName",
                                          "UrbanRural2fold2020",
                                          "UrbanRural3fold2020",
                                          "UrbanRural6fold2020",
                                          "UrbanRural8fold2020",
                                          "Data_zone_code",
                                          "Data_zone_name",
                                          "Council_area_code",
                                          "Council_area_name",
                                          "Sex",
                                          "Total_population")) {


  .DT <- data.table::setDT(filename)


  # persons data table doesn't have a 'Sex' column, so add it now
  # so it can be included in the melt operation

  if (!"Sex" %in% names(.DT)) {
.DT$Sex <- "persons"
  }

  #.DT[,`:=`(gender_level = ..gender)][]
  .DT_tidy <- data.table::melt(.DT, id.vars = names_to_keep)
  .DT_tidy[, value := as.integer(value)][]
  .DT_tidy[, variable := gsub("Age_", "", variable)][]
  .DT_tidy[variable == '90_and_over', variable := 90][]
  .DT_tidy[,variable := as.numeric(variable)][]
  .DT_tidy[variable %in% c(0:90),
           broad_age_band := data.table::fcase(
    variable %in% c(0:15), "0-15",
    variable %in% c(16:29), "16-29",
    variable %in% c(30:44), "30-44",
    variable %in% c(45:59), "45-59",
    variable %in% c(60:74), "60-74",
    variable %in% c(75:84), "75-84",
    variable >= 85, "85+",
    default = NA_character_
  )][]


  .DT_tidy[, broad_age_band := factor(broad_age_band,
                                      levels = c(
                                        "0-15",
                                        "16-29",
                                        "30-44",
                                        "45-59",
                                        "60-74",
                                        "75-84",
                                        "85+"))]


  .DT_tidy[!is.na(variable),
       age_band := data.table::fcase(
         variable %in% c(0:4), "00-04",
         variable %in% c(5:9), "05-09",
         variable %in% c(10:14), "10-14",
         variable %in% c(15:19), "15-19",
         variable %in% c(20:24), "20-24",
         variable %in% c(25:29), "25-29",
         variable %in% c(30:34), "30-34",
         variable %in% c(35:39), "35-39",
         variable %in% c(40:44), "40-44",
         variable %in% c(45:49), "45-49",
         variable %in% c(50:54), "50-54",
         variable %in% c(55:59), "55-59",
         variable %in% c(60:64), "60-64",
         variable %in% c(65:69), "65-69",
         variable %in% c(70:74), "70-74",
         variable %in% c(75:79), "75-79",
         variable %in% c(80:84), "80-84",
         variable %in% c(85:89), "85-89",
         variable >= 90, "90+",
         default = NA_character_)][]

  .DT_tidy[,age_band := factor(age_band,
                           levels = c("00-04",
                                      "05-09",
                                      "10-14",
                                      "15-19",
                                      "20-24",
                                      "25-29",
                                      "30-34",
                                      "35-39",
                                      "40-44",
                                      "45-49",
                                      "50-54",
                                      "55-59",
                                      "60-64",
                                      "65-69",
                                      "70-74",
                                      "75-79",
                                      "80-84",
                                      "85-89",
                                      "90+"),
                           ordered = TRUE)]


  .DT_tidy[!is.na(variable),
           child_age_band := fcase(
             variable == 0, "0",
             inrange(variable, 1, 4), "01-04",
             inrange(variable, 5, 11), "05-11",
             inrange(variable, 12, 17), "12-17",
             inrange(variable, 18, 24), "18-24",
             variable >= 25, "25+")][]

  .DT_tidy[, child_age_band := factor(child_age_band,
                                      levels = c("0",
                                                 "01-04",
                                                 "05-11",
                                                 "12-17",
                                                 "18-24",
                                                 "25+"),
                                      ordered = TRUE)][]
  .DT_tidy

}
