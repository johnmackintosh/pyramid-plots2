#' Title
#'
#' @param sourcedata csv file already downloaded from NHS open data
#' @param lookup lookup table converting datazone to IZ and HSCP
#'
#' @return long data.table with additional agebands for adult and children
#' @export
#'
#' @examples
#'
#'
transform_sape_long_term <- function(sourcedata,
                                     lookup){

  input <- data.table::fread(sourcedata)

  lookup <- data.table::fread(lookup)

  names_to_drop <- c("DataZoneQF", "SexQF", "AllAges")
  names_to_keep <- c("IntZone", "CP_Name", "CAName",
                     "Year", "DataZone","Sex")

  working_input <- input[!DataZoneQF %chin% "d", !..names_to_drop]

  setkeyv(working_input,"DataZone")
  setkeyv(lookup,"DataZone")

  .DTtemp <- lookup[working_input][]

  .DT <- .DTtemp[!is.na(CAName)][]

  rm(.DTtemp)
  rm(working_input)

  .DT_tidy <- data.table::melt(.DT, id.vars = names_to_keep)

  rm(.DT)
  .DT_tidy[, value := as.integer(value)][]
  .DT_tidy[, variable := gsub("Age", "", variable)][]
  .DT_tidy[variable == '90plus', variable := 90]
  .DT_tidy[,variable := as.numeric(variable)]
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

  .DT_tidy[!is.na(variable),
           broad_child_age_band := fcase(
             inrange(variable, 0, 17), "under 18",
             variable >= 18, "18+")][]

  .DT_tidy[, broad_child_age_band := factor(broad_child_age_band,
                                            levels = c("under 18",
                                                       "18+"),
                                            ordered = TRUE)][]


  .DT_tidy

}








