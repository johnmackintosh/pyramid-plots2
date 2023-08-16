
#' import_and_join
#'
#' @param .df source data, already saved as RDS
#' @param .hscp_lookup source lookup table which maps datazones to HSCP
#' Includes additional column for ease of joining to exisiting data
#' @param nhsh_only if \code{TRUE}, will filter and join on the lookup table to
#' return results for Highland and Argyll and Bute only.
#' Spaces in column names will be replaced with underscores.
#'
#' If \code{FALSE} will return the original data as a data.table with spaces
#' replaced by underscores
#'
#' @return datatable, with additional columns if processing for NHSH only
#' @export
#'
#' @examples
#'
#' import_data(sape-2021-male,hscp_lookup)
#'
#'
import_and_join <- function(.df,
                            .hscp_lookup = hscp_lookup,
                            .sgurc_lookup = sgurc_lookup,
                            nhsh_only = TRUE) {


  DT <- data.table::setDT(data.table::copy(.df))

  # need to remove spaces within the column names
  oldnames <- names(DT) # original column names
  newnames <- stringr::str_replace_all(oldnames," ", "_")
  data.table::setnames(DT, old =  oldnames, new = newnames)
  DT[, joinvar := Data_zone_code]




    # lookup and bring in HSCP names, and SGURC classifications
    DT2 <- data.table::fread(.hscp_lookup)
    DT2[, joinvar := DataZone] # make column to join on

    DT3 <- data.table::fread(.sgurc_lookup)
    DT3[, joinvar := DataZone] # make column to join on

    # join to sgurc first

    DT <- DT3[DT, on = "joinvar"][, joinvar := NULL]

    if (!nhsh_only) {
      return(DT)
    }


    nhsh <- DT[Council_area_name %in% c("Argyll and Bute", "Highland")
    ][, joinvar := DataZone]


    nhsh <- DT2[nhsh,  on = c("joinvar" = "DataZone")][, joinvar := NULL][]



}
