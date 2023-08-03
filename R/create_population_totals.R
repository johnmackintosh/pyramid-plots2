#' Title
#'
#' @param .DT source data, usually nhsh_combined
#' @param new_var_name desired name of the new totals column created in J
#' @param grouping_cols columns to pass to BY
#' @param ... passed to I to filter the source data, usually to exclude NAs and
#' / or filter so Sex == "persons"
#'
#' @return data.table with new summary column, grouped and filtered as specified
#' @export
#'
#' @examples
#'
#'create_population_totals(nhsh_combined,
#'new_var_name = "total_pop",
#'grouping_cols = c("SubHSCPName",
#'                 "Council_area_name"),
#'                 !is.na(age_band) & Sex == "persons")
#'

create_population_totals <- function(.DT = nhsh_combined,
                          new_var_name = NULL,
                          grouping_cols = c("Sex",
                                            "SubHSCPName",
                                            "pop_age_band",
                                            "Council_area_name"),
                          ...) {

  out <- .DT[...,
             (new_var_name = sum(value)),
              by = grouping_cols]

  data.table::setnames(out,
                       old = "V1",
                       new = eval(new_var_name))

  return(out)

}
