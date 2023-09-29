create_scotland_high_level <- function(.DT, ...,
                                col_to_sum,
                                new_var_name) {

  new_var_name <- substitute(new_var_name)
  col_to_sum <- deparse1(substitute(col_to_sum))

  out <- .DT[...][]

  out[, eval(new_var_name) := get(col_to_sum) / sum(get(col_to_sum))][]

  # data.table::setnames(out,
  #                      old = "new_var_name",
  #                      new = eval(new_var_name))

  return(out)

}



