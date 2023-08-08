create_percentage_totals <- function(.dt1,
                                     .dt2,
                                     joincols,
                                     grouping_cols = NULL,
                                     new_var_name,
                                     numerator,
                                     divisor){


  numerator <- eval(numerator)
  divisor <- eval(divisor)

  out <- .dt2[.dt1, on = joincols
              ][, eval(new_var_name) := signif(get(numerator)/get(divisor),3) * 100,
                by = grouping_cols][]

  # data.table::setnames(out,
  #                      old = "V1",
  #                      new = eval(new_var_name))
return(out)
}


