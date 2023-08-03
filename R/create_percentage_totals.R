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
              ][,V1 := signif((get(..numerator) / get(..divisor)) * 100,3), by = grouping_cols][]

  data.table::setnames(out,
                       old = "V1",
                       new = eval(new_var_name))
return(out)
}
