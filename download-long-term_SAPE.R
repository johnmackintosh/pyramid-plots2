obtain_sape_long_term <- function(path = "https://www.opendata.nhs.scot/dataset/7f010430-6ce1-4813-b25c-f7f335bdc4dc/resource/c505f490-c201-44bd-abd1-1bd7a64285ee/download/dz2011-pop-est_09092022.csv" ,
                                  newname = "./inputs/dz2011-pop-est_09092022.csv") {


data.table::fwrite(data.table::fread(path), newname)

}


