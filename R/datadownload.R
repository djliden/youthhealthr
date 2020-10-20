## download PUMS data
get_pums <- function(path = "~/data_share/youth/", pnv = TRUE, hnv = TRUE, refresh = FALSE) {
    pnv_pums_URL = "https://www2.census.gov/programs-surveys/acs/data/pums/2019/1-Year/csv_pnv.zip"
    hnv_pums_URL = "https://www2.census.gov/programs-surveys/acs/data/pums/2019/1-Year/csv_hnv.zip"
    if(pnv){
        if(!dir.exists(path)){
            dir.create(path)
        }
        
        if(refresh | !file.exists(stringr::str_c(path, "pnv.csv"))){
            tmp = tempfile()
            utils::download.file(pnv_pums_URL, tmp)
            pnv = readr::read_csv(unz(tmp, "psam_p32.csv"))
            readr::write_csv(pnv, file = stringr::str_c(path, "pnv.csv"))
            unlink(tmp)
        } else {print("pnv file already exists. Use refresh=TRUE to re-download.")}
    }
    

    if(hnv){
        if(!dir.exists(path)){
            dir.create(path)
        }
        
        if(refresh | !file.exists(stringr::str_c(path, "hnv.csv"))){
            tmp = tempfile()
            utils::download.file(hnv_pums_URL, tmp)
            hnv = readr::read_csv(unz(tmp, "psam_h32.csv"))
            readr::write_csv(hnv, file = stringr::str_c(path, "hnv.csv"))
            unlink(tmp)
        } else {print("hnv file already exists. Use refresh=TRUE to re-download.")}
    }
}


## Load PUMS Data
fetch_hnv <- function(path = "~/data_share/youth/") {
    if(!file.exists(stringr::str_c(path, "hnv.csv"))){
        get_pums(path=path, hnv=TRUE, pnv=FALSE, refresh = TRUE)
    }
    readr::read_csv(stringr::str_c(path, "hnv.csv"))
}

fetch_pnv <- function(path = "~/data_share/youth/") {
    if(!file.exists(stringr::str_c(path, "pnv.csv"))){
        get_pums(path=path, hnv=FALSE, pnv=TRUE, refresh = TRUE)
    }
    pnv = readr::read_csv(stringr::str_c(path, "pnv.csv"))
    urban_PUMAs = c(stringr::str_c("0010", 1:3), "00200", stringr::str_c("0040", 1:9),
                    stringr::str_c("0041", 0:3))
    pnv$rural = dplyr::case_when(pnv$PUMA %in% urban_PUMAs ~ "Urban",
                                 !(pnv$PUMA %in% urban_PUMAs) ~ "Rural",
                                 TRUE ~ "error")
    pnv    
}
