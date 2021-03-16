# Poverty Time Series

get_one_year <- function(year) {
    temp <- tempfile()
    url <- paste0(
        "https://www2.census.gov/programs-surveys/acs/",
        sprintf("data/pums/%s/1-Year/csv_pnv.zip", year)
    )
    download.file(url, temp)
    tryCatch(
        {
            data <- read.csv(unz(temp, sprintf("ss%spnv.csv", year%%100)))
            write.csv(data, sprintf(
                                "~/data_share/youth/multi/pnv_%s.csv",
                                year
                            ))
        },
        error = function(year) sprintf("%s didn't work", year)
    )

    unlink(temp)
}
    

poverty_age <- function(pums, age_breaks = c(14, 18, 24, 99),
                        labels = c(
                            "14-18",
                            "19-24",
                            "25+"
                        )) {
    . <- dplyr::filter(pums, AGEP <= max(age_breaks) & AGEP >= min(age_breaks))
    . <- dplyr::select(., AGEP, POVPIP, PWGTP)
    . <- dplyr::mutate(., agecat = cut(AGEP, age_breaks,
        include.lowest = TRUE,
        right = TRUE, labels = labels
    ))
    . <- dplyr::group_by(.,agecat)
    . <- dplyr::summarize(., total = sum(PWGTP, na.rm=TRUE),
                          n_poverty = sum(PWGTP[POVPIP < 100],
                                          na.rm = TRUE),
                          percent_poverty = sum(PWGTP[POVPIP < 100],
                                                na.rm = TRUE) /
                                 sum(PWGTP, na.rm = TRUE))
    pums <- .
    pums
}

process_poverty <- function() {
    out = data.frame(matrix(
        ncol = 5,
        nrow = 0
    ))
    names(out) = c("year", "agecat", "total", "n_poverty", "percent_poverty")

    file_dir = "~/data_share/youth/multi/"
    files <- list.files(file_dir)
    paths <- paste0(file_dir, files)
    for (path in paths) {
        year = substr(path, nchar(path) - 7, nchar(path) - 4)
        pnv = readr::read_csv(path)
        tmp_out = poverty_age(pums = pnv)
        tmp_out$year = year
        out = rbind(out, tmp_out)
    }
    return(out)
}
