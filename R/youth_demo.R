## Pulling Youth Demographics Data

##' Urban/Rural Breakdown by age group
##'
##' Pulls numbers and percentages of age groups by urban/rural.
##' @title Age by Urban/Rural County Classification
##' @param age_breaks min age, max age, and any breaks in between (numerical)
##' @param labels vector of strings showing age ranges e.g. c("14-24", "25+")
##' @return tibble of length 2*(number of age categories) with four columns
##' @author Daniel James Liden
get_rural_PUMS <- function(age_breaks = c(0,14,24,99),
                                 labels = c("0-13","14-24",
                                            "25+")) {
    pums <- fetch_pnv()
    . <- dplyr::filter(pums, AGEP <= max(age_breaks) & AGEP >= min(age_breaks))
    . <- dplyr::select(., AGEP, PWGTP, rural)
    . <- dplyr::mutate(., agecat = cut(AGEP, age_breaks, include.lowest = TRUE,
                                       right = TRUE, labels = labels))
    . <- dplyr::group_by(., rural, agecat)
    . <- dplyr::summarize(., number=sum(PWGTP, na.rm = TRUE))
    . <- dplyr::group_by(., rural)
    . <- dplyr::mutate(., pct = number/sum(number))
    pums <- .
    pums
}
##' race by age group
##'
##' numbers/percentages of Nevadans by race and age group.
##' @title 
##' @param age_breaks min age, max age, and any breaks in between (numerical)
##' @param age_labels vector of strings showing age ranges e.g. c("14-24", "25+")
##' @return tibble
##' @author Daniel James Liden
get_pums_race <- function(age_breaks = c(0,14,24,99),
                                 age_labels = c("0-13","14-24", "25+")) {
    pums <- fetch_pnv()
    . <- dplyr::filter(pums, AGEP <= max(age_breaks) & AGEP >= min(age_breaks))
    . <- dplyr::select(., AGEP, PWGTP, RAC1P, HISP)
    . <- dplyr::mutate(., agecat = cut(AGEP, age_breaks, include.lowest = TRUE,
                                       right = TRUE, labels = age_labels))
    . <- dplyr::mutate(.,
                       race_ethnicity = dplyr::case_when(
                                                   RAC1P == "1" ~ "White (all)",
                                                   RAC1P == "2" ~ "Black or African American",
                                                   RAC1P %in% c("3", "4", "5") ~
                                                       "American Indian and Alaska Native",
                                                   RAC1P == "6" ~ "Asian",
                                                   RAC1P == "7" ~
                                                       "Native Hawaiian and Other Pacific Islander",
                                                   RAC1P == "8" ~ "Some Other Race",
                                                   RAC1P == "9" ~ "Two or More Races",
                                                   TRUE ~ NA_character_),
                       hisp = dplyr::case_when(HISP != "01" ~ "Hispanic or Latino Origin",
                                               TRUE ~ "Not Hispanic or Latino"))
    tmp <- dplyr::group_by(., hisp, agecat)
    tmp <- dplyr::summarize(tmp,
                            number=sum(PWGTP, na.rm=TRUE))
    names(tmp) = c("race_ethnicity", "agecat", "number")
    . <- dplyr::group_by(., race_ethnicity, agecat)
    . <- dplyr::summarize(., number=sum(PWGTP, na.rm=TRUE))
    . <- rbind(.,tmp)
    . <- dplyr::group_by(., race_ethnicity)
    . <- dplyr::mutate(., pct = number/sum(number))
    pums <- .
    pums
}



plot_PUMS_urban_rural <- function(data = get_rural_PUMS(), save = FALSE,
                                  path = "~/images/") {
    data <- dplyr::filter(data, agecat != "0-13")
    fig = fig_base(data) +
        geom_col(width = 0.5, mapping = aes(x=rural, y=pct, fill=agecat),
                 position=position_dodge(0.5)) +
        scale_y_continuous(labels = function(x) stringr::str_c(x*100, "%"),
                           limits = c(0,1), name = "Percentage in Age Group",
                           expand = c(0,0)) +
        scale_fill_manual(values = c("#682977", "#928e96"),
                          name = "Age",
                          guide = guide_legend(nrow=1)) +
        ggtitle("Youth in Urban or Rural\nCounties in Nevada") +
        xlab(NULL) +
        theme(legend.position = "bottom",
              legend.text = element_text(size=8)) +
        apply_theme()
    if(!save){
        return(fig)
    } else {
        save_fig(fig=fig, loc=path, name="rural_age.png",
                 width = 4, height = 4)
        }
}


plot_PUMS_race_ethnicity <- function(data = get_pums_race(), save = FALSE,
                                             path = "~/images/") {
    data <- dplyr::filter(data, agecat != "0-13")
    fig = fig_base(data) +
        geom_col(width = 0.5, mapping = aes(x=stringr::str_wrap(race_ethnicity, 14),
                                            y=number, fill=agecat),
                 position=position_dodge(0.5)) +
        scale_y_continuous(labels = function(x) stringr::str_c(x*100, "%"),
                           limits = c(0,0.9), name = "Percentage in Age Group",
                           expand = c(0,0),
                           breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        scale_fill_manual(values = c("#682977", "#928e96"),
                          name = "Age",
                          guide = guide_legend(nrow=1)) +
        ggtitle("Youth in Poverty in Nevada\nby Race and Ethnicity") +
        xlab(NULL) +
        theme(legend.position = "bottom",
              legend.text = element_text(size=8),
              axis.text.x = element_text(size=7)) +
        apply_theme()
    if(!save){
        return(fig)
    } else {
        save_fig(fig=fig, loc=path, name="poverty_race.png",
                 width = 6.5, height = 4)
        }
}
