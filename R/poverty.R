## Imports



##' Pull income-to-poverty ratio from PUMS
##'
##' Poverty status is determined for all people except institutionalized people,
##' people in military group quarters, people in college dorms, and unrelated
##' individuals under the age of 15. These were excluded from the analysis.
##' @title Poverty from PUMS
##' @param age_breaks 
##' @param labels 
##' @return 
##' @author Daniel James Liden
get_poverty_PUMS <- function(age_breaks = c(14,18,24,99),
                                 labels = c("14-18",
                                            "19-24",
                                            "25+")) {
    pums <- fetch_pnv()
    . <- dplyr::filter(pums, AGEP <= max(age_breaks) & AGEP >= min(age_breaks))
    . <- dplyr::select(., AGEP, POVPIP, PWGTP, rural)
    . <- dplyr::mutate(., agecat = cut(AGEP, age_breaks, include.lowest = TRUE,
                                       right = TRUE, labels = labels))
    . <- dplyr::group_by(., rural, agecat)
    . <- dplyr::summarize(., number=sum(PWGTP[POVPIP<100], na.rm=TRUE)/sum(PWGTP, na.rm=TRUE))
    pums <- .
    pums
}

get_poverty_pums_race <- function(age_breaks = c(14,24,99),
                                 age_labels = c("14-24", "25+")) {
    pums <- fetch_pnv()
    . <- dplyr::filter(pums, AGEP <= max(age_breaks) & AGEP >= min(age_breaks))
    . <- dplyr::select(., AGEP, POVPIP, PWGTP, RAC1P, HISP)
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
                            number=sum(PWGTP[POVPIP<100], na.rm=TRUE)/sum(PWGTP, na.rm=TRUE))
    names(tmp) = c("race_ethnicity", "agecat", "number")
    . <- dplyr::group_by(., race_ethnicity, agecat)
    . <- dplyr::summarize(., number=sum(PWGTP[POVPIP<100], na.rm=TRUE)/sum(PWGTP, na.rm=TRUE))
    pums <- rbind(.,tmp)
    pums
}




##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param data 
##' @param save 
##' @param path 
##' @return 
##' @author Daniel James Liden
##' @importFrom ggplot2 ggplot geom_col position_dodge scale_y_continuous scale_fill_manual
##' ggtitle aes guide_legend element_text theme xlab
plot_poverty_PUMS_urban_rural <- function(data = get_poverty_PUMS(), save = FALSE,
                                  path = "~/images/") {
    fig = fig_base(data) +
        geom_col(width = 0.5, mapping = aes(x=rural, y=number, fill=agecat),
                 position=position_dodge(0.5)) +
        scale_y_continuous(labels = function(x) stringr::str_c(x*100, "%"),
                           limits = c(0,0.25), name = "Percent below Poverty Line",
                           expand = c(0,0)) +
        scale_fill_manual(values = c("#682977", "#f1d30e", "#928e96"),
                          name = "Age",
                          guide = guide_legend(nrow=1)) +
        ggtitle("Youth in Poverty\nin Nevada") +
        xlab(NULL) +
        theme(legend.position = "bottom",
              legend.text = element_text(size=8)) +
        apply_theme()
    if(!save){
        return(fig)
    } else {
        save_fig(fig=fig, loc=path, name="poverty.png",
                 width = 4, height = 4)
        }
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param data 
##' @param save 
##' @param path 
##' @return 
##' @author Daniel James Liden
##' @importFrom ggplot2 ggplot geom_col position_dodge scale_y_continuous scale_fill_manual
##' ggtitle aes guide_legend element_text theme xlab
plot_poverty_PUMS_race_ethnicity <- function(data = get_poverty_pums_race(), save = FALSE,
                                             path = "~/images/") {
    data <- dplyr::filter(data, race_ethnicity != "Not Hispanic or Latino")
    data$race_ethnicity = factor(data$race_ethnicity)
    data$race_ethnicity = forcats::fct_relevel(data$race_ethnicity,
                                               "Black or African American",
                                               "Hispanic or Latino Origin",
                                               "White (all)",
                                               "Two or More Races",
                                               "American Indian and Alaska Native",
                                               "Some Other Race",
                                               "Asian",
                                               "Native Hawaiian and Other Pacific Islander")
    fig = fig_base(data) +
        geom_col(width = 0.5, mapping = aes(x=race_ethnicity,
                                            y=number, fill=agecat),
                 position=position_dodge(0.5)) +
        scale_y_continuous(labels = function(x) stringr::str_c(x*100, "%"),
                           limits = c(0,0.3), name = "Percent below Poverty Line",
                           expand = c(0,0),
                           breaks = c(0,0.1,0.2,0.3)) +
        scale_fill_manual(values = c("#682977", "#928e96"),
                          name = "Age",
                          guide = guide_legend(nrow=1)) +
        scale_x_discrete(labels = function(x) stringr::str_wrap(x,14)) +
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
