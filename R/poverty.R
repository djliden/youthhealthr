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

plot_poverty_PUMS <- function(data = get_poverty_PUMS(), save = FALSE,
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
