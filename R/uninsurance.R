##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Parse PUMS uninsurance data
##' @param age_breaks vector of ages passed to cut(age, age_breaks, include.lowest = TRUE, right=TRUE)
##' @param labels vector of labels for age ranges specifed in age breaks
##' @return data frame with percent uninsured by age group and urban/rural
##' @author Daniel James Liden
get_uninsurance_PUMS <- function(age_breaks = c(14,18,24,99),
                                 labels = c("14-18",
                                            "19-24",
                                            "25+")) {
    pums <- fetch_pnv()
    . <- dplyr::filter(pums, AGEP <= max(age_breaks) & AGEP >= min(age_breaks))
    . <- dplyr::select(., AGEP, HICOV, PWGTP, rural)
    . <- dplyr::mutate(., agecat = cut(AGEP, age_breaks, include.lowest = TRUE,
                                       right = TRUE, labels = labels))
    . <- dplyr::group_by(., rural, agecat)
    . <- dplyr::summarize(., number=sum(PWGTP[HICOV==2])/sum(PWGTP))
    pums <- .
    pums
}

plot_uninsurance_PUMS <- function(data = get_uninsurance_PUMS(), save = FALSE,
                                  path = "~/images/") {
    fig = fig_base(data) +
        geom_col(width = 0.5, mapping = aes(x=rural, y=number, fill=agecat),
                 position=position_dodge(0.5)) +
        scale_y_continuous(labels = function(x) stringr::str_c(x*100, "%"),
                           limits = c(0,0.2), name = "Percent Uninsured",
                           expand = c(0,0)) +
        scale_fill_manual(values = c("#682977", "#f1d30e", "#928e96"),
                          name = "Age",
                          guide = guide_legend(nrow=1)) +
        ggtitle("Youth Without Health Insurance\nin Nevada") +
        xlab(NULL) +
        theme(legend.position = "bottom",
              legend.text = element_text(size=8)) +
        apply_theme()
    if(!save){
        return(fig)
    } else {
        save_fig(fig=fig, loc=path, name="uninsurance.png",
                 width = 4, height = 4)
        }
}

