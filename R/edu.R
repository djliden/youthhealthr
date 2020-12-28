##' 4-year graduation rate in Nevada
##'
##' Plots the four-year graduation rate in Nevada from 2015 through 2019
##' by race/ethnicity.
##' @title 
##' @param data 
##' @param save 
##' @param path 
##' @return 
##' @author Daniel James Liden
##' @importFrom ggplot2 scale_color_manual ylab xlab
plot_grad_rate <- function(data = gradrate, save = FALSE,
                           path = "~/images/") {
    fig = fig_base(data) +
        geom_line(mapping = aes(x=year, y=rate, color=Group), size=0.8) +
        scale_color_manual(values=unname(load_cols()[c(1,2,3,4,5,8,9,7)]),
                           labels = function(x) stringr::str_wrap(x, width=25),
                           name = "Race/Ethnicity") +
        scale_y_continuous(labels = function(x) stringr::str_c(x, "%")) +
        ggtitle("Four-Year Graduation Rate in Nevada") +
        xlab("Year") +
        ylab("Graduation Rate") +
        apply_theme()

    if(!save){
        return(fig)
    } else {
        save_fig(fig=fig, loc=path, name="gradrate.png",
                 width = 6.5, height = 3.5)
        }
}


#ed_attainment_us = data.frame(
#    attainment = ed_attainment$attainment,
#    percent = c(12.6, 31.4, 44.8, 11.2, 12.0, 27.0, 32.5, 32.1),
#    age = ed_attainment$age)



##' Educational Attainment in Nevada by Age Group
##'
##' Comparison of Educational Attainment Levels in Nevada for Nevadans
##' aged 18-24 and 25+
##' @title 
##' @param data 
##' @param save 
##' @param path 
##' @return 
##' @author Daniel James Liden
##' @importFrom ggplot2 scale_x_discrete
plot_ed_attainment <- function(data = ed_attainment,
                               save = FALSE,
                               path = "~/images/") {
    data = dplyr::filter(data, age=="18-24")
    fig = fig_base(data) +
        geom_col(width = 0.5, mapping = aes(x=attainment, y=percent,
                                            fill=geo),
                 position = position_dodge(0.5)) +
        scale_y_continuous(labels = function(x) stringr::str_c(x, "%"),
                           name = "Percent",
                           expand = c(0,0),
                           breaks = c(5,10,15,20,25,30,35, 40, 45)) +
        scale_fill_manual(values = c("#682977", "#928e96"),
                          name = "Age",
                          guide = guide_legend(nrow=1)) +
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 14)) +
        ggtitle("Educational Attainment in Nevada") +
        xlab(NULL) +
        theme(legend.position = "bottom",
              legend.text = element_text(size=8),
              axis.text.x = element_text(size=8)) +
        apply_theme()
    
    if(!save){
        return(fig)
    } else {
        save_fig(fig=fig, loc=path, name="edattain.png",
                 width = 4.5, height = 3.5)
        }
}
