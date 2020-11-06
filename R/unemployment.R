##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param data 
##' @return 
##' @author Daniel James Liden
##' @importFrom ggplot2 ggplot geom_col position_dodge scale_y_continuous scale_fill_manual
##' ggtitle aes guide_legend element_text theme xlab geom_line scale_x_date scale_color_manual
##' facet_grid vars 
plot_unemployment_age = function(data = unemployment_age, save=FALSE,
                                 path = "~/images/") {
    data$group[data$group=="55 and older"] = "55+"
    data$group[data$group=="Total"] = "All Ages"
    fig = fig_base(data) +
        geom_col(width = 0.5, mapping = aes(x=group, y=`unemployment rate`),
                 fill = "#682977") +
        scale_y_continuous(labels = function(x) stringr::str_c(x, "%"),
                           name = "2019 Unemployment Rate",
                           expand = c(0,0)) +
        ggtitle(stringr::str_wrap("Unemployment Rate in Nevada in 2019 by Age Group", 35)) +
        xlab("Age Group") +
        theme(legend.position = "bottom",
              legend.text = element_text(size=6)) +
        apply_theme()
    if(!save){
        return(fig)
    } else {
        save_fig(fig=fig, loc=path, name="unemp_age_2019.png",
                 width = 4, height = 3)
        }
}
