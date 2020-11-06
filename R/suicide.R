##' Plot suicide rates
##'
##' Plot suicide rates for NV and the US from 2010 through 2018
##' for 15-24 and all ages age groups
##' 
##' @title Plot suicide rates from 2010 through 2018
##' @importFrom ggplot2 ggplot geom_col position_dodge scale_y_continuous scale_fill_manual
##' ggtitle aes guide_legend element_text theme xlab geom_line scale_x_date scale_color_manual
##' facet_grid vars
##' @param data 
##' @param save Whether to save the image; defaults to FALSE
##' @param path directory to which to save the image
plot_suicide_time <- function(data = suicide, save = FALSE,
                              path = "~/images/") {
  #  data = dplyr::filter(data, state=="Nevada")
    data$year = lubridate::as_date(ISOdate(data$year,1,1))
    fig = fig_base(data) +
        geom_line(mapping = aes(x=year, y=rate_per_100000, color = age_range),
                  size = 0.8) +
        facet_grid(cols = vars(state)) +
        scale_x_date(date_breaks = "2 year",
                     date_labels = "%Y") +
        scale_y_continuous(name = "Suicides per 100,000 Population",
                           expand = c(0,0),
                           limits = c(9,24),
                           breaks = seq(10,24,2)) +
        scale_color_manual(values = c("#682977", "#928e96"),
                          name = "Age Group",
                          guide = guide_legend(nrow=1)) +
        ggtitle("Youth Suicides in Nevada and Nationally") +
        xlab("Year") +
        theme(legend.position = "bottom",
              legend.text = element_text(size=8),
              axis.title.x = element_text(size=9)) +
        apply_theme()
    if(!save){
        return(fig)
    } else {
        save_fig(fig=fig, loc=path, name="suicide.png",
                 width = 6.5, height = 3.5)
        }
}
