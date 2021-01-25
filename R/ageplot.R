
plot_14_24 <- function(path = "~/images/youthhealth/",
                      save = FALSE) {
    data = youth_age_race
    data$year = as.Date(ISOdate(data$year, 1, 1))
    
    fig <- fig_base(data) +
        geom_line(mapping = aes(x=year, y=pct_14_24),
                  size = 0.8, color = "#682977") +
        scale_x_date(date_breaks = "2 year",
                     date_labels = "%Y") +
        scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                           limits = c(0.13,.15)) +
        ggtitle("Nevadans Aged 14 through 24 as\nShare of Total State Population") +
        labs(caption = "Source: U.S. Census Bureau",
             subtitle = "2010-2019") +
        ylab("Percent") +
        xlab("Year") +
        theme(legend.position = "bottom",
              legend.justification = "left") +
        apply_theme()

    if(!save){
        return(fig)
    } else {
        save_fig(fig=fig, loc=path, name="age_14_24.png",
                 width = 6.5, height = 4)
    }

}
