plot_mobile <- function(data = mobile_crisis,
                        save=FALSE, path = "~/images/youthhealth/"){
    data = dplyr::group_by(data, Year)
    data = dplyr::summarize(data, total = sum(TOTAL))

    fig = fig_base(data) +
        geom_col(width = 0.5, mapping = aes(x=Year, y=total),
                 fill = "#682977") +
        scale_y_continuous(name = "Total Calls",
                           expand = c(0,0),
                           n.breaks = 8,
                           limits = c(0, 3600)) +
        ggtitle(stringr::str_wrap("Total Calls to Nevada DCFS Mobile Crisis Response Services",
                                  35)) +
        xlab("Year") +
        labs(subtitle = "2017-2020") +
        apply_theme()
    if(!save){
        return(fig)
    } else {
        save_fig(fig=fig, loc=path, name="DCFS_mobile.png",
                 width = 4, height = 4)
    }
}
