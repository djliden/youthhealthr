##' unique metrics in drug use dataset
##'
##' Helper function for obtaining vector of names of unique
##' metrics in the drug use survey data. Used to help with
##' plotting.
##' @title 
##' @param data 
##' @return 
##' @author Daniel James Liden
get_metrics = function(data=druguse){
    unique(data$metric)
}

druguse_single_plot <- function(data=druguse, metric, path = "~/images/") {
    data = dplyr::filter(druguse, metric==metric)
    fig = fig_base(data) +
        geom_col(width = 0.5, mapping = aes(x=state, y=estimate, fill=age_group),
                 position=position_dodge(0.5)) +
        scale_y_continuous(labels = function(x) stringr::str_c(x*100, "%"),
                           name = "Percent",
                           expand = c(0,0)) +
        scale_fill_manual(values = c("#682977", "#f1d30e", "#928e96"),
                          name = "Age",
                          guide = guide_legend(nrow=1)) +
        ggtitle(stringr::str_wrap(metric, 35)) +
        xlab(NULL) +
        theme(legend.position = "bottom",
              legend.text = element_text(size=8)) +
        apply_theme()
}

druguse_all_plots <- function(data = druguse, metrics = get_metrics(),
                              path = "~/images/"){
    for(metric in metrics){
        fig = druguse_single_plot(data=data, metric=metric)
        save_fig(fig=fig, loc=path, name=stringr::str_c(metric, ".png"),
                 width = 4, height = 4)
    }
}

