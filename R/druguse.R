##' unique metrics in drug use dataset
##'
##' Helper function for obtaining vector of names of unique
##' metrics in the drug use survey data. Used to help with
##' plotting.
##' @title 
##' @param data 
##' @return 
##' @author Daniel James Liden
#get_metrics = function(data=druguse){
#    unique(data$metric)
#}



metric_args = c("Major Depressive Episode in the Past Year",
            "Serious Mental Illness in the Past Year",
            "Received Mental Health Services in the Past Year",
            "Had Serious Thoughts of Suicide in the Past Year",
            "Illicit Drug Use Disorder in Past Year",
            "Needing But Not Receiving Treatment at a Specialty Facility for Illicit Drug Use in the Past Year")

title_args = c("Nevadans Reporting a Major Depressive\nEpisode in the Past Year",
           "Nevadans Reporting Serious Mental\nIllness in the Past Year",
           "Nevadans Reporting Receiving Mental\nHealth Services in the Past Year",
           "Nevadans Reporting Serious Thoughts\nof Suicide in the Past Year",
           "Nevadans Experiencing Illicit Drug Use Disorder in the Past Year",
           "Nevadans Needing but Not Receiving\nTreatment for Illicit Drug Use\nin the Past Year")

druguse_single_plot <- function(data=druguse, metric, title, path = "~/images/") {
    sub = data[data[['metric']] == metric,]
    sub = dplyr::filter(sub, !(is.na(estimate)))
    fig = fig_base(sub) +
        geom_col(width = 0.5, mapping = aes(x=state, y=estimate, fill=age_group),
                 position=position_dodge(0.5)) +
        scale_y_continuous(labels = function(x) stringr::str_c(x*100, "%"),
                           name = "Percent",
                           expand = c(0,0)) +
        scale_fill_manual(values = c("12-17" = "#682977", "18-25" = "#f1d30e",
                                     "26 or Older" = "#928e96"),
                          name = "Age",
                          guide = guide_legend(nrow=1)) +
        ggtitle(stringr::str_wrap(title, 35)) +
        xlab(NULL) +
        theme(legend.position = "bottom",
              legend.text = element_text(size=8)) +
        apply_theme()
    return(fig)
}

druguse_all_plots <- function(data = druguse, metrics = metric_args,
                             titles=title_args, path = "~/images/"){
    for(i in seq_along(metrics)){
        fig = druguse_single_plot(data=data, metric=metrics[i], title=titles[i])
        save_fig(fig=fig, loc=path, name=stringr::str_c(metrics[i], ".png"),
                 width = 4, height = 4)
    }
}


