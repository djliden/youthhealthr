##' Sets up base theme for figures
##'
##' Sets up base theme for figures by modifying the ggplot classic theme with a
##' few Guinn Center branded elements
##' @title base plot for themed figures
##' @param data data frame or tibble if some default data are used throughout
##' @return ggplot object
##' @author Daniel James Liden
fig_base <- function(data=NULL) {
  plt = ggplot2::ggplot(data) +
  ggplot2::theme_classic() +
  guinnr::guinn_gg_labels_theme(axis_size = 10, title_size = 14) +
    guinnr::guinn_gg_legend_theme(legend_position = "right", legend_text_size = 10)
  return(plt)
}

##' Applies preset theme options to figures including fonts, grids, axes
##'
##' Applies PT Sans font, blank element background, sizes for axis texts,
##' and other thematic options to keep 
##' @title applies preset theme options to figures
##' @return 
##' @author Daniel James Liden
apply_theme <- function(){
  theme = ggplot2::theme(panel.grid = ggplot2::element_line(color = "white"),
                panel.grid.major = ggplot2::element_line(size = 0.8),
                panel.grid.minor.y  = ggplot2::element_line(size=0.3),
                panel.grid.minor.x = ggplot2::element_blank(),
                axis.text = ggplot2::element_text(size = 10),
                legend.key = ggplot2::element_rect(fill = NA, color=NA),
              # plot.title = element_blank(),
                plot.caption = ggplot2::element_text(family = "PT Sans",
                                            hjust = 0),
                plot.subtitle = ggplot2::element_text(family="PT Sans",
                                             hjust = 0.5))
    return(theme)
}

##' Save ggplot figures with some standard formatting
##'
##' Save ggplot figures with defaults set according to project specifications
##' and append the guinn center logo to the figures.
##' @title saves figures with and without GC logo
##' @param fig ggplot figure
##' @param width default 6.9 in
##' @param height default 5 in
##' @param name defaults to fig_(date).png. Appended to `loc`.
##' @param loc directory in which to save figures; default "../figs/"
##' @return 
##' @author Daniel James Liden
save_fig <- function(fig, width = 6.9, height = 5,
                     name = paste0("fig_", Sys.Date(), ".png"),
                     loc = "../figs/") {
    logo = magick::image_read(system.file("/imgs/logo.png",
                                          package = "AZCovidData"))
    ggplot2::ggsave(fig, device = "png",
                    filename = paste0(loc, name), dpi = 300,
                    height = height, width = width, units = "in")

    plot_with_logo = magick::image_read(paste0(loc, name))
    plot_with_logo = magick::image_composite(plot_with_logo,
                                             magick::image_scale(logo, 350),
                                             offset="+1690+1350")
    magick::image_write(plot_with_logo,
                        paste0(loc, substring(name, 1, nchar(name)-4),"_logo.png"))
}


##' Loads color palette
##'
##' Loads color palette for use in plotting functions. Specifically
##' uses a selection of colors from the pals::cols25() function.
##' @title Color Palette utility for plotting
##' @return color palette
##' @author Daniel James Liden
load_cols = function(){
    cols = pals::cols25(10)
    names(cols) = c("Total", "White (all)", "Black or African American",
                    "American Indian and Alaska Native", "Asian",
                    "Native Hawaiian and Other Pacific Islander",
                    "Some other race", "Two or more races",
                    "Hispanic or Latino Origin",
                    "White (not Hispanic or Latino)")[c(4, 5, 3, 9,
                                                        6, 7, 1, 8, 2, 10)]
    return(cols)
}
