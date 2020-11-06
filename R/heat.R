get_agedist_ACS = function() {
    ## Poverty Data
    ages <- tidycensus::get_acs(geography = "tract",
                                survey = "acs5",
                                variables = c("S0101_C01_005E", "S0101_C01_006E"),
                                state = 32,
                                county = "Clark",
                                year = 2018,
                                geometry = TRUE)
    . <- dplyr::group_by(ages, GEOID)
    . <- dplyr::summarize(., estimate = sum(estimate))
    out = .
    
    return(out)
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param data 
##' @param path 
##' @return 
##' @author Daniel James Liden
##' @importFrom ggplot2 ggplot geom_col position_dodge scale_y_continuous scale_fill_manual
##' ggtitle aes guide_legend element_text theme xlab geom_line scale_x_date scale_color_manual
##' facet_grid vars scale_fill_gradient geom_sf element_blank element_rect labs 
##' @importFrom magrittr "%>%"
plot_age = function(data = get_agedist_ACS(), path ="~/images/"){
    label_data = tigris::places(state="NV") %>%
        sf::st_crop(xmin=-115.38, ymin=35.92, xmax = -114.88, ymax = 36.38) %>%
        dplyr::filter(NAME != "Blue Diamond")
    labels_LV = cbind(label_data, sf::st_coordinates(sf::st_centroid(label_data)))

    ages_lv = data %>%
        sf::st_crop(xmin=-115.38, ymin=35.92, xmax = -114.88, ymax = 36.38)

    LVTracts <- tigris::tracts(state="NV") %>%
        sf::st_crop(xmin=-115.38, ymin=35.92, xmax = -114.88, ymax = 36.38) 

    age_plt = ggplot(data = ages_lv) +
        geom_sf(aes(fill=estimate)) +
        shadowtext::geom_shadowtext(data = labels_LV, aes(X, Y, label = NAME), size = 3,
                                    family="PT Sans", color="black", bg.color = "white",
                                    fontface = "bold", check_overlap = TRUE) +
        scale_fill_gradient(low = "#fff6ff", high="#682977") +
        guinnr::guinn_gg_labels_theme() +
        guinnr::guinn_gg_legend_theme(legend_position = "right") +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_rect(colour = "darkgrey"),
              plot.title = element_text(hjust = 0.5),
              plot.caption = element_text(hjust=0)) +
        ggtitle("Number of Nevadans aged 15-24 by Census Tract") +
        labs(caption = "Source: U.S. Census Bureau American Community Survey, 2014-2018",
             fill = "Number of Nevadans aged 15-24")
    age_plt
}
