## Data Source: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-detail.html
## Table SC-EST2019-ALLDATA6
## Documentation https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/sc-est2019-alldata6.pdf

##########################################################
## Race Coding in Dataset:                              ##
## 1 = White Alone                                      ##
## 2 = Black or African American Alone                  ##
## 3 = American Indian or Alaska Native Alone           ##
## 4 = Asian Alone                                      ##
## 5 = Native Hawaiian and Other Pacific Islander Alone ##
## 6 = Two or more races                                ##
##########################################################

#######################################
## Origin Coding in Dataset          ##
## The key for ORIGIN is as follows: ##
## 0 = Total                         ##
## 1 = Not Hispanic                  ##
## 2 = Hispanic                      ##
#######################################

## Age is single year ages (0-84 and 85+)

gen_age_sheet = function() {
    ## Data
    age_race = readr::read_csv("../inst/data/census_age_race.csv") %>%
        filter(STATE == 32, # Nevada
               SEX == 0     # Total; don't separate by sex
               ) %>%
        mutate(age_class = case_when(
                   AGE > 65 ~ "over 65",
                   dplyr::between(AGE,14,24) ~ "14-24",
                   TRUE ~ "between 18 and 65"))



    data = data.frame(year=NULL, race_ethnicity = NULL,
                      pct_over_65 = NULL, pct_14_24 = NULL)


    for(year in 2010:2019){
        year = year
        colname = str_c("POPESTIMATE", year)
        ## Get Median
        tmp = age_race %>%
            select(ORIGIN, RACE, AGE, colname, age_class)
        race_ethnicity = c("Total", "White (all)", "Black or African American",
                           "American Indian and Alaska Native", "Asian",
                           "Native Hawaiian and Other Pacific Islander",
                           "Two or more races",
                           "Hispanic or Latino Origin",
                           "White (not Hispanic or Latino)")
        ## OVER 65
        pct_over_65 = c(
            ## Total
            sum(filter(tmp, ORIGIN==0 & age_class == "over 65")[[colname]])/
            sum(filter(tmp, ORIGIN==0)[[colname]]),
            ## White (all)
            sum(filter(tmp, ORIGIN==0 &
                            RACE == 1 &
                            age_class == "over 65")[[colname]])/
            sum(filter(tmp, ORIGIN==0 & RACE == 1)[[colname]]),
            ## Black or African American
            sum(filter(tmp, ORIGIN==0 &
                            RACE == 2 &
                            age_class == "over 65")[[colname]])/
            sum(filter(tmp, ORIGIN==0 & RACE == 2)[[colname]]),
            ## American Indian or Alaskan Native
            sum(filter(tmp, ORIGIN==0 &
                            RACE == 3 &
                            age_class == "over 65")[[colname]])/
            sum(filter(tmp, ORIGIN==0 & RACE == 3)[[colname]]),
            ## Asian
            sum(filter(tmp, ORIGIN==0 &
                            RACE == 4 &
                            age_class == "over 65")[[colname]])/
            sum(filter(tmp, ORIGIN==0 & RACE == 4)[[colname]]),
            ## Native Hawaiian and Other Pacific Islander
            sum(filter(tmp, ORIGIN==0 &
                            RACE == 5 &
                            age_class == "over 65")[[colname]])/
            sum(filter(tmp, ORIGIN==0 & RACE == 5)[[colname]]),
            ## Two or More Races
            sum(filter(tmp, ORIGIN==0 &
                            RACE == 6 &
                            age_class == "over 65")[[colname]])/
            sum(filter(tmp, ORIGIN==0 & RACE == 6)[[colname]]),
            ## Hispanic or Latino Origin
            sum(filter(tmp, ORIGIN==2 &
                            age_class == "over 65")[[colname]])/
            sum(filter(tmp, ORIGIN==2)[[colname]]),
            ## White (not Hispanic or Latino
            sum(filter(tmp, ORIGIN==1 &
                            RACE == 1 &
                            age_class == "over 65")[[colname]])/
            sum(filter(tmp, ORIGIN==1 & RACE == 1)[[colname]]))
        ## UNDER 18
        pct_14_24 = c(
            ## Total
            sum(filter(tmp, ORIGIN==0 & age_class == "14-24")[[colname]])/
            sum(filter(tmp, ORIGIN==0)[[colname]]),
            ## White (all)
            sum(filter(tmp, ORIGIN==0 &
                            RACE == 1 &
                            age_class == "14-24")[[colname]])/
            sum(filter(tmp, ORIGIN==0 & RACE == 1)[[colname]]),
            ## Black or African American
            sum(filter(tmp, ORIGIN==0 &
                            RACE == 2 &
                            age_class == "14-24")[[colname]])/
            sum(filter(tmp, ORIGIN==0 & RACE == 2)[[colname]]),
            ## American Indian or Alaskan Native
            sum(filter(tmp, ORIGIN==0 &
                            RACE == 3 &
                            age_class == "14-24")[[colname]])/
            sum(filter(tmp, ORIGIN==0 & RACE == 3)[[colname]]),
            ## Asian
            sum(filter(tmp, ORIGIN==0 &
                            RACE == 4 &
                            age_class == "14-24")[[colname]])/
            sum(filter(tmp, ORIGIN==0 & RACE == 4)[[colname]]),
            ## Native Hawaiian and Other Pacific Islander
            sum(filter(tmp, ORIGIN==0 &
                            RACE == 5 &
                            age_class == "14-24")[[colname]])/
            sum(filter(tmp, ORIGIN==0 & RACE == 5)[[colname]]),
            ## Two or More Races
            sum(filter(tmp, ORIGIN==0 &
                            RACE == 6 &
                            age_class == "14-24")[[colname]])/
            sum(filter(tmp, ORIGIN==0 & RACE == 6)[[colname]]),
            ## Hispanic or Latino Origin
            sum(filter(tmp, ORIGIN==2 &
                            age_class == "14-24")[[colname]])/
            sum(filter(tmp, ORIGIN==2)[[colname]]),
            ## White (not Hispanic or Latino
            sum(filter(tmp, ORIGIN==1 &
                            RACE == 1 &
                            age_class == "14-24")[[colname]])/
            sum(filter(tmp, ORIGIN==1 & RACE == 1)[[colname]]))
        out = tibble(year, race_ethnicity, pct_over_65,
                     pct_14_24)
        data = rbind(data, out)
    }

    write_csv(data, "../inst/data/age_race_ethnicity_2010_2019.csv")

}
