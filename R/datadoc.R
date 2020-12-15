##' youth and all-ages suicide rates in NV and nationally
##'
##' A dataset containing suicide rates from 2010 through
##' 2018 for youths aged 15-24 and for all ages in Nevada
##' and in the United States as a whole.
##'
##' @format a tibble with 36 rows and 6 columns:
##' \describe{
##'   \item{state}{Nevada or United States}
##'   \item{age_range}{15-24 or all ages}
##'   \item{year}{2010-2018}
##'   \item{deaths}{number of deaths by suicide}
##'   \item{population}{total individuals in age range}
##'   \item{rate_per_100000}{suicides per 100000 people in population}
##' }
##'
##' @source \url{https://wonder.cdc.gov/mcd-icd10.html}
"suicide"



##' youth and all-ages drug use metrics in NV and the US
##'
##' A dataset containing estimates of rates of illicit drug use,
##' need for treatment for substance abuse, mental illness, and
##' reception of mental health services.
##'
##' @format a tibble with 48 rows and 4 columns:
##' \describe{
##'   \item{state:}{Nevada or United States}
##'   \item{metric:}{rate being estimated}
##'   \item{age_group:}{12-17, 18-25, or 26+}
##'   \item{estimate:}{percentage}
##' }
##'
##' @source \url{https://www.samhsa.gov/data/report/2017-2018-nsduh-state-specific-tables}
"druguse"


##' 2019 Unemployment by Age Group in Nevada
##'
##' A dataset showing the unemployment rate, by age group, in Nevada in 2019.
##' (Annual Average unemployment rate)
##'
##' @format a tibble with 4 rows and 2 columns:
##' \describe{
##'   \item{group:}{Age group (total, 16-24, 25-54, 55+}
##'   \item{unemployment rate:}{unemployment rate as % of labor force}
##' }
##'
##' @source \url{https://www.samhsa.gov/data/report/2017-2018-nsduh-state-specific-tables}
"unemployment_age"


##' 2015-2019 4-year graduation rates in Nevada
##'
##' A dataset showing the 4-year graduation rate by race in Nevada 
##'
##' @format a tibble with 40 rows and 3 columns:
##' \describe{
##'   \item{Group:}{Race/Ethnicity}
##'   \item{rate:}{4-year graduation rate}
##'   \item{year:}{Graduating class year (e.g. 2015 = class of 2015}
##' }
##'
##' @source \url{https://www.samhsa.gov/data/report/2017-2018-nsduh-state-specific-tables}
"gradrate"



##' educational attainment by age group in Nevada
##'
##' A dataset showing educational attainment by age group for Nevadans
##' aged 18-24 and 25+. Education categories are:
##' - Less than high school
##' - High school graduate or equivalent
##' - Some college or associate's degree
##' - Bachelor's degree or Higher
##'
##' @format a data frame with 16 rows and 4 columns:
##' \describe{
##'   \item{attainment:}{level of educational attainment}
##'   \item{percent:}{percent of Nevadans within age group with specified level of education}
##'   \item{age:}{age group, 18-24 or 25+}
##'   \item{geo:}{geography (Nevada or National)
##' }
##'
##' @source \url{https://data.census.gov/cedsci/table?q=Educational%20Attainment&g=0400000US32&tid=ACSST5Y2019.S1501&moe=false&hidePreview=true}
"ed_attainment"
