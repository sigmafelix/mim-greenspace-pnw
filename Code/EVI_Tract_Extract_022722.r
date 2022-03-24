##
if (!require(pacman)) {install.packages('pacman')}
p_load(sf, stars, exactextractr, tidyverse, terra)

evi2017 = read_stars('/mnt/d/1YR_MEDIAN/L8_2018_2018_Median.tif')
evi2017 = rast('/mnt/d/1YR_MEDIAN/L8_2018_2018_Median.tif')
orwa_tracts_0618 = read_rds('./Data/ACS_Tracts_OR_WA_Cleaned_021322.rds')

orwa_tracts = orwa_tracts_0618 %>%
    filter(year == 2018)

orwa_tracts_evi = exactextractr::exact_extract(evi2017, orwa_tracts, fun = function(c) mean(c, na.rm = TRUE), summarize_df = TRUE)

mean_extract = function(ras, pol, foo = 'mean') {
    orwa_tracts_evi = exactextractr::exact_extract(ras, pol, foo)
    return(orwa_tracts_evi)
}

evi_list = list.files('/mnt/d/1YR_MEDIAN/',
                      pattern = '*.tif',
                      full.names = TRUE)
evi_list_l = evi_list %>%
    split(.,.) %>%
    lapply(function(x) mean_extract(terra::rast(x), orwa_tracts))
evi_list_ld = evi_list_l %>%
    lapply(function(x) { 
    })
evi_vec = Reduce(c, evi_list_l[-14])
#orwa_tracts_evi_m %>% Reduce(c, .) %>% fivenum
exactextractr::exact_extract(rast(evi_list[1]), orwa_tracts_0618[2,], 'mean')

orwa_tracts_0618 = orwa_tracts_0618 %>%
    arrange(year, GEOID10) %>%
    mutate(EVI_1y = evi_vec)

form_mentaln = n_mentaln ~ offset(log(tpop)) + EVI_1y:p_unemp + EVI_1y + n_medincome + p_elderly + p_poverty + p_edubac + p_unemp + p_renter + p_vacant + p_singleheaded + p_noncitizen + p_veteran + p_bmarital + p_fulltime
form_mentaln = n_mentaln ~ EVI_1y:p_unemp + EVI_1y + n_medincome + p_elderly + p_poverty + p_edubac + p_unemp + p_renter + p_vacant + p_singleheaded + p_noncitizen + p_veteran + p_bmarital + p_fulltime

glm(formula = form_mentaln, data = orwa_tracts_0618, family = poisson(link = 'log')) %>%
    summary

