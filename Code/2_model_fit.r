## INLA model fit: SPT type I-IV ##
## Author: Insang Song (sigmafelix@hotmail.com)
## Oct 30 2022
## Suppose the home directory is the parent directory of the code

## Package ####
if (!require(pacman)) { install.packages('pacman'); library(pacman)}
if (!require(INLA)) { 
    install.packages('INLA', 
                     repos = c(getOption('repos'), "INLA"="https://inla.r-inla-download.org/R/testing"),
                     dependencies = TRUE)
}
p_load(tidyverse, spdep, tmap, rmapshaper,sf, dtplyr, data.table, CCA, FactoMineR, car)
sf_use_s2(FALSE)
source("./Code/1_functions.r")

## Warning: you can get PARDISO license (yearly registration is required) for faster model fitting.
## Please refer to INLA::inla.pardiso() for detail.
inla.setOption(
        pardiso.license = 'pardiso_license.lic',
        mkl = FALSE,
        inla.mode = 'experimental')

scalemodel = TRUE
inla.pardiso.check()

orwa_tracts_0618_mdpp = read_rds("./Data/ORWA_tracts_covariates.rds") %>%
    arrange(GEOID10, year) %>%
    # exclude any (tpop < 100)
    filter(!GEOID10 %in% c('53059950100', '41005980000')) %>%
    mutate(GEOID_int1 = as.numeric(plyr::mapvalues(GEOID10, unique(GEOID10), seq.int(1, length(unique(GEOID10))))),
           year_int1 = year - 2005,
           GEOID_year1 = seq.int(1, nrow(.))) %>%
           mutate(GEOID_int2 = GEOID_int1,
                  GEOID_int3 = GEOID_int1,
                  GEOID_int4 = GEOID_int1,
                  year_int2 = year_int1,
                  year_int3 = year_int1,
                  year_int4 = year_int1,
                  GEOID_year2 = GEOID_year1) %>%
    mutate(EVI_1y_modis_nw_frac = EVI_1y_modis_nw / 1e3,
           EVI_5y_modis_nw_frac = EVI_5y_modis_nw / 1e3,
           EVI_1y_nw_frac = EVI_1y_nw * 10,
           EVI_1y_mean_nw_frac = EVI_1y_mean_nw * 10)

### Spatial weight matrix (first-order) ####
swm_orwa_q1 = 
    orwa_tracts_0618_mdpp %>%
    filter(year == 2010) %>%
    poly2nb %>%
    nb2mat(style = 'B')
graph = swm_orwa_q1



# Model building blocks: random effects ####
form_base_spt <- outcome ~ 
    offset(log(tpop)) +
    f(GEOID_int1, model = 'bym2', graph = graph, constr = TRUE, scale.model = scalemodel) +
    f(year_int1, model = 'ar1', constr = TRUE) +
    State +
    green + 
    pm25_mean +
    log(p_water_pct_strict+ exp(-25)) +
    d_nursing_10k +
    SES + 
    p_elderly + p_singleheaded + p_bmarital + log(p_nonwhite + 0.1)



form_base_spt1 = update(form_base_spt,
            ~.+ f(GEOID_year2, model = 'iid', constr = TRUE)
            )
form_base_spt2 = update(form_base_spt,
            ~.+ f(GEOID_int2, model = 'bym2', group = year_int2, control.group = list(model = 'ar1'), graph = graph, constr = TRUE)
            )
form_base_spt3 = update(form_base_spt,
            ~.+ f(GEOID_int2, model = 'bym2', group = year_int2, control.group = list(model = 'iid'), graph = graph, constr = TRUE)
            )
form_base_spt4 = update(form_base_spt,
            ~.+ f(GEOID_int2, model = 'bym2', group = year_int2, control.group = list(model = 'ar1'), graph = graph, constr = TRUE)
            )

hyper3 = list(prec = list(theta1 = 'loggamma', param = c(1, 0.01)))

year_length = length(unique(orwa_tracts_0618_mdpp$year))
tract_length = length(unique(orwa_tracts_0618_mdpp$GEOID10))

# Linear combination definition for the combined effect
lcs_bym2 = inla.make.lincombs(green = Matrix(1, nrow = tract_length, ncol = 1),
                         GEOID_int3 = cbind(Diagonal(tract_length), Diagonal(tract_length)))#,

# spatial random slope
form_nsub_4ext = update(form_base_spt4,
                    ~ . + f(GEOID_int3, green, model = 'bym2', graph = graph, constr=TRUE, scale.model = scalemodel))
form_nsub_3ext = update(form_base_spt3,
                    ~ . + f(GEOID_int3, green, model = 'bym2', graph = graph, constr=TRUE, scale.model = scalemodel))
form_nsub_2ext = update(form_base_spt2,
                    ~ . + f(GEOID_int3, green, model = 'bym2', graph = graph, constr=TRUE, scale.model = scalemodel))
form_nsub_1ext = update(form_base_spt1,
                    ~ . + f(GEOID_int3, green, model = 'bym2', graph = graph, constr=TRUE, scale.model = scalemodel))

# determine the thread composition in your own discretion
threads = "8:1"

## Main model fitting: takes some time (3+ hours)
# Spatial only random slope models (Model 5)
mod_nsub_aq1_4pois = fit_model(form_nsub_4ext, orwa_tracts_0618_mdpp, lincomb=lcs_bym2, outcome = 'n_nonsubstance2', family = 'poisson', graph = graph, greenvar = "d_parks_nonpriv_10k", threads = threads, thetas = NULL)
mod_nsub_aq1_4pois = repeat_refit(mod_nsub_aq1_4pois)

# Models 1-4
mod_nsub_aq1_0pois = fit_model(form_base_spt, orwa_tracts_0618_mdpp, outcome = 'n_nonsubstance2', family = 'poisson', graph = graph, greenvar = "d_parks_nonpriv_10k", threads = threads, thetas = NULL)
mod_nsub_aq1_1pois = fit_model(form_nsub_1ext, orwa_tracts_0618_mdpp, lincomb=NULL, outcome = 'n_nonsubstance2', family = 'poisson', graph = graph, greenvar = "d_parks_nonpriv_10k", threads = threads, thetas = NULL)
mod_nsub_aq1_2pois = fit_model(form_nsub_2ext, orwa_tracts_0618_mdpp, lincomb=NULL, outcome = 'n_nonsubstance2', family = 'poisson', graph = graph, greenvar = "d_parks_nonpriv_10k", threads = threads, thetas = NULL)
mod_nsub_aq1_3pois = fit_model(form_nsub_3ext, orwa_tracts_0618_mdpp, lincomb=NULL, outcome = 'n_nonsubstance2', family = 'poisson', graph = graph, greenvar = "d_parks_nonpriv_10k", threads = threads, thetas = NULL)

mod_nsub_aq1_0pois = repeat_refit(mod_nsub_aq1_0pois)
mod_nsub_aq1_1pois = repeat_refit(mod_nsub_aq1_1pois)
mod_nsub_aq1_2pois = repeat_refit(mod_nsub_aq1_2pois)
mod_nsub_aq1_3pois = repeat_refit(mod_nsub_aq1_3pois)


## Output
# Export plots ####
vis_spt_effect(map = orwa_tracts_0618_mdpp,
                inla_fit = mod_nsub_aq1_4pois, effect_id = NULL, file_export = TRUE,
                filepath = "Fig2.png")
map_emarginal(mod_nsub_aq1_4pois, map = orwa_tracts_0618_mdpp %>% filter(year == 2010), rand.d = NULL,
              file.export = TRUE, pdir = "./",
              filename = "Fig3.png")

## Collinearity test ####
form.subs <- n_substance ~ offset(log(tpop)) +
    urban + n_medincome_100k + EVI_1y + pm25_mean +
    p_nonwhite + p_youngadult + p_poverty + p_edubac + p_unemp + 
    p_renter + p_vacant + p_singleheaded + p_noncitizen + p_veteran + p_bmarital + p_fulltime + p_substancefac_100k

car::vif(glm(form.subs, poisson, data = orwa_tracts_0618_mdpp))
car::vif(glm(form.nsub, poisson, data = orwa_tracts_0618_mdpp))
