## INLA model fit: SPT type I-IV ##
## March 22 2022
## Suppose the home directory is the parent directory of the code

## Package ####
if(!require(pacman)) { install.packages('pacman')}
p_load(tidyverse, INLA, spdep, tmap, rmapshaper,sf, data.table)
inla.setOption(
        mkl = TRUE,
        inla.mode = 'experimental')


## Data load ####
orwa_tracts_0618_mdpp = read_rds("./Data/OR_WA_covariates_EVI_parks_030222.rds")
## URBAN AREA
urban_or = st_read("./Data/Urban/Oregon_State_City_Urban_Growth_Areas.geojson") %>%
    st_transform(st_crs(orwa_tracts_0618_mdpp))
urban_wa = st_read("./Data/Urban/Washington_State_City_Urban_Growth_Areas.geojson") %>%
    st_transform(st_crs(orwa_tracts_0618_mdpp))
urban_orwa = list(urban_or, urban_wa) %>%
    rbindlist(., use.names = TRUE, fill = TRUE, idcol = NULL) %>%
    st_as_sf(., sf_column_name = 'geometry') %>%
    st_union

## SAMHSA mental health facs
samhsa = st_read("./Data/MentalHealthFacility/SAMHSA_Mental_Substance_Fac.geojson") %>%
    st_transform(st_crs(orwa_tracts_0618_mdpp))
samhsa_mh = samhsa %>% filter(mh == 1)
samhsa_sa = samhsa %>% filter(sa == 1)

# orwa_tracts_0618_mdpp %>% sf::st_join()

# urban_orwa = urban_orwa %>%
#     #filter(is.na(effDate) | effDate < 20190000) %>%
#     transmute(urban = 1)
orwa_tracts_0618_mdpp = orwa_tracts_0618_mdpp %>%
    mutate(urban = st_intersects(geometry, urban_orwa, sparse = FALSE) %>% unlist,
           n_mentalfac = lengths(st_intersects(geometry, samhsa_mh$geometry)),
           n_substancefac = lengths(st_intersects(geometry, samhsa_sa$geometry)),
           n_msfac = lengths(st_intersects(geometry, samhsa$geometry))) %>%
    mutate(urban = factor(urban, levels = c('TRUE','FALSE'), labels = c('Urban', 'NonUrban')))
orwa_tracts_0618_mdpp = orwa_tracts_0618_mdpp %>%
    mutate(n_nonsubstance = n_mentaln - ifelse(is.na(n_substance), 0, n_substance)) %>%
    mutate(GEOID_year = 1:nrow(.)) %>%
    mutate(fips_cnty = str_sub(GEOID10, 1, 5)) %>%
    group_by(fips_cnty) %>%
    mutate(p_mentalfac_100k = 1e5 * sum(n_mentalfac, na.rm = TRUE) / sum(tpop),
           p_substancefac_100k = 1e5 * sum(n_substancefac, na.rm = TRUE) / sum(tpop),
           p_msfac = 1e5 * sum(n_msfac, na.rm = TRUE) / sum(tpop)) %>%
    ungroup


swm_orwa_q1 = 
    orwa_tracts_0618_mdpp %>%
    filter(year == 2010) %>%
    st_transform(2163) %>%
    poly2nb %>%
    nb2mat(style = 'B')

swm_orwa_q2 = 
    orwa_tracts_0618_mdpp %>%
    filter(year == 2010) %>%
    st_transform(2163) %>%
    poly2nb %>%
    nblag(2) %>%
    nblag_cumul %>%
    nb2mat(style = 'B')

graph = swm_orwa_q1


form.subs <- n_substance ~ 
    urban + n_medincome_100k + EVI_1y + 
    p_youngadult + p_poverty + p_edubac + p_unemp + 
    p_renter + p_vacant + p_singleheaded + p_noncitizen + p_veteran + p_bmarital + p_fulltime + p_substancefac_100k
form.nsub <- n_substance ~ 
    urban + n_medincome_100k + EVI_1y + 
    p_youngadult + p_poverty + p_edubac + p_unemp + 
    p_renter + p_vacant + p_singleheaded + p_noncitizen + p_veteran + p_bmarital + p_fulltime + p_mentalfac_100k

car::vif(glm(form.subs, poisson, data = orwa_tracts_0618_mdpp))
car::vif(glm(form.nsub, poisson, data = orwa_tracts_0618_mdpp))

# final
form.m40_subs <- n_substance ~ 
    offset(log(tpop)) +
    f(GEOID_int1, model = "bym", graph = graph, constr = TRUE, scale.model = TRUE) +
    f(year_int1, model = 'ar1', constr = TRUE) +
    f(GEOID_int2, green, model = "bym", graph = graph, constr = TRUE, scale.model = TRUE) +
    f(year_int2, green, model = 'rw1', constr = TRUE) +
    #dist_park_wavg_1k:n_medincome_100k + 
    #green:n_medincome_100k +
    #p_vacant + p_veteran + p_fulltime + 
    urban + n_medincome_100k + green + 
    p_youngadult + p_poverty + p_edubac + p_unemp + 
    p_renter + p_singleheaded + p_noncitizen + p_bmarital + p_substancefac_100k

form.m40_nsub <- n_nonsubstance ~ 
    offset(log(tpop)) +
    f(GEOID_int1, model = "bym", graph = graph, constr = TRUE, scale.model = TRUE) +
    f(year_int1, model = 'ar1', constr = TRUE) +
    f(GEOID_int2, green, model = "bym", graph = graph, constr = TRUE, scale.model = TRUE) +
    f(year_int2, green, model = 'rw1', constr = TRUE) +
    #dist_park_wavg_1k:n_medincome_100k + 
    #green:n_medincome_100k +
    #p_vacant + p_veteran + p_fulltime + 
    urban + n_medincome_100k + green + 
    p_youngadult + p_poverty + p_edubac + p_unemp + 
    p_renter + p_singleheaded + p_noncitizen + p_bmarital + p_mentalfac_100k


## Model update: sp-int in the main count component
# Substance
form.m40_subs_1 = update(form.m40_subs,
                    ~ . + f(GEOID_year, model = 'iid', constr = TRUE))
form.m40_subs_2 = update(form.m40_subs,
                    ~ . + f(GEOID_int3, model = 'iid', group = year_int3, graph = graph, control.group = list(model = "ar1"), constr = TRUE))
form.m40_subs_3 = update(form.m40_subs,
                    ~ . + f(GEOID_int3, model = 'besag', group = year_int3, control.group = list(model = "iid"), constr = TRUE, scale.model = TRUE))
form.m40_subs_4 = update(form.m40_subs,
                    ~ . + f(GEOID_int3, model = 'besag', group = year_int3, graph = graph, control.group = list(model = "ar1"), constr = TRUE, scale.model = TRUE))


# Non-substance mental
form.m40_nsub_1 = update(form.m40_nsub,
                    ~ . + f(GEOID_year, model = 'iid', constr = TRUE))
form.m40_nsub_2 = update(form.m40_nsub,
                    ~ . + f(GEOID_int3, model = 'iid', group = year_int3, graph = graph, control.group = list(model = "ar1"), constr = TRUE))
form.m40_nsub_3 = update(form.m40_nsub,
                    ~ . + f(GEOID_int3, model = 'besag', group = year_int3, control.group = list(model = "iid"), constr = TRUE, scale.model = TRUE))
form.m40_nsub_4 = update(form.m40_nsub,
                    ~ . + f(GEOID_int3, model = 'besag', group = year_int3, graph = graph, control.group = list(model = "ar1"), constr = TRUE, scale.model = TRUE))


# Previous base model
form.m40 <- n_substance ~ 
    f(GEOID_int1, model = "bym", graph = graph, constr = TRUE, scale.model = TRUE) +
    f(year_int1, model = 'ar1', constr = TRUE) +
    f(GEOID_int2, green, model = "bym", graph = graph, constr = TRUE, scale.model = TRUE) +
    f(year_int2, green, model = 'ar1', constr = TRUE) +
    #dist_park_wavg_1k:n_medincome_100k + 
    green:n_medincome_100k +
    urban + n_medincome_100k + green + 
    p_youngadult + p_poverty + p_edubac + p_unemp + 
    p_renter + p_vacant + p_singleheaded + p_noncitizen + p_veteran + p_bmarital + p_fulltime



#--- Type IV interaction ---#
#form.m5IV <- update(form.m40, ~ . + f(GEOID_int3, green, model = "bym", graph = graph, group = year3, control.group = list(model = "rw1"),
#                          constr = TRUE, hyper = hyp.prior, scale.model = TRUE) )
#--- Type IV interaction with besag ICAR ---#
form.m5IV <- update(form.m40, ~ . + f(GEOID_int3, green, model = "besag", graph = graph, group = year_int3, control.group = list(model = "rw1"),
                          constr = TRUE, scale.model = TRUE) )
#--- Type I interaction ---#
form.m5I <- update(form.m40, ~ . + f(GEOID_int3, green, model = "iid", constr = TRUE))
#--- Type II interaction ---#
form.m5II <- update(form.m40, ~ . + f(GEOID_int3, green, model = "iid", group = year_int3, 
                                        control.group = list(model = "rw1"),
                                        constr = TRUE))
#--- Type III interaction ---#
## added following generic0 trick

Wn = apply(graph, 1, sum)
Wm = graph

r.def <- 13
A.constr <- kronecker(diag(13),matrix(1,1,2269))
A.constr <- A.constr[-1,]
Q.xi = Diagonal(x = Wn) - Matrix(Wm)
Q.Leroux <- diag(2269)-Q.xi


#########################################################
##  Define the temporal structure matrix of a RW1/RW2  ##
#########################################################
D1 <- diff(diag(13),differences=1)
Q.gammaRW1 <- t(D1)%*%D1
R <- kronecker(Diagonal(13),Q.xi)


form.m5III <- update(form.m40, ~ . + f(GEOID_int3, green, model = "generic0", 
                                      Cmatrix = R, rankdef = r.def,
                                      constr = TRUE, extraconstr = list(A = A.constr, e = rep(0, 12)) ))




# exploratory histogram
ggplot(data = orwa_tracts_0618_mdpp,
       mapping = aes(x = year, y = n_substance, group = GEOID10)) +
       #geom_histogram() +
       geom_line()# +
       #facet_wrap(~year)

hyperprior_sp = list(prec = list(prior = 'loggamma', param = c(100, 0.05)))
hyperprior_sp = list(prec.spatial = list(prior = 'loggamma', param = c(100, 0.05)),
                      prec.unstruct = list(prior = 'loggamma', param = c(100, 1/125)))
hyperprior_spt = list(prec = list(prior = 'loggamma', param = c(1000, 1/125)))

theta = rep(0, 4)

lcs1 <- inla.make.lincombs(GEOID_int2 = kronecker(Matrix(1, 13, 1), cbind(Diagonal(2269), Diagonal(2269))), 
                           year_int2 = kronecker(Diagonal(13), Matrix(1, 2269, 1)),
                           GEOID_int3 = Diagonal(2269 * 13))
lcs2 <- inla.make.lincombs(GEOID_int2 = kronecker(Matrix(1, 13, 1), cbind(Diagonal(2269), Diagonal(2269))), 
                           year_int2 = kronecker(Diagonal(13), Matrix(1, 2269, 1)),
                           GEOID_int3 = kronecker(Diagonal(2269), cbind(Diagonal(13))))
lcs3 <- inla.make.lincombs(GEOID_int2 = kronecker(Matrix(1, 13, 1), cbind(Diagonal(2269), Diagonal(2269))), 
                           year_int2 = kronecker(Diagonal(13), Matrix(1, 2269, 1)),
                           GEOID_int3 = Diagonal(2269 * 13))
lcs4 <- inla.make.lincombs(GEOID_int2 = kronecker(Matrix(1, 13, 1), cbind(Diagonal(2269), Diagonal(2269))), 
                           year_int2 = kronecker(Diagonal(13), Matrix(1, 2269, 1)),
                           GEOID_int3 = kronecker(Diagonal(13), cbind(Diagonal(2269))))
lcs4 <- inla.make.lincombs(GEOID_int2 = cbind(Diagonal(2269), Diagonal(2269)), 
                           year_int2 = kronecker(Diagonal(13), Matrix(1, 2269, 1)))
lcs = inla.make.lincombs(GEOID_int2 = kronecker(Matrix(1,13,1), cbind(Diagonal(2269), Diagonal(2269))),
                         year_int2 = kronecker(Diagonal(13), Matrix(1, 2269, 1)))



## Main function ####
fit_model = function(formula, data, greenvar = "EVI_1y", lincomb, thetas = NULL, graph = graph, family = 'poisson', threads = "12:1") {
    data_norm = data %>%
        mutate(green = !!sym(greenvar))
    
    inla(
        formula = formula,
            data = data_norm,
            family = family, verbose = TRUE,
            lincomb = lincomb,
            control.lincomb = list(verbose = FALSE),
            control.inla = control.inla(b.strategy = 'keep', 
                                        strategy = "laplace", 
                                        int.strategy = 'eb',
                                        restart = 3,
                                        stupid.search.factor = 1.25,
                                        tolerance = 1e-3, 
                                        npoints = 25,
                                        use.directions = TRUE),
            control.predictor = list(compute = TRUE),
            control.compute = list(cpo = TRUE, waic = TRUE, dic = TRUE),
            num.threads = threads)
}

## Model fitting ####
# Naming standard
# mod_[sub/nsub]_inla_[neigh]_[spint]
mod_subs_inla_aq1_1 = fit_model(form.m40_subs_1, orwa_tracts_0618_mdpp, lincomb = lcs, graph = graph, threads = "12:2")
mod_subs_inla_aq1_2 = fit_model(form.m40_subs_2, orwa_tracts_0618_mdpp, lincomb = lcs, graph = graph, threads = "12:2")
#mod_subs_inla_aq1_3 = fit_model(form.m40_subs_1, orwa_tracts_0618_mdpp, lincomb = lcs, graph = graph, threads = "12:2")
mod_subs_inla_aq1_4zinf = fit_model(form.m40_subs_4, orwa_tracts_0618_mdpp, lincomb = lcs, graph = graph, threads = "12:2", family = 'zeroinflatedpoisson0')

mod_nsub_inla_aq1_1 = fit_model(form.m40_nsub_1, orwa_tracts_0618_mdpp, lincomb = lcs, graph = graph, threads = "12:2")
mod_nsub_inla_aq1_2 = fit_model(form.m40_nsub_2, orwa_tracts_0618_mdpp, lincomb = lcs, graph = graph, threads = "12:2")
#mod_nsub_inla_aq1_3 = fit_model(form.m40_nsub_1, orwa_tracts_0618_mdpp, lincomb = lcs, graph = graph, threads = "12:2")
mod_nsub_inla_aq1_4zinf = fit_model(form.m40_nsub_4, orwa_tracts_0618_mdpp, lincomb = lcs, graph = graph, threads = "12:2", family = 'zeroinflatedpoisson0')


save(list = ls()[grep('^mod_(subs|nsub)_inla*.*[1-4]$', ls())], file = '/mnt/s/MISU_Tract_INLA_032222.RData')


orwa_tracts_0618_mdpf = orwa_tracts_0618_mdpp %>%
  mutate(#pred_lag1spt_pois = mod_moodanxiety_orwa_inla_pois_aq1$summary.fitted.values[,1] %>% unlist,
         #delta_1it_park = mod_moodanxiety_orwa_inla_pois_aq1$summary.random[[2]][,'mean'] %>% unlist,
         beta_1it_evi = mod_subs_inla_aq1_4$summary.lincomb.derived[,'mean'] %>% unlist) #%>%
  #mutate(resid_lag1spt_pois = n_moodanxiety - pred_lag1spt_pois)
tm_shape(orwa_tracts_0618_mdpf) +
  #tm_style('classic') +
  tm_fill('beta_1it_evi', pal = '-RdBu', n = 6, style = 'fisher', aes.palette = 'seq', midpoint = 0) +
  tm_borders(lwd = 0.15, col = 'dark grey') +
  tm_facets('year') +
  tm_layout(inner.margins = 0.01)


mod_subs_inla_aq1_4$summary.random[[4]]


mod_substance_orwa_inla_pois_aq1_0 = fit_model(form.m40, orwa_tracts_0618_mdpp, lincomb = NULL, graph = graph, threads = "12:2")
mod_substance_orwa_inla_pois_aq1_1 = fit_model(form.m5I, orwa_tracts_0618_mdpp, lincomb = NULL, graph = graph, threads = "12:2")
mod_substance_orwa_inla_pois_aq1_2 = fit_model(form.m5II, orwa_tracts_0618_mdpp, lincomb = NULL, graph = graph, threads = "12:2")
mod_substance_orwa_inla_pois_aq1_3 = fit_model(form.m5III, orwa_tracts_0618_mdpp, lincomb = lcs3, graph = graph, threads = "12:2")
mod_substance_orwa_inla_pois_aq1_4 = fit_model(form.m5IV, orwa_tracts_0618_mdpp, lincomb = lcs4, graph = graph, threads = "12:2")

summary(mod_substance_orwa_inla_pois_aq1_0, include.lincomb = FALSE)
summary(mod_substance_orwa_inla_pois_aq1_1, include.lincomb = FALSE)
summary(mod_substance_orwa_inla_pois_aq1_2, include.lincomb = FALSE)
summary(mod_substance_orwa_inla_pois_aq1_3, include.lincomb = FALSE)
summary(mod_substance_orwa_inla_pois_aq1_4, include.lincomb = FALSE)

mod_substance_orwa_inla_pois_aq1_0pd = fit_model(form.m40, orwa_tracts_0618_mdpp, greenvar = "dist_park_wavg_1k", lincomb = NULL, graph = graph, threads = "12:2")
mod_substance_orwa_inla_pois_aq1_1pd = fit_model(form.m5I, orwa_tracts_0618_mdpp, greenvar = "dist_park_wavg_1k", lincomb = NULL, graph = graph, threads = "12:2")
mod_substance_orwa_inla_pois_aq1_2pd = fit_model(form.m5II, orwa_tracts_0618_mdpp, greenvar = "dist_park_wavg_1k", lincomb = NULL, graph = graph, threads = "12:2")
mod_substance_orwa_inla_pois_aq1_3pd = fit_model(form.m5III, orwa_tracts_0618_mdpp, greenvar = "dist_park_wavg_1k", lincomb = NULL, graph = graph, threads = "12:2")
mod_substance_orwa_inla_pois_aq1_4pd = fit_model(form.m5IV, orwa_tracts_0618_mdpp, greenvar = "dist_park_wavg_1k", lincomb = NULL, graph = graph, threads = "12:2")

summary(mod_substance_orwa_inla_pois_aq1_0pd, include.lincomb = FALSE)
summary(mod_substance_orwa_inla_pois_aq1_1pd, include.lincomb = FALSE)
summary(mod_substance_orwa_inla_pois_aq1_2pd, include.lincomb = FALSE)
summary(mod_substance_orwa_inla_pois_aq1_3pd, include.lincomb = FALSE)
summary(mod_substance_orwa_inla_pois_aq1_4pd, include.lincomb = FALSE)


save(list = ls()[grep('^mod_substance_orwa_inla*.*[1-4]$', ls())], file = '/mnt/s/Substance_Tract_INLA_030222.RData')


summary(mod_moodanxiety_orwa_inla_pois_aq1)
mod_moodanxiety_orwa_inla_pois_aq1$mode$mode.status

orwa_tracts_0618_mdpf = orwa_tracts_0618_mdpp %>%
  mutate(pred_lag1spt_pois = mod_moodanxiety_orwa_inla_pois_aq1$summary.fitted.values[,1] %>% unlist,
         delta_1it_park = mod_moodanxiety_orwa_inla_pois_aq1$summary.random[[2]][,'mean'] %>% unlist,
         beta_1it_park = mod_moodanxiety_orwa_inla_pois_aq1$summary.lincomb.derived[,'mean'] %>% unlist) %>%
  mutate(resid_lag1spt_pois = n_moodanxiety - pred_lag1spt_pois)
tm_shape(orwa_tracts_0618_mdpf) +
  #tm_style('classic') +
  tm_fill('beta_1it_park', pal = '-RdBu', n = 6, style = 'fisher', aes.palette = 'seq', midpoint = 0) +
  tm_borders(lwd = 0.15, col = 'dark grey') +
  tm_facets('year') +
  tm_layout(inner.margins = 0.01)

## EVI model ####
mod_substance_orwa_inla_pois_aq1 = inla(
    formula = n_substance ~ 
        f(GEOID_int1, EVI_1y, model = 'bym', graph = graph, hyper = hyperprior_sp) + 
        f(GEOID_int2, EVI_1y, group = year_int2, model = 'besag', graph = graph, control.group = list(model = 'rw1'), hyper = hyperprior_spt) + 
        f(year_int1, EVI_1y, model = 'ar', order = 1) + 
        EVI_1y:n_medincome_100k + EVI_1y + n_medincome_100k + #d_park_tract_sqkm:n_medincome_100k +
        p_youngadult + p_poverty + p_edubac + p_unemp + 
        p_renter + p_vacant + p_singleheaded + p_noncitizen + p_veteran + p_bmarital + p_fulltime,
                        data = orwa_tracts_0618_mdpp,
                        family = 'poisson', verbose = TRUE,
                        lincomb = lcs,
                        control.lincomb = list(verbose = FALSE),
                        control.inla = control.inla(b.strategy = 'keep', strategy = "adaptive", 
                                                    restart = 3,
                                                    stupid.search.factor = 2,
                                                    tolerance = 1e-3, npoints = 25), 
                        control.predictor = list(compute = TRUE),
                        control.compute = list(cpo = TRUE, waic = TRUE, dic = TRUE),
                        #control.mode = list(theta = theta),
                        num.threads = '12:1')
summary(mod_substance_orwa_inla_pois_aq1, include.lincomb=FALSE)
mod_moodanxiety_orwa_inla_pois_aq1$mode$mode.status



## sptvariogram
orwa_tracts_stv_base = orwa_tracts_0618_mdpp %>%
    dplyr::select(1:2, n_substance, n_nonsubstance) %>%
    bind_cols(st_coordinates(st_centroid(.)) %>% data.frame) %>%
    st_drop_geometry %>%
    mutate(datetime = lubridate::ymd(str_c(2022, '01', sprintf('%02d', year - 2005)))) %>%
    as.data.frame %>%
    stConstruct(x = .,
                space= c('X','Y'), time = c('datetime'), 'GEOID10')
orwa_tracts_stv = variogramST(n_substance~1, data = orwa_tracts_stv_base,
                            tlags = 0:10,
                            cutoff = 500000,
                            width = 16000,
                            cores = 16,
                            tunit = 'days')
