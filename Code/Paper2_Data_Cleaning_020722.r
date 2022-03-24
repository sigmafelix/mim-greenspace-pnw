### Paper 2 data cleaning
### 02/07/2022 08:06 PST
### Insang Song
library(pacman)
p_load(tmap, tmaptools)
p_load(tidyverse, dtplyr, sf, spdep, rvest)
# For paper 2 main data: ASMR ####
drive = sprintf('/mnt/c/Users/%s/OneDrive/', username)
source('./Code/Spatial_OR_cleaning_012922.r')
load('./Data/Tracts_ACS_ORWA_0919_121621.RData')
load('./Data/ACS_Population_ORWA_012722.RData')
load('./Data/OR_WA_ACS_sf.RData')


## Spatial data filtering ####
ex_geoids_or = c('41015990101', '41011990101', '41019990000', '41039990000', '41041990100', '41057990100', '41007990000')
ex_geoids_wa = c('53009990100', '53055990100', '53031990000', '53027990000', '53049990100', '53029992201', '53057990100', 
                '53035990100', '53033990100')
crosswalk = read_csv("Data/TractCrosswalk_0010.csv")
cw_or = read_csv("Data/or41trf.txt", col_names = FALSE)
cw_wa = read_csv("Data/wa53trf.txt", col_names = FALSE)

colnames(cw_or) = crosswalk$`Column Name`
colnames(cw_wa) = crosswalk$`Column Name`

## Oregon data cleaning ####
## Oregon ####
ord <- read_rds('./Data/Raw/ORD_2006_2019_Raw.rds')

ord_s <- ord %>% 
#ord <- ord %>% 
  # Refactorize
  mutate(mental_underly = ifelse(grepl('F.*', dmcaACME), 1, 0)) %>% 
  mutate(ageunit = plyr::mapvalues(ddageunit, c(1,2,4,5,6,9), c(1,2,3,4,5,9)),
         married = plyr::mapvalues(ddmarital, c("A","D","M","S","W","U"), c("U","D","M","S","W","U")),
         educ = ddeduc,
         sex = ddsex,
         pregnancy = plyr::mapvalues(ddpregoutcome, c(1,2,3,4,7,8,9), c(1,2,3,4,7,8,9)),
         smoking = ddtobacco,
         dth_year = ddodyear,
         dth_month = ddodmonth %>% as.integer,
         dth_day = ddodday
  ) %>% 
  mutate(age = case_when(
    ageunit %in% c(5:6) ~ 0,
    ageunit == 4 ~ floor((1 * ddagenum)/365.24),
    ageunit == 2 ~ floor((30 * ddagenum)/365.24),
    ageunit == 1 ~ ddagenum,
    ageunit == 9 ~ 9999,
    ddagenum == 999 ~ 9999,
    TRUE ~ 9999
  )) %>% 
  mutate(race = case_when(
    ddracewh=='Y' & ddethnicmex!='H' & ddethnicpr!='H' & ddethniccuban!='H' & ddethnicoth!='H' ~ 1,
    ddracebl=='Y' ~ 2,
    ddethnicmex=='H' | ddethnicpr=='H' | ddethniccuban=='H' | ddethnicoth=='H' ~ 3,
    ddethnicmex=='U' | ddethnicpr =='U'| ddethniccuban=='U' | ddethnicoth=='U' | ddracewh=='U' | ddracebl=='U' | ddraceaian=='U' | ddraceasianind=='U' |
    ddracech=='U' | ddracefi=='U' | ddracekor=='U' | ddracevt=='U' | ddraceoasian=='U' | ddracenh =='U' | ddracegu=='U' | ddracesm=='U' |
    ddraceopi=='U' | ddraceospf == 'U' ~ 9,
    TRUE ~ 4
  )) %>% 
  rename_with(.cols = colnames(.)[grep('^dmca([1-9]|1[0-9]|2[0])$', colnames(.))],
              .fn = ~gsub('dmca', 'mltcse', .x, fixed = TRUE)) %>% 
  mutate(mental = grepl('F.*', mltcse1),
         mental3 = as.logical(grepl('F.*', mltcse1)+grepl('F.*', mltcse2)+grepl('F.*', mltcse3)),
         mental_new = ifelse(grepl('F[0-4][0-9]', mltcse1) + grepl('F[0-4][0-9]', mltcse2) + grepl('F[0-4][0-9]', mltcse3) + 
                               grepl('F[0-4][0-9]', mltcse4) + grepl('F[0-4][0-9]', mltcse5) + grepl('F[0-4][0-9]', mltcse6) +
                               grepl('F[0-4][0-9]', mltcse7) + grepl('F[0-4][0-9]', mltcse8) + grepl('F[0-4][0-9]', mltcse9) + grepl('F[0-4][0-9]', mltcse10), 1, 0),
         substance = ifelse(grepl('F1[0-9]', mltcse1) + grepl('F1[0-9]', mltcse2) + grepl('F1[0-9]', mltcse3) + 
                               grepl('F1[0-9]', mltcse4) + grepl('F1[0-9]', mltcse5) + grepl('F1[0-9]', mltcse6) +
                               grepl('F1[0-9]', mltcse7) + grepl('F1[0-9]', mltcse8) + grepl('F1[0-9]', mltcse9) + grepl('F1[0-9]', mltcse10), 1, 0),
         moodanxiety = ifelse(grepl('F[3-4][0-9]', mltcse1) + grepl('F[3-4][0-9]', mltcse2) + grepl('F[3-4][0-9]', mltcse3) + 
                               grepl('F[3-4][0-9]', mltcse4) + grepl('F[3-4][0-9]', mltcse5) + grepl('F[3-4][0-9]', mltcse6) +
                               grepl('F[3-4][0-9]', mltcse7) + grepl('F[3-4][0-9]', mltcse8) + grepl('F[3-4][0-9]', mltcse9) + grepl('F[3-4][0-9]', mltcse10), 1, 0),
         parkinson = ifelse(grepl('^(A521|F023|G20|G21|G22|G903).*', mltcse1) | grepl('^(A521|F023|G20|G21|G22|G903).*', mltcse2) | grepl('^(A521|F023|G20|G21|G22|G903).*', mltcse3), 1, 0),
         dementia = ifelse(grepl('^(F00|F01|F02|F03).*', mltcse1) | grepl('^(F00|F01|F02|F03).*', mltcse2) | grepl('^(F00|F01|F02|F03).*', mltcse3), 1, 0),
         alzheimer = ifelse(grepl('^(G30).*', mltcse1) | grepl('^(G30).*', mltcse2) | grepl('^(G30).*', mltcse3), 1, 0),
         #age_f = as.factor(cut(age, c(-Inf, 15, 40, 50, 60, 75, Inf))),
         educ_f = factor(educ),
         #cbsa = factor(ifelse(is.na(cbsa), 'Rural', 'Urban')),
         #risk_metal_env = factor(ifelse(is.na(risk_metal_env), 0, risk_metal_env)),
         #risk_metal_outdoor = factor(ifelse(is.na(risk_metal_outdoor), 0, risk_metal_outdoor)),
         married_f = factor(married),
         smoking_f = factor(smoking),
         race_f = factor(plyr::mapvalues(race, LETTERS[1:8], rep('Others',8)))) %>% 
  mutate(mental_u = as.integer(ifelse(grepl('F.*', dmcaACME), 1, 0)),
         parkinson_u = as.integer(ifelse(grepl('^(A521|F023|G20|G21|G22|G903).*', dmcaACME), 1, 0)),
         dementia_u = as.integer(ifelse(grepl('^(F00|F01|F02|F03).*', dmcaACME), 1, 0)),
         alzheimer_u = as.integer(ifelse(grepl('^(G30).*', dmcaACME), 1, 0)))

#   mutate(mental = factor(ifelse(grepl('F.*', mltcse1), 'Mental', 'Others')),
#          mental3 = factor(ifelse(grepl('F.*', mltcse1)+grepl('F.*', mltcse2)+grepl('F.*', mltcse3), 'Mental', 'Others')),
#          mental_new = ifelse(grepl('F[0-4][0-9]', mltcse1) + grepl('F[0-4][0-9]', mltcse2) + grepl('F[0-4][0-9]', mltcse3) + 
#                                grepl('F[0-4][0-9]', mltcse4) + grepl('F[0-4][0-9]', mltcse5) + grepl('F[0-4][0-9]', mltcse6) +
#                                grepl('F[0-4][0-9]', mltcse7) + grepl('F[0-4][0-9]', mltcse8) + grepl('F[0-4][0-9]', mltcse9) + grepl('F[0-4][0-9]', mltcse10), 1, 0),
#          parkinson = factor(ifelse(grepl('^(A521|F023|G20|G21|G22|G903).*', mltcse1) | grepl('^(A521|F023|G20|G21|G22|G903).*', mltcse2) | grepl('^(A521|F023|G20|G21|G22|G903).*', mltcse3), 'A_Parkinson', 'Others')),
#          dementia = factor(ifelse(grepl('^(F00|F01|F02|F03).*', mltcse1) | grepl('^(F00|F01|F02|F03).*', mltcse2) | grepl('^(F00|F01|F02|F03).*', mltcse3), 'Dementia', 'Others')),
#          alzheimer = factor(ifelse(grepl('^(G30).*', mltcse1) | grepl('^(G30).*', mltcse2) | grepl('^(G30).*', mltcse3), 'Alzheimer', 'Others')),
#          age_f = as.factor(cut(age, c(-Inf, 15, 40, 50, 60, 75, Inf))),
#          educ_f = factor(educ),
#          #cbsa = factor(ifelse(is.na(cbsa), 'Rural', 'Urban')),
#          #risk_metal_env = factor(ifelse(is.na(risk_metal_env), 0, risk_metal_env)),
#          #risk_metal_outdoor = factor(ifelse(is.na(risk_metal_outdoor), 0, risk_metal_outdoor)),
#          married_f = factor(married),
#          smoking_f = factor(smoking),
#          race_f = factor(plyr::mapvalues(race, LETTERS[1:8], rep('Others',8)))) %>% 



## Clear coordinates
ord_sweird <- ord_s %>% 
  filter(ddreslong >= 180) %>% 
  st_as_sf(coords = c('ddreslong', 'ddreslat'), crs = 2992) %>% 
  st_transform(4326) %>% 
  mutate(ddreslong = st_coordinates(.)[,1],
         ddreslat = st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)

ord_snormal <- ord_s %>% 
  filter(ddreslong <= 180)

ord_sre <- ord_sweird %>% 
    bind_rows(ord_snormal) %>% 
    filter(ddreslat >= 40 & ddreslong < -80)

ord_sf <- ord_sre %>% 
    st_as_sf(coords = c('ddreslong', 'ddreslat'), crs = 4326)



or_tract_pop_0919_n = or_tract_pop_0919 %>%
  lapply(function(x) { x$est %>% 
      dplyr::select(1, 3, 4) %>% 
      pivot_wider(names_from = variable, 
                  values_from = estimate) %>%
      transmute(GEOID = GEOID,
             n_00_05 = B01001_003 + B01001_027,
             n_05_10 = B01001_004 + B01001_028,
             n_10_15 = B01001_005 + B01001_029,
             n_15_20 = B01001_006 + B01001_007 + B01001_030 + B01001_031,
             n_20_25 = B01001_008 + B01001_009 + B01001_010 + B01001_032 + B01001_033 + B01001_034,
             n_25_30 = B01001_011 + B01001_035,
             n_30_35 = B01001_012 + B01001_036,
             n_35_40 = B01001_013 + B01001_037,
             n_40_45 = B01001_014 + B01001_038,
             n_45_50 = B01001_015 + B01001_039,
             n_50_55 = B01001_016 + B01001_040,
             n_55_60 = B01001_017 + B01001_041,
             n_60_65 = B01001_018 + B01001_019 + B01001_042 + B01001_043,
             n_65_70 = B01001_020 + B01001_021 + B01001_044 + B01001_045,
             n_70_75 = B01001_022 + B01001_046,
             n_75_80 = B01001_023 + B01001_047,
             n_80_85 = B01001_024 + B01001_048,
             n_85_Inf = B01001_025 + B01001_049)}) %>%
  mapply(function(x, y) { x %>% mutate(year = y) %>%
              pivot_longer(cols = 2:(ncol(.) - 1))}, 
              ., 2009:2019 %>% split(.,.), SIMPLIFY = FALSE) %>%
  do.call(bind_rows, .)

# 10 year basis

or_tract_pop_0919_n = or_tract_pop_0919 %>%
  lapply(function(x) { x$est %>% 
      dplyr::select(1, 3, 4) %>% 
      pivot_wider(names_from = variable, 
                  values_from = estimate) %>%
      transmute(GEOID = GEOID,
             n_00_10 = B01001_003 + B01001_027 + B01001_004 + B01001_028,
             n_10_20 = B01001_005 + B01001_029 + B01001_006 + B01001_007 + B01001_030 + B01001_031,
             n_20_30 = B01001_008 + B01001_009 + B01001_010 + B01001_032 + B01001_033 + B01001_034 + B01001_011 + B01001_035,
             n_30_40 = B01001_012 + B01001_036 + B01001_013 + B01001_037,
             n_40_50 = B01001_014 + B01001_038 + B01001_015 + B01001_039,
             n_50_60 = B01001_016 + B01001_040 + B01001_017 + B01001_041,
             n_60_70 = B01001_018 + B01001_019 + B01001_042 + B01001_043 + B01001_020 + B01001_021 + B01001_044 + B01001_045,
             n_70_80 = B01001_022 + B01001_046 + B01001_023 + B01001_047,
             n_80_Inf = B01001_024 + B01001_048 + B01001_025 + B01001_049)})
or_tract_pop_0919_n[[1]] = or_tract_pop_0919_n[[1]] %>%
  full_join(cw_or %>% mutate(GEOID00 = as.character(GEOID00)), by = c('GEOID' = 'GEOID00')) %>%
  group_by(GEOID10) %>%
  summarize_at(.vars = vars(n_00_10:n_80_Inf),
               .funs = list(~floor(sum(. * ((POP00 * POPPCT00 / 100)/ sum(POP00 * POPPCT00 / 100)))))) %>%
  ungroup %>%
  mutate(GEOID = as.character(GEOID10)) %>%
  dplyr::select(-GEOID10) %>%
  dplyr::select(GEOID, n_00_10:n_80_Inf)
# pop_origin00 * pop_pct00 / 100 


or_tract_pop_0919_df = or_tract_pop_0919_n %>%
  mapply(function(x, y) { x %>% mutate(year = y) %>%
              pivot_longer(cols = 2:(ncol(.) - 1))}, 
              ., 2009:2019 %>% split(.,.), SIMPLIFY = FALSE) %>%
  do.call(bind_rows, .)

or_tract_pop_0619_df =
  bind_rows(
      or_tract_pop_0919_df %>% filter(year == 2009) %>% mutate(year = 2006),
      or_tract_pop_0919_df %>% filter(year == 2009) %>% mutate(year = 2007),
      or_tract_pop_0919_df %>% filter(year == 2009) %>% mutate(year = 2008),
      or_tract_pop_0919_df) %>%
  rename(agegroup = name,
         tpop = value) %>%
  mutate(agegroup = str_replace_all(agegroup, 'n_', ''))

# total population
or_tract_pop_0619_total = 
  or_tract_pop_0619_df %>%
  group_by(GEOID, year) %>%
  summarize(tpop = sum(tpop)) %>%
  # NA filling with 2009 population (tract 9705 in Malheur county)
  mutate(tpop = ifelse(is.na(tpop), tpop[which(!is.na(tpop))[1]], tpop)) %>%
  ungroup

# or_tract_pop_0619_total = or_tract_pop_0619_df %>%
#   filter(year >= 2010) %>%
#   group_by(GEOID, year) %>%
#   summarize(tpop = sum(tpop, na.rm = TRUE)) %>%
#   ungroup %>%
#   bind_rows(or_tract_pop_0609_total %>% mutate(year = 2006),
#             or_tract_pop_0609_total %>% mutate(year = 2007),
#             or_tract_pop_0609_total %>% mutate(year = 2008),
#             or_tract_pop_0609_total %>% mutate(year = 2009),
#             .) 
  
# tract data
or_tract = tigris::tracts(state = 'OR', year = 2010, refresh = T) %>%
  rmapshaper::ms_simplify(keep = 0.33, keep_shapes = TRUE)
or_tract_lite = or_tract %>%
  dplyr::select(GEOID10) %>%
  st_transform(4326)
ord_sf_tr = ord_sf %>%
  st_join(or_tract_lite)
ord_sf_tr_agecounts = ord_sf_tr %>%
  st_drop_geometry %>%
  mutate(agegroup = cut(age, c(seq(0, 80, 10), Inf), right = FALSE),
         agegroup = str_replace_all(agegroup, '[[:punct:]]', '_')) %>%
  group_by(GEOID10, dth_year, agegroup) %>%
  summarize(n_mental1 = sum(mental, na.rm = TRUE),
            n_mental3 = sum(mental3, na.rm = TRUE),
            n_mentaln = sum(mental_new, na.rm = TRUE),
            n_moodanxiety = sum(moodanxiety, na.rm = TRUE),
            n_substance = sum(substance, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(agegroup = str_sub(agegroup, 2, nchar(agegroup)-1))
  #pivot_wider(names_from = agegroup, names_prefix = 'dc', values_from = d_mental)

pop_agestr = tribble(
  ~agegroup,  ~n_all, ~n_male,  ~n_female,  ~newgroup,
  "00_05",  20201362, 10319427, 9881935,  "00_10",
  "05_10", 20348657, 10389638, 9959019, "00_10",
  "10_15",  20677194, 10579862, 10097332, "10_20",
  "15_20",  22040343, 11303666, 10736677, "10_20",
  "20_25",  21585999, 11014176, 10571823, "20_30",
  "25_30",  21101849, 10635591, 10466258, "20_30",
  "30_35",  19962099, 9996500,  9965599,  "30_40",
  "35_40",  20179642, 10042022, 10137620, "30_40",
  "40_45",  20890964, 10393977, 10496987, "40_50",
  "45_50",  22708591, 11209085, 11499506, "40_50",
  "50_55",  22298125, 10933274, 11364851, "50_60",
  "55_60",  19664805, 9523648,  10141157, "50_60",
  "60_65",  16817924, 8077500,  8740424,  "60_70",
  "65_70",  12435263, 5852547,  6582716,  "60_70",
  "70_75",  9278166,  4243972,  5034194,  "70_80",
  "75_80",  7317795,  3182388,  4135407,  "70_80",
  "80_85",  5743327,  2294374,  3448953,  "80_Inf",
  "85_Inf", 5493433,  1789679,  3703754,  "80_Inf",
)

# CPI (from Minneapolis Fed; base 1982-84)
cpi = rvest::read_html("https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1913-") %>%
  rvest::html_table() %>%
  .[[1]] %>%
  .[,-3]
cpi_0619 = cpi %>%
  filter(Year %in% 2006:2019) %>%
  mutate(Year = as.integer(Year),
         CPI10 = `Annual Average`[5] / `Annual Average`) %>%
  dplyr::select(1,3)

pop_agestr = pop_agestr %>%
  # regroup
  group_by(newgroup) %>%
  summarize_at(.vars = vars(n_all:n_female),
               .funs = list(~sum(.))) %>%
  ungroup %>%
  rename(agegroup = newgroup) %>%
  # regroup end
  pivot_longer(cols = 2:4) %>%
  group_by(name) %>% 
  mutate_at(vars(value), list(~(./sum(.)))) %>%
  ungroup %>%
  pivot_wider(names_from = name,
              values_from = value)
or_tract_asmr_base = or_tract_pop_0619_df %>%
  left_join(ord_sf_tr_agecounts, by = c('GEOID' = 'GEOID10', 'year' = 'dth_year', 'agegroup' = 'agegroup')) %>%
  left_join(pop_agestr, by = c('agegroup' = 'agegroup')) %>%
  mutate_at(.vars = vars(n_mental1:n_substance),
            .funs = list(~ifelse(is.na(.), 0, .))) %>%
  mutate(r_mental1_100k = 1e5 * (n_mental1 / tpop),
         r_mental3_100k = 1e5 * (n_mental3 / tpop),
         r_mentaln_100k = 1e5 * (n_mentaln / tpop))
or_tract_asmr = or_tract_asmr_base %>%
  group_by(GEOID, year) %>%
  summarize_at(.vars = vars(r_mental1_100k:r_mentaln_100k),
               .funs = list(~sum(.*n_all, na.rm = TRUE))) %>%
  ungroup
or_tract_n = or_tract_asmr_base %>%
  group_by(GEOID, year) %>%
  summarize_at(.vars = vars(n_mental1:n_substance),
               .funs = list(~sum(., na.rm = T))) %>%
  ungroup

or_tract_asmr_gg = ggplot(or_tract_asmr,
                          mapping = aes(x = year, y = r_mental3_100k)) +
                  geom_line(alpha = 0.3, lwd = 0.66, mapping = aes(group = GEOID)) +
                  stat_summary(geom = 'line', fun = mean, color = 'red', lwd = 1.5)
or_tract_n_gg = ggplot(or_tract_n,
                          mapping = aes(x = year, y = d_mental3)) +
                  geom_line(alpha = 0.3, lwd = 0.66, mapping = aes(group = GEOID)) +
                  stat_summary(geom = 'line', fun = mean, color = 'red', lwd = 1.5)

# or_tract_asmr06 = or_tract %>%
#   left_join(or_tract_asmr %>% filter(year == 2010),
#             by = c('GEOID10' = 'GEOID'))
# plot(or_tract_asmr06 %>% filter(GEOID10 == '41067032300'))

###
# composite map
2006:2019 %>% 
  split(.,.) %>%
  lapply(function(x) or_tract_lite %>% st_transform(3857) %>% mutate(year = x)) %>%
  do.call(rbind, .) -> or_tracts_0619
or_tracts_0619_md = or_tracts_0619 %>%
  left_join(or_tract_n, by = c('GEOID10' = 'GEOID', 'year' = 'year'))
# tm_shape(or_tracts_0619_md) +
#   #tm_style('classic') +
#   tm_fill('d_mental3', pal = 'Reds') +
#   tm_borders(lwd = 0.15, col = 'dark grey') +
#   tm_facets('year') +
#   tm_layout(inner.margins = 0.01)


## TODO: smooth the fluctuation of age-group population in each census tract / no need;
## TODO: to search the crisp definition of 'zero-inflated data or model'
## Figure
# or_tract_asmr_base %>% 
#   filter(GEOID == unique(GEOID)[835] & year >= 2009) %>%
#   ggplot(data = .,
#          mapping = aes(x = year, y = tpop, group = agegroup, color = agegroup)) +
#   geom_line(lwd = 1.25) -> one_tract_gg
# ggsave(plot = one_tract_gg, filename = '/mnt/d/tendency_one_tract_013022.png', width = 15, height = 12, units = 'cm', dpi = 300)


## ready-made ACS covariates (00-10 population weighted mean) ####
or_tract_0919_d = 2009:2019 %>%
  split(.,.) %>%
  mapply(function(x, y) x$est %>% mutate(year = y),
         or_tract_0919, ., SIMPLIFY = FALSE)
or_tract_0919_d[[1]] = or_tract_0919_d[[1]] %>%
  full_join(cw_or %>% mutate(GEOID00 = as.character(GEOID00)), by = c('GEOID' = 'GEOID00')) %>%
  group_by(GEOID10) %>%
  summarize_at(.vars = vars(n_medincome:p_uninsured),
               .funs = list(~sum(sum(. * ((POP00 * POPPCT00 / 100)/ sum(POP00 * POPPCT00 / 100)))))) %>%
  ungroup %>%
  mutate(GEOID = as.character(GEOID10)) %>%
  dplyr::select(-GEOID10) %>%
  dplyr::select(GEOID, n_medincome:p_uninsured)
or_tract_0919_df = or_tract_0919_d %>%
  do.call(bind_rows, .) %>%
  mutate(year = ifelse(is.na(year), 2009, year))
or_tract_0619_df = 
  bind_rows(
    or_tract_0919_d[[1]] %>% mutate(year = 2006),
    or_tract_0919_d[[1]] %>% mutate(year = 2007),
    or_tract_0919_d[[1]] %>% mutate(year = 2008),
    or_tract_0919_df)    




## Washington data cleaning ####

## Location filtering
wad = read_rds('./Data/Raw/WAD_DF_111420.rds')
wad_sf = wad %>% 
  filter(!is.na(long) & !is.na(lat)) %>% 
  filter(long < 0 & lat > 40) %>% 
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
  mutate(
        record_year = as.integer(str_sub(certno, 1, 4)),
        fac_type = as.numeric(as.character(fac_type)),
        educ = as.numeric(as.character(educ)),
        #ageunit = plyr::mapvalues(ageunit, c(0,1,2,4,5,6,9), c(1,1,2,3,4,5,9)),
         fac_type = 
           case_when(
             record_year > 2003 & record_year <= 2015 ~ plyr::mapvalues(fac_type, c(0,1,2,3,4,5,6,7,9), c(0,1,1,3,4,5,4,5,9)),
             record_year <= 2003 ~ plyr::mapvalues(fac_type, c(0,1,2,3,4,5,6,7,8,9), c(0,1,2,3,4,5,4,5,1,9)),
             TRUE ~ fac_type
           ),
         armforce = plyr::mapvalues(armforce, c(1,2,9), c('Y', 'N', 'U')),
         married = 
           case_when(
             record_year > 2003 & record_year <= 2015 ~ plyr::mapvalues(married, c(1:6,9), c('S', 'M', 'D', 'W', 'M', 'M', 'U')),
             record_year <= 2003 ~ plyr::mapvalues(married, c(1,2,3,4,9), c('S', 'M', 'D', 'W', 'U')),
             TRUE ~ married
             ),
         educ =
           case_when(
             record_year > 2003 & record_year <= 2015 ~ plyr::mapvalues(educ, 1:9, c(1,2,3,4,4,5,7,7,9)),
             record_year <= 2003 ~ plyr::mapvalues(educ, c(0:17,99), c(1,1,1,1,1,1,1,1,1,2,2,2,3,4,4,4,6,7,9)),
             TRUE ~ educ
             ),
         resunit = ifelse(is.na(resunit), res_lena, resunit),
         resunit = 
           ifelse(record_year <= 2015, plyr::mapvalues(resunit, c(0,1,2,3,4,5,6,9), c('Y','Y','M', 'W', 'D', 'H', 'M', 'U')), resunit),
         #referred = ifelse(record_year <= 2015, plyr::mapvalues(referred, c(1,2,9), c('Y', 'N', 'U')), referred),
         #smoking = ifelse(is.na(smoking), tbcontri, smoking),
         smoking = case_when(
           record_year <= 2003 ~ plyr::mapvalues(smoking, c(1,2,9), c('Y', 'N', 'U')),
           record_year > 2003 & record_year <= 2015 ~ plyr::mapvalues(smoking, c(1,2,7,8,9), c('Y', 'N', 'P', 'U','U')),
           TRUE ~ smoking
         )
         )

wad_sf <- wad_sf %>%
  filter(dth_yr >= 2006) %>% 
#   filter(!is.na(long) & !is.na(lat)) %>% 
#   filter(long < 0 & lat > 40) %>% 
  mutate(resunum = as.integer(resunum)) %>% 
  mutate(parkinson = ifelse(grepl('^(A521|F023|G20|G21|G22|G903).*', mltcse1), 'Parkinson', 'Others'),
         mental = ifelse(grepl('F.*', mltcse1), 1, 0),
         mental2 = ifelse(grepl('F.*', mltcse1) + grepl('F.*', mltcse2) + grepl('F.*', mltcse3), 1, 0),
         mental3 = ifelse(grepl('F.*', mltcse1) + grepl('F.*', mltcse2) + grepl('F.*', mltcse3) + 
                        grepl('F.*', mltcse4) + grepl('F.*', mltcse5) + grepl('F.*', mltcse6) +
                        grepl('F.*', mltcse7) + grepl('F.*', mltcse8) + grepl('F.*', mltcse9) + grepl('F.*', mltcse10), 1, 0),
         substance = ifelse(grepl('F1[0-9]', mltcse1) + grepl('F1[0-9]', mltcse2) + grepl('F1[0-9]', mltcse3) + 
                               grepl('F1[0-9]', mltcse4) + grepl('F1[0-9]', mltcse5) + grepl('F1[0-9]', mltcse6) +
                               grepl('F1[0-9]', mltcse7) + grepl('F1[0-9]', mltcse8) + grepl('F1[0-9]', mltcse9) + grepl('F1[0-9]', mltcse10), 1, 0),
         moodanxiety = ifelse(grepl('F[3-4][0-9]', mltcse1) + grepl('F[3-4][0-9]', mltcse2) + grepl('F[3-4][0-9]', mltcse3) + 
                               grepl('F[3-4][0-9]', mltcse4) + grepl('F[3-4][0-9]', mltcse5) + grepl('F[3-4][0-9]', mltcse6) +
                               grepl('F[3-4][0-9]', mltcse7) + grepl('F[3-4][0-9]', mltcse8) + grepl('F[3-4][0-9]', mltcse9) + grepl('F[3-4][0-9]', mltcse10), 1, 0),
         mental_new = ifelse(grepl('F[0-4][0-9]', mltcse1) + grepl('F[0-4][0-9]', mltcse2) + grepl('F[0-4][0-9]', mltcse3) + 
                               grepl('F[0-4][0-9]', mltcse4) + grepl('F[0-4][0-9]', mltcse5) + grepl('F[0-4][0-9]', mltcse6) +
                               grepl('F[0-4][0-9]', mltcse7) + grepl('F[0-4][0-9]', mltcse8) + grepl('F[0-4][0-9]', mltcse9) + grepl('F[0-4][0-9]', mltcse10), 1, 0)) %>%
  mutate(mental_underly = ifelse(grepl('F.*', underly), 1, 0)) %>% 
  mutate(resdays = case_when(
        resunit %in% c('H', 'N') ~ 1,
        resunit == 'D' ~ (1 * resunum),
        resunit == 'W' ~ (7 * resunum),
        resunit == 'M' ~ (30 * resunum),
        resunit == 'Y' ~ (365 * resunum),
        resunit == 'U' ~ as.numeric(resunum)
  )) %>% 
  mutate(race = factor(plyr::mapvalues(race, c(1:8, LETTERS[1:8]), c(1,2,rep(4,8), 3, rep(4,5))))) %>% 
  mutate(educ = factor(educ),
         race = factor(race),
         sex = factor(sex)) %>%
  # filter(educ != '99' & !married %in% c('P','A','U') & educ != '9' & race != '9' & sex != 'U') %>% 
#   filter(!smoking %in% c('C','U') & !is.na(smoking)) %>% 
#   mutate(sex = factor(sex, levels = c('M', 'F'), labels = c('Male', 'Female')),
#          educ = factor(educ, levels = 1:8, labels = c('<9yr', '9-12', 'HSdiploma', 'CollegeNoD', 'Associate', 'Bachelors', 'Masters', 'PhD/Professional')),
#          race = factor(race, levels = 1:4, labels = c('White', 'Black', 'Hispanic', 'Others')),
#          married = factor(married, levels = c('M', 'S','W','D'), labels = c('Married', 'Never married', 'Widowed', 'Divorced')),
#          smoking = factor(smoking, levels = c('Y', 'N', 'P'), labels = c('Yes', 'No', 'Paused'))) %>% 
#   filter(resdays >= 365) %>% 
  #as_tibble %>% 
  mutate_if(is.factor, droplevels) 



wa_tract_pop_0919_n = wa_tract_pop_0919 %>%
  lapply(function(x) { x$est %>% 
      dplyr::select(1, 3, 4) %>% 
      pivot_wider(names_from = variable, 
                  values_from = estimate) %>%
      transmute(GEOID = GEOID,
             n_00_05 = B01001_003 + B01001_027,
             n_05_10 = B01001_004 + B01001_028,
             n_10_15 = B01001_005 + B01001_029,
             n_15_20 = B01001_006 + B01001_007 + B01001_030 + B01001_031,
             n_20_25 = B01001_008 + B01001_009 + B01001_010 + B01001_032 + B01001_033 + B01001_034,
             n_25_30 = B01001_011 + B01001_035,
             n_30_35 = B01001_012 + B01001_036,
             n_35_40 = B01001_013 + B01001_037,
             n_40_45 = B01001_014 + B01001_038,
             n_45_50 = B01001_015 + B01001_039,
             n_50_55 = B01001_016 + B01001_040,
             n_55_60 = B01001_017 + B01001_041,
             n_60_65 = B01001_018 + B01001_019 + B01001_042 + B01001_043,
             n_65_70 = B01001_020 + B01001_021 + B01001_044 + B01001_045,
             n_70_75 = B01001_022 + B01001_046,
             n_75_80 = B01001_023 + B01001_047,
             n_80_85 = B01001_024 + B01001_048,
             n_85_Inf = B01001_025 + B01001_049)}) %>%
  mapply(function(x, y) { x %>% mutate(year = y) %>%
              pivot_longer(cols = 2:(ncol(.) - 1))}, 
              ., 2009:2019 %>% split(.,.), SIMPLIFY = FALSE) %>%
  do.call(bind_rows, .)

# 10 year basis

wa_tract_pop_0919_n = wa_tract_pop_0919 %>%
  lapply(function(x) { x$est %>% 
      dplyr::select(1, 3, 4) %>% 
      pivot_wider(names_from = variable, 
                  values_from = estimate) %>%
      transmute(GEOID = GEOID,
             n_00_10 = B01001_003 + B01001_027 + B01001_004 + B01001_028,
             n_10_20 = B01001_005 + B01001_029 + B01001_006 + B01001_007 + B01001_030 + B01001_031,
             n_20_30 = B01001_008 + B01001_009 + B01001_010 + B01001_032 + B01001_033 + B01001_034 + B01001_011 + B01001_035,
             n_30_40 = B01001_012 + B01001_036 + B01001_013 + B01001_037,
             n_40_50 = B01001_014 + B01001_038 + B01001_015 + B01001_039,
             n_50_60 = B01001_016 + B01001_040 + B01001_017 + B01001_041,
             n_60_70 = B01001_018 + B01001_019 + B01001_042 + B01001_043 + B01001_020 + B01001_021 + B01001_044 + B01001_045,
             n_70_80 = B01001_022 + B01001_046 + B01001_023 + B01001_047,
             n_80_Inf = B01001_024 + B01001_048 + B01001_025 + B01001_049)})
wa_tract_pop_0919_n[[1]] = wa_tract_pop_0919_n[[1]] %>%
  full_join(cw_wa %>% mutate(GEOID00 = as.character(GEOID00)), by = c('GEOID' = 'GEOID00')) %>%
  group_by(GEOID10) %>%
  summarize_at(.vars = vars(n_00_10:n_80_Inf),
               .funs = list(~floor(sum(. * ((POP00 * POPPCT00 / 100)/ sum(POP00 * POPPCT00 / 100)))))) %>%
  ungroup %>%
  mutate(GEOID = as.character(GEOID10)) %>%
  dplyr::select(-GEOID10) %>%
  dplyr::select(GEOID, n_00_10:n_80_Inf)
# pop_origin00 * pop_pct00 / 100 


wa_tract_pop_0919_df = wa_tract_pop_0919_n %>%
  mapply(function(x, y) { x %>% mutate(year = y) %>%
              pivot_longer(cols = 2:(ncol(.) - 1))}, 
              ., 2009:2019 %>% split(.,.), SIMPLIFY = FALSE) %>%
  do.call(bind_rows, .)

wa_tract_pop_0619_df =
  bind_rows(
      wa_tract_pop_0919_df %>% filter(year == 2009) %>% mutate(year = 2006),
      wa_tract_pop_0919_df %>% filter(year == 2009) %>% mutate(year = 2007),
      wa_tract_pop_0919_df %>% filter(year == 2009) %>% mutate(year = 2008),
      wa_tract_pop_0919_df) %>%
  rename(agegroup = name,
         tpop = value) %>%
  mutate(agegroup = str_replace_all(agegroup, 'n_', ''))

# total population
wa_tract_pop_0619_total = 
  wa_tract_pop_0619_df %>%
  #filter(year == 2009) %>%
  #full_join(cw_or %>% mutate(GEOID00 = as.character(GEOID00)), by = c('GEOID' = 'GEOID00')) %>%
  group_by(GEOID, year) %>%
  summarize(tpop = sum(tpop)) %>%
  #summarize_at(.vars = vars(tpop),
  #             .funs = list(~floor(sum((. * POP00 * POPPCT00 / 100), na.rm = TRUE)/ sum(POP00 * POPPCT00 / 100, na.rm = TRUE)))) %>%
  ungroup #%>%

# wa_tract_pop_0619_total = wa_tract_pop_0619_df %>%
#   filter(year >= 2010) %>%
#   group_by(GEOID, year) %>%
#   summarize(tpop = sum(tpop, na.rm = TRUE)) %>%
#   ungroup %>%
#   bind_rows(wa_tract_pop_0609_total %>% mutate(year = 2006),
#             wa_tract_pop_0609_total %>% mutate(year = 2007),
#             wa_tract_pop_0609_total %>% mutate(year = 2008),
#             wa_tract_pop_0609_total %>% mutate(year = 2009),
#             .) 
  
# tract data
wa_tract = tigris::tracts(state = 'WA', year = 2010, refresh = T) %>%
  rmapshaper::ms_simplify(keep = 0.33, keep_shapes = TRUE)
wa_tract_lite = wa_tract %>%
  dplyr::select(GEOID10) %>%
  st_transform(4326)
wad_sf_tr = wad_sf %>%
  st_join(wa_tract_lite)
wad_sf_tr_agecounts = wad_sf_tr %>%
  st_drop_geometry %>%
  mutate(agegroup = cut(age, c(seq(0, 80, 10), Inf), right = FALSE),
         agegroup = str_replace_all(agegroup, '[[:punct:]]', '_'),
         dth_year = dth_yr) %>%
  group_by(GEOID10, dth_year, agegroup) %>%
  summarize(n_mental1 = sum(mental, na.rm = TRUE),
            n_mental3 = sum(mental3, na.rm = TRUE),
            n_mentaln = sum(mental_new, na.rm = TRUE),
            n_moodanxiety = sum(moodanxiety, na.rm = TRUE),
            n_substance = sum(substance, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(agegroup = str_sub(agegroup, 2, nchar(agegroup)-1))
  #pivot_wider(names_from = agegroup, names_prefix = 'dc', values_from = d_mental)

wa_tract_asmr_base = wa_tract_pop_0619_df %>%
  left_join(wad_sf_tr_agecounts, by = c('GEOID' = 'GEOID10', 'year' = 'dth_year', 'agegroup' = 'agegroup')) %>%
  left_join(pop_agestr, by = c('agegroup' = 'agegroup')) %>%
  mutate_at(.vars = vars(n_mental1:n_substance),
            .funs = list(~ifelse(is.na(.), 0, .))) %>%
  mutate(r_mental1_100k = 1e5 * (n_mental1 / tpop),
         r_mental3_100k = 1e5 * (n_mental3 / tpop),
         r_mentaln_100k = 1e5 * (n_mentaln / tpop))
wa_tract_asmr = wa_tract_asmr_base %>%
  group_by(GEOID, year) %>%
  summarize_at(.vars = vars(r_mental1_100k:r_mentaln_100k),
               .funs = list(~sum(.*n_all, na.rm = TRUE))) %>%
  ungroup
wa_tract_n = wa_tract_asmr_base %>%
  group_by(GEOID, year) %>%
  summarize_at(.vars = vars(n_mental1:n_substance),
               .funs = list(~sum(., na.rm = T))) %>%
  ungroup

wa_tract_asmr_gg = ggplot(wa_tract_asmr,
                          mapping = aes(x = year, y = r_mental3_100k)) +
                  geom_line(alpha = 0.3, lwd = 0.66, mapping = aes(group = GEOID)) +
                  stat_summary(geom = 'line', fun = mean, color = 'red', lwd = 1.5)
wa_tract_n_gg = ggplot(wa_tract_n,
                          mapping = aes(x = year, y = d_mental3)) +
                  geom_line(alpha = 0.3, lwd = 0.66, mapping = aes(group = GEOID)) +
                  stat_summary(geom = 'line', fun = mean, color = 'red', lwd = 1.5)

wa_tract_asmr06 = wa_tract %>%
  left_join(wa_tract_asmr %>% filter(year == 2010),
            by = c('GEOID10' = 'GEOID'))
#plot(wa_tract_asmr06 %>% filter(GEOID10 == '41067032300'))

2006:2019 %>% 
  split(.,.) %>%
  lapply(function(x) wa_tract_lite %>% st_transform(3857) %>% mutate(year = x)) %>%
  do.call(rbind, .) -> wa_tracts_0619
wa_tracts_0619_md = wa_tracts_0619 %>%
  left_join(wa_tract_n, by = c('GEOID10' = 'GEOID', 'year' = 'year'))


## ready-made ACS covariates (00-10 population weighted mean) ####
wa_tract_0919_d = 2009:2019 %>%
  split(.,.) %>%
  mapply(function(x, y) x$est %>% mutate(year = y),
         wa_tract_0919, ., SIMPLIFY = FALSE)
wa_tract_0919_d[[1]] = wa_tract_0919_d[[1]] %>%
  full_join(cw_wa %>% mutate(GEOID00 = as.character(GEOID00)), by = c('GEOID' = 'GEOID00')) %>%
  group_by(GEOID10) %>%
  summarize_at(.vars = vars(n_medincome:p_uninsured),
               .funs = list(~sum(sum(. * ((POP00 * POPPCT00 / 100)/ sum(POP00 * POPPCT00 / 100)))))) %>%
  ungroup %>%
  mutate(GEOID = as.character(GEOID10)) %>%
  dplyr::select(-GEOID10) %>%
  dplyr::select(GEOID, n_medincome:p_uninsured)
wa_tract_0919_df = wa_tract_0919_d %>%
  do.call(bind_rows, .) %>%
  mutate(year = ifelse(is.na(year), 2009, year))
wa_tract_0619_df = 
  bind_rows(
    wa_tract_0919_d[[1]] %>% mutate(year = 2006),
    wa_tract_0919_d[[1]] %>% mutate(year = 2007),
    wa_tract_0919_d[[1]] %>% mutate(year = 2008),
    wa_tract_0919_df)    



### combine data
orwa_tract_pop_0619_total = 
  bind_rows(or_tract_pop_0619_total         ,
            wa_tract_pop_0619_total)
