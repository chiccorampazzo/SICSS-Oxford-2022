rm(list=ls())

# tidyverse packages
# install.packages(c("srvyr", "tidybayes", "tidyverse"))
library(srvyr)
library(tidybayes)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
# miscellaneous packages
# install.packages(c("patchwork"))
library(patchwork)

# statistical packages
# install.packages(c("brms"))
library(brms)

##### Reading shuffled data from CHBS for Germany ##### 
### Data collected during a week between March and April 2020
### 1,292 observations
### 14 variables

DataDE <- 
  read_csv( "./input/CHBS_resampled_MarApr2020_DE.csv") %>% 
  filter(educ != "NoResponse",
         is.na(hhsize_grp) == F) %>%
  mutate(
    educ = case_when(
      educ ==  "PrimaryOrLess" ~ "1-PrimaryOrLess",
     educ == "Secondary" ~ "2-Secondary",
     educ == "GraduateOrMore" ~ "3-GraduateOrMore")) %>% 
  mutate(sex = as.factor(sex),
         agegroup = as.factor(agegroup),
         area = as.factor(area),
         educ = as.factor(educ),
         hhsize_grp = as.factor(hhsize_grp))

print(DataDE)

### Six outcomes:
# 1. No. total contacts (count variable): cnt_count_30
# 2. No. contacts at work (count variable): cnt_count_w_30
# 3. No. contacts in the general community (count variable): cnt_count_gc_30
# 4. Wearing a face mask (dummy variable): worn_face_mask
# 5. Threat to the world, to the country or to the local community (0-1 variable): threat_community_q
# 6. Threat to oneself or to the family (0-1 variable): threat_personal_q

### Distribution names 
# help("brmsfamily")
# poisson: poisson
# negative binomial: negbinomial
# Bernoulli: bernoulli
# Beta: Beta
# log-normal: lognormal

##### Data by age group, sex, and region (DESTATIS 2019) #####
### Population aged 18 years old or more
variables <- c("sex", "agegroup", "area", "educ", "hhsize_grp")
setlabels <- list(
  sex = c("Female", "Male"),
  agegroup = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49",
               "50-54", "55-59", "60-64", "65-69", "70-74", "75+"),
  area = c("DE-BW","DE-BY","DE-BE","DE-BB",
           "DE-HB","DE-HH","DE-HE","DE-MV",
           "DE-NI","DE-NW","DE-RP","DE-SL",
           "DE-SN","DE-ST","DE-SH","DE-TH"),
  educ = c("Primary \neducation or less",
           "Secondary \neducation",
           "Graduate or \npost-graduate"),
  hhsize_grp = c("1 p.", "2 pp.", "3+ pp.")
)
settitles <- c("Sex", "Age", "Region", "Education level", "HH size")

TableCensusDE <-
  read_csv("./input/PopulationCensus2019DE.csv") %>% 
  rename_all(tolower) %>% 
  rename("pop_count" = "count") %>% 
  filter(sex != "Total",
         agegroup != "0-17")

##### Data by age group, sex, region, education and HH size (ESS 9, 2018) #####
### Population aged 18 years old or more
TableESS9DE  <- read_csv("./input/PopulationESS9DE.csv")

###### Multilevel regression (MR) ######
### Bayesian hierarchical modeling using Hamiltonian Monte Carlo (HMC) in Stan
### Weakly informative priors (brms defaults): 
### Intercept: student_t(3, 1.1, 2.5)
### sd varying intercepts: student_t(3, 0, 2.5)
### shape parameter of NB: gamma(0.01, 0.01)

### Including sex, age, and area
mr_fit <- brm(RESPONSE ~ (1 | sex) + (1 | agegroup) + (1 | area) + (1 | educ) + (1 | hhsize_grp),
                data = DataDE, family = FAMILY(),
                chains = 4, cores = 4, iter = 3000, warmup = 2000, 
                control = list(adapt_delta = 0.95)) # 18.5 min.
# save(mr_fit_1, file = "./output/mrfit_contacts_3var_de.RData")
# load("./output/mrfit_contacts_3var_de.RData")
summary(mr_fit)
loo(mr_fit) # All Pareto k estimates are good (k < 0.5).

### Define new data for predictions

# Dataset by sex, age, and area (for poststratification with census data)
new_data_1 <-
  DataDE %>% 
  filter(educ != "NoResponse") %>%
  group_by(country, area, sex, agegroup, .drop = F) %>%
  summarise(sample_count = n()) %>%
  ungroup()

# Dataset by sex, age, area, education, and HH size 
# (for poststratification with ESS survey data)
new_data_2 <-
  DataDE %>% 
  filter(educ != "NoResponse") %>%
  group_by(country, area, sex, agegroup, educ, hhsize_grp, .drop = F) %>%
  summarise(sample_count = n()) %>%
  ungroup()

### Select variable (from 1 to 5)
# generare il grafico  sesso....
v <- 5

##### Population mean #####
tmp0 <-
  DataDE %>%
  group_by(country) %>% 
  summarise(mean = mean(cnt_count_30, na.rm = T)) %>%
  mutate(m0_mean = mean) %>%  
  select(-c(mean))

##### Group means #####
tmp1 <-
  DataDE %>%
  group_by(country, !!sym(variables[v])) %>% 
  summarise(mean = mean(cnt_count_30, na.rm = T)) %>%
  mutate(m1_mean = mean) %>%  
  select(-c(mean))

##### Group means adjusted for pseudo-design unit weights #####
tmp2 <-
  DataDE %>%
  as_survey(weights = d.weight) %>% 
  group_by(country, !!sym(variables[v])) %>% 
  summarise(m2 = survey_mean(cnt_count_30, vartype = "ci", na.rm = T)) %>% 
  rename("m2_mean" = "m2")

##### Bayesian MRP #####
### Post-stratification of predictions from multilevel model, using ESS (2018)
tmp3 <- 
  mr_fit %>%
  add_predicted_draws(newdata = new_data_2,
                      allow_new_levels=TRUE) %>%
  left_join(TableESS9DE) %>%
  select(-c(pop_prop, pop_count_se)) %>% 
  group_by(country, !!sym(variables[v]), .draw) %>% 
  summarise(wgt_pred = sum(.prediction * pop_count/sum(pop_count))) %>% 
  group_by(country, !!sym(variables[v])) %>% 
  summarise(m3_mean = mean(wgt_pred), 
            m3_low = quantile(wgt_pred, 0.025), 
            m3_upp = quantile(wgt_pred, 0.975))

### Post-stratification of predictions from multilevel model, using Census (2019)
### (Not running if variables are educ or hhsize_grp!)
tmp4 <-
  mr_fit %>%
  add_predicted_draws(newdata = new_data_1,
                      allow_new_levels=TRUE) %>%
  # group_by(country, area, sex, agegroup, .draw) %>%
  # summarise(mean = mean(.prediction)) %>%
  left_join(TableCensusDE) %>%
  group_by(country, !!sym(variables[v]), .draw) %>%
  summarise(wgt_pred = sum(.prediction * pop_count/sum(pop_count))) %>%
  group_by(country, !!sym(variables[v])) %>%
  summarise(m4_mean = mean(wgt_pred),
            m4_low = quantile(wgt_pred, 0.025),
            m4_upp = quantile(wgt_pred, 0.975))

### Combining results
CompareResults <-
  tmp1 %>%        
  left_join(tmp0) %>% 
  left_join(tmp2) %>%
  left_join(tmp3) %>%
  left_join(tmp4) %>%
  pivot_longer(
    cols = colnames(.[-c(1,2)]),
    names_to = c("model","type"),
    names_sep = "_",
    values_to = "value") %>% 
  pivot_wider(
    names_from = "type",
    values_from = "value")

### Plot results
plt_compare <-
  CompareResults %>% 
  ggplot(aes(y = mean, x = forcats::fct_inorder(!!sym(variables[v])))) + 
  geom_point(aes(color = model), position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = low, ymax = upp, color = model), 
                position = position_dodge(0.3), width = 0.2) + 
  theme_bw() +
  labs(title = "Germany",
       subtitle = "March - April, 2020",
       caption = "Original data source: COVID-19 Health Behaviour Survey (CHBS)",
       y = "Average number of overall social contacts",
       x = settitles[v],
       color = "Estimation \nmethod") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2),
                   labels = setlabels[[v]]) +
  scale_color_discrete(labels = c("Population mean",
                                  "Group means",
                                  "Group means with pseudo-design weights",
                                  "Bayesian MRP")) +
  theme(text = element_text(size = 14),
        legend.position = "bottom",
        legend.box="vertical", 
        legend.margin=margin()) +
  guides(colour = guide_legend(nrow = 3))
