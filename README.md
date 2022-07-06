# SICSS-Oxford-2022

This is a lecture on "Online Survey and Post-Stratification" for SICSS-Oxford 2022 organised by Francesco Rampazzo and Maksim Zubok. It is inspired from previous lectures prepared with and by Dr. Emanuele Del Fava. 

## Online Surveys

We start with examples from using Facebook as a way to recruit survey respondents. We stress the importance of using quota sampling.  


## Post-Stratification

We introduce post-stratification. Advatanges and disadvantages of multilevel regression with post-stratification (MRP) are discussed. 

### Exercise

There are six outcome variables: 

- No. total contacts (count variable): cnt_count_30
- No. contacts at work (count variable): cnt_count_w_30
- No. contacts in the general community (count variable): cnt_count_gc_30
- Wearing a face mask (dummy variable): worn_face_mask
- Threat to the world, to the country or to the local community (0-1 variable): threat_community_q
- Threat to oneself or to the family (0-1 variable): threat_personal_q

Each group should use a different dependent variable. The goal of the exercise it to run a Bayesian MRP. 

The data for the exercise come from a Facebook survey. The sample is a shuffled data from the Covid-19 Health Behavior Survey (CHBS) for Germany. The data were collected during a week between March and April 2020. The sample contains 1,292 observations and 14 variables. 

Other data sets: 

- DESTATIS 2019 data by age group, sex, and region (DESTATIS 2019), with population aged 18 years old or more
- European Social Survey (ESS) Data by age group, sex, region, education and HH size (ESS 9, 2018), data on population aged 18 years old or more


More information on the Facebook survey data: 

- https://epjdatascience.springeropen.com/articles/10.1140/epjds/s13688-021-00270-1
- https://bmjopen.bmj.com/content/bmjopen/11/10/e050651.full.pdf
- https://www.jmir.org/2020/12/e20653/


