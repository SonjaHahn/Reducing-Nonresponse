#### Background jobs ####

 library(tidyverse)
 library(brms)

###############
# # null modell mit person als clustervariable
# mod0 <- brm(answerlength_incl_zero ~ 1 + (1|respondent_hash),
#             family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
#             data = data_poisson)
# 
# # null modell mit Kursleiter als clustervariable
# mod00 <- brm(answerlength_incl_zero ~ 1 + (1|teacher_id),
#              family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
#              data = data_poisson)

# #null modell mit Kursleiter und person als clustervariable
# mod000 <- brm(answerlength_incl_zero ~ 1 + as.factor(question_id) + (1|respondent_hash + teacher_id),
#               family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
#               data = data_poisson)
# 
# # Family Specific Parameters: 
# #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # zi     0.23      0.01     0.22     0.24 1.00     2729     2375
# 
# # null modell neu geschrieben (konsistente Syntax mit den Modellen unten)
# # ToDo: Abgleich mit Mod000
# 
# 
# mod000sep <- brm(bf(answerlength_incl_zero  ~ 1 + as.factor(question_id) + (1|respondent_hash + teacher_id),
#                                       zi ~ 1),
#               family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
#               data = data_poisson)
# 
# # Population-Level Effects: 
# #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # Intercept                  4.25      0.03     4.20     4.30 1.02      248      667
# # zi_Intercept              -1.19      0.03    -1.25    -1.14 1.00     3273     3063
# # as.factorquestion_id48    -0.35      0.00    -0.35    -0.34 1.00     6405     3112
# 
# # mod000r <- brm(answerlength_incl_zero ~ 1 + as.factor(question_id) + (1|respondent_hash),
# #               family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
# #               data = data_poisson)
# 
# 
# ###################
# 
# # Model with parameters for position effects
# 
# #fit_zinb2<-brm(bf(count~persons+child+camper,zi~child), data=zinb,family=zero_inflated_poisson())
# 
# mod_position <- brm(bf(answerlength_incl_zero  ~ 1 + as.factor(question_id) +
#                       as.factor(position) +
#                       (1|respondent_hash + teacher_id),
#                     zi ~ 1 + as.factor(position)),
#                  family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
#                  data = data_poisson)
# 
# # Das funktioniert gut und gibt erwartungskonforme Ergebnisse aus
# 
# # Model with parameters for motivational cues
# 
# 
# mod_motCue <- brm(bf(answerlength_incl_zero  ~ 1 + as.factor(question_id) +
#                        motiv_cue +
#                          (1|respondent_hash + teacher_id),
#                        zi ~ 1 + motiv_cue),
#                     family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
#                     data = data_poisson)

# Das konvergiert nicht so schön, auf Länge keinen einfluss (und schlechtes Rhat)
# auf ZI tendenziell negativen Einfluss; da hatten wir aber auch eine Interaktion
# mit der anderen UV in den deskriptiven Ergenissen gesehen.


# Model with parameters for both (position and motivational cues)

# mod_motCueaAndPosition <- brm(bf(answerlength_incl_zero  ~ 1 + as.factor(question_id) +
#                                  motiv_cue + as.factor(position) +
#                                  (1|respondent_hash + teacher_id),
#                                zi ~ 1 + motiv_cue + as.factor(position)),
#                             family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
#                             data = data_poisson)

# Hat etwas Problemchen; v.a. der Poisson-Parameter von motivating instruction

# Model with parameters for both including interactions

# mod_motCueByPosition <- brm(bf(answerlength_incl_zero  ~ 1 + as.factor(question_id) +
#                        motiv_cue*as.factor(position) +
#                        (1|respondent_hash + teacher_id),
#                      zi ~ 1 + motiv_cue*as.factor(position)),
#                   family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
#                   data = data_poisson)

# das sieht ganz gut aus. Position führt zu längeren Texten (erwartungskonform)
# Mot cue zu höherer ZI (gegen Erwartung), Position at end zu noch höherer ZI (Erwartungskonform, Fatigue)
# Interaktion beider Faktoren ist negativ (das ist vermutlich das, was deskriptiv auch zu sehen war: Am Ende mit Mot. Cue geringere Missings)
# und zwar so stark, dass der einfache Effekt von Mot Cue ausgeglichen wird.


# Der Vollständigkeit halber noch komplettere Modelle

mod_0000 <- brm(bf(answerlength_incl_zero  ~ 1 + as.factor(question_id) +
                             #  motiv_cue*as.factor(position) +
                               (1|respondent_hash + teacher_id),
                             zi ~ 1 + as.factor(question_id) +
                           #   motiv_cue*as.factor(position)+
                               (1|respondent_hash + teacher_id)),
                          family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
                          data = data_poisson)

# > summary(mod_0000)
# Family: zero_inflated_poisson 
# Links: mu = log; zi = logit 
# Formula: answerlength_incl_zero ~ 1 + as.factor(question_id) + (1 | respondent_hash + teacher_id) 
# zi ~ 1 + as.factor(question_id) + (1 | respondent_hash + teacher_id)
# Data: data_poisson (Number of observations: 6282) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
# 
# Group-Level Effects: 
#   ~respondent_hash (Number of levels: 3141) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)        0.73      0.01     0.71     0.75 1.01      301      685
# sd(zi_Intercept)     3.04      0.19     2.67     3.41 1.02      508     1200
# 
# ~teacher_id (Number of levels: 105) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)        0.22      0.02     0.17     0.26 1.02      184      437
# sd(zi_Intercept)     0.80      0.13     0.58     1.06 1.01      635     1261
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                     4.25      0.02     4.20     4.30 1.01      441     1089
# zi_Intercept                 -4.37      0.23    -4.84    -3.93 1.01      588     1586
# as.factorquestion_id48       -0.35      0.00    -0.35    -0.34 1.00     7166     3237
# zi_as.factorquestion_id48     3.05      0.17     2.74     3.39 1.01      748     1798
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

mod_predictors <- brm(bf(answerlength_incl_zero  ~ 1 + as.factor(question_id) +
                               motiv_cue + as.factor(position) +
                               (1|respondent_hash + teacher_id),
                             zi ~ 1 + as.factor(question_id) +
                              motiv_cue + as.factor(position)+
                               (1|respondent_hash + teacher_id)),
                          family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
                          data = data_poisson)

# > summary(mod_predictors)
# Family: zero_inflated_poisson 
# Links: mu = log; zi = logit 
# Formula: answerlength_incl_zero ~ 1 + as.factor(question_id) + motiv_cue + as.factor(position) + (1 | respondent_hash + teacher_id) 
# zi ~ 1 + as.factor(question_id) + motiv_cue + as.factor(position) + (1 | respondent_hash + teacher_id)
# Data: data_poisson (Number of observations: 6282) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
# 
# Group-Level Effects: 
#   ~respondent_hash (Number of levels: 3141) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)        0.73      0.01     0.71     0.75 1.01      397      880
# sd(zi_Intercept)     2.95      0.19     2.60     3.35 1.01      556      889
# 
# ~teacher_id (Number of levels: 105) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)        0.22      0.02     0.18     0.27 1.03      170      533
# sd(zi_Intercept)     0.78      0.12     0.56     1.01 1.00     1055     2221
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                             4.20      0.03     4.14     4.26 1.01      509     1248
# zi_Intercept                         -4.99      0.29    -5.60    -4.45 1.00      799     1025
# as.factorquestion_id48               -0.35      0.00    -0.36    -0.34 1.00     7463     2995
# motiv_cuemotivatinginstruction       -0.00      0.03    -0.05     0.06 1.01      212      406
# as.factorpositionattheend             0.10      0.03     0.05     0.16 1.03      234      571
# zi_as.factorquestion_id48             3.05      0.17     2.73     3.41 1.00      867     1341
# zi_motiv_cuemotivatinginstruction    -0.04      0.15    -0.34     0.25 1.00     2574     3158
# zi_as.factorpositionattheend          1.30      0.16     0.99     1.64 1.00     1638     2453
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).


mod_predictors_and_interaction <- brm(bf(answerlength_incl_zero  ~ 1 + as.factor(question_id) +
                                 motiv_cue*as.factor(position) +
                                 (1|respondent_hash + teacher_id),
                               zi ~ 1 + as.factor(question_id) +
                                motiv_cue*as.factor(position)+
                                 (1|respondent_hash + teacher_id)),
                            family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
                            data = data_poisson)


# > summary(mod_predictors_and_interaction)
# Family: zero_inflated_poisson 
# Links: mu = log; zi = logit 
# Formula: answerlength_incl_zero ~ 1 + as.factor(question_id) + motiv_cue * as.factor(position) + (1 | respondent_hash + teacher_id) 
# zi ~ 1 + as.factor(question_id) + motiv_cue * as.factor(position) + (1 | respondent_hash + teacher_id)
# Data: data_poisson (Number of observations: 6282) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
# 
# Group-Level Effects: 
#   ~respondent_hash (Number of levels: 3141) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)        0.73      0.01     0.71     0.75 1.01      410      620
# sd(zi_Intercept)     2.94      0.19     2.59     3.34 1.01      538     1012
# 
# ~teacher_id (Number of levels: 105) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)        0.22      0.02     0.17     0.27 1.01      175      458
# sd(zi_Intercept)     0.77      0.12     0.54     1.02 1.00      670     1505
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                                                       4.21      0.03     4.14     4.27 1.01      262      472
# zi_Intercept                                                   -5.35      0.31    -5.99    -4.76 1.01      689     1434
# as.factorquestion_id48                                         -0.35      0.00    -0.35    -0.34 1.00     6457     3166
# motiv_cuemotivatinginstruction                                 -0.01      0.04    -0.09     0.06 1.03      162      248
# as.factorpositionattheend                                       0.09      0.04     0.01     0.17 1.02      130      371
# motiv_cuemotivatinginstruction:as.factorpositionattheend        0.03      0.06    -0.08     0.14 1.02      150      438
# zi_as.factorquestion_id48                                       3.05      0.17     2.74     3.39 1.01      921     1737
# zi_motiv_cuemotivatinginstruction                               0.67      0.23     0.23     1.11 1.00     1497     2012
# zi_as.factorpositionattheend                                    1.96      0.23     1.53     2.44 1.00     1290     2017
# zi_motiv_cuemotivatinginstruction:as.factorpositionattheend    -1.32      0.31    -1.94    -0.71 1.00     1208     1606
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).