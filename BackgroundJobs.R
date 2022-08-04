#### Background jobs ####

library(tidyverse)
library(brms)

###############
# null modell mit person als clustervariable
mod0 <- brm(answerlength_incl_zero ~ 1 + (1|respondent_hash),
            family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
            data = data_poisson)

# null modell mit Kursleiter als clustervariable
mod00 <- brm(answerlength_incl_zero ~ 1 + (1|teacher_id),
             family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
             data = data_poisson)

# null modell mit Kursleiter und person als clustervariable
mod000 <- brm(answerlength_incl_zero ~ 1 + as.factor(question_id) + (1|respondent_hash + teacher_id),
              family = zero_inflated_poisson(), # zero_inflated_poisson(link = "log", link_zi = "logit")
              data = data_poisson)
###################