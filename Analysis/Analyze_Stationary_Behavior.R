# This script explores is there is evidence of worsening performance (non-stationary behavior) 
library(ggplot2)
library(dplyr)
#library(data.table)
#library(tidyr)
library(stringr)
library(lme4)
library(dplyr)
library(pbkrtest)
use_df <- read.csv("./cleaned_data.csv")



######################################
##### I just want to rerun this including continuum and paradigm as explicit factors ########################
endpts <- use_df %>% 
  #filter(str_detect(stimulus, "Ba")) %>%
  subset(str_detect(stimulus,"1")| str_detect(stimulus,"7")) %>%
  group_by(run_id, stimulus, trial, paradigm)

# Mutate to decide if correct or not
endpts <- mutate(endpts, correct = ifelse(step == 1 & response == 0, "1",
                                                  ifelse( step == 7 & response == 1, "1", "0")))
endpts$correct <- as.numeric(endpts$correct)
## Deviation contrasts
endpts$paradigm <- factor(endpts$paradigm, levels=c("Single", "ABX"))
endpts$continuum <- factor(endpts$continuum,
                                 levels=c("/ba/-/da/", "/ʃa/-/sa/"))
contrasts(endpts$paradigm) <- matrix(c(-0.5, 0.5), nrow=2, 
                                        dimnames=list(levels(endpts$paradigm), "ABX"))
contrasts(endpts$continuum) <- matrix(c(-0.5, 0.5), nrow=2, 
                                            dimnames=list(levels(endpts$continuum), "/ʃa/-/sa/"))
## center reading score, etc
endpts$adhd_dx <- as.logical(endpts$adhd_dx)
endpts$wj_brs <- scale(endpts$wj_brs, scale=FALSE)
endpts$age <- scale(endpts$age_at_testing, scale=FALSE)
endpts$trial <- scale(endpts$trial, scale=FALSE)

# Make the biggest model
lmfit <- lmer(correct ~ trial*wj_brs*paradigm*continuum + adhd_dx + age +  (1|subject_id), endpts)
summary(lmfit)
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs

# Remove some parameters
lmfit <- lmer(correct ~ trial + wj_brs + paradigm + adhd_dx + age + (1|subject_id), endpts)
lmfit <- lmer(correct ~ trial*wj_brs*paradigm + (1|subject_id), endpts)


# Remove ADHD
lmfit_no_adhd <- update(lmfit, ~ . - adhd_dx)
anova(lmfit, lmfit_no_adhd) #OK to remove adhd

# Remove age
lmfit_no_age <- update(lmfit, ~ . - age)
anova(lmfit, lmfit_no_age) #OK to remove age

# Remove both nuisances
lmfit_no_nuisance <- update(lmfit, ~ . - age -adhd_dx)
anova(lmfit, lmfit_no_nuisance) #OK to remove adhd and age


######### Go deeper into model ###################
summary(lmfit_no_nuisance)
coefs <- data.frame(coef(summary(lmfit_no_nuisance)))
df.KR <- get_ddf_Lb(lmfit_no_nuisance, fixef(lmfit_no_nuisance))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs

# Remove paradigm
lmfit_no_paradigm <- lmer(correct ~ trial*wj_brs*continuum  +  (1|subject_id), endpts)
anova(lmfit_no_nuisance, lmfit_no_paradigm) # not OK to remove paradigm

# Remove continuum
lmfit_no_continuum <- lmer(correct ~ trial*wj_brs*paradigm  +  (1|subject_id), endpts)
anova(lmfit_no_nuisance, lmfit_no_continuum) # OK to remove continuum

summary(lmfit_no_continuum)
coefs <- data.frame(coef(summary(lmfit_no_continuum)))
df.KR <- get_ddf_Lb(lmfit_no_continuum, fixef(lmfit_no_continuum))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs


lmfit_no_interaction_trialpara <- lmer(correct ~ trial + wj_brs + paradigm + wj_brs:trial+ wj_brs:paradigm + trial:paradigm:wj_brs + (1|subject_id), endpts)
anova(lmfit_no_continuum, lmfit_no_interaction_trialpara) #OK to remove paradigm:trial int

lmfit_no_interaction_three <- lmer(correct ~ trial + wj_brs + paradigm + wj_brs:trial+ wj_brs:paradigm + (1|subject_id), endpts)
anova(lmfit_no_interaction_trialpara, lmfit_no_interaction_three) #OK to remove paradigm:trial:wj interaction

lmfit_no_main_para <- lmer(correct ~ trial + wj_brs + wj_brs:trial+ wj_brs:paradigm + (1|subject_id), endpts)
anova(lmfit_no_interaction_three, lmfit_no_main_para) #OK to remove main effect of paradigm


lmfit_no_trialwj <- lmer(correct ~ trial + wj_brs +wj_brs:paradigm + (1|subject_id), endpts)
anova(lmfit_no_main_para, lmfit_no_trialwj) #OK to remove trial:wj

### Final Model ###
summary(lmfit_no_trialwj)
coefs <- data.frame(coef(summary(lmfit_no_trialwj)))
df.KR <- get_ddf_Lb(lmfit_no_trialwj, fixef(lmfit_no_trialwj))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs








