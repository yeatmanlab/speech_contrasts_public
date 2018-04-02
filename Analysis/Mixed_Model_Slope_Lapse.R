#!/usr/bin/env Rscript

## figure_individ_and_mean_fitted_models_by_group.R
## aggregates the data by group and cue type and plots model fits

library(lme4)
library(pbkrtest)

## When loading/saving, this script assumes the working directory is set to the
## root directory of the repo. Relative to this script's location that is:
#setwd("..")

psychometrics <- read.csv("cleaned_psychometrics.csv")

## set deviation contrasts
psychometrics$paradigm <- factor(psychometrics$paradigm, levels=c("Single", "ABX"))
psychometrics$continuum <- factor(psychometrics$continuum,
                                 levels=c("/ba/-/da/", "/ʃa/-/sa/"))
paradigm_dimnames <- list(levels(psychometrics$paradigm),
                          levels(psychometrics$paradigm)[2])
continuum_dimnames <- list(levels(psychometrics$continuum),
                           levels(psychometrics$continuum)[2])
contrasts(psychometrics$paradigm) <- matrix(c(-0.5, 0.5), nrow=2, dimnames=paradigm_dimnames)
contrasts(psychometrics$continuum) <- matrix(c(-0.5, 0.5), nrow=2, dimnames=continuum_dimnames)

## center reading score, etc
psychometrics$adhd_dx <- as.logical(psychometrics$adhd_dx)
psychometrics$wj_brs <- scale(psychometrics$wj_brs, scale=FALSE)
psychometrics$wasi_mr_ts <- scale(psychometrics$wasi_mr_ts, scale=FALSE)

## ## ## ## ## ## ##
##  SLOPE MODELS  ##
## ## ## ## ## ## ##
full_model <- lmer(slope ~ wj_brs*paradigm*continuum + adhd_dx + wasi_mr_ts + (1|subject_id),
              data=psychometrics)
# p-values for full model
coefs <- data.frame(coef(summary(full_model)))
df.KR <- get_ddf_Lb(full_model, fixef(full_model))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)

## model selection: nuisance variables
no_wasi_model <- update(full_model, ~ . - wasi_mr_ts)
anova(full_model, no_wasi_model)

no_adhd_model <- update(full_model, ~ . - adhd_dx)
anova(full_model, no_adhd_model)

no_nuisance_model <- update(full_model, ~ . - wasi_mr_ts - adhd_dx)
anova(full_model, no_nuisance_model)

## model selection: non-significant predictors of interest
no_continuum_model <- lmer(slope ~ wj_brs*paradigm + (1|subject_id), data=psychometrics)
anova(no_nuisance_model, no_continuum_model)

no_paradigm_model <- lmer(slope ~ wj_brs*continuum + (1|subject_id), data=psychometrics)
anova(no_nuisance_model, no_paradigm_model)

no_continuum_or_paradigm_model <- lmer(slope ~ wj_brs + (1|subject_id), data=psychometrics)
anova(no_nuisance_model, no_continuum_or_paradigm_model)

# Summary for final model
coefs <- data.frame(coef(summary(no_continuum_or_paradigm_model)))
df.KR <- get_ddf_Lb(no_continuum_or_paradigm_model, fixef(no_continuum_or_paradigm_model))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)

## ## ## ## ## ## ## ##
##  LAPSE RATE MODEL ##
## ## ## ## ## ## ## ##
psychometrics$lapse_rate <- with(psychometrics, (lo_asymp + hi_asymp) / 2)


# Summary for full model
coefs <- data.frame(coef(summary(full_model)))
df.KR <- get_ddf_Lb(full_model, fixef(full_model))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)

## model selection: nuisance variables
no_wasi_model <- update(full_model, ~ . - wasi_mr_ts)
anova(full_model, no_wasi_model)

no_adhd_model <- update(full_model, ~ . - adhd_dx)
anova(full_model, no_adhd_model)

no_nuisance_model <- update(full_model, ~ . - wasi_mr_ts - adhd_dx)
anova(full_model, no_nuisance_model)

## model selection: non-significant predictors of interest
no_paradigm_model <- lmer(lapse_rate ~ wj_brs*continuum + (1|subject_id), data=psychometrics)
anova(no_nuisance_model, no_paradigm_model)

<<<<<<< HEAD
lmfit_no_wjbrs <- lmer(lapse_rate ~ contrast*type + (1|subject_id), data=psychometrics)
anova(lmfit_no_nuisance, lmfit_no_wjbrs)
## DON'T DO IT!  

# Remove interaction term?
lmfit_no_interaction <- lmer(lapse_rate ~ wj_brs + contrast + (1|subject_id), data=psychometrics)
anova(lmfit_no_type, lmfit_no_interaction) #OK- Therefore best model is lmfit_no_interaction


# Summary for the best model
coefs <- data.frame(coef(summary(lmfit_no_interaction)))
df.KR <- get_ddf_Lb(lmfit_no_interaction, fixef(lmfit_no_interaction))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)



library(ggplot2)
# Plot a 2x2 grid of correlations of reading score and slope for the 4 conditions
px <- ggplot(psychometrics, aes(wj_brs, lapse_rate)) +
    geom_point() +
    facet_wrap(contrast ~ type) + 
    xlab("WJ-BRS")+
    ylab("Psychometric Lapse")+
    #geom_text(data=labeldata, aes(x=125,y=0,label=eqn),fontface="italic")+
    geom_smooth(method='lm',formula=y~x, alpha = 0.25, color = "gray7")+
    theme_bw()+
    theme(text = element_text(size = 15)) 
px

# Compare lapse rate across groups
lapse_sum <- psychometrics %>% group_by(group, contrast,type) %>%
  summarise(mean_lapse = mean(lapse_rate))

# plot
ggplot(lapse_sum, aes(type, mean_lapse, fill = group)) +
  geom_bar(stat="identity", position= "dodge")+
  facet_wrap(~contrast)
=======
lmfit_no_wjbrs <- lmer(lapse_rate ~ continuum*paradigm + (1|subject_id), data=psychometrics)
anova(no_nuisance_model, lmfit_no_wjbrs)
## DON'T DO IT!  Therefore best model is no_paradigm_model

# Summary for final model
coefs <- data.frame(coef(summary(no_paradigm_model)))
df.KR <- get_ddf_Lb(no_paradigm_model, fixef(no_paradigm_model))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)
>>>>>>> 62da9893605e324cfcdf329f82cc025aff376d69
