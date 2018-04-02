#!/usr/bin/env Rscript

## figure_individ_and_mean_fitted_models_by_group.R
## aggregates the data by group and cue type and plots model fits

library(lme4)
library(dplyr)
library(pbkrtest)

## When loading/saving, this script assumes the working directory is set to the
## root directory of the repo. Relative to this script's location that is:
#setwd("..")

clean_data <- read.csv("cleaned_data.csv")
clean_data$group <- factor(clean_data$group,
                           levels=c("Dyslexic", "Below Average", "Above Average"))
## ## ## ## ## ## ## ## ## ## ##
## LOAD PSYCHOMETRIC FIT DATA ##
## ## ## ## ## ## ## ## ## ## ##
fpath <- file.path("Results", "Psychometrics", "Fit_Flat_0")
flist <- file.path(fpath, list.files(fpath))
psychometrics <- do.call(rbind, lapply(flist, read.csv))
## make column name consistent
colnames(psychometrics)[colnames(psychometrics) == "SubjectID"] <- "subject_id"
psychometrics$contrast <- with(psychometrics,
                               ifelse(sound2 %in% c("Ba", "Da"), "/ba/-/da/",
                                      ifelse(sound2 %in% c("Sa", "Sha"),
                                             "/ʃa/-/sa/", NA)))
## na.locf is "last observation carry forward". This works because we know the
## rows of the psychometrics dataframe are loaded in groups of 3, where all 3
## rows of the CSV file are the same contrast, and "single" is the last row.
psychometrics$contrast <- zoo::na.locf(psychometrics$contrast)
## add group and reading ability to psychometrics dataframe
group_table <- unique(clean_data[c("subject_id", "group", "wj_brs", "adhd_dx", "wasi_mr_ts")])
psychometrics <- merge(psychometrics, group_table, all.x=TRUE, all.y=FALSE)
## omit subjects missing from clean_data (must have been excluded earlier)
psychometrics <- na.omit(psychometrics)
## add "type" column
psychometrics$type <- ifelse(psychometrics$sound2 == "Single", "Single", "ABX")
## rename asymptote columns
psychometrics <- rename(psychometrics, lo_asymp=guess, hi_asymp=lapse)

## Deviation contrasts
psychometrics$type <- factor(psychometrics$type, levels=c("Single", "ABX"))
psychometrics$contrast <- factor(psychometrics$contrast,
                                 levels=c("/ba/-/da/", "/ʃa/-/sa/"))
contrasts(psychometrics$type) <- matrix(c(-0.5, 0.5), nrow=2, 
                                        dimnames=list(levels(psychometrics$type), "ABX"))
contrasts(psychometrics$contrast) <- matrix(c(-0.5, 0.5), nrow=2, 
                                            dimnames=list(levels(psychometrics$contrast), "/ʃa/-/sa/"))
## center reading score, etc
psychometrics$adhd_dx <- as.logical(psychometrics$adhd_dx)
psychometrics$wj_brs <- scale(psychometrics$wj_brs, scale=FALSE)
psychometrics$wasi_mr_ts <- scale(psychometrics$wasi_mr_ts, scale=FALSE)

## model
lmfit <- lmer(slope ~ wj_brs*type*contrast + adhd_dx + wasi_mr_ts + (1|subject_id),
              data=psychometrics)
# Summary for full model
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)

## model selection: nuisance variables
lmfit_no_wasi <- update(lmfit, ~ . - wasi_mr_ts)
anova(lmfit, lmfit_no_wasi)

lmfit_no_adhd <- update(lmfit, ~ . - adhd_dx)
anova(lmfit, lmfit_no_adhd)

lmfit_no_nuisance <- update(lmfit, ~ . - wasi_mr_ts - adhd_dx)
anova(lmfit, lmfit_no_nuisance)

## model selection: non-significant predictors of interest
lmfit_no_contrast <- lmer(slope ~ wj_brs*type + (1|subject_id), data=psychometrics)
anova(lmfit_no_nuisance, lmfit_no_contrast)

lmfit_no_type <- lmer(slope ~ wj_brs*contrast + (1|subject_id), data=psychometrics)
anova(lmfit_no_nuisance, lmfit_no_type)

lmfit_no_contrast_or_type <- lmer(slope ~ wj_brs + (1|subject_id), data=psychometrics)
anova(lmfit_no_nuisance, lmfit_no_contrast_or_type)

# Remove interaction term?
lmfit_no_interaction <- lmer(slope ~ wj_brs + contrast + (1|subject_id), data=psychometrics)
anova(lmfit_no_type, lmfit_no_interaction)

coefs <- data.frame(coef(summary(lmfit_no_interaction)))
df.KR <- get_ddf_Lb(lmfit_no_interaction, fixef(lmfit_no_interaction))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)

