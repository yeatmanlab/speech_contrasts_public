# Do a PCA analysis to see if lapse and slope together contribute anything

setwd("..")

# Load in the psychometrics
psychometrics <- read.csv("cleaned_psychometrics.csv")

# Filter so that we're only looking at the 
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
psychometrics <- plyr::rename(psychometrics, c("lo_asymp"="guess", "hi_asymp"="lapse"))

# DO PCA
params <- psychometrics[,5:7]
PCA<- prcomp(params, scale=TRUE)
psychometrics <- cbind(psychometrics, PCA$x)

summary(PCA)

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

# Now let's see what predicts the first PCA component
library(lme4)
lmfit <- lmer(PC1 ~ wj_brs*contrast*type + adhd_dx + wasi_mr_ts + (1|subject_id), psychometrics)
# Summary for full model
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)

# Remove nuisance variables
lmfit_no_wasi <- update(lmfit, ~ . - wasi_mr_ts)
anova(lmfit, lmfit_no_wasi)

lmfit_no_adhd <- update(lmfit, ~ . - adhd_dx)
anova(lmfit, lmfit_no_adhd)

lmfit_no_nuisance <- update(lmfit, ~ . - wasi_mr_ts - adhd_dx)
anova(lmfit, lmfit_no_nuisance) # OK to remove both nuisances!

coefs <- data.frame(coef(summary(lmfit_no_nuisance)))
df.KR <- get_ddf_Lb(lmfit_no_nuisance, fixef(lmfit_no_nuisance))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)


# To the main model
lmfit_no_contrast <- lmer(PC1 ~ wj_brs*type + (1|subject_id), data=psychometrics)
anova(lmfit_no_nuisance, lmfit_no_contrast) # can remove contrast

lmfit_no_type <- lmer(PC1 ~ wj_brs*contrast + (1|subject_id), data=psychometrics)
anova(lmfit_no_nuisance, lmfit_no_type) #can't remove type

# To the main model
lmfit_no_cont_interaction <- lmer(PC1 ~ wj_brs:type + wj_brs +  (1|subject_id), data=psychometrics)
anova(lmfit_no_contrast, lmfit_no_cont_interaction) # can remove 


# This is the winner
coefs <- data.frame(coef(summary(lmfit_no_cont_interaction)))
df.KR <- get_ddf_Lb(lmfit_no_cont_interaction, fixef(lmfit_no_cont_interaction))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
print(coefs)



