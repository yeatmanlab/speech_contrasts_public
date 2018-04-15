# Analyze reaction times
library(stringr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(DescTools)
library(lme4)
library(pbkrtest)
library(tidyr)
rm(list = ls())


# Load in cleaned data
df<- read.csv("cleaned_data.csv")
df$group <- factor(df$group,
                   levels=c("Dyslexic", "Below Average", "Above Average"))

df2 <- read.csv("./Analysis/Poly_fits_RT.csv")
df2 <- gather(df2, continuum, coef, X.ʃa...sa.:X.ba...da.)

df3 <- merge(df[c("subject_id", "wj_brs", "adhd_dx", "wasi_mr_ts")],df2, by = "subject_id")
df3 <- df3[!duplicated(df3), ]
# Winsorize RTs
#rt_wins <- Winsorize(df$RT, probs = c(0.01, 0.99)) # Use a conservative winsorization- only below 1% and above 99% are removed.
#df$RT <- rt_wins
#hist(df$RT)

# Truncate
##df <- subset(df, RT <= 3)
cutoff <- 2.5

## Center the wj scores
#mean_wj <- df %>% group_by(subject_id) %>%
# summarise(wj_brs = unique(wj_brs)) %>% 
#  summarise(avg = mean(wj_brs))


#df$wj_brs = df$wj_brs - mean_wj$avg
##### Get the mixed effects model for what Dan fit
lmfit1 <- lmer(coef ~ wj_brs*continuum + wasi_mr_ts + adhd_dx +  (1|subject_id), df3)
coefs <- data.frame(coef(summary(lmfit1)))
df.KR <- get_ddf_Lb(lmfit1, fixef(lmfit1))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs

lmfit2 <- lmer(coef ~ wj_brs + wasi_mr_ts + adhd_dx + (1|subject_id), df3)
anova(lmfit1, lmfit2) # OK to remove continuum

lmfit3 <- lmer(coef ~ wj_brs + adhd_dx + (1|subject_id), df3)
anova(lmfit1, lmfit3) # OK to remove nonverbal IQ

lmfit4 <- lmer(coef ~ wj_brs + (1|subject_id), df3)
anova(lmfit1, lmfit4) # OK to remove ADHD


lmfit <- lmer(coef ~ wj_brs + (1|subject_id), df3)
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs




###################################### What if we take the means? #####################################

RT_sum = df %>%
  subset(RT <= cutoff) %>%
  group_by(step, subject_id) %>%
  summarise(RT = mean(RT, na.rm = TRUE),
            wj_brs = unique(wj_brs))



# Model polynomial fit
lmfit <- lmer(RT ~ poly(step,2)*wj_brs + (1|subject_id), RT_sum)
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs


############### What if we took the medians? ####################################
RT_med = df %>%
  group_by(step, subject_id) %>%
  summarise(RT = median(RT, na.rm = TRUE),
            wj_brs = unique(wj_brs))

lmfit <- lmer(RT ~ poly(step,2)*wj_brs + (1|subject_id), RT_med)
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs


############### What if we used the raw data? ###################
df_trunc <- subset(df, RT <= cutoff)
lmfit <- lmer(RT ~ poly(step,2)*wj_brs + (1|subject_id), df_trunc)
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs


# Plot the RT as a function of level
group_cols = c("#5554AD","#7B4582","#A53551")
ggplot(df, aes(step, RT, colour = group)) +
  geom_point()+
  geom_jitter(width = 0.2)+
  #facet_wrap(~condition)+
  scale_color_manual(values=mypalette)+
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE))

# Plot the individual curves for each
ggplot(df, aes(step, RT, colour = wj_brs, group = subject_id)) + 
  geom_point()+
  facet_wrap(~condition)

# Plot the group data
group_RT <- df %>% group_by(step, group, condition) %>%
  summarise(RT = mean(RT))

ggplot(group_RT, aes(step, RT, group = group))+
  scale_colour_manual("Group",values=group_cols)+
  stat_smooth(method = "lm", se = TRUE, fill = NA,
              formula = y ~ poly(x,2,raw=TRUE),
              aes(colour = group))+ 
  theme_bw() +
  scale_x_continuous(breaks = seq(1,7))+
  facet_wrap(~condition)+
  ylim(0,1.1)+
  labs(x = "Step", y = "Response Time (s)")+
  theme(text = element_text(size = 18))+
  theme(legend.position = c(0.18, 0.17),
        legend.background = element_rect(fill = alpha("white",0.6), colour = NA))+
  labs(title = "Mean Response Time Within Group")+
  guides(fill=guide_legend(title="Group"))


# # Analyze reaction times
# ### This script gets the mean psychometrics. ###
# library(stringr)
# library(reshape2)
# library(dplyr)
# library(ggplot2)
# rm(list = ls())
# # Make subject list
# # Get the list of all files- in the future, make this not an absolute path
# file_list = list.files(path = "/home/eobrien/bde/Projects/Speech_contrasts/Results/Psychometrics/Raw")
# file_str = str_extract_all(file_list, "[A-Za-z0-9]+")
# subj_id <- character(length(file_list))
# for (i in 1:length(file_list)){
#   subj_id[i] <- file_str[[i]][4]
# }
# # Get the unique elements of this list of subject ids
# subj_id_list = unique(subj_id)
# 
# # For each of these subjects, open up their psychometrics and merge into a master data frame
# ba_da_pm <- data.frame()
# for (i in 1:length(subj_id_list)){
#   file_name = paste0("/home/eobrien/bde/Projects/Speech_contrasts/Results/Psychometrics/Raw/", "Psychometrics_Sa_Sha_", subj_id_list[i], ".csv")
#   df_tmp <- read.csv(file_name)
#   df_tmp$subject_id <- subj_id_list[i]
#   df_tmp$run_id <- paste0(df_tmp$subject_id, '_', df_tmp$sound2)
#   df_tmp <- mutate(df_tmp, Condition = ifelse(sound2 == "Single", "Single", "ABX"))
# 
# 
#   # Which continuum is it?
#   if(grepl("Ba", file_name)){
#     continuum = "Ba-Da"
#   } else {
#     continuum = "Sa-Sha"
#   }
#   df_tmp$continuum <- continuum
#   ba_da_pm <- rbind(ba_da_pm, df_tmp)
# }
# for (i in 1:length(subj_id_list)){
#   file_name = paste0("/home/eobrien/bde/Projects/Speech_contrasts/Results/Psychometrics/Raw/", "Psychometrics_Ba_Da_", subj_id_list[i], ".csv")
#   df_tmp <- read.csv(file_name)
#   df_tmp$subject_id <- subj_id_list[i]
#   df_tmp$run_id <- paste0(df_tmp$subject_id, '_', df_tmp$sound2)
#   df_tmp <- mutate(df_tmp, Condition = ifelse(sound2 == "Single", "Single", "ABX"))
#   
#   
#   # Which continuum is it?
#   if(grepl("Ba", file_name)){
#     continuum = "Ba-Da"
#   } else {
#     continuum = "Sa-Sha"
#   }
#   df_tmp$continuum <- continuum
#   ba_da_pm <- rbind(ba_da_pm, df_tmp)
# }
#   
#   
# 
# # Now we just need to decide if each subject is in the dyslexic group or not.
# ###### Load in the repository and registry
# filepath = '/home/eobrien/bde/Projects/Speech_contrasts/'
# repos_df <- read.csv( paste0(filepath, "RDRPRepository_DATA_2018-01-02_1209.csv"))
# registry_df<- read.csv( paste0(filepath, "RDRPRegistry_DATA_2018-01-02_1138.csv"))
# master_df <- merge(repos_df, registry_df, by = "record_id")
# # Just get the subjects we care about
# subj_record_id <- master_df %>% filter(sid.x %in% subj_id_list) %>% select(record_id)
# subject_df <- master_df %>% filter(record_id %in% subj_record_id$record_id)
# ###### Just get the scores we care about
# reading_df <- subject_df[c("record_id","dys_dx","adhd_dx","brain_injury","aud_dis","wj_brs","twre_index","wasi_fs2")]
# reading_df <- reading_df[!duplicated(reading_df),]
# # Remove duplicate rows...
# reading_df <- reading_df %>% group_by(record_id) %>% summarise_all(funs(mean(as.numeric(.), na.rm=TRUE)))
# # Make a biographical dataframe containing full subject ID and birthdate
# bio_df <- subject_df[c("sid.x","dob","record_id")]
# bio_df[bio_df==""] <- NA
# bio_df <- na.omit(bio_df)
# #### What day were the subjects tested on?
# timestamp = numeric(length(subj_id_list))
# for (i in 1:length(subj_id_list)){
#   con <- file(paste0(filepath, "Results/Raw/", subj_id_list[i], "_categorization_Ba_Da_1.txt"))
#   timestamp[i] <- readLines(con,n=1)
#   close(con)
# }
# timestamp_df = data.frame(psych_date = timestamp,
#                           sid.x = subj_id_list)
# vars <- colsplit(timestamp_df$psych_date, ",", c("date","time"))
# timestamp_df$psych_date <- vars$date
# # Convert timestamps to datestrings
# bio_df <- merge(bio_df, timestamp_df)
# bio_df$dob <- as.POSIXct(bio_df$dob, format="%Y-%m-%d")
# bio_df$psych_date <- as.POSIXct(bio_df$psych_date, format="%Y-%m-%d")
# bio_df$age_at_testing <- as.numeric(difftime(bio_df$psych_date,bio_df$dob,units="weeks")/52.25)
# colnames(bio_df)[1] <- "subject_id"
# ################ Bind together the reading results, bio, and psychometrics
# master_df <- merge(bio_df, ba_da_pm)
# master_df <- merge(master_df, reading_df)
# # Remove anyone tested before the experiment was finalized on 8-15
# use_df <- subset(master_df, psych_date >= "2017-08-15")
# # Remove any subjects less than 8 years old
# use_df <- subset(use_df, age_at_testing >= 8 & age_at_testing < 13)
# # Remove any kids who have an auditory disorder
# use_df <- subset(use_df, aud_dis == 0)
# # Remove subjects who did not pass the WASI criterion
# use_df <- subset(use_df, wasi_fs2 >= 70)
# # Remove subjects who did not pass hearing screening or are marked as having a "speech disorder"
# use_df <- subset(use_df, !(subject_id %in% c("IB701","KB630","IB706","IB263","HB656","HB213","IB319","HB742","GB727")))
# 
# # Classify as dyslexic versus control
# use_df <- mutate(use_df, is_dyslexic = ifelse( (wj_brs <= 85 & twre_index <= 85) | subject_id == "HB664","dyslexic",
#                                                ifelse((wj_brs >= 100 & twre_index >= 100) , "control", "in-between")))
# 
# # Analyze the response time data
# use_df2 =  use_df %>%
#   group_by(subject_id,step,continuum,Condition,is_dyslexic,wj_brs) %>%
#   summarise(RT = mean(RT, na.rm = TRUE))
# 
# # Winsorize the RTs
# rt_wins <- Winsorize(use_df2$RT)
# use_df2$RT <- rt_wins
# RT_sum = use_df2 %>%
#   group_by(step, subject_id) %>%
#   summarise(RT = mean(RT, na.rm = TRUE),
#             wj_brs = unique(wj_brs))
# 
# # Model the difference in polynomial terms as a function of reading score
# lmfit <- lmer(RT ~ poly(step,2)*wj_brs + (1|subject_id), RT_sum)
# coefs <- data.frame(coef(summary(lmfit)))
# df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
# coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
# coefs
# 
# 
# 
# lmfit <- lmer(RT ~ poly(step,2)*wj_brs*Condition*continuum + (1|subject_id), blocks)
# coefs <- data.frame(coef(summary(lmfit)))
# df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
# coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
# coefs
# 
# 

