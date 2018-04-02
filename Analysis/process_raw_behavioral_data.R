library(stringr)
library(reshape2)
library(dplyr)
library(ggplot2)
rm(list = ls())


# set working directory
setwd('../Results/')

# subject_id
id <- "JB783"

# Load in Ba-Da categorization
df1 <- read.csv(paste0("./Raw/", id, "_categorization_Ba_Da_1.txt"), skip = 1)
df1$stimulus <- gsub(".wav","", df1$stimulus)
audio_cols <- colsplit(df1$stimulus, "_", names=c("junk","junk","junk","step"))
df1$step <- audio_cols$step
df1 <-mutate(df1, selection_code = ifelse(selection == "Ba", 0, 1), 
             run = 1)

# Load in my second trial
df2 <- read.csv(paste0("./Raw/", id, "_categorization_Ba_Da_2.txt"),skip = 1)
df2$stimulus <- gsub(".wav","", df2$stimulus)
audio_cols <- colsplit(df2$stimulus, "_", names=c("junk","junk","junk","step"))
df2$step <- audio_cols$step
df2 <-mutate(df2, selection_code = ifelse(selection == "Ba", 0, 1),
             run = 1)

# Load in the third trial, which is the single interval
df3 <- read.csv(paste0("./Raw/", id, "_categorization_Ba_Da_3.txt"),skip = 1)
df3$stimulus <- gsub(".wav","", df3$stimulus)
df3$sound2 <- "Single"
audio_cols <- colsplit(df3$stimulus, "_", names=c("junk","junk","junk","step"))
df3$step <- audio_cols$step
df3 <-mutate(df3, selection_code = ifelse(selection == "Ba", 0, 1),
             run = 1)

# Just make sure there aren't any other files.
is_more = file.exists(paste0("./Raw/", id, "_categorization_Ba_Da_4.txt"))
if (is_more) {
  print("There may be more than 3 categorization trials.")
}
  

# Bind them together
sum_ba_da <- rbind(df1, df2,df3)

# Process percents scored
df_sum <- sum_ba_da %>%
  group_by(step, sound2) %>%
  summarise(response = mean(selection_code, na.rm = TRUE),
            RT = mean(RT,na.rm=TRUE))

px1 <- ggplot(df_sum, aes(step, response, colour = sound2))+
  geom_point(size = 3)+
  geom_line()+
  scale_x_continuous(breaks = seq(1,7))+
  scale_y_continuous(labels = scales::percent)+
  labs(title = paste0("Categorization, subject ", id), subtitle = "Ba-Da")+
  labs(x = "Step", y = "Proportion Answered 'Da'")+
  scale_color_discrete(name = "Last sound heard",
                       breaks = c("Ba","Da","Single"))+
  #theme(axis.text = element_text(size = 12),
  #     axis.title = element_text(size = 14))
  theme(text = element_text(size = 18))

px1


#ggsave(paste0("Ba_Da_Cat_Summary_", id, ".png"), px1)
write.csv(df_sum, file = paste0("./Psychometrics/Raw/","Psychometrics_Ba_Da_", id, ".csv"))
#### Print the psychometrics to a .txt file so they can be read into MATLAB and fit with psychometrics. 

######################## SA-SHA categorization #########################################

# Load in Sha-Sa categorization
df1 <- read.csv(paste0("./Raw/", id, "_categorization_Sa_Sha_1.txt"), skip = 1)
df1$stimulus <- gsub(".wav","", df1$stimulus)
audio_cols <- colsplit(df1$stimulus, "_", names=c("junk","junk","junk","step"))
df1$step <- audio_cols$step
df1 <-mutate(df1, selection_code = ifelse(selection == "Sha", 0, 1), 
             run = 1)



# Load in my second sa-sha trial
df2 <- read.csv(paste0("./Raw/", id, "_categorization_Sa_Sha_2.txt"),skip = 1)
df2$stimulus <- gsub(".wav","", df2$stimulus)
audio_cols <- colsplit(df2$stimulus, "_", names=c("junk","junk","junk","step"))
df2$step <- audio_cols$step
df2 <-mutate(df2, selection_code = ifelse(selection == "Sha", 0, 1),
             run = 1)

# Load in my third sa-sha trial
df3 <- read.csv(paste0("./Raw/", id, "_categorization_Sa_Sha_3.txt"),skip = 1)
df3$stimulus <- gsub(".wav","", df3$stimulus)
df3$sound2 <- "Single"
audio_cols <- colsplit(df3$stimulus, "_", names=c("junk","junk","junk","step"))
df3$step <- audio_cols$step
df3 <-mutate(df3, selection_code = ifelse(selection == "Sha", 0, 1),
             run = 1)

# Just make sure there aren't any other files.
is_more = file.exists(paste0("./Raw/", id, "_categorization_Sa_Sha_4.txt"))
if (is_more) {
  print("There may be more than 3 categorization trials.")
}


# Bind them together
sum_Sha_Sa <- rbind(df1, df2,df3)

# Process percents scored
df_sum <- sum_Sha_Sa %>%
  group_by(step, sound2) %>%
  summarise(response = mean(selection_code, na.rm=TRUE),
            RT = mean(RT, na.rm= TRUE))

px2 <- ggplot(df_sum, aes(step, response, colour = sound2))+
  geom_point(size = 3)+
  geom_line()+
  scale_x_continuous(breaks = seq(1,7))+
  scale_y_continuous(labels = scales::percent)+
  labs(title = paste0("Categorization, subject ", id), subtitle = "Sa-Sha")+
  labs(x = "Step", y = "Proportion Answered 'Sa'")+
  scale_color_discrete(name = "Last sound heard",
                       breaks = c("Sha","Sa","Single"))+
  #theme(axis.text = element_text(size = 12),
  #     axis.title = element_text(size = 14))
  theme(text = element_text(size = 18))

px2


#ggsave(paste0("./Images/","Sa_Sha_Cat_Summary_", id, ".png"), px2)
write.csv(df_sum, file = paste0("./Psychometrics/Raw/", "Psychometrics_Sa_Sha_", id, ".csv"))


