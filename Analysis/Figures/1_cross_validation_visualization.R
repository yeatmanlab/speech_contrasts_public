library(stringr)
library(reshape2)
library(dplyr)
library(ggplot2)


###### Load in the repository and registry
setwd("../..")
#filepath = "P:\\bde\\Projects\\Speech_contrasts\\"
psychometrics <- read.csv("cleaned_psychometrics.csv")
group_levels <- c("Dyslexic", "Below Average", "Above Average", "Group Means")
psychometrics$group <- factor(psychometrics$group, levels=group_levels)


#### Now read in the psychometrics
filepath = './Results/CV_Leave_Out'
filelist = list.files(path = filepath,pattern = ".*.csv")
full_file_list <- paste0(filepath, '/',filelist)
pm <- data.frame()
#assuming tab separated values with a header    
for (i in 1:length(full_file_list)){
  data<- read.csv(paste0(full_file_list[i]))
  if (grepl("Ba_Da", full_file_list[i])){
    data$continuum = "Ba-Da"
  }
  else{
    data$continuum = "Sa-Sha" 
    data$p <- 1 - data$p}
  pm <- rbind(pm, data)}

colnames(pm)[1] <- "subject_id"

# Filter the cross validation data to only include subjects in our study
subj_list <- unique(psychometrics$subject_id)

use_df <- pm %>% filter(subject_id %in% subj_list)



#use_df <- subset(use_df, !(sid.x %in% excl_df$sid.x & continuum == "Sa-Sha"))
# Which subjects do we have incomplete data for?


# Get the average deviance for each level
detach("package:plyr", unload=TRUE) 


summary <- use_df %>% group_by(width) %>% summarise(avg_l = median(p, na.rm = TRUE))
px <- ggplot(summary, aes(width, avg_l)) +
  geom_point(size = 4) + 
  geom_line(size = 2)+
  xlab("Asymptotic Prior Width") +
  ylab("Median Prediction Likelihood")+
  ggtitle("")+
  theme_bw() + 
  theme(text = element_text(size=24),
        plot.margin = unit(c(1,1,1,2),"lines"))
px

## SAVE TO FILE
setwd('./Analysis/Figures')
ggsave("figure_cross_validation.pdf", px,
       device=cairo_pdf, width=7, height=7)
ggsave("figure_cross_validation.png", px,
       width=7, height=7, dpi = 300)
