## Read in the megarepository and merge with our other data about subjects.
library(stringr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(tidyr)
library(plyr)
library(broom)
rm(list = ls())


###### Load in the repository and registry
setwd("../..")

df <- read.csv("cleaned_data.csv")
group_levels <- c("Dyslexic", "Below Average", "Above Average", "Group Means")
df$group <- factor(df$group, levels=group_levels)


###################################################################################
##### Plot demographic information ##########################3
##################################################################################
detach("package:plyr", unload=TRUE) 
# Aggregate summary statistics
sum_bio <- df %>% group_by(subject_id) %>% summarise(wj_brs = unique(wj_brs),
                                                    wj_lwid = unique(wj_lwid_ss),
                                                    wj_wa = unique(wj_wa_ss),
                                                    twre_index = unique(twre_index),
                                                    twre_swe = unique(twre_swe_ss),
                                                    twre_pde = unique(twre_pde_ss),
                                                    wasi_mr_ts = unique(wasi_mr_ts),
                                                    wasi_fs2 = unique(wasi_fs2),
                                                    ctopp_pa = unique(ctopp_pa),
                                                    ctopp_pm = unique(ctopp_pm),
                                                    ctopp_rapid = unique(ctopp_rapid),
                                                    age_at_testing = unique(mean(age_at_testing)),
                                                    #gender = unique(gender),
                                                    is_dyslexic = unique(group))


# Mean values across groups
aggregate(. ~ is_dyslexic, sum_bio, mean)
# Standard deviations in each group
aggregate(. ~ is_dyslexic, sum_bio, sd)

# First get into longform, then plot
sum_bio_long <- gather(sum_bio, test, score, wj_brs:ctopp_rapid, factor_key = TRUE)

mypalette <- c(colorRampPalette(c("royalblue3","firebrick3"))(3))
px <- ggplot(sum_bio_long, aes(test, score, color = is_dyslexic)) + 
  geom_boxplot(outlier.colour = NA)+
  scale_colour_manual("Group", values=mypalette)+
  scale_y_continuous(breaks = c(40,55,70,85,100,115,130,145),minor_breaks=NULL)+
  geom_quasirandom(dodge.width = .75) +
  xlab("") +
  ylab("Standardized Score")+
  scale_x_discrete(labels=c("WJ-BRS","WJ\n Real Word","WJ\n Pseudoword",
                            "TOWRE\n Index","TOWRE\n Real Word", "TOWRE\n Pseudoword",
                            "WASI\n Non-Verbal", "WASI\n FS2", "CTOPP\n Phonological\n Awareness",
                            "CTOPP\n Phonological\n Memory", "CTOPP\n Rapid Symbol\n Naming"))+
  theme(text = element_text(size = 12))

px
# Save this figure
w = 12
h = 5
ggsave("./Analysis/Figures/s1_demographics.eps", px, device = cairo_ps, width = w, height = h)
ggsave("./Analysis/Figures/s1_demographics.png", px,  width = w, height = h)
ggsave("./Analysis/Figures/s1_demographics.tiff", px,  width = w, height = h, dpi = 300)

