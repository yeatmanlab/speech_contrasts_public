# Read all the cross validation data into one .csv table so it doesn't have to be done again
library(tidyr)
library(stringr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

rm(list=ls())

###### Load in the repository and registry
psychometrics <- read.csv("cleaned_psychometrics.csv")
group_levels <- c("Dyslexic", "Below Average", "Above Average", "Group Means")
psychometrics$group <- factor(psychometrics$group, levels=group_levels)


#### Now read in the psychometrics
filepath = './Results/Psychometrics/Fit'
filelist = list.files(path = filepath,pattern = ".*.csv")
full_file_list <- paste0(filepath, '/',filelist)
pm <- data.frame()
#assuming tab separated values with a header    
for (i in 1:length(full_file_list)){
  data<- read.csv(paste0(full_file_list[i]))
  pm <- rbind(pm, data)
}

colnames(pm)[1] <- "subject_id"
# Filter the cross validation trials so we are only including the subjects in the experiment
wj_df <- psychometrics %>% select(c("wj_brs","subject_id")) %>% unique()
use_df <- pm %>% filter(subject_id %in% wj_df$subject_id) %>% merge(.,wj_df)

# Get just a subset of the psychometric functions- only ABX 
sub_df <- subset(use_df, sound2 != "Single")
sub_df <- mutate(sub_df, continuum = ifelse(sound2 %in% c("Sa","Sha"), "/ʃa/~/sa/", "/ba/~/da/"))
sub_df$id <- paste0(sub_df$subject_id, "_", sub_df$sound2)#

# Summarise all the slope and lap estimates per subject per prior width. Also include WJ-BRS.
ABX_sum_big <- sub_df %>% group_by(subject_id,width,continuum) %>% summarise(mean_slope = mean(slope),
                                                                        mean_end = mean((lapse+guess)/2),
                                                                        wj_brs = unique(wj_brs),
                                                                        mean_residuals = mean(residuals),
                                                                        mean_deviance = mean(deviance))

# Define a function that assess the correlation between slope & lapse parameters and WJ score
bootstrap_correlation <- function(ABX_sum){
  r2slope <- vector("numeric", 10L)
  r2end <- vector("numeric", 10L)
  residuals <- vector("numeric", 10L)
  deviance <- vector("numeric", 10L)
  for (i in 1:length(unique(ABX_sum$width))){
    this_width = unique(ABX_sum$width)[i]
    lmfit <- lm(mean_slope ~ wj_brs, subset(ABX_sum, width == this_width))
    r2slope[i] <- sqrt(summary(lmfit)$r.squared)
    
    # Same thing for the endpoints
    lmfit <- lm(mean_end ~ wj_brs, subset(ABX_sum, width == this_width))
    r2end[i] <- sqrt(summary(lmfit)$r.squared)
    
    # Get the summed deviance
    sub <- subset(ABX_sum, width == this_width)
    deviance[i] <- sum(sub$mean_deviance)
    residuals[i] <- sum(sub$mean_residuals)
    
  }
  
  ##### Here be bootstraps ####
  nsim <- 10000 # Number of bootstrap simulations to run
  rslope_sim <- vector("numeric", 1000L)
  pslope_sim <- vector("numeric", 1000L)
  rend_sim <- vector("numeric", 1000L)
  pend_sim <- vector("numeric", 1000L)
  mu_slope <- vector("numeric", 10L)
  mu_p_slope <- vector("numeric", 10L)
  mu_p_end <- vector("numeric", 10L)
  mu_end <- vector("numeric", 10L)
  se_slope <- vector("numeric", 10L)
  se_end <- vector("numeric", 10L)
  
  for (h in 1:length(unique(ABX_sum$width))){
    sub <- subset(ABX_sum, width == unique(ABX_sum$width)[h])
    for (i in 1:nsim){
      sampler <- sub[sample(nrow(sub), replace = TRUE), ]
      lmfit <- lm(mean_slope ~ wj_brs, sampler)
      rslope_sim[i] <- sqrt(summary(lmfit)$r.squared)
      pslope_sim[i] <- anova(lmfit)$'Pr(>F)'[1]
      
      # Same thing for the endpoints
      lmfit <- lm(mean_end ~ wj_brs, sampler)
      rend_sim[i] <- sqrt(summary(lmfit)$r.squared)
      pend_sim[i] <- anova(lmfit)$'Pr(>F)'[1]
      
    }
    mu_slope[h] <- mean(rslope_sim)
    mu_end[h] <- mean(rend_sim)
    mu_p_slope[h] <- mean(pslope_sim)
    mu_p_end[h] <- mean(pend_sim)
    se_slope[h] <- sd(rslope_sim)
    se_end[h] <- sd(rend_sim)
    
  }
  mu_end[1] <- 0
  se_end[1] <- 0
  mu_p_end[1] <-1
  dfslope <- data.frame(Correlation = mu_slope, SE = se_slope, p = mu_p_slope, Type = "Slope", Width =  unique(ABX_sum$width))
  dfasym <- data.frame(Correlation = mu_end, SE = se_end, p = mu_p_end,Type = "Asymptote", Width = unique(ABX_sum$width))
  df <- rbind(dfslope, dfasym)
  df$is_significant = df$p < 0.05 
  
  return(df)}

df1 <- bootstrap_correlation(subset(ABX_sum_big, continuum == "/ba/~/da/"))
df2 <- bootstrap_correlation(subset(ABX_sum_big, continuum == "/ʃa/~/sa/"))

px1 <- ggplot(df1, aes(Width, Correlation, group = Type, colour = Type)) +
  geom_point(size=4, aes(shape = is_significant))+
  scale_shape_manual(values = c(1,16))+
  geom_smooth(se = FALSE)+
  geom_ribbon(aes(ymin=Correlation - SE, ymax= Correlation + SE, fill = Type),alpha = 0.2,size = 0.1)+
  scale_fill_manual("Parameter",values=c("gray10","darkseagreen"))+
  theme_bw()+
  scale_color_manual("Parameter",values=c("gray7","darkseagreen"))+
  xlab("Asymptotic Prior Width")+
  ylab("Correlation with WJ-BRS")+
  ggtitle("/ba/~/da/")+
  theme(legend.position = c(0.8,0.1),
        legend.background = element_rect(fill = alpha("white",0.4), colour = NA),
        text = element_text(size=24),
        plot.margin = unit(c(1,1,1,2),"lines"))+
  guides(fill=guide_legend(title="Parameter"),
         shape = FALSE)
px1


px2 <- ggplot(df2, aes(Width, Correlation, group = Type, colour = Type)) +
  geom_point(size=4, aes(shape = is_significant))+
  scale_shape_manual(values = c(1,16))+
  geom_smooth(se = FALSE)+
  geom_ribbon(aes(ymin=Correlation - SE, ymax= Correlation + SE, fill = Type),alpha = 0.2,size = 0.1)+
  scale_fill_manual("Parameter",values=c("gray10","darkseagreen"))+
  theme_bw()+
  scale_color_manual("Parameter",values=c("gray7","darkseagreen"))+
  xlab("Asymptotic Prior Width")+
  ylab("Correlation with WJ-BRS")+
  ggtitle('/ʃa/~/sa/')+
  theme(legend.position = c(0.8,0.1),
        legend.background = element_rect(fill = alpha("white",0.4), colour = NA),
        text = element_text(size=24),
        plot.margin = unit(c(1,1,1,2),"lines"))+
  guides(fill=guide_legend(title="Parameter"),
         shape = FALSE)
px2


# Arrange the plots on one grid
myplot1 <- arrangeGrob(px1, top = textGrob("A", x = unit(0, "npc")
                                           , y = unit(0.5, "npc"), just=c("left","top"),
                                           gp=gpar(col="black", fontsize=54)))

myplot2 <- arrangeGrob(px2, top = textGrob("B", x = unit(0, "npc")
                                           , y = unit(0.5, "npc"), just=c("left","top"),
                                           gp=gpar(col="black", fontsize=54)))


px <- grid.arrange(myplot1, myplot2, nrow = 1)
setwd("./Analysis/Figures")
ggsave("figure_correlation_wrt_prior", px, device = cairo_ps, width = 15, height = 10)

ggsave("figure_correlation_wrt_prior.png", px, width = 15, height = 10)
ggsave("figure_correlation_wrt_prior.pdf", px, width = 15, height = 10)                   
                   
                   ("./Images/","Figure_7.tiff"), px,  width = 15, height = 10, dpi = 300)
