### This script gets the mean psychometrics. ###
library(stringr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
rm(list = ls())


# Load in the biographical data
setwd("../..")
clean_data <- read.csv("cleaned_psychometrics.csv")
clean_data$group <- factor(clean_data$group,
                           levels=c("Dyslexic", "Below Average", "Above Average"))


use_df <- clean_data %>% filter(threshold >= 1 & threshold <= 7)
use_df$continuum 

## ## ## ## ## ## ##
## COLORMAP STUFF ##
## ## ## ## ## ## ##
n_steps <- 256
cmap <- colorRampPalette(c("royalblue3", "firebrick3"))(n_steps)

## FUNCTION FOR COMPUTING COLORS FROM COLORMAPS (FOR GROUP-LEVEL COLORS)
get_cmap_color <- function(x, cmap) {
  ramp <- grDevices::colorRamp(cmap, space="Lab", interpolate="spline")
  rgbs <- ramp(x) / 255
  grDevices::rgb(rgbs[,1], rgbs[,2], rgbs[,3])
}

# Get colors for group endpoints
endpoints <- range(na.omit(clean_data$wj_brs))
c_range <- diff(endpoints)
group_vals <- clean_data %>% group_by(group) %>% summarise(wj_brs=mean(wj_brs))
group_vals <- (group_vals$wj_brs - endpoints[1]) / c_range
group_cols <- get_cmap_color(group_vals, cmap)
names(group_cols) <- levels(c("Dyslexic", "Below Average",
                       "Above Average"))




########################### Inset bar graph #################################################################

#### Summarize the ABX results- get the mean of each.
ABX_sum <- use_df %>% 
  group_by(paradigm, continuum, subject_id) %>% 
  summarise(mean_slope = mean(slope),
                         mean_thresh = mean(threshold),                                          
                         mean_lapse = mean(hi_asymp), 
                         mean_guess = mean(lo_asymp),
                         mean_end = mean((hi_asymp + lo_asymp)/2),
                         group = unique(group))

# Get the average parameters in each group
detach("package:plyr", unload=TRUE) 
summary_df <- ABX_sum %>% 
  group_by(group, continuum, paradigm) %>% summarise(group_mean_slope = mean(mean_slope),
                                                            slope_se = sd(mean_slope)/sqrt(n()),
                                                            group_mean_end = mean(mean_end),
                                                            end_se = sd(mean_end)/sqrt(n()))


## set the levels in order we want
pd <- position_dodge(.9) # move them .05 to the left and right
soft_palette <- c(colorRampPalette(c("royalblue1","firebrick1"))(3))
bar_palette <- c("#3A5FCD","#4876FF","#834279","#A35397","#CD2626","#FF3030")  
summary_df$id <- c(5,6,3,4,1,2)
summary_df<- within(summary_df, 
                    group <- factor(group,
                                          levels=c("Dyslexic","Below Average", "Above Average")))
# Slope by group- Ba & Da
px1 <- ggplot(subset(summary_df, continuum == "/ba/-/da/"), aes(paradigm,group_mean_slope, fill=group))+
  geom_bar(stat= "identity", position = "dodge", aes(colour = group, fill = group),size=1)+
  scale_fill_manual("Group     ",values = group_cols, 
                    labels = c("Dyslexic    ",
                               "Below Average     ",
                               "Above Average     "))+
  scale_y_continuous(limits = c(0,3.2))+
  scale_color_manual("Group     ",values = group_cols,
                     labels = c("Dyslexic    ",
                                "Below Average     ",
                                "Above Average     "))+
  geom_errorbar(aes(ymin=group_mean_slope-slope_se, ymax=group_mean_slope+slope_se), width=.1, position = pd,size=0.75)+
  xlab("Paradigm")+
  ylab("Average Psychometric Slope")+
  labs(title = "Slope by Group\n /ba/~/da/")+
  theme_bw()+
  theme(legend.text = element_text(size = 32),
        text = element_text(size=28),
        plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
        plot.margin = unit(c(1,1,1,2),"lines"))+
  guides(fill = guide_legend(        key.width = 0.1,
                                     keyheight = 0.1))

px1

# Lapse by group- Ba & Da
px2 <- ggplot(subset(summary_df, continuum == "/ba/-/da/"), aes(paradigm,group_mean_end, fill=group))+
  geom_bar(stat= "identity", position = "dodge", aes(colour = group, fill = group),size=1)+
  scale_fill_manual("Group",values = group_cols)+
  scale_y_continuous(labels = scales::percent, limits = c(0,0.07)) +
  scale_color_manual("Group",values = group_cols)+
  geom_errorbar(aes(ymin=group_mean_end-end_se, ymax=group_mean_end+end_se), width=.1, position = pd,size=0.75)+
  xlab("Paradigm")+
  ylab("Average Lapse Rate")+
  labs(title = "Lapse Rate by Group\n /ba/~/da/")+
  theme_bw()+
  theme(#legend.position = "none",
        #legend.background = element_rect(fill = alpha("white",0.4), colour = NA),
        #legend.title = element_blank(),
        text = element_text(size=28),
        plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
        plot.margin = unit(c(1,1,1,2),"lines"))

px2




# Slope by group- Sa & Sha
px3 <- ggplot(subset(summary_df, continuum == "/ʃa/-/sa/"), aes(paradigm,group_mean_slope, fill=group))+
  geom_bar(stat= "identity", position = "dodge", aes(colour = group, fill = group),size=1)+
  scale_fill_manual("Group",values = group_cols)+
  scale_y_continuous(limits = c(0,3.2))+
  scale_color_manual("Group",values = group_cols)+
  geom_errorbar(aes(ymin=group_mean_slope-slope_se, ymax=group_mean_slope+slope_se), width=.1, position = pd,size=0.75)+
  xlab("Paradigm")+
  ylab("Average Psychometric Slope")+
  labs(title = "Slope by Group\n /ʃa/~/sa/")+
  theme_bw()+
  theme(legend.position = "none",
        legend.background = element_rect(fill = alpha("white",0.4), colour = NA),
        legend.title = element_blank(),
        text = element_text(size=28),
        plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
        plot.margin = unit(c(1,1,1,2),"lines"))
px3


# Lapse by group- Sa & Sha
px4 <- ggplot(subset(summary_df, continuum == "/ʃa/-/sa/"), aes(paradigm,group_mean_end, fill=group))+
  geom_bar(stat= "identity", position = "dodge", aes(colour = group, fill = group),size=1)+
  scale_fill_manual("Group",values = group_cols)+
  scale_y_continuous(labels = scales::percent, limits = c(0,0.07)) +
  scale_color_manual("Group",values = group_cols)+
  geom_errorbar(aes(ymin=group_mean_end-end_se, ymax=group_mean_end+end_se), width=.1, position = pd,size=0.75)+
  xlab("Paradigm")+
  ylab("Average Lapse Rate")+
  labs(title = "Lapse Rate by Group\n /ʃa/~/sa/")+
  theme_bw()+
  theme(legend.position = "none",
        legend.background = element_rect(fill = alpha("white",0.4), colour = NA),
        legend.title = element_blank(),
        text = element_text(size=28),
        plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
        plot.margin = unit(c(1,1,1,2),"lines"))

px4



library(lemon)
px <- grid_arrange_shared_legend(px1,px2,px3,px4, ncol = 2, nrow = 2, position='top' )

px
setwd("./Analysis/Figures")
ggsave("figure_slope_lapse_by_group.eps", px, device = cairo_ps, width = 20, height = 20)
ggsave("figure_slope_lapse_by_group.png", px,  width = 19, height = 18)
#ggsave(paste0("./Images/","Figure_4.tiff"), px,  width = 19, height = 18, dpi = 300)

