#!/usr/bin/env Rscript

## Pearson pairwise correlations
## Linear modeling of slope and lapse predicted by reading ability, continuum, etc.

## When loading/saving, this script assumes the working directory is set to the
## root directory of the repo. Relative to this script's location that is:
#setwd("..")
library(dplyr)
library(ggplot2)
library(reshape2)

psychometrics <- read.csv("../cleaned_psychometrics.csv")
bio <- read.csv("../cleaned_data.csv")


## add group and reading ability to psychometrics dataframe
columns <- c("subject_id", "wj_lwid_ss", "wj_wa_ss", "twre_pde_ss", "twre_swe_ss",
             "ctopp_pa", "ctopp_pm", "ctopp_rapid")
group_table <- unique(bio[columns])
use <- merge(group_table, psychometrics, all.x=TRUE, all.y=FALSE)

df_sum <- sum
get_cors_mean <- function(dsub){
  # dsub <- df_sum

  wj_wa_ss <-   cor.test(dsub$mean_slope, dsub$wj_wa_ss)
  wj_wa_ss.r <- wj_wa_ss$estimate
  wj_wa_ss.p <- wj_wa_ss$p.value

  wj_lwid_ss <-   cor.test(dsub$mean_slope, dsub$wj_lwid_ss)
  wj_lwid_ss.r <- wj_lwid_ss$estimate
  wj_lwid_ss.p <- wj_lwid_ss$p.value

  twre_swe <-   cor.test(dsub$mean_slope, dsub$twre_swe)
  twre_swe.r <- twre_swe$estimate
  twre_swe.p <- twre_swe$p.value
  
  twre_pde <-   cor.test(dsub$mean_slope, dsub$twre_pde)
  twre_pde.r <- twre_pde$estimate
  twre_pde.p <- twre_pde$p.value
  
  ctopp_pa <-   cor.test(dsub$mean_slope, dsub$ctopp_pa)
  ctopp_pa.r <- ctopp_pa$estimate
  ctopp_pa.p <- ctopp_pa$p.value
  
  ctopp_pm <-   cor.test(dsub$mean_slope, dsub$ctopp_pm)
  ctopp_pm.r <- ctopp_pm$estimate
  ctopp_pm.p <- ctopp_pm$p.value
  
  ctopp_rapid <-   cor.test(dsub$mean_slope, dsub$ctopp_rapid)
  ctopp_rapid.r <- ctopp_rapid$estimate
  ctopp_rapid.p <- ctopp_rapid$p.value
  
  output <- data.frame(wj_wa_ss.r, wj_wa_ss.p,
                       wj_lwid_ss.r, wj_lwid_ss.p,
                       twre_swe.r, twre_swe.p,
                       twre_pde.r, twre_pde.p,
                       ctopp_pa.r, ctopp_pa.p,
                       ctopp_pm.r, ctopp_pm.p,
                       ctopp_rapid.r, ctopp_rapid.p,
                       num_obs = nrow(dsub))
  
  return(output)
}

get_cors_lapse <- function(dsub){
  # dsub <- df_sum
  
  wj_wa_ss <-   cor.test(dsub$mean_lapse, dsub$wj_wa_ss)
  wj_wa_ss.r <- wj_wa_ss$estimate
  wj_wa_ss.p <- wj_wa_ss$p.value
  
  wj_lwid_ss <-   cor.test(dsub$mean_lapse, dsub$wj_lwid_ss)
  wj_lwid_ss.r <- wj_lwid_ss$estimate
  wj_lwid_ss.p <- wj_lwid_ss$p.value
  
  twre_swe <-   cor.test(dsub$mean_lapse, dsub$twre_swe)
  twre_swe.r <- twre_swe$estimate
  twre_swe.p <- twre_swe$p.value
  
  twre_pde <-   cor.test(dsub$mean_lapse, dsub$twre_pde)
  twre_pde.r <- twre_pde$estimate
  twre_pde.p <- twre_pde$p.value
  
  ctopp_pa <-   cor.test(dsub$mean_lapse, dsub$ctopp_pa)
  ctopp_pa.r <- ctopp_pa$estimate
  ctopp_pa.p <- ctopp_pa$p.value
  
  ctopp_pm <-   cor.test(dsub$mean_lapse, dsub$ctopp_pm)
  ctopp_pm.r <- ctopp_pm$estimate
  ctopp_pm.p <- ctopp_pm$p.value
  
  ctopp_rapid <-   cor.test(dsub$mean_lapse, dsub$ctopp_rapid)
  ctopp_rapid.r <- ctopp_rapid$estimate
  ctopp_rapid.p <- ctopp_rapid$p.value
  
  output <- data.frame(wj_wa_ss.r, wj_wa_ss.p,
                       wj_lwid_ss.r, wj_lwid_ss.p,
                       twre_swe.r, twre_swe.p,
                       twre_pde.r, twre_pde.p,
                       ctopp_pa.r, ctopp_pa.p,
                       ctopp_pm.r, ctopp_pm.p,
                       ctopp_rapid.r, ctopp_rapid.p,
                       num_obs = nrow(dsub))
  
  return(output)
}


# Get the summary
df_sum <- use %>%
  group_by(subject_id, continuum, paradigm)%>%
  summarise(mean_slope = mean(slope),
            mean_hi_asymp = mean(hi_asymp), 
            mean_lo_asymp = mean(lo_asymp),
            mean_lapse = mean((hi_asymp+lo_asymp)/2),
            wj_wa_ss = unique(wj_wa_ss),
            wj_lwid_ss = unique(wj_lwid_ss),
            twre_swe = unique(twre_swe_ss),
            twre_pde = unique(twre_pde_ss),
            ctopp_pa = unique(ctopp_pa),
            ctopp_pm = unique(ctopp_pm),
            ctopp_rapid = unique(ctopp_rapid))

# Correlation matrixdf_cors <- df_sum %>%
df_cors_mean <- df_sum %>% 
  group_by(continuum, paradigm) %>%
  do(., get_cors_mean(.)) %>%
  mutate(type = "Slope")

df_cors_lapse <- df_sum %>% 
  group_by(continuum, paradigm) %>%
  do(., get_cors_lapse(.)) %>%
  mutate(type = "Lapse")

df_cors <- rbind(df_cors_mean, df_cors_lapse)
# Melt it
cor_mat <- df_cors %>% 
  melt() %>%
  subset(!(grepl("\\.p", variable) | grepl("obs", variable))) %>%
  mutate(label = paste(continuum,'\n', paradigm))



colourCount = length(unique(cor_mat$value))
getPalette = colorRampPalette(brewer.pal(100, "PRGn"))

px <- ggplot(data = cor_mat, aes(x=label, y=variable, fill=value)) + 
  geom_tile() + 
  facet_wrap(~type)+
  scale_fill_distiller(palette = "PRGn", name = "Correlation")+
  xlab("")+
  ylab("")+
  theme_minimal()+
  scale_y_discrete(labels=c("WJ\n Real Word","WJ\n Pseudoword",
                            "TOWRE\n Real Word", "TOWRE\n Pseudoword",
                            "CTOPP\n Phonological\n Awareness",
                            "CTOPP\n Phonological\n Memory", "CTOPP\n Rapid Symbol\n Naming"))+
  theme(strip.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 14))
px
  
w = 12
h = 7
ggsave("./Figures/pairwise_corr.eps", px, device = cairo_ps, width = w, height = h)
ggsave("./Figures/pairwise_corr.png", px,  width = w, height = h)
ggsave("./Figures/pairwise_corr.tiff", px,  width = w, height = h, dpi = 300)
