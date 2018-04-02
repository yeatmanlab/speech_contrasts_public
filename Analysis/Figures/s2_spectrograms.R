library(dplyr)
library(ggplot2)
library(reshape2)

rm(list=ls())

dir_sound_files <- "../../Stimuli/Spectrograms"
dir_plots <- getwd()
setwd(dir_sound_files)

spect_files <- list.files(pattern = "Spectrogram")

convert_spectrogram_to_df <- function(spect_name){
  
  spect <- readLines(spect_name) %>% 
    colsplit(., pattern = " = ", names = c("coordinate","Value")) 
  
  spect <- spect[4:nrow(spect),] %>%
    mutate(Value = as.numeric(Value))
  
  Freq_floor <- spect[spect$coordinate=="ymin", "Value"]
  Freq_max <- spect[spect$coordinate=="ymax", "Value"]
  Time_start <- spect[spect$coordinate=="xmin", "Value"]
  Time_end <- spect[spect$coordinate=="xmax", "Value"]
  num_times <- spect[spect$coordinate=="nx", "Value"]
  time_bin_size <- spect[spect$coordinate=="dx", "Value"]
  num_freq_bins <- spect[spect$coordinate=="ny", "Value"]
  freq_bin_size <- spect[spect$coordinate=="dy", "Value"]
  
  spect_vals <- spect$coordinate[12:length(spect$coordinate)] %>% 
    data.frame(meas = .) %>%
    mutate(meas = as.character(meas)) %>%
    mutate(is.Freqstep = ifelse(grepl(x = meas, pattern=":"), 1, 0)) %>%
    mutate(Freqstep = cumsum(is.Freqstep)) %>%
    mutate(Timestep = {meas %>% 
        colsplit(., pattern = "\\]|\\]", names = c("junk","Timebin")) %>%
        mutate(Timebin = gsub(replacement = "", x = Timebin, pattern="\\[|\\]")) %>%
        mutate(Timebin = as.numeric(Timebin)) %>%
        `[[`(., "Timebin")}) %>%
    mutate(Time = (Timestep - 1) * time_bin_size) %>%
    mutate(Frequency = (Freqstep - 1) * freq_bin_size) 
  
  reference_SP <- 0.00002
  
  df <- cbind(spect[12:nrow(spect),], spect_vals) %>%
    mutate(filename = spect_name) %>%
    select(filename, Timestep, Freqstep, Time, Frequency, Pressure = Value) %>%
    # mutate(Pressure = round(Pressure, 5)) %>%
    mutate(Level = 20 * log10(Pressure / reference_SP))
  
  df <- df[!is.na(df$Time),]
  
  rm(spect, spect_vals)
  return(df)
}

dfx <- spect_files %>% 
  lapply(., convert_spectrogram_to_df) %>%
  bind_rows()

# Assign labels
dfx <- dfx %>% 
  mutate(sound = ifelse(grepl("Ba_Da",filename) & grepl("1", filename), "/ba/",
                            ifelse(grepl("Ba_Da", filename) & grepl("7", filename), "/da/",
                            ifelse(grepl("Sa_Sha", filename) & grepl("1", filename), "/\u0283a/",
                                   "/sa/"))))

# Constrain dynamic range
dynamic_range <- 150
dfx$Level_dr <- dfx$Level
dfx$Level_dr[dfx$Level_dr < (max(dfx$Level_dr, na.rm=TRUE) - dynamic_range)] <- NA
hist(dfx$Level)
hist(dfx$Level_dr)

dfx2 <- dfx %>% subset(!((sound %in% c("/ba/","/da/")) & Frequency > 5000))
dfx2$Frequency <- dfx2$Frequency/1000

dfx2$sound = factor(dfx2$sound, levels=c('/ba/','/da/','/sa/','/\u0283a/'))

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#

px_spectrograms <- ggplot(dfx2)+
  aes(x = Time, y=Frequency, fill=Level_dr)+
  geom_tile()+
  scale_fill_gradient(high="black", low="white", na.value = "white")+
  #coord_cartesian(xlim = c(0, 0.35), ylim=c(0, 11))+
  xlab("Time (s)")+
  scale_y_continuous("Frequency (kHz)", breaks=c(0, 5, 10))+
  theme_bw()+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 24)) +
  facet_wrap(~sound, scales = "free_y")
px_spectrograms



setwd(dir_plots)
ggsave(px_spectrograms, file="s2_spectrograms.png", height=7, width=7, dpi=300)
ggsave(px_spectrograms, file="s2_spectrograms.tiff", height=7, width=7, dpi=300)
# save as EPS
ggsave(px_spectrograms, file="s2_spectrograms.eps", height=7, width=7, device = cairo_ps)

