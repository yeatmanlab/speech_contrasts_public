#!/usr/bin/env Rscript

## figure_individ_and_mean_fitted_models_by_group.R
## aggregates the data by group and cue type and plots model fits

library(dplyr)
library(ggplot2)

## When loading/saving, this script assumes the working directory is set to the
## root directory of the repo. Relative to this script's location that is:
setwd("../..")

psychometrics <- read.csv("cleaned_psychometrics.csv")
group_levels <- c("Dyslexic", "Below Average", "Above Average", "Group Means")
psychometrics$group <- factor(psychometrics$group, levels=group_levels)

# Remove any psychometrics that do not meet the fit quality standard
psychometrics <- psychometrics %>% filter(threshold <= 7 & threshold >= 1)


## ## ## ## ## ## ## ## ## ## ## ## ##
## FUNCTION TO COMPUTE FITTED CURVE ##
## ## ## ## ## ## ## ## ## ## ## ## ##
logistic <- function(thresh, slope, lower, upper, endpoints=c(1, 7)) {
    ## NOTE: upper should be amount below 1 (upper of 0.1 is asymptote of 0.9)
    x <- seq(endpoints[1], endpoints[2], length.out=diff(endpoints) * 10 + 1)
    y <- lower + (1 - lower - upper) / (1 + exp(-1 * slope * (x - thresh)))
    data.frame(cbind(x, y))
}

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

## ## ## ## ## ## ## ## ## ## ##
## AGGREGATE AT SUBJECT LEVEL ##
## ## ## ## ## ## ## ## ## ## ##
psych_subj_agg <- psychometrics %>%
    group_by(subject_id, continuum) %>%
    summarise(group=unique(group),
              wj_brs=unique(wj_brs),
              threshold=mean(threshold),
              slope=mean(slope),
              lo_asymp=mean(lo_asymp),
              hi_asymp=mean(hi_asymp),
              lapse=(lo_asymp + hi_asymp) / 2)
psych_subj_agg$plot_group <- with(psych_subj_agg, interaction(subject_id, continuum))
psych_subj_agg <- na.omit(psych_subj_agg)
psych_subj_agg$orig_group <- psych_subj_agg$group

# mean + SE by group, across subjects
psych_group_ribbon <- psych_subj_agg %>%
    group_by(group, continuum) %>%
    summarise(wj_brs=mean(wj_brs, na.rm=TRUE),
              threshold_mu=mean(threshold),
              slope_mu=mean(slope),
              lo_mu=mean(lo_asymp),
              hi_mu=mean(hi_asymp),
              threshold_se=sd(threshold) / sqrt(n()),
              slope_se=sd(slope) / sqrt(n()),
              lo_se=sd(lo_asymp) / sqrt(n()),
              hi_se=sd(hi_asymp) / sqrt(n()),
              threshold_lower=threshold_mu - threshold_se,
              threshold_upper=threshold_mu + threshold_se,
              slope_lower=slope_mu - slope_se,
              slope_upper=slope_mu + slope_se,
              lo_lower=lo_mu - lo_se,
              lo_upper=lo_mu + lo_se,
              hi_lower=hi_mu - hi_se,
              hi_upper=hi_mu + hi_se) %>%
    rename(orig_group=group, slope=slope_mu, threshold=threshold_mu,
           lo_asymp=lo_mu, hi_asymp=hi_mu)
psych_group_ribbon$group <- "Group Means"
psych_group_ribbon$group <- factor(psych_group_ribbon$group, levels=group_levels)
psych_group_ribbon$plot_group <- with(psych_group_ribbon, 
                                      interaction(orig_group, continuum))

## avoid the annoying "unequal factor levels" warning
plot_group_levels <- c(levels(psych_subj_agg$plot_group),
                       levels(psych_group_ribbon$plot_group))
psych_subj_agg$plot_group <- factor(psych_subj_agg$plot_group,
                                    levels=plot_group_levels)
psych_group_ribbon$plot_group <- factor(psych_group_ribbon$plot_group,
                                        levels=plot_group_levels)

## unite the 2 tibbles
this_data <- bind_rows(psych_subj_agg, psych_group_ribbon)

## compute fitted curves
logist_subj_agg <- this_data %>% 
    group_by(plot_group) %>%
    mutate(x=list(logistic(threshold, slope, lo_asymp, hi_asymp)$x),
           y=list(logistic(threshold, slope, lo_asymp, hi_asymp)$y),
           y_lower=list(logistic(threshold_lower, slope_lower, lo_lower, hi_lower)$y),
           y_upper=list(logistic(threshold_upper, slope_upper, lo_upper, hi_upper)$y)) %>%
    tidyr::unnest(x, y, y_lower, y_upper)

## GET THE COLORMAP VALUES FOR THE GROUP MEANS
endpoints <- range(na.omit(psychometrics$wj_brs))
c_range <- diff(endpoints)
group_vals <- psych_subj_agg %>% group_by(group) %>% summarise(wj_brs=mean(wj_brs))
group_vals <- (group_vals$wj_brs - endpoints[1]) / c_range
group_cols <- get_cmap_color(group_vals, cmap)
group_cols <- c(group_cols, "#808080")  # for the group-means facet
names(group_cols) <- levels(this_data$group)

## custom labeller for the strip
continuum_labeller <- as_labeller(c(`/ʃa/-/sa/`="/ʃa/~/sa/\nPercent /sa/ response",
                                    `/ba/-/da/`="/ba/~/da/\nPercent /da/ response",
                                    `Dyslexic`="Dyslexic",
                                    `Below Average`="Below Average",
                                    `Above Average`="Above Average",
                                    `Group Means`="Group Means"))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##  FACET GRID OF FITTED LOGISTIC CURVES W/ WJBRS COLORMAP ON CURVES ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
fig <- ggplot(data=logist_subj_agg, mapping=aes(x=x, y=y, group=plot_group)) +
    geom_line(aes(colour=wj_brs), size=0.75, alpha=1) +
    geom_ribbon(aes(ymin=y_lower, ymax=y_upper, fill=wj_brs), alpha=0.2, size=0) +
    scale_x_continuous(labels=as.character, breaks=seq(1, 7)) +
    scale_y_continuous(labels=scales::percent, breaks=seq(0, 1, by=0.25)) +
    scale_colour_gradientn(colours=cmap) +
    scale_fill_gradientn(colours=cmap) +
    theme_bw() +
    theme(text=element_text(size=12, colour="black", family="Open Sans"),
          strip.text.x=element_text(face="bold", colour="white"),
          strip.text.y=element_text(face="plain", size=10),
          strip.placement="outside", panel.grid.minor=element_blank()) +
    labs(title="", colour="WJ-BRS", fill="WJ-BRS") +
    xlab("Continuum step") + ylab("") +
    facet_grid(continuum ~ group, labeller=continuum_labeller, switch="y")

## HACK INTO THE GUTS OF GGPLOT TO SET INDIVIDUAL STRIP COLORS
gtable <- ggplotGrob(fig)
strip_ix <- grep(pattern="strip-", gtable$layout$name)
for (i in strip_ix) {
    lab <- gtable$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$label
    col <- unname(group_cols[lab])
    if (!is.na(col)) {
        gtable$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- col
    } else {
        gtable$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- NA
        gtable$grobs[[i]]$grobs[[1]]$children[[1]]$gp$col <- NA
    }
}

## PLOT IT INTERACTIVELY
grid::grid.draw(gtable)

## SAVE TO FILE
setwd('./Analysis/Figures')
ggsave("figure_individ_and_mean_fitted_models_by_group.pdf", gtable,
       device=cairo_pdf, width=7.5, height=4.5)
ggsave("figure_individ_and_mean_fitted_models_by_group.png", gtable,
       width=7.5, height=4.5, dpi = 300)
