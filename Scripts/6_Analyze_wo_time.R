# Analyze data without reference to time point ----------------------------


# Zach Colburn
# May 7, 2018

# Windows 10, 64-bit.
# R 3.5.0, RStudio 1.1.383

# This script will analyze the summarized results without regard to the time 
# point within the experiment.



# Retrieve data -----------------------------------------------------------

cacheDir <- file.path("Cache")
resultsDir <- file.path("Results")

awot <- readRDS(file.path(cacheDir, "awot.rds"))


# Load libraries ----------------------------------------------------------

library(magrittr)
library(ggplot2)

# Edit levels and labels --------------------------------------------------

levels(awot$angle_bin) <- gsub("\\(-Inf", "[0", levels(awot$angle_bin))
levels(awot$cos_bin) <- gsub("\\(-Inf", "[-1", levels(awot$cos_bin))



# Modify cell labels ------------------------------------------------------

awot$positiveCell[awot$positiveCell == "rjeb"] <- "rJEB"
awot$negativeCell[awot$negativeCell == 'jeb'] <- "JEB"


# Summarize data ----------------------------------------------------------

sawot <- awot %>%
  dplyr::group_by(
    positiveCell, negativeCell, posCellProp, negCellProp,# Obligatory groupings.
    band_low, band_high,# Band group.
    angle_bin, cos_bin,# Angle and cos similarity group.
    neighbor_time_fwd# Forward time.
  ) %>%
  dplyr::summarise(
    N= n(),
    
    PROP_median = median(PROP),
    PROP_mean = mean(PROP),
    PROP_sd = sd(PROP),
    PROP_SE = PROP_sd/sqrt(N),
    
    c_gfp_median = median(MEAN_c_gfp),
    c_gfp_mean = mean(MEAN_c_gfp),
    c_gfp_sd = sd(MEAN_c_gfp),
    c_gfp_se = c_gfp_sd/sqrt(N),
    
    n_gfp_median = median(MEAN_n_gfp),
    n_gfp_mean = mean(MEAN_n_gfp),
    n_gfp_sd = sd(MEAN_n_gfp),
    n_gfp_se = n_gfp_sd/sqrt(N)
  )

# Add gfp bins.
num_gfp_bins <- 3
sawot <- sawot %>%
  dplyr::mutate(
    c_gfp_bin = cut(c_gfp_mean, num_gfp_bins, c("Low","Medium","High")),
    n_gfp_bin = cut(n_gfp_mean, num_gfp_bins, c("Low","Medium","High"))
  )
awot <- awot %>%
  dplyr::mutate(
    c_gfp_bin = cut(MEAN_c_gfp, num_gfp_bins, c("Low","Medium","High")),
    n_gfp_bin = cut(MEAN_n_gfp, num_gfp_bins, c("Low","Medium","High"))
  )


# Plot summarized data ----------------------------------------------------

ggplot(
  sawot,
  aes(c_gfp_bin, cos_bin)
)+
  geom_tile(aes(fill = PROP_mean))+
  facet_grid(~band_low+positiveCell+negativeCell+posCellProp+negCellProp)

ggplot(
  awot,
  aes(c_gfp_bin, cos_bin)
)+
  geom_tile(aes(fill = MEAN_c_gfp))+
  facet_grid(~band_low+positiveCell+negativeCell+posCellProp+negCellProp)


awot %>%
  dplyr::mutate(
    condition = paste(band_high,positiveCell,posCellProp,negativeCell,negCellProp)
  ) %>%
  ggplot(
    .,
    aes(condition, MEAN_c_gfp)
  )+
  geom_boxplot()


awot %>%
  dplyr::mutate(
    condition = paste(band_high,positiveCell,posCellProp,negativeCell,negCellProp)
  ) %>%
  ggplot(
    .,
    aes(MEDIAN_c_gfp)
  )+
  geom_histogram()+
  facet_wrap(~condition)





sawot %>%
  dplyr::filter(
    cos_bin == "(0.8,1]"
  ) %>%
  ggplot(
  .,
  aes(neighbor_time_fwd, PROP_median, colour = c_gfp_bin)
)+
  geom_jitter()+
  facet_grid(~band_high+positiveCell+negativeCell+posCellProp+negCellProp)




sawot %>%
  ggplot(
    .,
    aes(neighbor_time_fwd, PROP_mean, colour = c_gfp_bin)
  )+
  geom_boxplot()+
  facet_grid(~band_high+positiveCell+negativeCell+posCellProp+negCellProp)



awot %>%
  dplyr::mutate(
    c_gfp_bin = cut(MEAN_c_gfp, 3, labels = c("L","M","H"))
  ) %>%
  ggplot(
    .,
    aes(factor(neighbor_time_fwd), PROP)
  )+
  geom_boxplot()+
  facet_grid(~band_high+positiveCell+negativeCell+posCellProp+negCellProp+c_gfp_bin)+
  coord_cartesian(ylim=c(0,0.002))




sawot %>%
  dplyr::filter(
    angle_bin == "[0,0.785]",
    cos_bin == "(0.8,1]"
  ) %>%
  dplyr::group_by(
    positiveCell, negativeCell, posCellProp, negCellProp,
    band_low, band_high,
    angle_bin, cos_bin
  ) %>%
  dplyr::summarise(
    PROP = mean(PROP_mean)
  )





awot %>%
  dplyr::filter(
    angle_bin == "[0,0.785]",
    cos_bin == "(0.8,1]",
    neighbor_time_fwd == 000
  ) %>%
  dplyr::group_by(
    band_high,
    positiveCell, negativeCell, posCellProp, negCellProp, angle_bin, cos_bin
  ) %>%
  ggplot(
    .,
    aes(factor(paste(band_high, positiveCell, negativeCell, posCellProp, negCellProp, angle_bin, cos_bin, neighbor_time_fwd)), PROP)
  )+
  geom_boxplot()+
  coord_flip(ylim=c(0,0.008))
ggsave(
  file.path(resultsDir, "PROP_boxplot.tiff"),
  width = 10,
  height = 10,
  units = "in"
)




#####################
#####################
#####################
#####################
#####################
#####################
awot %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    #angle_bin %in% c("[0,0.785]","(2.36,3.14]"),
    cos_bin %in% c("(0.8,1]")
  ) %>%
  dplyr::mutate(
    PROP = PROP*1000,# If you change this, then also change ylab.
    neighbor_time_fwd = neighbor_time_fwd/300*5# Check x lab units.
  ) %>%
  dplyr::group_by(
    band_high,
    positiveCell, negativeCell, posCellProp, negCellProp,
    neighbor_time_fwd
  ) %>%
  dplyr::summarise(
    MEAN = mean(PROP),
    SD = sd(PROP),
    N = n(),
    SE = SD/sqrt(N)
  ) %>%
  dplyr::mutate(
    LABEL = paste0(
      positiveCell, posCellProp, negativeCell, negCellProp
    )
  ) %>%
  dplyr::filter(
    LABEL != "rJEB1JEB4"
  ) %>%
  ggplot(
    .,
    aes(neighbor_time_fwd, MEAN, colour = factor(
      LABEL,
      levels = c(
        "NANAJEBNA",
        "rJEBNANANA",
        "rJEB1JEB9"
      ),
      labels = c(
        "JEB",
        "rJEB",
        "1 rJEB : 9 JEB"
      )
      )
    )
  )+
  geom_line(stat = "identity")+
  facet_wrap(~factor(
    band_high,
    levels = c("60", "120", "180"),
    labels = c("0-60 µm", "60-120 µm", "120-180 µm")
  ))+
  geom_errorbar(aes(
    x = neighbor_time_fwd,
    ymin = MEAN - SE, ymax = MEAN + SE
  ), size = 0.8, width = 1)+
  theme_bw()+
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 18),
    axis.text.x = element_text(colour = "black", size = 16),
    axis.text.y = element_text(colour = "black", , size = 16),
    panel.spacing = unit(1.5, "lines"),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.25),
    plot.margin = unit(c(1,1,1,1), "cm"),
  )+
  coord_cartesian(ylim = c(0,6.5))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,6.5,0.5))+
  scale_x_continuous(expand = c(0,0))+
  ylab(bquote("Fraction of pegs with \n cos similarity > 0.8 ("*10**-3*")"))+
  #ylab(expression("Fraction of pegs with",bquote("cos similarity > 0.8 ("*10**-3*")")))
  xlab("Time (min)")+
  ggtitle("Events in the uppermost similarity bin")

ggsave(
  file.path(resultsDir, "PROP.tiff"),
  width = 7.5,
  height = 5,
  units = "in"
)
