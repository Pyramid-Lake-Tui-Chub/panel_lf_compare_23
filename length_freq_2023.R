## length frequency histograms comparing length distribution from different panels

#### INSTALL PACKAGES ----
library(tidyverse)
library(tidyr)
library(reshape2)
library(dplyr)
library(ggplot2)

# read in data
fish_dat <- read.csv("C:\\Documents\\Pyramid_Lake\\RCreations\\csv_files\\fishdata23.csv")

## DATA MANIPULATION ##

# convert panel= NA to panel = 0 
fish_dat["panel"][fish_dat["panel"] == "NA"] <- "0"

# delete all species except for TUI
fish_sub <- subset(fish_dat, species == "TUI")

# add a column that is a unique identifier by net and panel
fish_sub <- fish_sub %>%
  mutate(net_panel = (paste(net, panel)))

# delete larval fish
fish_sub <- subset(fish_sub, set_order != "NA")

# assign panels to mesh size
fish_sub <- fish_sub %>% 
  mutate(mesh = case_when(net == "1" & panel == "1" ~".75 in.",
                          net == "1" & panel == "2" ~"1.5 in.",
                          net == "2" & panel == "1" ~"1 in.",
                          net == "2" & panel == "2" ~"2 in.",
                          net == "3" & panel == "1" ~"1.75 in.",
                          net == "3" & panel == "2" ~"1.25 in.",
                          net == "4" & panel == "1" ~"1.5 in.",
                          net == "4" & panel == "2" ~"1.25 in.",
                          net == "5" & panel == "1" ~"1.75 in.",
                          net == "5" & panel == "2" ~".75 in.",
                          net == "6" & panel == "1" ~"1 in.",
                          net == "6" & panel == "2" ~"2 in.",
                          net == "7" & panel == "1" ~"1.5 in.",
                          net == "7" & panel == "2" ~".75 in.",
                          net == "8" & panel == "1" ~"2 in.",
                          net == "8" & panel == "2" ~"1 in.",
                          net == "9" & panel == "1" ~"1.75 in.",
                          net == "9" & panel == "2" ~"1.25 in.",
                          net == "10" & panel == "1" ~"1.25 in.",
                          net == "10" & panel == "2" ~"1.5 in.",
                          net == "11" & panel == "1" ~".75 in.",
                          net == "11" & panel == "2" ~"1.75 in.",
                          net == "12" & panel == "1" ~"1 in.",
                          net == "12" & panel == "2" ~"2 in.",
                          net == "CORE10" | net == "CORE20" ~ "AFS Experimental"))

# delete unknown fish in experimental nets (aka experimental net with panel = 0/NA)
fish_sub <- subset(fish_sub, mesh != "NA")

# facet_wrap on unique identifier for geom_histogram
lf <- ggplot(fish_sub, aes(x=tl)) +
  geom_histogram(binwidth=10,boundary=0,closed="left",
                 fill="gray80",color="black") +
  scale_y_continuous(name="Number of Fish",expand=expansion(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw()+
  facet_wrap(~factor(net_panel, levels=c("1 0", "1 1", "1 2", "2 0", "2 1", "2 2", "3 0", "3 1", "3 2", 
                                         "4 1", "4 2", "5 1", "5 2", "6 1", "6 2", "7 1", "7 2", "8 1",
                                         "8 2", "9 1", "9 2", "10 1", "10 2", "11 1", "11 2", "12 1", 
                                         "CORE10 0", "CORE20 0")), ncol = 1)
lf

# facet wrap on mesh
lf_mesh <- ggplot(data=subset(fish_sub, expressing == "Y"), aes(x=tl)) +
  geom_histogram(binwidth=10,boundary=0,closed="left",
                 fill="gray40",color="black") +
  scale_y_continuous(name="Number of Fish",expand=expansion(mult=c(0,0.05))) +
  scale_x_continuous(name="Total Length (mm)") +
  theme_bw()+
  facet_wrap(~mesh, ncol = 1)
lf_mesh

# Export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "lf_fec_byMesh_23.png", units = "in", width = 7, height = 8, res=600)
lf_mesh
dev.off()

# count by mesh size
fish_sub_fec <- subset(fish_sub, expressing == "Y")

fish_sub_fec %>% count(mesh)
