library(readxl)
library('Hmisc')
library(tidyverse)
library("dplyr")
library(ggplot2)
library("reshape2")
library(corrplot)
library(fauxnaif)
library(naniar)
library(ggpubr)
library("gridExtra")
library("cowplot")
library("rnaturalearth")
library("rnaturalearthdata")
library("sf")
library(janitor)
library(DSL)
library(pastecs)

# the script and CSV must be in the same directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
hz <- read.csv("DT-hz-IJDRR.csv")
coords <- read.csv("merged_coords.csv")


#CLEANING THE DATA 
  #change cases with not enough information to NA
  hz <<- hz%>%
    mutate(across(c(H.1.:H.12.,), ~ if_else((.x==77|.x==88|.x==99|.x=="NA"|.x=="Y"),NA,.x)))
  
  #combined hazard types with droughts, floods, and pests
  hz <<- hz %>%
    mutate(
      total_types = case_when(
        H.5. == "PE" | H.5. == "CP" | H.5. == "Lo" | H.5. == "PD" ~ "pests",
        H.5. == "Dr" ~ "droughts",
        H.5. == "Fl" ~ "floods",
        H.5. == "" ~ NA,
        .default = H.5.
      )
    )
  
  
  #creates variable for GEN (general events that have occurred but do not have an exact date) vs dated events
  hz <<- hz %>%
    rowwise() %>%
    mutate(GEN_or_date = case_when(H.2.a. == "GEN" ~ "GEN",
                                   .default = "Dated"))
  
  
  #creates new var by region
  hz <<- hz %>%
    mutate(
      region = case_when(
        str_detect(OWC, "^A") ~ "Asia",
        str_detect(OWC, "^E") ~ "Europe",
        str_detect(OWC, "^F") ~ "Africa",
        str_detect(OWC, "^M") ~ "Middle East",
        str_detect(OWC, "^N") ~ "North America",
        str_detect(OWC, "^O") ~ "Oceania",
        str_detect(OWC, "^R") ~ "Russia",
        str_detect(OWC, "^S") ~ "South America"
      )
    )
  
  
  #creates a new variable which characterizes hazards if they are a top three hazard (droughts, floods, pests) or all else 
  hz <<- hz %>%
    mutate(top_three_var = case_when(
      total_types != "droughts" & total_types != "floods" & total_types != "pests" & !is.na(total_types) ~ "all other",
      .default = total_types
    ))
  
  #transforms code 0.5 (inferred absence) to 1 (absent) 
  hz <<- hz%>%
    mutate(H.9.a. = case_when(H.9.a. == 0.5 ~ 1,
                              .default = H.9.a.))
  
  #creates new variable denoting whether the event is severe or not (H.10. overall severity is 3+)
  hz <- hz %>% mutate(
    sev_or_not = case_when(
      H.10. > 2.5 ~ "severe",
      .default = "not severe"
    )
  )
  


#---------------------------------------------------------------------------------------------------------
#creating new dataframes for analysis

#modifying dataset so 1s for H12 Occurrence are removed (only keeping occurred events and threats)
hz <- hz %>%
  subset(H.12. > 1)

#creates dataset where all 1s, 2s, and 99s for H12 are removed (only occurred events)
hz_freq_subset <- hz %>%
  subset(H.12. > 2)
  
#creates dataset where all events  in EP 90 are removed, and all 1s, 2s, and 99s for H12 are removed. (only occurred events in time 30, 60)
hz_no90 <- hz %>%
  filter(time != "90")


#creates separate dataframes for each time period
time30.df <- hz %>%
  filter(time == "30")
time60.df <- hz %>%
  filter(time == "60")
time90.df <- hz %>%
  filter(time == "90")


#creates list for top three types of hazards (using total_types)
top_three <- c("pests", "floods", "droughts")

#creates new dataframe for top three hazards 
top_three_subset <- hz %>% 
  subset(total_types %in% top_three)


#stores the number of rows in the original hz dataset
nrow_hz <- nrow(hz)


# usable data set to start most things, besides hz_no90 ---------------------------------------
d <- hz_no90
#combine PD
d <- d %>%
  mutate(H.5. = ifelse(H.5. %in% c("PD", "CP", "PE","Lo"), "PD", H.5.))


