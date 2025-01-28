#input any event level dataset for hz 
# hz <- entire dataset
# hz_freq_subset <- entire dataset with only occurred events (no threats)
# hz_no90 <- entire dataset with only occurred events (no threat) in 30, 60 time periods (no 90 time period)
# time30.df / time60.df / time90/df <- datasets with only occurred events in either the 30, 60, or 90 time period
# top_three_subset <- dataset with only droughts, floods, pests in 30,60,90 time period. threats included. 


# OVHZ TO REMOVE -----------------------------------------------------------
#input any society level dataset for ovhz
# ovhz <- entire dataset
# ovhz30 / ovhz60 / ovhz90 <- society level datasets for either the 30, 60, or 90 time period

#------------------------------------------------------------------------------------------------------------------------------------------------

#function to create basic stats about hazards
basic_table_data = function(hz) {
  #all events
  #summary table which outputs number of hazards, percentage of all hazards, number of societies, and percentage of all societies for inputted dataframe
  
  all_events_table <<- data.frame(
    n = nrow(hz),
    perc_hz = (nrow(hz) / nrow_hz) * 100,
    num_soc = length(unique(hz$OWC)),
    perc_soc = (length(unique(hz$OWC)) / 132) * 100
    
  )
  
  
}


#creates tables with details about hazards per time periods
time_period_data = function(hz, hz_no90)  {
  
  #summary of events by time period 
  sum_time_period <<- hz %>%
    group_by(time) %>%
    summarize(
      n = n(),
      percentage = (n() / nrow(hz) * 100),
      num_soc = length(unique(OWC)),
      perc_soc = (num_soc / 132) * 100
    ) %>%
    adorn_totals("row") %>%
    mutate(
      time = case_when(is.na(time) ~ "threats",
      .default = time
    ))
    
  
  #summary of GEN / Dated events
  #summarizes number of hazards, percentage of hazards, number of societies, and percentage of all societies for GEN or dated events
  sum_GEN_dated_table <<- hz %>%
    group_by(GEN_or_date) %>%
    summarize(
      n = n(),
      percentage = (n() / nrow(hz) * 100),
      num_soc = length(unique(OWC)),
      perc_soc = (num_soc / 132) * 100
    ) %>%
    adorn_totals("row")
  
  
  #this is to create a summary of societies with only threats
    #summary of societies with occurred events H.12. == 3
    sum_hist_freq_sub <- hz %>%
      group_by(OWC) %>%
      summarize(n = n(),
                H.12. = 3)
    
    #summary of societies events that are not hazards but are threats H.12. == 2
    sum_just_two <- hz %>%
      subset(H.12. == 2) %>%
      group_by(OWC) %>%
      summarize (n = n(),
                 H.12. = 2)
    
    #creates dataframe for societies with only threats 
    socs_only_threats <<- anti_join(sum_just_two, sum_hist_freq_sub, by = "OWC")
    
}



#  Flag does not run - is not used -------------------------------------------------------
#function to get summary tables on each type of hazard
#also to get information on societies with droughts and floods
hazard_types_data = function(hz) {
  #summary table of each type of hazard (pests are combined)
  sum_types <<- hz %>%
    group_by(total_types) %>%
    summarize(
      n = n(),
      percentage = (n() / nrow(hz) * 100),
      num_soc = length(unique(OWC)),
      perc_soc = (num_soc / 132) * 100
    )
  
  #summary table of all hazards (none combined)
  sum_types_H5 <<- hz %>%
    group_by(H.5.) %>%
    summarize(
      n = n(),
      percentage = (n() / nrow(hz) * 100),
      num_soc = length(unique(OWC)),
      perc_soc = (num_soc / 132) * 100
    )
  

  #find societies that have both droughts and floods
  droughts <- hz %>%
    subset(total_types == "droughts") %>%
    count(OWC, total_types)
  
  floods <- hz %>%
    subset(total_types == "floods") %>%
    count(OWC, total_types)
  
  #create new dataframe for societies that have both droughts and floods
  dr_fl <<- inner_join(droughts, floods, by = "OWC")
  
  #summarise the societies that have both droughts and floods
  sum_dr_fl <<- data.frame(
    n = sum(dr_fl$n.x, dr_fl$n.y),
    perc = sum(dr_fl$n.x, dr_fl$n.y) / nrow(hz) * 100,
    num_soc = nrow(dr_fl),
    perc_soc = nrow(dr_fl) / 132 * 100
    
  )

  
}


## end is not used ---------------------------------------------------------



#function to get number of hazard types per society
#also to compare all hazard types with a certain dimension variable, e.g. H8 predictability
diversity_of_types = function(hz, var1) {

  #dataframe with number of hazard TYPES per society
  count_types_OWC <<- hz %>%
    group_by(OWC) %>%
    summarize(n = length(unique(H.5.)))

  
  #dataframe with dimension var and all hazard types
  var1_hztypes <<- hz %>%
    count({{var1}}, H.5.) %>%
    spread(key = {{var1}}, value = n) %>%
    adorn_totals("col") %>%
    mutate(across(where(is.numeric), ~ .x / Total * 100, .names = "{.col}_perc"))
  

}


#function to create new data set with most severe hazards per OWC
severe_data = function(hz) {
  
  #creates dataframe with most severe hazards per OWC
  severe_types <- hz %>%
    count(OWC, H.10., total_types, H.9.a., H.12.) %>%
    group_by(OWC) %>%
    filter(H.10. == max(H.10., na.rm = TRUE))
  
  #finds NAs for H.10. severity
  severe_types_NA <- hz %>%
    filter(is.na(H.10.)) %>%
    count(OWC, H.10., total_types, H.9.a., H.12.) %>%
    filter(!(OWC %in% severe_types$OWC))
  
  #binds the NAs to the severe hazards dataframe
  severe_types <<- rbind(severe_types, severe_types_NA) %>%
    arrange(OWC, -H.10.)
  
  #summary table of how many societies for each hazard type, average severity, and standard deviation
  types_severity <<- hz %>%
    group_by(total_types) %>%
    summarise(
      n = length(unique(OWC)),
      avg_severity = mean(H.10., na.rm = TRUE),
      SD = sd(H.10., na.rm= TRUE)
    )
  
  
}


#function that creates new dataframe which summarises top types of hazards per region
region_data = function(hz) {
  
  #summary of hazards per region
  sum_reg <<- hz %>%
    group_by(region) %>%
    summarise(
      n = n(),
      percentage = (n() / nrow(hz) * 100),
      num_soc = length(unique(OWC)),
      perc_soc = (num_soc / 132) * 100,
      prop = n / num_soc
    ) 
  
}


#function to summarize total number of hazards/ societies for dimensions variables H.7.-10.
H7_H10_summary_tables = function(hz, var1) {
  
  #general summary of dimension variable
  sum_table <<- hz %>%
    group_by({{var1}}) %>%
    summarize(
      n = n(),
      percentage = (n() / nrow(hz) * 100),
      num_soc = length(unique(OWC)),
      perc_soc = (num_soc / 132) * 100
    )
  
  #calculate table of averages for droughts, floods, pests
  sum_table_avgs <<- hz %>%
    group_by(top_three_var) %>%
    summarise(across(
      c(H.7.:H.10.),
      list(
        avg = mean,
        max = max,
        median = median,
        min = min,
        SD = sd
      ),
      na.rm = TRUE,
      .names = "{.col}.{.fn}"
    )) 
  

  
}


#function that creates graphics/ tables/ analyses for looking at the relationship between H.8. predictability and H.10.severity
#relationship reflects degree of adaptation
adaptability = function(hz) {
  
  #creates boxplot looking at relationship between H.8. predictability and H.10. severity
  #divides up by top three hazards and all hazards
  adapt_boxplot <<- hz %>%
    filter(!is.na(H.8.)) %>%
    ggplot(aes(as.factor(H.8.), H.10.)) +
    geom_boxplot(aes(fill = top_three_var)) + 
    scale_fill_manual(
      values = c("snow3", "sienna2", "skyblue", "darkolivegreen2"),
      name = "Hazard type",
      labels = c("Other", "Droughts", "Floods", "Pests and Diseases")
    ) + labs(x = "H.8.")
  
  #frequency table for H.8. predictability and H.10. severity
  adaptation_table <<- table(hz$H.8., hz$H.10.)
  
  #dataframe which looks at each value for predictability by society (OWC)
  predict_socs <<- hz %>%
    count(OWC, H.8.) %>%
    spread(key = H.8., value = n)
  
  #dataframe which counts the occurences of high predictability low severity by OWC
  adapt_socs <<- hz %>%
    filter(H.8. == 3 & H.10. < 2.5 ) %>%
    count(OWC)
  
}


# OVHZ TO REMOVE ----------------------------------------------------------
#this function calculates averages, min, max, median, SD for # of hazards and hazard types by time period
recurrence_of_events = function(ovhz) {
  
  
  #calculate table of averages: number of hazards, number of severe hazards, number of hazard types, and number of severe hazard types, per soc
  avgs_summary <<- ovhz %>%
    filter(period != 90) %>%
    group_by(period) %>%
    summarise(across(
      c(OvHz.all.count_dq12, OvHz.all.scount_3_dq12, OvHz.all.div_dq12, OvHz.all.sdiv_3_dq12),
      list(
        n = sum,
        avg = mean,
        max = max,
        median = median,
        min = min,
        SD = sd
      ),
      na.rm = TRUE,
      .names = "{.col}.{.fn}"
    )) %>%
    rbind(allperiodsdf <- ovhz %>%
            filter(period != 90) %>%
            subset(select = c(OWC, OvHz.all.count_dq12, OvHz.all.scount_3_dq12, OvHz.all.div_dq12, OvHz.all.sdiv_3_dq12, na.rm = TRUE)) %>%
            summarise(across(
              c(OvHz.all.count_dq12, OvHz.all.scount_3_dq12, OvHz.all.div_dq12, OvHz.all.sdiv_3_dq12),
              list(n = sum,
                   avg = mean,
                   max = max,
                   median = median,
                   min = min,
                   SD = sd), na.rm = TRUE,
              .names = "{.col}.{.fn}")) %>%
            mutate(
              period = "all")
            )
          

#creating summary value for 30 60 in society level dataset to find mean, max, min, med, sd    
  ovhz3060_summary <- ovhz %>%
    filter(period == 30 | period == 60) %>%
    select(c(OWC, OvHz.all.count_dq12, OvHz.all.scount_3_dq12, OvHz.all.div_dq12, OvHz.all.sdiv_3_dq12)) %>%
    group_by(OWC) %>%
    summarise_all(list(sum), na.rm = TRUE) %>%
    summarise(across(
      c(OvHz.all.count_dq12, OvHz.all.scount_3_dq12),
      list(
        n = sum,
        avg = mean,
        max = max,
        median = median,
        min = min,
        SD = sd
      ),
      na.rm = TRUE,
      .names = "{.col}.{.fn}"
    )) 
    
  


# OVHZ TO REMOVE ----------------------------------------------------------
#returns number of societies that are within 3 standard deviations of whatever mean and SD you want
  #this is specifically for overall diversity of hazards using the mean and sd calculated in avgs_summary for OvHz.all.div_dq12
  #can change to another variable, just have to change the mean and SD 
  owc_summary <- ovhz %>%
    filter(period == 30) %>%
    subset(select = c(OWC, OvHz.all.div_dq12)) %>% 
    group_by(OWC) %>%
    summarise(
      n = sum(OvHz.all.div_dq12),
      std = (n - 2.685039) / 1.7214832
    ) %>%
    ungroup() %>%
    filter(std < 3.0 & std > -3.0)
  

}


#function returns a dataframe based on the single most frequent hazard per society
socs_by_most_freq_hz = function(hz) {
  
  #creates dataframe with single most frequent hazard per society
  most_freq_hz <- hz %>%
    count(OWC, total_types) %>%
    group_by(OWC) %>%
    filter(n == max(n, na.rm = TRUE))
  
  #merge dataframe with expanded hz dataframe to retain all of the other data
  most_freq_merge <- right_join(most_freq_hz, hz, by = c("OWC",  "total_types")) %>%
    filter(!is.na(n))
  
  #dataframe which includes the single most frequent hazard per society, the # of occurrences, most common timeframe, H9a, H10, and H8 values. 
  soc_by_freq_hz <<- most_freq_merge %>%
    group_by(OWC, total_types) %>%
    summarise(
      n = n(),
      period = paste(as.list(unique(H.3.)), collapse = "/"),
      period2 = paste(as.list(unique(time)), collapse = ","),
      H9a_avg = mean(H.9.a., na.rm = TRUE),
      H9a_max = max(H.9.a., na.rm = TRUE),
      H10_avg = mean(H.10., na.rm = TRUE),
      H10_max = max(H.10., na.rm = TRUE),
      H8_avg = mean(H.8., na.rm = TRUE),
      .groups = "keep"
    )
  
}


#function to calculate the proportion of missing data for any variable
missing_data = function(hz, var1) {
   
  #calculate how many missing values there are for any variable
  missing_vals <<- hz %>%
    count(is.na({{var1}})) %>%
    adorn_totals() %>%
    mutate(
      prop = n / nrow(hz) * 100
    )
  
  #shows the proportion of NAs amongst the H.9.a - H.10. vars 
  severity_NA_distric <- hz %>%
    count(!is.na(H.10.), is.na(H.9.a.), is.na(H.9.b.), is.na(H.9.c.), is.na(H.9.d.)) %>%
    adorn_totals() %>%
    mutate(
      prop = n / nrow(hz) * 100
    )

  
}


#GRAPH FUNCTIONS --------------------------------------------------------------------------------


#functions creates graphs by region
region_graphs = function(hz) {
  
  #bar graph of all hazards per region
  all_region_graph <<- hz %>%
    filter(!is.na(region)) %>%
    ggplot(aes(as.factor(region))) +
    geom_bar(stat = "count", aes(fill = region)) +
    theme(legend.position = "none") +
    labs(x = "Region", y = "Number of hazards", title = "Total number of hazards per world region")
  

   #graph of droughts, floods, pests, per region
   dr_fl_pe_graph_region <<- hz %>%
     ggplot(aes(as.factor(region))) +
     geom_bar(position = "fill",
              aes(fill = top_three_var)) +
     scale_fill_manual(
       values = c(
         "snow4",
         "sienna2",
         "steelblue1",
         "darkolivegreen2"
       ),
       name = "Hazard type",
       labels = c(
         "All Other Hazards",
         "Droughts",
         "Floods",
         "Pests and Diseases"
       )
     ) + labs(x = "Region", y = "Proportion of Hazards", title = "Top three hazards per region") +
     geom_text(stat = "count", aes(label = after_stat(count)), position = position_fill(vjust = 0))
   
  
   #create new dataframe which has the top three types of hazards for each region
   most_freq_reg <- hz %>%
     count(region, total_types) %>%    
     arrange(desc(n)) %>%
     group_by(region) %>%
     slice(1:3)
   
   
   #graph of top three types of hazards for each region
   most_freq_graph <<- most_freq_reg %>%
     ggplot(aes(x = as.factor(region), fill = total_types))+
     geom_col(aes(y = n), position = "dodge")  +
     scale_fill_manual(
       values = c(
         "sienna2",
         "steelblue1",
         "aquamarine1",
         "darkolivegreen2",
         "darkgoldenrod2",
         "chocolate4",
         "ivory3",
         "lightblue1",
         "plum1"
         
       ),
       name = "Hazard type",
       labels = c(
         "Droughts",
         "Floods",
         "Frost",
         "Pests and Diseases",
         "Sand/Dust Storm",
         "Soil or Land Erosion",
         "Severe or Early/Long Winter",
         "Snowstorm", 
         "Windstorm/ Gale"
       )
     ) + labs(x = "Region", y = "Number of Hazards", title = "Top three hazards per region") 
   
}


#creates graphs for one dimension of hazards i.e. distribution of predictability
H7_H10_graphs = function(hz, var1, var1name) {
  
  #customizable graph for dimensions of hazards variables
  var1_bargraph <<- hz %>%
    filter(!is.na({{var1}})) %>%
    ggplot(aes(x = as.factor({{var1}}), fill = factor(top_three_var, levels = c("droughts", "floods", "pests", "all other")))) +
    geom_bar(position = "fill") +
    scale_fill_manual(
      values = c("#FDE725FF", "#287D8EFF", "#73D055FF", "#453781FF"),
      name = "Hazard type",
      labels = c("Droughts", "Floods", "Pests and diseases", "Other")
    ) + labs(x = var1name, y = "Proportion of events")  +
   
    theme(legend.position = "bottom",
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                          colour = "lightgrey"),
          panel.background = element_rect(fill = "white", color = "lightgrey", size = 0.5),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16)
    )
  
  
}


#creates graphs regarding recurrence of events and time period distribution of events 
recurrence_of_events_graphs = function(hz) {
  
  #count  number of hazards per society
  hz_count <-  hz %>%
    count(OWC) %>%
    subset(select = -c(OWC)) %>%
    mutate(
      sev = "not severe"
    )
  
  #count number of severe hazards per osc
  sev_hz_count <- hz %>%
    filter(H.10. > 2.5) %>%
    count(OWC) %>%
    subset(select = -c(OWC)) %>%
    mutate(
      sev = "severe"
    )
  
  nonsev_count <- hz %>%
    filter(H.10. < 3) %>%
    count(OWC) %>%
    subset(select = -c(OWC)) %>%
    mutate(
      sev = "non severe"
    )
  
  #bind severe and all events  
  sev_hz_count <- rbind(nonsev_count, sev_hz_count)
  
  #create histogram overlaying all hazard events and severe events
  sev_hz_hist <<- 
    ggplot(data = sev_hz_count, aes(n, fill = sev)) +
    geom_histogram(binwidth = 1,
                   alpha = 1, 
                   position = "stack") +
    scale_fill_manual(
      values = c("#2e6d8e", "#b0dd2f"),
      name = "Severity",
      labels = c("Non-severe hazards", "Severe hazards")
    )  +
    labs(x = "Number of hazard events", y = "Number of societies") + 
    theme(
      legend.position= c(0.85, 0.9),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "lightgrey"),
      panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                      colour = "lightgrey"),
      panel.background = element_rect(fill = "white", color = "lightgrey", size = 0.5),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill = "white", color = "lightgrey", size = 0.5),
      
    )
  
  
  #customized histogram for all events
   ovhz3060 %>%
    ggplot(aes(OvHz.all.count_dq12)) +
    geom_histogram(binwidth = 1,
                   alpha = .9,
                   fill = "#89d548") +
    labs(x = "Number of hazard events", y = "Number of societies") +
    geom_vline(
      aes(xintercept = mean(OvHz.all.count_dq12), color = as.factor(mean(OvHz.all.count_dq12)), linetype = as.factor(mean(OvHz.all.count_dq12))),
      size = 1.2
    ) +
    geom_vline(
      aes(xintercept = median(OvHz.all.count_dq12), color = as.factor(median(median(OvHz.all.count_dq12))), linetype = as.factor(median(OvHz.all.count_dq12))),
      size = 1.2
    ) + 
    geom_text(
      aes(
        label = round(mean(OvHz.all.count_dq12), 1),
        y = 0,
        x = mean(OvHz.all.count_dq12) + 2
      ),
      vjust = -.7,
      col = "#1f978b",
      size = 6
    ) +
     geom_text(
       aes(
         label = round(median(OvHz.all.count_dq12), 1),
         y = 0,
         x = median(OvHz.all.count_dq12) + 1
       ),
       vjust = -.7,
       col = "#472a7a",
       size = 6
     ) +
     scale_colour_manual(values = c("#1f978b", "#472a7a"), labels = c("Mean - 10.8", "Median - 8"))+
     scale_linetype_manual(values = c("dashed", "solid"), labels = c("Mean - 10.8", "Median - 8")) +
    theme(
      legend.position = c(0.80, 0.865),
      panel.grid.major = element_line(
        size = 0.5,
        linetype = 'solid',
        colour = "lightgrey"
      ),
      panel.grid.minor = element_line(
        size = 0.5,
        linetype = 'solid',
        colour = "lightgrey"
      ),
      panel.background = element_rect(
        fill = "white",
        color = "lightgrey",
        size = 0.5
      ),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.background = element_rect(
        fill = "white",
        color = "lightgrey",
        size = 0.5
      ),
      legend.key.width = unit(1.5, "cm"),
      legend.key.size = unit(1.75, "cm"),
      legend.key = element_rect(fill = "white", color = "lightgrey")
    ) +
     labs(color = element_blank(), linetype = element_blank())
  
  
   
}


#CORRELATIONS ------------------------------------------------------------------------------------------

#function that runs correlations across hz variables as well as across hazard types
correlations = function(hz, start_col, end_col, type_hazard) {
  

#basic correlation matrix code
flattenCorrMatrix <<- function(cormat, pmat, nmat) {
  #anj code
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = colnames(cormat)[col(cormat)[ut]],
    cor  = (cormat)[ut],
    p = pmat[ut],
    n = nmat[ut]
  )
}

#runs correlation matrix between different dimensions of hazards
cor <-
  rcorr(data.matrix(hz[, c(start_col:end_col)]), type = "spearman")
dimensions_cor <<- flattenCorrMatrix(cor$r, cor$P, cor$n) 


#runs correlation matrix between different dimensions of hazards, subsetting for a specific hazard type
cor <-
  rcorr(data.matrix(subset(hz, total_types == {{type_hazard}})[, c(start_col:end_col)]), type = "spearman")
types_cor <<- flattenCorrMatrix(cor$r, cor$P, cor$n) 

}

#Mann Whitney U test
#returns result of mann whitney test between floods and droughts predictability H8
mann_whitney = function(hz) {
  
  fl <- hz %>%
    filter(top_three_var == "floods") %>%
    select(c("H.8.", "top_three_var"))
  
  dr <- hz %>%
    filter(top_three_var == "droughts") %>%
    select(c("H.8.", "top_three_var"))
  
  
  print(wilcox.test(fl$H.8., dr$H.8., paired = FALSE))
  
  
}

#MAPS -------------------------------------------------------------------------------------------------------

# OVHZ TO REMOVE OR PAIR DOWN ---------------------------------------------
#the figure produced by the function below needs to be reproduced using
# just the event-level data set. Consider if it needs to be narrowed down
# to just the 30 60 time frames.

#function to create maps using the sample

maps = function(ovhz) {

  ovhz_coord <- full_join(coords, ovhz, by = "OWC") %>%
    mutate(count = case_when(
      OvHz.all.count_dq12 >= 0 & OvHz.all.count_dq12 <= 10 ~ 1, 
      OvHz.all.count_dq12 >= 11 & OvHz.all.count_dq12 <= 20 ~ 2,
      OvHz.all.count_dq12 >= 21 & OvHz.all.count_dq12 <= 30 ~ 3,
      OvHz.all.count_dq12 > 30 ~ 4
    ))

  #do this to create MAPS 
  world <- ne_countries(scale = "medium", returnclass = "sf")
  class(world)

  #map of all hazards
  samplemap <<- ggplot(data = world) +
    geom_sf(color = "lightgrey", fill = "lightgrey") +
    geom_point(data = ovhz_coord,
               aes(x = longitude,
                   y = latitude,
                   colour = as.factor(count)),
               size = 4,
               alpha = 1) +
    theme(axis.title.x =  element_blank(),
          axis.title.y =  element_blank()) +
    scale_size_continuous(
      range = c(3, 6),
      name = "Number of hazard events",
      labels = c("< 10", "11-20", "21-30",  "31+")
    )  +
    scale_color_manual(
      values = c("#90d743", "#35b779", "#31688e", "#440154"),
      name = "Number of hazard events",
      labels = c("< 10", "11-20", "21-30",  "31+")
    ) +
    theme(
      legend.position = "bottom",
      panel.background = element_rect(
        fill = "white",
        color = "white",
        size = 0.5
      ),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      plot.title = element_text(size = 18))
}


