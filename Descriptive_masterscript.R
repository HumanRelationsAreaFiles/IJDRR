# instructions to replicate all figures, analyses and supp materials in King et al., 
# An ethnographic approach to the global study of the ecological dimensions of hazards
# assumes access to GitHub














# initializing-----------------------------------------------------------------
# note: all operations in this script use the data file DT-hz-event-level-expanded-FA.csv
# before running the below script, run hz.initialize.R to generate the object "d"  
#           (among others) in your local R environment
#------------------------------------------------------------------------------



















# figures----------------------------------------------------------------------

# Figure 1 --------------------------------------------------------------------
      # call the following packages 
      library(dplyr)
      library(ggplot2)
      library(rnaturalearth)
      
      # read in data directly from csv files:
      counts <- read.csv("DT-hz-event-level-expanded-FA.csv")
      counts <- counts %>% filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.)) 
      counts <- cbind(EVENTID = 1:nrow(counts), counts) 
      
      #count the number of observations per unique ID
      events <- counts %>%
        filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.))
      events_per_soc <- events %>% distinct(OWC, EVENTID) %>%  count(OWC, name = "Number_of_hazards")
      eventscounter <- as.data.frame(events_per_soc) 
      #add the 14 societies not included in the filtered data set, with counts cases with 0 for "Number_of_events"; 4 cases are NA for frequency of events
      #note: this step is just for the first row, where we need to include all cases that are coded as 0/no events. 
      ghost_socstype <- data.frame(OWC = c("MQ08","MP19","RI03","MA11","AK05","OI20","OZ04","NE09","NE06","NS18","NS29","SQ18","SP26","SM04"), 
                                   Number_of_hazards = 0)
      eventsmap_df <- rbind(eventscounter, ghost_socstype)
      colnames(eventsmap_df) <- c("OWC", "Number_of_hazards")
      
      maptable <- data.frame(Number_hazard_events = c("0", "1-10", "11-20", "21-30", "31+"),
                             Map_color = c("Yellow", "Lightgreen", "Green", "Blue", "Puprple"),
                             Number_of_cases = c(14,66,28,14,6))
      
      maps = function(eventsmap_df) {
        
        ovhz_coord <- full_join(coords, eventsmap_df, by = "OWC") %>%
          mutate(count = case_when(
            Number_of_hazards == 0 ~ 0,
            Number_of_hazards >= 0 & Number_of_hazards <= 10 ~ 1, 
            Number_of_hazards >= 11 & Number_of_hazards <= 20 ~ 2,
            Number_of_hazards >= 21 & Number_of_hazards <= 30 ~ 3,
            Number_of_hazards > 30 ~ 4
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
                     size = 2,
                     alpha = 1) +
          theme(axis.title.x =  element_blank(),
                axis.title.y =  element_blank()) +
          scale_size_continuous(
            range = c(2, 7),
            name = "Number of hazard events",
            labels = c("0", "1-10", "11-20", "21-30",  "31+")
          )  +
          scale_color_manual(
            values = c("#FDE725FF", "#90d743", "#35b779", "#31688e", "#440154"),
            name = "Number of hazard events",
            labels = c("0", "1-10", "11-20", "21-30",  "31+")
          ) +
          theme(
            legend.position = "bottom",
            panel.background = element_rect(
              fill = "white",
              color = "white",
              size = 0.5
            ),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 18))
      }
      maps(eventsmap_df)
      #map of all hazards
      samplemap  


# Figure 2---------------------------------------------------------------------
  # use hz_msp_figure.R


# Figure 3---------------------------------------------------------------------
      # call packages if not done already
      library(dplyr)
      library(ggplot2)
      
      # read in data directly from csv files:
      counts <- read.csv("DT-hz-event-level-expanded-FA.csv")
      counts <- counts %>% filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.)) 
      counts <- cbind(EVENTID = 1:nrow(counts), counts) 

      #count the number of observations per unique ID
      events <- counts %>%
        filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.))
      events_per_id <- events %>% distinct(ID, EVENTID) %>%  count(ID, name = "Number_of_hazards")
      eventscounter <- as.data.frame(events_per_id) 
      #add the 18 societies not included in the filtered data set, with counts of 0 for "Number_of_events"
      #note: this step is just for the first row, where we need to include all societies with 0/no events. The other three rows can use a subset of the 132 society sample.
      ghost_socstype <- data.frame(ID = c(29,32,55,57,68,90,104,131,132,135,136,163,179,181), 
                                   Number_of_hazards = 0)
      events_df <- rbind(eventscounter, ghost_socstype)
      colnames(events_df) <- c("ID", "Number_of_hazards")
      
      #convert data to histogram format
      data421 <- events_df %>%
        group_by(Number_of_hazards) %>%                 
        summarise(`Number_of_societies` = n_distinct(ID)) %>% 
        ungroup()
      
      
      
      # Calculate mean and median of the x-axis (number of hazard events)
      mean_hazard <- mean(data421$Number_of_societies, na.rm = TRUE)
      median_hazard <- median(data421$Number_of_societies, na.rm = TRUE)
      # Turn them into dynamic labels
      mean_label <- paste("Mean -", round(mean_hazard, 1))
      median_label <- paste("Median -", median_hazard)
      
      
      
      # Create the plot
      fig421 <- ggplot(data421, aes(x = Number_of_hazards, y = Number_of_societies)) +
        geom_bar(stat = "identity", fill = "yellowgreen") +
        
        # Add vertical lines for mean and median, and map them to aesthetics for the legend
        geom_vline(aes(xintercept = mean_hazard, color = "Mean - 12.5", linetype = "Mean - 12.5"), size = 1) +
        geom_vline(aes(xintercept = median_hazard, color = "Median - 9", linetype = "Median - 9"), size = 1) +
        
        # Define colors and line types for the legend
        scale_color_manual(name = NULL, values = c("Mean - 12.5" = "darkgreen", "Median - 9" = "purple")) +
        scale_linetype_manual(name = NULL, values = c("Mean - 12.5" = "dashed", "Median - 9" = "solid")) +
        
        # Set axis labels
        labs(x = "Number of hazard events", y = "Number of societies") +
        
        # Customize the theme
        theme_minimal(base_size = 15) + 
        theme(legend.position = c(0.95, 0.95),
              legend.justification = c(1, 1))
      fig421


# Figure 4---------------------------------------------------------------------
      # write the necessary data to a data frame:
      hz_431 <- hz %>% filter(time %in% c(30,60)) %>%
        filter(!is.na(H.8.)) %>%
        mutate(H.5. = ifelse(H.5. == "PD" | H.5. =="PE"|H.5.=="CP"|H.5.=="Lo", "P&D", H.5.)) %>%
        mutate(H.5. = ifelse(H.5. != "Fl"& H.5. != "Dr" & H.5. != "P&D", "Z_OTHER", H.5.)) %>%
        mutate(H.8. = case_when(
          H.8. == 1 ~ "Unpredictable (1)",
          H.8. == 2 ~ "Moderately predictable (2)",
          H.8. == 3 ~ "Very predictable (3)",
          TRUE ~ as.character(H.8.)
        ))
      
      hz_431$H.8. <- factor(hz_431$H.8., levels = c("Unpredictable (1)", "Moderately predictable (2)", "Very predictable (3)"))
      
      
      # plot the actual graph: 
      ggplot(data = hz_431, aes(x = as.factor(H.8.), fill = factor(H.5.))) +
        geom_bar(position = "fill") +
        scale_fill_manual(
          values = c("#FDE725FF", "#287D8EFF", "#73D055FF", "#453781FF"),
          name = "Hazard type",
          labels = c("Drought", "Flood", "Pests and disease (P&D)", "Other")
        ) + labs(x = "Predictability", y = "Percent of events")  +
        
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


# Figure 5---------------------------------------------------------------------
      # write the following function:
      H7_H10_graphs = function(hz, var1, var1name) {
        
        #customizable graph for dimensions of hazards variables
        var1_bargraph <<- hz %>%
          filter(!is.na({{var1}})) %>%
          ggplot(aes(x = as.factor({{var1}}), fill = factor(top_three_var, levels = c("droughts", "floods", "pests", "all other")))) +
          geom_bar(position = "fill") +
          scale_fill_manual(
            values = c("#FDE725FF", "#287D8EFF", "#73D055FF", "#453781FF"),
            name = "Hazard type",
            labels = c("Drought", "Flood", "Pests and disease (P&D)", "Other")
          ) + labs(x = var1name, y = "Percent of events")  +
          
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

      # then write the necessary data to a df
      hz_441 <- hz %>% filter(time %in% c(30,60)) %>%
        mutate(H.5. = ifelse(H.5. == "PD" | H.5. =="PE"|H.5.=="CP"|H.5.=="Lo", "P&D", H.5.)) %>%
        mutate(H.5. = ifelse(H.5. != "Fl"& H.5. != "Dr" & H.5. != "P&D", "Z_OTHER", H.5.)) %>%
        filter(!is.na(H.7.)) %>%
        mutate(H.7. = as.character(H.7.)) %>%
        mutate(H.7. = ifelse(H.7. == 1, "Slow (1)", H.7.)) %>%  mutate(H.7. = ifelse(H.7. == 2, "Fast (2)", H.7.))
      hz_441$H.7. <- factor(hz_441$H.7., levels = c("Slow (1)", "Fast (2)"))
      
      # make the figure
      H7_H10_graphs(hz_441, H.7., "Onset")
      # and display it
      var1_bargraph


# end of figures section-------------------------------------------------------


      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      


# tables------------------------------------------------------------------------
      # call dplyr if not already called
      library(dplyr)
      
      #read in data
      meta <- read.csv("DT-meta-hz-clean.csv") 
      finaldata <- read.csv("DT-hz-event-level-expanded-FA.csv") 
      
      
# "sample" table----------------------------------------------------
      #first half of the table
      sample <- data.frame(
        OWCcount_full_dataset = length(unique(finaldata$OWC)),  #number of unique values in OWC
        OWCcount_full_dataset3060 = length(unique(finaldata$OWC[finaldata$time %in% c(30, 60)]))  #unique OWC values where time is 30 or 60
      )
      view(sample) # to get the values
      
      #second half
      meta_OWC <- unique(meta$OWC)
      finaldata_OWC <- unique(finaldata$OWC)
      OWC_time3060 <- unique(finaldata$OWC[finaldata$time != 90])
      missing_OWCs <- setdiff(meta_OWC, finaldata_OWC) #this concat is all the OWCs missing from the final dataset
      missing_OWCs3060 <- setdiff(meta_OWC, OWC_time3060)
      print(missing_OWCs) #to print the full list (for copying and pasting)
      print(missing_OWCs3060) #to print the full list (for copying and pasting)
      
      
      
      
# table 1 (4.2.1. - Event Freq and Type by Society)---------------------------------
      
      #row 1: event types
      types <- finaldata %>%
        filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.))
      types_per_id <- types %>% distinct(ID, H.5.) %>%  count(ID, name = "types_experienced")
      row1_114 <- as.data.frame(types_per_id) #put those counts into a df and run summary stats
      #add the 14 societies not included in the filtered data set, with counts of 0 for "Number_of_events"
      #note: this step is just for the first row, where we need to include all societies even though some have
      #0/no events. The other three rows can use a subset of the 132 society sample.
      ghost_socstype <- data.frame(ID = c(29,32,55,57,68,90,104,131,132,135,136,163,179,181), 
                                   types_experienced = 0)
      types_df <- rbind(row1_114, ghost_socstype)
      colnames(types_df) <- c("SCCS.ID", "Number_of_hztypes_experienced")
      type_freqs <- data.frame(
        Average = mean(types_df$Number_of_hztypes_experienced),
        Maximum = max(types_df$Number_of_hztypes_experienced),
        Minimum = min(types_df$Number_of_hztypes_experienced),
        Median = median(types_df$Number_of_hztypes_experienced),
        Standard_dev = sd(types_df$Number_of_hztypes_experienced),
        Society_count = c(128),
        Event_count = c(1575)
      )
      
      #row 2: all events
      all <- finaldata %>%
        filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.))
      counts_per_id <- table(all$ID) #get a count of hazards per society
      all_df114 <- as.data.frame(counts_per_id) #put those counts into a df and run summary stats
      #add the 14 societies not included in the filtered data set, with counts of 0 for "Number_of_events"
      #note: this step is just for the first row, where we need to include all societies even though some have
      #0/no events. The other three rows can use a subset of the 132 society sample.
      ghost_socs <- data.frame(Var1 = c(29,32,55,57,68,90,104,131,132,135,136,163,179,181), 
                               Freq = 0)
      all_df <- rbind(all_df114, ghost_socs)
      colnames(all_df) <- c("SCCS.ID", "Number_of_events")
      freqs_all <- data.frame(
        Average = mean(all_df$Number_of_events),
        Maximum = max(all_df$Number_of_events),
        Minimum = min(all_df$Number_of_events),
        Median = median(all_df$Number_of_events),
        Standard_dev = sd(all_df$Number_of_events),
        Society_count = c(128),
        Event_count = c(1575)
      )
      
      #row 3: droughts
      droughts <- finaldata %>%
        filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.) & H.5. %in% c("Dr"))
      droughts_per_id <- table(droughts$ID) #get a count of droughts per society
      droughts_df <- as.data.frame(droughts_per_id) #put those counts into a df and run summary stats
      colnames(droughts_df) <- c("SCCS.ID", "Number_of_events")
      freqs_dr <- data.frame(
        Average = mean(droughts_df$Number_of_events),
        Maximum = max(droughts_df$Number_of_events),
        Minimum = min(droughts_df$Number_of_events),
        Median = median(droughts_df$Number_of_events),
        Standard_dev = sd(droughts_df$Number_of_events),
        Society_count = length(unique(droughts$ID)),
        Event_count = sum(droughts_df$Number_of_events)
      )
      
      #row 4: floods
      floods <- finaldata %>%
        filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.) & H.5. %in% c("Fl"))
      floods_per_id <- table(floods$ID) #get a count of floods per society
      floods_df <- as.data.frame(floods_per_id) #put those counts into a df and run summary stats
      colnames(floods_df) <- c("SCCS.ID", "Number_of_events")
      freqs_fl <- data.frame(
        Average = mean(floods_df$Number_of_events),
        Maximum = max(floods_df$Number_of_events),
        Minimum = min(floods_df$Number_of_events),
        Median = median(floods_df$Number_of_events),
        Standard_dev = sd(floods_df$Number_of_events),
        Society_count = length(unique(floods$ID)),
        Event_count = sum(floods_df$Number_of_events)
      )
      
      #row 5: P&D
      PD <- finaldata %>%
        filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.) & H.5. %in% c("PE", "CP", "Lo", "PD")) %>%
        mutate(H.5. = case_when(H.5. %in% c("PD", "Lo", "CP", "PE") ~ "P&D", TRUE ~ H.5.))
      pd_per_id <- table(PD$ID) #get a count of P&D hazards per society
      pd_df <- as.data.frame(pd_per_id) #put those counts into a df and run summary stats
      colnames(pd_df) <- c("SCCS.ID", "Number_of_events")
      freqs_pd <- data.frame(
        Average = mean(pd_df$Number_of_events),
        Maximum = max(pd_df$Number_of_events),
        Minimum = min(pd_df$Number_of_events),
        Median = median(pd_df$Number_of_events),
        Standard_dev = sd(pd_df$Number_of_events),
        Society_count = length(unique(PD$ID)),
        Event_count = sum(pd_df$Number_of_events)
      )
      #each row is now saved as a separate data frame. combine into "table421"
      table421 <- bind_rows(type_freqs, freqs_all,freqs_dr,freqs_fl,freqs_pd)
      #and save
      writexl::write_xlsx(table421, "table1")
      
      
      
      
      
      
      
      
      
# table 3 (4.6.1. - Significant Corrs Across H8, H7, H9)-----------------------------
      data461 <- finaldata %>%
        filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.)) %>%
        mutate(H.9.a. = ifelse(H.9.a. == 0.5, 1.0, H.9.a.))
      
      h9arhoNODR <- data461 %>% filter(!H.5. %in% c("Dr"))
      
      getrho <- function(df, col1, col2) { #make a function that outputs the rho, p-val and event count 
        x <- df[[col1]]
        y <- df[[col2]]
        test <- cor.test(x, y, method = "spearman", use = "complete.obs")
        rho_value <- test$estimate
        p_value <- test$p.value
        n_observations <- sum(complete.cases(x, y))
        result <- list(
          rho_value = rho_value,
          p_value = p_value,
          n_observations = n_observations
        )
      }
      
      row1 <- getrho(data461, "H.8.", "H.9.a.")
      row2 <- getrho(data461, "H.8.", "H.9.b.")
      row3 <- getrho(data461, "H.8.", "H.9.c.")
      row4 <- getrho(data461, "H.8.", "H.9.d.")
      row5 <- getrho(data461, "H.8.", "H.10.")
      row6 <- getrho(data461, "H.7.", "H.9.a.")
      row7 <- getrho(h9arhoNODR, "H.7.", "H.9.a.")
      row8 <- getrho(data461, "H.7.", "H.9.d.")
      row9 <- getrho(data461, "H.7.", "H.10.")
      
      table461 <- data.frame(Vars = c("H8_H9a","H8_H9b","H8_H9c","H8_H9d","H8_H10",
                                      "H7_H9a","H7_H9aNODR","H7_H9d","H7_H10"),
                             Rho = c(row1$rho_value, row2$rho_value, row3$rho_value, row4$rho_value, row5$rho_value, 
                                     row6$rho_value, row7$rho_value, row8$rho_value, row9$rho_value),
                             p = c(row1$p_value, row2$p_value, row3$p_value, row4$p_value, row5$p_value, 
                                   row6$p_value, row7$p_value, row8$p_value, row9$p_value),
                             hz_count = c(row1$n_observations, row2$n_observations, row3$n_observations, 
                                          row4$n_observations, row5$n_observations, row6$n_observations, 
                                          row7$n_observations, row8$n_observations, row9$n_observations)
      )
      writexl::write_xlsx(table461, "table3")
      
      

# table 4 (4.6.3. - Duration of Dated Events)
      dated3060 <- finaldata %>% filter(time %in% c(30, 60) 
                                        & !H.2.a. %in% c("GEN")
                                        & !H.2.b. %in% c("GEN"))
      #for the box in table 4.6.3 labeled "Number of dated events in time30+time60",  
      #use the value of the number of observations in this data frame (dated3060).
      #For "% of dated events in time30+time60", divide 
      #that count by the number of obs. in the df "all" (should be 1424)
      
      data463 <- dated3060 %>%  filter(!H.12. %in% c(1, 2) & !is.na(H.12.))
      data463_205 <- finaldata %>% filter(time %in% c(30, 60) & !H.2.a. %in% c("GEN"))
      data463GEN <- finaldata %>%  filter(time %in% c(30, 60) &
                                            H.2.a. %in% c("GEN") &
                                            H.2.b. %in% c("GEN"))
      data463ALL <- finaldata %>%  filter(time %in% c(30, 60))
      
      fast <- data463 %>% filter(H.7. %in% c(2)) %>%
        mutate(Duration = as.numeric(H.2.b.) - as.numeric(H.2.a.))
      slow <- data463 %>% filter(H.7. %in% c(1)) %>%
        mutate(Duration = as.numeric(H.2.b.) - as.numeric(H.2.a.))
      data463dr <- slow %>% filter(H.5.%in% c("Dr")) 
      data463nodr <- slow %>% filter(!H.5. %in% c("Dr"))
      data463NAs <- data463 %>% filter(is.na(H.7.)) %>%
        mutate(Duration = as.numeric(H.2.b.) - as.numeric(H.2.a.))
      data463onlystart <- finaldata %>% filter(time %in% c(30, 60) & !H.2.a. %in% c("GEN")
                                               & H.2.b. %in% c("GEN"))
      
      sumstats <- function(df) {
        data.frame(
          Event_count = nrow(df),
          Average = mean(df$Duration, na.rm = TRUE),
          Max = max(df$Duration, na.rm = TRUE),
          Median = median(df$Duration, na.rm = TRUE),
          Min = min(df$Duration, na.rm = TRUE),
          SD = sd(df$Duration, na.rm = TRUE)
        )
      }
      
      fast_stats <- sumstats(fast)
      slow_stats <- sumstats(slow)
      na_stats <- sumstats(data463NAs)
      data463dr_stats <- sumstats(data463dr)
      data463nodr_stats <- sumstats(data463nodr)
      data463onlystart_stats <- data.frame(Event_count = 6, Average = NA, Max = NA, Median = NA, Min = NA, SD = NA)
      
      # Combine the summary statistics into a single data frame
      table463 <- bind_rows(
        fast_stats,
        slow_stats,
        na_stats,
        data463dr_stats,
        data463nodr_stats,
        data463onlystart_stats,
        .id = "DataFrame"
      )
      table463$DataFrame <- c("fast", "slow", "NA", "data463dr", "data463nodr", "startdate_only")
      #all results are now saved to the df "table463"
      writexl::write_xlsx(table463, "table4")
      
      #for the addendum to table 4.6.3 (add means):
      table463BONUS <- data.frame(Average_for = c("GEN_only", "Dated_only", "All_events"),
                                  H.8. = c(mean(data463GEN$H.8., na.rm = TRUE), 
                                           mean(data463_205$H.8., na.rm = TRUE), 
                                           mean(data463ALL$H.8., na.rm = TRUE)),
                                  H.9.a. = c(mean(data463GEN$H.9.a., na.rm = TRUE), 
                                             mean(data463_205$H.9.a., na.rm = TRUE), 
                                             mean(data463ALL$H.9.a., na.rm = TRUE)),
                                  H.9.b. = c(mean(data463GEN$H.9.b., na.rm = TRUE), 
                                             mean(data463_205$H.9.b., na.rm = TRUE), 
                                             mean(data463ALL$H.9.b., na.rm = TRUE)),
                                  H.9.c. = c(mean(data463GEN$H.9.c., na.rm = TRUE), 
                                             mean(data463_205$H.9.c., na.rm = TRUE), 
                                             mean(data463ALL$H.9.c., na.rm = TRUE)),
                                  H.9.d. = c(mean(data463GEN$H.9.d., na.rm = TRUE), 
                                             mean(data463_205$H.9.d., na.rm = TRUE), 
                                             mean(data463ALL$H.9.d., na.rm = TRUE)),
                                  H.10. = c(mean(data463GEN$H.10., na.rm = TRUE), 
                                            mean(data463_205$H.10., na.rm = TRUE), 
                                            mean(data463ALL$H.10., na.rm = TRUE)))
      writexl::write_xlsx(table463BONUS, "table4_bonus")
      
      
# end of tables section-----------------------------------------------

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      


# supplementary materials------------------------------------------------------
      # again, call dplyr if not already called
      library(dplyr)
      
      #read in data directly from csv
      finaldata <- read.csv("DT-hz-event-level-expanded-FA.csv")
      
      
      
      
      
# table S4. (hazard types)------------------------------------------------------
      data.s4 <- finaldata %>%
        filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.)) %>%
        mutate(H.5. = case_when(H.5. %in% c("PD", "Lo", "CP", "PE") ~ "P&D", TRUE ~ H.5.))
      table.s4 <- data.s4 %>%
        group_by(H.5.) %>%
        summarise(Total_events = n(),
                  Percent_total_ev = (Total_events / 1424) * 100,
                  Societies = n_distinct(ID), 
                  Percent_total_soc = (Societies / 132) * 100) %>% 
        arrange(desc(Total_events)) %>%  
        mutate(rank = row_number())      
      
      PDspecifics <- data.frame(H.5. = c("P&D_CP","P&D_Lo","P&D_PD","P&D_PE"),
                                Total_events = c(48,24,7,98),
                                Percent_total_ev = c(3.370787,1.685393,
                                                     0.491573,6.882022),
                                Societies = c(10,8,4,25),
                                Percent_total_soc = c(7.575758,6.060606,
                                                      3.030303,18.93939),
                                rank = c(3,3,3,3))
      
      table.s4 <- bind_rows(table.s4, PDspecifics)
      
      writexl::write_xlsx(table.s4, "S4")
      
      
      
      
# table S6. (description of hazard events)
      #make the subsets for each row
      rowlist <- list(
        row1 <- finaldata,
        row2 <- finaldata %>% filter(is.na(H.12.)),
        row3 <- finaldata %>% filter(H.12. %in% c(1)),
        row4 <- finaldata %>% filter(H.12. %in% c(2)),
        row5 <- finaldata %>% filter(!H.12. %in% c(1, 2) & !is.na(H.12.)),
        
        row6 <- row5 %>% filter(time %in% c(30)),
        row7 <- finaldata %>% filter(time %in% c(60)),
        row8 <- finaldata %>% filter(time %in% c(90)),
        row9 <- finaldata %>% filter(time %in% c(30)) %>% filter(H.2.a. %in% c("GEN")),
        row10 <- finaldata %>% filter(time %in% c(30)) %>% filter(!H.2.a. %in% c("GEN") & !H.2.b. %in% c("GEN")),
        row11 <- finaldata %>% filter(time %in% c(60)) %>% filter(H.2.a. %in% c("GEN")),
        row12 <- finaldata %>% filter(time %in% c(60)) %>% filter(!H.2.a. %in% c("GEN") & !H.2.b. %in% c("GEN")),
        row13 <- finaldata %>% filter(time %in% c(90)) %>% filter(H.2.a. %in% c("GEN")),
        row14 <- finaldata %>% filter(time %in% c(90)) %>% filter(!H.2.a. %in% c("GEN") & !H.2.b. %in% c("GEN")),
        row15 <- finaldata %>% filter(H.2.a. %in% c("GEN")) %>% filter(time %in% c(30,60,90)),
        row16 <- finaldata %>% filter(!H.2.a. %in% c("GEN") & !H.2.b. %in% c("GEN")) %>% filter(time %in% c(30,60,90))
      )
      
      Counts1 <- c(nrow(row1), nrow(row2),nrow(row3),nrow(row4),nrow(row5))
      Counts2 <- c(nrow(row5), nrow(row15),nrow(row16), nrow(row6),nrow(row9),
                   nrow(row10), nrow(row7),nrow(row11),nrow(row12),nrow(row8),
                   nrow(row13), nrow(row14))
      Societies <- c(n_distinct(row1$ID), n_distinct(row2$ID), n_distinct(row3$ID), 
                     n_distinct(row4$ID), n_distinct(row5$ID), n_distinct(row5$ID), 
                     n_distinct(row15$ID), n_distinct(row16$ID), n_distinct(row6$ID), 
                     n_distinct(row9$ID), n_distinct(row10$ID), n_distinct(row7$ID), 
                     n_distinct(row11$ID), n_distinct(row12$ID), n_distinct(row8$ID),
                     n_distinct(row13$ID), n_distinct(row14$ID))
      
      
      table.s6 <- data.frame(Subset = c("Total_dataset", "NA_events","Absent-rare_events",
                                        "Threats","All-occurred","All-occurred_subsample","Subsample_gen","Subsample_dated",
                                        "time30all","30_gen","30_dated","time60all","60gen","60dated","time90all","90gen", "90dated"),
                             Count = c(Counts1, Counts2), Percent_all = c(((Counts1/1575)*100), ((Counts2/1504)*100)), Num_soc = Societies, 
                             Percent_socs = c((Societies/132)*100))
      writexl::write_xlsx(table.s6,"S6")
      
      

# table S7. (Missing Data by Dimension)-----------------------------------------
      data.s7 <- finaldata %>% 
        filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.))
      cols_of_interest <- c("H.7.", "H.8.", "H.9.a.", "H.9.b.", "H.9.c.", "H.9.d.", "H.10.")
      coded_obs <- colSums(!is.na(data.s7[cols_of_interest])) #calculate number of non-na observations for each col
      na_obs <- colSums(is.na(data.s7[cols_of_interest])) #and number of na observations
      totals <- coded_obs + na_obs
      percent <- (na_obs / totals)*100
      
      table.s7 <- data.frame(Var = cols_of_interest, 
                             Coded_observations_count = coded_obs, 
                             Missing_observations_count = na_obs,
                             Percent_observations_missing = percent)
      writexl::write_xlsx(table.s7, "tables7")
      
      
# table S8. (Correlations between Hz types)-------------------------------------
      # start by calling Hmisc if not already
      library(Hmisc)
      
      #then read in data
      hz_type <-  d %>% select(c(1,12))
      #omit all types with less than 30 observation
      # Count the number of OWCs for each event type
      event_counts <- hz_type %>%
        group_by(H.5.) %>%
        summarise(OWC_count = n()) %>%
        filter(OWC_count >= 30)
      
      # Filter the original dataframe to include only those event types
      hz_type <- hz_type %>%
        filter(H.5. %in% event_counts$H.5.)
      
      presence_matrix <- hz_type %>%
        distinct() %>%  # Remove duplicates
        mutate(presence = 1) %>%
        pivot_wider(names_from = H.5., values_from = presence, values_fill = list(presence = 0))
      
      
      
      # Remove the OWC column for correlation calculation
      correlation_data <- presence_matrix %>% select(-OWC)
      
      #  put in order
      desired_order <- c("Dr", "Ea", "ER", "Fi", "Fl", "GsEp", "Hu", "PD", "SEW", "SS", "Th", "Wi")  # Update this list as needed
      
      correlation_data <- correlation_data[, desired_order]
      
      # correlation matrix and p-values
      correlation_results <- rcorr(as.matrix(correlation_data), type="spearman")
      
      
      # Extract correlation coefficients and p-values
      correlation_matrix <- correlation_results$r
      p_value_matrix <- correlation_results$P
      Rhos <- as.data.frame(as.table(correlation_matrix))
      Ps <- as.data.frame(as.table(p_value_matrix))
      table.s8 <- cbind(Rhos, Ps$Freq)
      names(table.s8) <- c("Hz type 1", "Hz type 2", "Rho", "p-value")
      #remove duplicates
      table.s8 <- table.s8[!duplicated(t(apply(table.s8[, c("Hz type 1", "Hz type 2")], 1, sort))), ]
      table.s8 <- table.s8[!(table.s8[["Hz type 1"]] == "SEW" & table.s8[["Hz type 2"]] == "SS" | 
                               table.s8[["Hz type 1"]] == "SS" & table.s8[["Hz type 2"]] == "SEW"), ]
      table.s8 <- table.s8[table.s8$`Hz type 1` != table.s8$`Hz type 2`, ]
      table.s8 <- table.s8[order(table.s8$`p-value`), ]
      writexl::write_xlsx(table.s8, "S8")
      
      
      
     
      
      
# table S9. (predictability of Hz by type)-------------------------------------
      data.s9 <- finaldata %>% filter(time %in% c(30,60)) %>%
        mutate(H.5. = ifelse(H.5. %in% c("PE", "CP", "Lo", "PD"), "P&D", H.5.))
      
      #get flat counts
      table.s9 <- data.s9 %>%
        group_by(H.5.) %>%
        summarize(
          total_count = n(),
          #unpredictable
          Unpred = sum(H.8. == 1, na.rm = TRUE),
          percent1 = (Unpred / total_count) * 100,
          #moderately predictable
          Mod = sum(H.8. == 2, na.rm = TRUE),
          percent2 = (Mod / total_count) * 100,
          #very predictable
          Very = sum(H.8. == 3, na.rm = TRUE),
          percent3 = (Very / total_count) * 100,
          #NA 
          NA_count = sum(is.na(H.8.)),
          percentNA = (NA_count / total_count) * 100,
          Mean = mean(c(rep(1, Unpred), rep(2, Mod), rep(3, Very))),
          Max = max(c(rep(1, Unpred), rep(2, Mod), rep(3, Very))),
          Median = median(c(rep(1, Unpred), rep(2, Mod), rep(3, Very))),
          Min = min(c(rep(1, Unpred), rep(2, Mod), rep(3, Very))),
          SD = sd(c(rep(1, Unpred), rep(2, Mod), rep(3, Very)))
        ) %>%
        rename(Hz = H.5.)

      #add a row with all the totals
      total_unpred <- sum(table.s9$Unpred)
      total_mod <- sum(table.s9$Mod)
      total_very <- sum(table.s9$Very)
      grandtotal <- sum(table.s9$total_count)
      
      longform_data <- c(
        rep(1, total_unpred),
        rep(2, total_mod),
        rep(3, total_very)
      )
      
      totals <- tibble(Hz = c("total"),
                       total_count = grandtotal,
                       Unpred = total_unpred,
                       percent1 = (100*(total_unpred / grandtotal)),
                       Mod = total_mod,
                       percent2 = (100*(total_mod / grandtotal)),
                       Very = total_very,
                       percent3 = (100*(total_very / grandtotal)),
                       Mean = mean(longform_data),
                       Max = max(longform_data),
                       Median = median(longform_data),
                       Min = min(longform_data),
                       SD = sd(longform_data)
      )
      
      table.s9 <- bind_rows(table.s9, totals)
      
      writexl::write_xlsx(table.s9, "S9")
      
      
      
      
#table S9b. (Wilcoxon rank sums test for floods vs. droughts)-------------------
      s9bfl <- finaldata %>% filter(time %in% c(30,60) & H.5. %in% c("Fl"))
      s9bdr <- finaldata %>% filter(time %in% c(30,60) & H.5. %in% c("Dr"))
      
      #part 1: calculate the mean ranks
      ranks_Dr <- rank(c(s9bdr$H.8.))
      sum_ranks_Dr <- sum(ranks_Dr)
      mean_ranks_Dr <- mean(ranks_Dr)
      
      ranks_Fl <- rank(c(s9bfl$H.8.))
      sum_ranks_Fl <- sum(ranks_Fl)
      mean_ranks_Fl <- mean(ranks_Fl)
      
      table.s9b1 <- data.frame(Hz_type = c("Dr", "Fl", "Total"), Num_events =
                                 c(293,220,513), Mean_rank = c(mean_ranks_Dr,mean_ranks_Fl,NA), sum_ranks = c(sum_ranks_Dr,sum_ranks_Fl,NA))
      writexl::write_xlsx(table.s9b1, "S9b-1")
      
      #part 2: calculate the actual U and W scores
      combined_data <- c(s9bdr$H.8., s9bfl$H.8.)
      group_indicator <- c(rep("Dr", length(s9bdr$H.8.)), rep("Fl", length(s9bfl$H.8.)))
      #calculate ranks for the combined data
      combined_data <- data.frame(H8 = combined_data, type = group_indicator)
      W_score <- wilcox.test(H8 ~ type, data = combined_data, exact = FALSE)
      #the W values can now be printed and copy-pasted into the table:
      print(W_score$statistic)
      print(W_score$p.value)
      
      n1 <- sum(combined_data$type == "Dr") #for the U score
      n2 <- sum(combined_data$type == "Fl")
      U <- W_score$statistic - (n1 * (n1 + 1)) / 2
      
      mu_U <- (n1 * n2) / 2 #and then the Z score
      sigma_U <- sqrt((n1 * n2 * (n1 + n2 + 1)) / 12)
      Z <- (U - mu_U) / sigma_U
      table.s9b2 <- data.frame(Stat = c("U","W","Z","p"), Value = c(U, W_score$statistic,
                                                                    Z, W_score$p.value))
      writexl::write_xlsx(table.s9b2, "S9b-2")
      
      #part 3: finally, calculate general summary stats for H8
      dr <- c(s9bdr$H.8.)
      fl <- c(s9bfl$H.8.)
      table.s9b3 <- data.frame(Hz_type = c("Droughts", "Floods"), Num_events = c(nrow(s9bdr),nrow(s9bfl)),
                               Mean = c(mean(dr, na.rm = TRUE), mean(fl, na.rm = TRUE)), 
                               Min = c(min(dr, na.rm = TRUE), min(fl, na.rm = TRUE)), 
                               Median = c(median(dr, na.rm = TRUE), median(fl, na.rm = TRUE)), 
                               Max = c(max(dr, na.rm = TRUE), max(fl, na.rm = TRUE)), 
                               SD = c(sd(dr, na.rm = TRUE), sd(fl, na.rm = TRUE)))
      writexl::write_xlsx(table.s9b3,"S9b-3")
      
      #the 3 components of table S9b are now saved to data frames called table.s9b1,
      #table.s9b2 and table.s9b3
      
      
#table S10 (frequency of hazards by onset (fast vs slow - H7 = 2 vs H7 = 1)--------
      data.s10FAST <- finaldata %>%  filter(time %in% c(30, 60)) %>%
        mutate(H.5. = ifelse(H.5. %in% c("PE", "CP", "Lo", "PD"), "P&D", H.5.)) %>%
        filter(H.7. %in% c(2))
      data.s10SLOW <- finaldata %>%  filter(time %in% c(30, 60)) %>%
        mutate(H.5. = ifelse(H.5. %in% c("PE", "CP", "Lo", "PD"), "P&D", H.5.)) %>%
        filter(H.7. %in% c(1))
      
      table.s10FAST <- data.s10FAST %>%
        group_by(H.5.) %>%
        summarise(n = n(),
                  Percent_total = (n / 893) * 100) %>%  
        arrange(desc(n)) %>%
        mutate(rank = row_number())
      
      table.s10SLOW <- data.s10SLOW %>%
        group_by(H.5.) %>%
        summarise(n = n(),
                  Percent_total = (n / 436) * 100) %>%  
        arrange(desc(n)) %>%
        mutate(rank = row_number())
      
      writexl::write_xlsx(table.s10FAST, "S10_pt1")
      writexl::write_xlsx(table.s10SLOW, "S10_pt2")
      
      
# table S11 (severity of top Hz types)------------------------------------------
      only.5h9a <- nrow(finaldata %>% filter(H.9.a. %in% c(0.5))) #get the number of events coded 0.5 for H.9.a.(answer: 183)
      data.s11 <- finaldata %>% filter(time %in% c(30,60)) %>%
        mutate(H.9.a. = ifelse(H.9.a. == 0.5, 1.0, H.9.a.))
      droughts.s11 <- data.s11 %>% filter(H.5. %in% c("Dr"))
      floods.s11 <- data.s11 %>% filter(H.5. %in% c("Fl"))
      PD.s11 <- data.s11 %>%   mutate(H.5. = ifelse(H.5. %in% c("PE", "CP", "Lo", 
                                                                "PD"), "P&D", H.5.)) %>%
        filter(H.5. %in% c("P&D"))
      
      #write a new function to summarize each variable
      generate_summary <- function(data, df_name) {
        relevant_vars <- c("H.10.", "H.9.a.", "H.9.b.", "H.9.c.", "H.9.d.")
        summary_data <- lapply(relevant_vars, function(var) {
          total_NAs <- sum(is.na(data[[var]])) #get a count of the NAs before filtering them out
          values <- data[[var]][!is.na(data[[var]])] #filter out NAs for given variable
          value_counts <- as.data.frame(table(factor(values, levels = c(1, 1.5, 2, 2.5, 3, 3.5, 4))))
          colnames(value_counts) <- c("Value", "Count") #generate counts for each unique value
          total <- sum(value_counts$Count, na.rm = TRUE)
          total_sev <- sum(value_counts$Count[as.numeric(as.character(value_counts$Value)) >= 3], na.rm = TRUE)
          percent_sev <- (total_sev / total) * 100
          #combine the results for the given variable
          result <- data.frame(t(value_counts$Count), total_NAs, total, total_sev, percent_sev)
          rownames(result) <- var
          return(result)
        })
        
        # Bind rows of summary data for each variable
        summary_data <- do.call(rbind, summary_data)
        
        # Rename columns to match unique value names for clarity
        colnames(summary_data)[1:7] <- paste0("Count_", c(1, 1.5, 2, 2.5, 3, 3.5, 4))
        
        # Add a column indicating the source data frame name
        summary_data$source_df <- df_name
        
        return(summary_data)
      }
      
      #run the function on each subset of data
      summary_all <- generate_summary(data.s11, "s11.all")
      summary_droughts <- generate_summary(droughts.s11, "droughts.s11")
      summary_floods <- generate_summary(floods.s11, "floods.s11")
      summary_PD <- generate_summary(PD.s11, "PD.s11")
      
      
      #combine all summaries into one data frame
      table.s11 <- bind_rows(summary_all, summary_droughts, summary_floods, summary_PD)
      
      #add summary stats
      values <- c(1, 1.5, 2, 2.5, 3, 3.5, 4)
      
      calculate_stats <- function(row) {
        # Repeat each value according to the corresponding count in the row
        expanded_data <- unlist(mapply(rep, values, row))
        
        mean_value <- mean(expanded_data)
        min_value <- min(expanded_data)
        median_value <- median(expanded_data)
        max_value <- max(expanded_data)
        sd_value <- sd(expanded_data)
        
        return(c(Mean = mean_value, Min = min_value, Median = median_value, Max = max_value, SD = sd_value))
      }
      
      stats <- t(apply(table.s11[, c("Count_1", "Count_1.5", "Count_2", "Count_2.5", "Count_3", "Count_3.5", "Count_4")], 1, calculate_stats))
      
      table.s11$Mean <- stats[, "Mean"]
      table.s11$Min <- stats[, "Min"]
      table.s11$Median <- stats[, "Median"]
      table.s11$Max <- stats[, "Max"]
      table.s11$SD <- stats[, "SD"]
      
      #save it all to an excel file
      writexl::write_xlsx(table.s11, "S11")     
      
      
      
      
      
      
# table S12. (complete correlations across dimensions)---------------------------
      data.s12 <-  finaldata %>% filter(time %in% c(30,60)) %>%
        mutate(H.9.a. = ifelse(H.9.a. == 0.5, 1.0, H.9.a.))
      data.s12NODR <- data.s12 %>% filter(!H.5. %in% c("Dr"))
      
      #make sure you have a function to calculate rho and p-values
      getrho <- function(df, col1, col2) {  
        x <- df[[col1]]
        y <- df[[col2]]
        test <- cor.test(x, y, method = "spearman", use = "complete.obs")
        rho_value <- test$estimate
        p_value <- test$p.value
        n_observations <- sum(complete.cases(x, y))
        result <- list(
          rho_value = rho_value,
          p_value = p_value,
          n_observations = n_observations
        )
      }
      
      
      table.s12 <- do.call(rbind, list(getrho(data.s12, "H.7.","H.8."), 
                                       getrho(data.s12, "H.7.","H.9.a."),
                                       getrho(data.s12NODR, "H.7.","H.9.a."),
                                       getrho(data.s12, "H.7.","H.9.b."),
                                       getrho(data.s12, "H.7.","H.9.c."),
                                       getrho(data.s12, "H.7.","H.9.d."),
                                       getrho(data.s12, "H.7.","H.10."),
                                       
                                       getrho(data.s12, "H.8.","H.9.a."),
                                       getrho(data.s12, "H.8.","H.9.b."),
                                       getrho(data.s12, "H.8.","H.9.c."),
                                       getrho(data.s12, "H.8.","H.9.d."),
                                       getrho(data.s12, "H.8.","H.10."),
                                       
                                       getrho(data.s12, "H.9.a.","H.9.b."),
                                       getrho(data.s12, "H.9.a.","H.9.c."),
                                       getrho(data.s12, "H.9.a.","H.9.d."),
                                       getrho(data.s12, "H.9.a.","H.10."),
                                       
                                       getrho(data.s12, "H.9.b.","H.9.c."),
                                       getrho(data.s12, "H.9.b.","H.9.d."),
                                       getrho(data.s12, "H.9.b.","H.10."),
                                       
                                       getrho(data.s12, "H.9.c.","H.9.d."),
                                       getrho(data.s12, "H.9.c.","H.10."))) %>%
        as.data.frame()
      colnames(table.s12) <- c("Rho", "p", "Total_events")
      table.s12$Rho <- sapply(table.s12$Rho, as.numeric)
      table.s12$p <- sapply(table.s12$p, as.numeric)
      table.s12$Total_events <- sapply(table.s12$Total_events, as.numeric)
      
      writexl::write_xlsx(table.s12, "S12")

      


# table S13. (Correlations by Hz Type)------------------------------------------
      data.s13 <- finaldata %>%
        filter(time %in% c(30, 60) & !H.12. %in% c(1, 2) & !is.na(H.12.)) %>%
        mutate(H.9.a. = ifelse(H.9.a. == 0.5, 1.0, H.9.a.))
      s13_dr <- data.s13 %>% filter(H.5. %in% c("Dr"))
      s13_fl <- data.s13 %>% filter(H.5. %in% c("Fl"))
      s13_pd <- data.s13 %>% filter(H.5. %in% c("PD","PE","CP","Lo")) %>%
        mutate(H.5. = case_when(H.5. %in% c("PD", "Lo", "CP", "PE") ~ "P&D", TRUE ~ H.5.))
      
      getrho <- function(df, col1, col2) { #make a function that outputs the rho, p-val and event count 
        x <- df[[col1]]
        y <- df[[col2]]
        test <- cor.test(x, y, method = "spearman", use = "complete.obs")
        rho_value <- test$estimate
        p_value <- test$p.value
        n_observations <- sum(complete.cases(x, y))
        result <- list(
          rho_value = rho_value,
          p_value = p_value,
          n_observations = n_observations
        )
      }
      
      #make the rows
      h9a <- getrho(s13_dr, "H.8.", "H.9.a.")
      h9b <- getrho(s13_dr, "H.8.", "H.9.b.")
      h9c <- getrho(s13_dr, "H.8.", "H.9.c.")
      h9d <- getrho(s13_dr, "H.8.", "H.9.d.")
      h10 <- getrho(s13_dr, "H.8.", "H.10.")
      
      h9afl <- getrho(s13_fl, "H.8.", "H.9.a.")
      h9bfl <- getrho(s13_fl, "H.8.", "H.9.b.")
      h9cfl <- getrho(s13_fl, "H.8.", "H.9.c.")
      h9dfl <- getrho(s13_fl, "H.8.", "H.9.d.")
      h10fl <- getrho(s13_fl, "H.8.", "H.10.")
      
      h9apd <- getrho(s13_pd, "H.8.", "H.9.a.")
      h9bpd <- getrho(s13_pd, "H.8.", "H.9.b.")
      h9cpd <- getrho(s13_pd, "H.8.", "H.9.c.")
      h9dpd <- getrho(s13_pd, "H.8.", "H.9.d.")
      h10pd <- getrho(s13_pd, "H.8.", "H.10.")
      
      table.s13 <- data.frame(Var = c("H9a_Dr", "H9b_Dr", "H9c_Dr", "H9d_Dr", "H10_Dr", 
                                      "H9a_Fl", "H9b_Fl", "H9c_Fl", "H9d_Fl", "H10_Fl",
                                      "H9a_PD", "H9b_PD", "H9c_PD", "H9d_PD", "H10_PD"), 
                              Rho = c(h9a$rho_value, h9b$rho_value, h9c$rho_value, 
                                      h9d$rho_value, h10$rho_value, h9afl$rho_value, h9bfl$rho_value, h9cfl$rho_value, 
                                      h9dfl$rho_value, h10fl$rho_value, h9apd$rho_value, h9bpd$rho_value, h9cpd$rho_value, 
                                      h9dpd$rho_value, h10pd$rho_value),
                              P = c(h9a$p_value, h9b$p_value, h9c$p_value, h9d$p_value, h10$p_value,
                                    h9afl$p_value, h9bfl$p_value, h9cfl$p_value, h9dfl$p_value, h10fl$p_value,
                                    h9apd$p_value, h9bpd$p_value, h9cpd$p_value, h9dpd$p_value, h10pd$p_value),
                              Number_of_events = c(h9a$n_observations, h9b$n_observations, h9c$n_observations,
                                                   h9d$n_observations, h10$n_observations, 
                                                   h9afl$n_observations, h9bfl$n_observations, h9cfl$n_observations,
                                                   h9dfl$n_observations, h10fl$n_observations,
                                                   h9apd$n_observations, h9bpd$n_observations, h9cpd$n_observations,
                                                   h9dpd$n_observations, h10pd$n_observations))
      writexl::write_xlsx(table.s13, "S13")
      
      
      
# Figure S1--------------------------------------------------------------------
      # call tidyverse if not already called
      library(tidyverse)
      
      # read in the data directly from csv format
      haz <- read.csv("DT-hz-event-level-expanded-FA.csv")
      str(haz)
      haz <- haz %>%
        mutate(H.5. = str_replace_all(H.5., "\\s", ""))
      
      haz_f <- haz %>%
        filter(
          time %in% c(30, 60), 
          # !is.na(H.12.), 
          H.5. %in% c("PE", "CP", "Lo", "PD") #,
          #!is.na(H.9.d.), 
          # !is.na(H.8.)
        )
      
      haz_f$H.5. <- factor(haz_f$H.5., levels = sort(unique(haz_f$H.5.)))
      
      
      # make the graph object
      pd_scat_plot <- haz_f %>%
        ggplot(aes(x = H.9.d., y = H.8., color = H.5.)) + # Use color instead of fill
        geom_point() +
        geom_jitter(width = 0.35, height = 0.4) +
        scale_color_manual( # Use scale_color_manual for color aesthetic
          values = c("#FDE725FF", "#73D055FF", "#287D8EFF", "#453781FF"),
          name = "P&D type (H.5.)",
          labels = c("Livestock epidemic (CP)", "Locust infestation (Lo)", "Plant disease (PD)", "Pest animals (PE)"),
          guide = guide_legend(override.aes = list(size = 3)) # Adjust size here
        ) +
        scale_y_continuous(
          breaks = c(1, 2, 3) # Specify y-axis breaks
        ) +
        labs(x = "Infrastructure destroying (H.9.d.)", y = "Predictability (H.8.)") +
        theme_bw() +
        theme(
          axis.title = element_text(size = 18), # X-axis title size
          axis.text = element_text(size = 16), # X-axis title size
          legend.title = element_text(size = 18), # Legend title size
          legend.text = element_text(size = 16)  # Legend text size
        )
      
      # view graph
      pd_scat_plot
      
# end of supp materials section------------------------------------------------
      
      
      
      
      
      
      
      
      
      
