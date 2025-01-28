#function calls from hz.functions
#input any event level dataset for hz 
# hz <- entire dataset
# hz_freq_subset <- entire dataset with only occurred events (no threats)
# hz_no90 <- entire dataset with only occurred events (no threat) in 30, 60 time periods (no 90 time period)
# time30.df / time60.df / time90/df <- datasets with only occurred events in either the 30, 60, or 90 time period
# top_three_subset <- dataset with only droughts, floods, pests in 30,60,90 time period. threats included. 


# OVHZ TO REMOVE ----------------------------------------------------------
#input any society level dataset for ovhz
# ovhz <- entire dataset
# ovhz30 / ovhz60 / ovhz90 <- society level datasets for either the 30, 60, or 90 time period

#--------------------------------------------------------------------------------------------------------------------

#basic_table_data = function(hz)
  #parameters:
  #hz (dataframe) input an event-level dataframe
basic_table_data(hz_no90)
  #summary of all events (threats and occurred)
  all_events_table
  
## Flag -------------------------------------------------------------------
#does not run  CH - lso not predent in paper
#time_period_data = function(hz)
  #parameters:
  #hz (dataframe) input an event-level dataframe
time_period_data(hz)
  #summary of hazards by time period - 30, 60, or 90 (includes threats)
  #*USE FULL hz dataframe here
  view(sum_time_period)
  #summary of events that are GEN or dated
  view(sum_GEN_dated_table)
  #returns all the societies that have lived ONLY under threat (H12 == 2)
  #*USE FULL hz dataframe here
  view(socs_only_threats)


  # Flag does not run -------------------------------------------------------
#hazard_types_data = function(hz)
  #parameters:
  #hz (dataframe) input an event-level dataframe
hazard_types_data(hz_no90)
  #summary table of each type of hazard (pests are combined)
  view(sum_types)
  #summary table of each type of hazard (none combined)
  view(sum_types_H5)
  #creates new dataframe for societies that have both droughts and floods
  view(dr_fl)
  #summarise the societies that have both droughts and floods
  view(sum_dr_fl)


# does not run CH  
#diversity_of_types = function(hz, var1)
  #parameters:
  #hz (dataframe) input an event-level dataframe
  #var1 (vector) variable that you want to compare with hazard types, e.g. predictability H.8.
diversity_of_types(hz_no90, H.7.)
  #dataframe with number of hazard TYPES per society
  view(count_types_OWC)
  #dataframe with var1 (e.g. predictability) and all hazard types
  view(var1_hztypes)
  
  
#severe_data = function(hz)
  #parameters:
  #hz (dataframe) input an event-level dataframe
severe_data(hz_no90)
  #returns dataframe with the most severe types of hazards per society
  view(severe_types)
  #summary table of how many societies for each hazard type, average severity, and standard deviation
  view(types_severity)
  

#region_data = function(hz)
  #parameters:
  #hz (dataframe) input an event-level dataframe
region_data(hz_no90)
  #summary of hazards per region
  view(sum_reg)

  # Flag does not run -------------------------------------------------------
#H7_H10_summary_tables = function(hz, var1)
  #parameters:
  #hz (dataframe) input an event-level dataframe
  #var1 (vector) variable that you want to create a summary table for
H7_H10_summary_tables(hz_no90, H.9.a.)
  #returns dataframe with summary of var1 including n, percentage, number societies, percentage of societies
  sum_table
  #calculate table of averages for droughts, floods, pests
  view(sum_table_avgs)



  
#adaptability = function(hz) 
  #parameters:
  #hz (dataframe) input an event-level dataframe
adaptability(hz_no90)
  #creates boxplot looking at relationship between H.8. predictability and H.10. severity
  #divides up by top three hazards and all hazards
  adapt_boxplot
  #frequency table for H.8. predictability and H.10. severity
  adaptation_table
  #dataframe which looks at each value for predictability by society (OWC)
  view(predict_socs)
  #dataframe which counts the occurences of high predictability low severity by OWC
  view(adapt_socs)
  

# Flag Uses ovhz - remove -------------------------------------------------
#recurrence_of_events = function(ovhz)
  #parameters:
  #ovhz (dataframe) input a society level dataframe
recurrence_of_events(ovhz)
  #calculate table of averages: number of hazards, number of severe hazards, number of hazard types, and number of severe hazard types, per soc
  view(avgs_summary)
  #returns number of societies that are within 3 standard deviations of whatever mean and SD you want
  n_socs
  
  
#socs_by_most_freq_hz = function(hz)
  #parameters:
  #hz (dataframe) input an event-level dataframe
socs_by_most_freq_hz(time90.df)
  #dataframe which includes the single most frequent hazard per society, the # of occurrences, most common timeframe, H9a, H10, and H8 values. 
  view(soc_by_freq_hz)
  

#missing_data = function(hz, var1) 
  #parameters:
  #hz (dataframe) input an event-level dataframe
  #var1 (vector) variable that you want to find the missing data for
missing_data(hz_no90, H.7.)
  #calculate how many missing values there are for any variable
  missing_vals
  
  
#GRAPHS -------------------------------------------------------------------------------------------------
#region_graphs = function(hz)
  #parameters:
  #hz (dataframe) input an event-level dataframe
region_graphs(hz_no90)
  #graph of all hazards per region
  all_region_graph
  #graph of droughts, floods, pests, per region
  dr_fl_pe_graph_region
  #graph of top three types of hazards for each region
  most_freq_graph
  

## Fig. 4.2.1: Frequency of hazards by society -------------------------------
  #recurrence_of_events_graphs = function(hz)
  #parameters:
  #hz (dataframe) input an event-level dataframe
  recurrence_of_events_graphs(hz_no90)
  
  
  #create histogram overlaying all hazard events and severe events
  sev_hz_hist 

  
  
## Fig. 4.3.1: Predictability by hazard type -------------------------------------------------------------
  #H7_H10_graphs = function(hz, var1, var1name)
  #parameters:
  #hz (dataframe) input an event-level dataframe
  #var1 (vector), input which variable you want to graph
  #var1name (string), name of var1
  #CH
  H7_H10_graphs(hz_no90, H.8., "Predictability")
  #returns the bargraph of inputted variable
  var1_bargraph
  
  
## Fig. 4.4.1: Speed of Onset by Hazard Type -------------------------------
  H7_H10_graphs(hz_no90, H.7., "Onset")
  #returns the bargraph of inputted variable
  var1_bargraph


## Fig. 4.5.1: Overall severity by hazard type -----------------------------
  
  #H7_H10_graphs = function(hz, var1, var1name)
  #parameters:
  #hz (dataframe) input an event-level dataframe
  #var1 (vector), input which variable you want to graph
  #var1name (string), name of var1
  H7_H10_graphs(hz_no90, H.10., "Overall Severity")
  #returns the bargraph of inputted variable
  var1_bargraph
  
  
#correlations = function(hz, start_col, end_col, type_hazard)
  #parameters:
  #hz (dataframe) input an event-level dataframe
  #start_col (int) input the index # of the column of the dataframe that you want to start correlating across from
  #end_col (int) input the index # of the last column of the dataframe that you want to correlate across
  #type_hazard (string) input the variable name in quotes of which type of hazard you want to run correlations with
correlations(hz_no90, 11, 17, "pests")
  #runs correlation matrix between different dimensions of hazards
  view(dimensions_cor)
  #runs correlation matrix across all types of hazards, subsetting for a specific hazard type
  view(types_cor)

  
#mann_whitney = function(hz)
    #parameters:
    #hz (dataframe) input an event-level dataframe
mann_whitney(hz_no90)
  #returns result of mann whitney test between floods and droughts predictability H8
# CH: Returns Wilcoxon rank sum test


#MAPS -------------------------------------------------------------------------------------------------


#maps = function(ovhz)
  #parameters:
  #ovhz (dataframe) input a society level dataframe
maps(ovhz3060)
  #map of all hazards
  samplemap
