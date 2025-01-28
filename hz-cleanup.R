setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(lubridate)
library(Hmisc)
library(readxl)

hz <- read.csv("Raw/DT-hz-raw.csv")
dq <- read.csv("Raw/DT-dq1-hz-raw.csv")
meta <- read.csv("Raw/DT-meta-hz-raw.csv")
ct <- read.csv("../Codesheets/recurrence-DRAD.csv") #dataset with coded years between hazard occurrences
id <- readxl::read_xlsx("../../Sample/Minerva Starting Sample.xlsx")
#remove the .0 after entirely numeric PSF IDs
id$PSF.Cluster <- str_remove(id$PSF.Cluster,"\\.0")

drop <- readxl::read_xlsx("../../Sample/Minerva Starting Sample.xlsx",sheet = "excluded cases")%>%
  pull(OWC) #OWCs of dropped cases from the Minerva sample

# clean meta data table ---------------------------------------------------
colnames(meta)[6] <- "Principle.Authorities"
meta[meta==""] <- NA

#save coder information from file names which have been condensed into an ID--SCCS ID, OWC, Coder Initials as one string
#put length of ID into own column to make it easier to refer to
meta$ID.length <- str_length(str_extract(meta$ID,"(?<=([:upper:]{2}\\d{2})|X{4}).{2,8}$"))
#record coder information depending on length of ID
#short IDs were coded once, middle-length IDs have resolved codes, long IDs were revised
meta$Coder <- ifelse(meta$ID.length==2,str_extract(meta$ID,"[:upper:]{2}$"),
                     ifelse(meta$ID.length==5,paste("Resolved between",
                                                    str_extract(meta$ID,"[:upper:]{2}(?=\\.)"),
                                                    "and",str_extract(meta$ID,"[:upper:]{2}$"),
                                                    sep = " "),
                            ifelse(meta$ID.length==8&str_detect(meta$ID,"\\.XX\\.")==TRUE,
                                   paste("Coded by",str_extract(meta$ID,"[:upper:]{2}(?=\\.XX)"),
                                         "and revised by",str_extract(meta$ID,"[:upper:]{2}$"),
                                         sep = " "),paste("Resolved between",
                                                          str_extract(meta$ID,"[:upper:]{2}(?=\\.[:upper:]{2}\\.)"),
                                                          "and",str_extract(meta$ID,"(?<=\\.)[:upper:]{2}(?=\\.)"),
                                                          "then revised by",str_extract(meta$ID,"[:upper:]{2}$"),
                                                          sep = " "))))

# clean coding dates
#separate original coding and revision dates
meta$Revision.Date <- str_extract(meta$Coding.Date,"(?<=Revised\\:?\\s{1}).+$")
meta$Coding.Date <- str_remove(meta$Coding.Date,"Revised\\:?\\s{1}.+$")

#one coder didn't put the year sometimes, so add that
meta$Coding.Date[str_length(meta$Coding.Date)<6] <- paste0(meta$Coding.Date[str_length(meta$Coding.Date)<6],"/2022")

#save the length of the dates, for determining if month was written out or not
meta$date.length <- str_length(meta$Coding.Date)

d <- "1970-01-01" #save the start date for measuring intervals and determining dates
#dates are days recorded as the number of days since January 1, 1970

#get number of days since January 1, 1970, then dates
meta$date <- ifelse(meta$date.length>10,as.numeric(as.duration(ymd(d)%--%dmy(meta$Coding.Date)))/86400,
                    as.numeric(as.duration(ymd(d)%--%mdy(meta$Coding.Date)))/86400)

meta$Coding.Date <- as_date(days(meta$date))

#clean revision dates--repeat of above
meta$date.length <- str_length(meta$Revision.Date)
meta$date <- ifelse(meta$date.length>10,as.numeric(as.duration(ymd(d)%--%dmy(meta$Revision.Date)))/86400,
                    as.numeric(as.duration(ymd(d)%--%mdy(meta$Revision.Date)))/86400)

meta$Revision.Date <- as_date(days(meta$date))

#keep only relevant columns and reorder
meta.keep <- meta[,c("SCCS.ID","SCCS.Name","PSF.Cluster.Number","OWC","eHRAF.Name",
                       "Principle.Authorities","Place.Focus","Ethnographic.Present",
                       "Coder","Coding.Date","Revision.Date")]

#make EP data frame with EP as just numerical year, important for determining time periods later
EP <- data.frame(OWC = meta$OWC,EP = as.numeric(as.character(str_extract(meta$Ethnographic.Present,"^\\d{4}"))))

# clean data quality table -----------------------------------------------

#add identification 
dq$OWC <- str_extract(dq$ID,"(?<=\\d|X)(\\D{2})(\\d{2})")
dq$SCCS.ID <-str_extract(dq$ID,"^\\d{3}")
dq$SCCS.ID <- ifelse(dq$SCCS.ID=="XXX",NA,dq$SCCS.ID)

#take non-PSF OWCs from meta df--these didn't have the OWC in the file name
meta.sccs <- meta%>%
  filter(!is.na(SCCS.ID))
meta.sccs$SCCS.ID <- ifelse(str_length(meta.sccs$SCCS.ID)==2,paste0(0,meta.sccs$SCCS.ID),
                            ifelse(str_length(meta.sccs$SCCS.ID)==1,paste0("00",meta.sccs$SCCS.ID),meta.sccs$SCCS.ID))
dq.sccs <- merge(meta.sccs[,c("SCCS.ID","OWC")],dq[,c("D.1.","ID","SCCS.ID")],by = "SCCS.ID")

#rbind and subset columns
dq <- rbind(dq.sccs,dq[is.na(dq$SCCS.ID),])

#merge in coder information so that doesn't all have to be repeated
dq.keep <- merge(dq,meta[,c("OWC","Coder")],by = "OWC")[,c("SCCS.ID","OWC","D.1.","Coder")]


# clean hazard events table -----------------------------------------------

#clean H.1. so it's just digits
hz$H.1. <- str_remove(hz$H.1.,"Hz\\-")

#add identification, get OWCs for non-PSF cases from meta df again
hz$SCCS.ID <-str_extract(hz$ID,"^\\d{3}")
hz.sccs <- merge(meta.sccs[,c("SCCS.ID","OWC")],hz[!is.na(hz$SCCS.ID),],by = "SCCS.ID",all.y = TRUE)
hz$OWC <- str_extract(hz$ID,"(?<=\\d|X)(\\D{2})(\\d{2})")

#rbind and remove ID column
hz.keep <- rbind(hz.sccs,hz[is.na(hz$SCCS.ID),])[,c(-18)]

#merge in coder information
hz.keep <- merge(hz.keep,meta[,c("OWC","Coder")],by = "OWC",all.x = TRUE)

#clean H.3. so it's all positive numbers
hz.keep$H.3. <- str_remove_all(hz.keep$H.3.,pattern = "\\-")

#replace 2s in predictability with 1.5s--a coding decision made late in the coding process
hz.keep$H.8.[hz.keep$H.8.==2] <- 1.5

#fix coding mistakes in severity columns
#scales never went lower than 1 or higher than 4, 99 indicates "don't know"
hz.keep[,c("H.9.b.","H.9.c.","H.9.d..")][hz.keep[,c("H.9.b.","H.9.c.","H.9.d..")]==0.5] <- 99
hz.keep[,"H.10."][hz.keep[,"H.10."]==0.5] <- 1
hz.keep[,c("H.9.a.","H.9.b.","H.9.c.","H.9.d..","H.10.")][hz.keep[,c("H.9.a.","H.9.b.","H.9.c.","H.9.d..","H.10.")]==5] <- 4

#remove NAs from H.11. and assign them a value of 3 
#NA was assigned to dated hazard events, which occurred once, giving them a frequency score of 3
hz.keep$H.11.[is.na(hz.keep$H.11.)] <- 3

#rename columns
colnames(hz.keep)[c(5,7,9,15)] <- c("H.2.b.","H.4.","H.6.","H.9.d.")

#convert to numeric type data, create "occurrence" variable, and fix onset to actually be binary
hz.keep <- hz.keep%>%
  mutate(across(c(SCCS.ID:H.1.,H.4.,H.6.:H.11.),as.numeric),
         H.12. = if_else(H.11.>=3&H.11.<=5,3,H.11.),
         H.7. = case_when(H.7.==1 ~ 1,
                                 H.7.==2 ~ 2,
                                 H.7.==1.5&(H.5.=="Fl"|H.5.=="FL"|H.5.=="Ea"|H.5.=="Wi"|H.5.=="Fi"|H.5.=="TW"|H.5.=="SS"|H.5.=="ET") ~ 2,
                                 H.7.==1.5&(H.5.=="CP"|H.5.=="DRn"|H.5.=="SW") ~ 1, #anything coded 1.5 in onset was coded incorrectly, per Carol, recoded here depending on type of hazard and onset determined by Rachele Pierro
                                 H.7.==1.5&(H.5.=="Vo"|H.5.=="PE"|H.5.=="GsEp"|H.5.=="SEW") ~ 88, #if the hazard type wasn't clearly fast or slow onset, it was assigned 88 (confusing/contradictory)
                                 .default = H.7.)) #maintain the rest of the codes as they are--should all be codes for missing information

hz.keep$hz.id <- paste(hz.keep$OWC,str_pad(hz.keep$H.1.,2,"left","0"),sep = ".")

#clean environment
rm(dq,dq.sccs,EP,hz,hz.sccs,meta,meta.sccs,d)


# add ID to replace sccs ID -----------------------------------------------

#gather into list for application of functions and iteration
file.list <- list(dq = dq.keep,hz = hz.keep,meta = meta.keep) 

#function to merge by OWC
mergeFun <- function(x,y){
  left_join(x,y)
}

#apply the merge function for both ID and psf cluster across the list
file.list <- lapply(file.list,left_join,id)

#use iteration over list to fill in missing IDs with the SCCS.ID and then drop the SCCS.ID column
for (i in seq_along(file.list)){
  nam <- names(file.list)[i]
  assign(nam,file.list[[i]]%>%
           select(-SCCS.ID)%>%
           filter(!OWC %in% drop)) #just in case some cases slipped in that should have been dropped (ideally these codesheets will not have been scraped)
}

rm(list = ls(pattern = "keep"))

# count hazards and duplicate rows accordingly ----------------------------

#separate timeframes into different columns for separating by time later on
hz$time30 <- str_extract(hz$H.3.,"30") #30 years before ethnographic present (EP)
hz$time60 <- str_extract(hz$H.3.,"60") #60 to 30 years before EP
hz$time90 <- str_extract(hz$H.3.,"90") #90 to 60 years before EP

#some events were coded in multiple time periods (EP to EP-30, EP-60 to EP-30, and/or EP-90 to EP-60)
#make a variable that indicates how many of these 3 times periods a hazard event was coded in
hz <- hz%>%
  rowwise%>%
  mutate(periods = sum(!is.na(time30),!is.na(time60),!is.na(time90)))

#function to undo the rowwise function above
unrowwise <- function(x) {
  class(x) <- c("tbl_df","data.frame")
  x
}

hz <- unrowwise(hz)


hc <- full_join(ct,hz)%>% #keep all cases
  select(-SCCS.ID,-DRAD)%>% #get rid of identifiers not needed
  mutate(years = case_when(str_detect(H.3.,",")==FALSE ~ 30, #determine number of years event was assumed to occur in
                           str_length(H.3.)>2&str_length(H.3.)<9 ~60, #if no comma, was not rated in more than one period=30 years
                           str_length(H.3.)>6 ~ 90), #otherwise use length of H.3. (periods rated) to determine how many periods and thus years rated in
         years.between = if_else(years.between<1,1,years.between), #round all estimates of years between hazards that are less than 1 to 1
         est.count = case_when(!is.na(count) ~ count, #count is the absolute count of hazards reported in the ethnographic record
                               !is.na(years.between) ~ round(years/years.between), #round all recurrence estimates for the purpose of uncounting into rows
                               .default = 0))%>% #turn NAs into 0s for the purpose of uncounting into rows
  uncount(est.count,.remove = FALSE)  #add duplicate rows based on the estimated count of hazards


hz <- hz%>% #recode H.11. (frequency) to reflect actual measure
  mutate(period.count = case_when(H.11.<=2 ~ 0, #less than/equal to rating 2 didn't occur
                                  H.11.==3 ~ 1, #rating 3 occurred once
                                  H.11.==4 ~ 2, #rating 4 occurred 2-3 times (HRAF group decision to estimate 2)
                                  H.11.==5 ~ 4, #rating 5 occurred 4 or more times (HRAF group decision to estimate 4)
                                  .default = 0),
         est.count.H.11. = period.count*periods) #multiple the number of periods rated time count per period for full number of hazards estimated
  

he <- hz%>%
  uncount(est.count.H.11.) #add duplicate rows based on the estimated count of hazards

th <- hz%>%
  filter(H.11.==1|H.11.==2|H.11.>5) #dataframe that includes events that didn't occur (rated 1 or 2) or unknown frequency (>5)

#make time variable that indicates what time period the duplicated hazard is theorized to have occurred in 
time30 <- hz%>%
  mutate(time = str_extract(H.3.,"30"))%>%
  filter(!is.na(time))%>%
  uncount(period.count,.remove = FALSE)

time60 <- hz%>%
  mutate(time = str_extract(H.3.,"60"))%>%
  filter(!is.na(time))%>%
  uncount(period.count,.remove = FALSE)

time90 <- hz%>%
  mutate(time = str_extract(H.3.,"90"))%>%
  filter(!is.na(time))%>%
  uncount(period.count,.remove = FALSE)

full <- rbind(time30,time60,time90)
full <- full_join(full,hz)%>% #keep all cases to include events that were rated as not occurring or unknown
  group_by(OWC)%>%
  arrange(H.1.,.by_group = TRUE)%>%
  ungroup()%>%
  mutate(H.12. = if_else(H.11.>=3&H.11.<=5,3,H.11.))
  

# clean up files ----------------------------------------------------------
hz$time <- hz$H.3.

#add hazard id for unique rows for later joining
ct$hz.id <- paste(ct$OWC,ct$H.1.,sep = ".")

#variable for the frequency of hazards per period as measured by length of time period (30)/years between recurring hazards
ct <- ct%>%
  select(-(SCCS.ID))%>% #drop SCCS.ID (changing to ID to include non-SCCS cases) and H.11./coded frequency (already in other data frame)
  mutate(period.freq = if_else(!is.na(count),count,30/years.between))

#get hazards data (non-duplicated and duplicated rows) into list
clean.list <- list(hz = hz,full = full)

#join with data on recurrence, keeping all cases in hazards
clean.list <- lapply(clean.list,left_join,y = ct)

clean.list <- clean.list%>%
  map(~select(.,OWC,eHRAF.Name,ID,EA.ID,PSF.Cluster,Coder,H.1.:H.11.,periods,time,period.count,years.between))%>% #select rows to keep/reorder
  map(~mutate(.,across(c(H.2.a.:H.2.b.),~if_else(.x=="GEN","GEN",.x)), #make sure both columns for date or GEN are consistent (should both or neither be GEN)
              H.5. = case_when(H.5.=="FL"~"Fl", #correct typos in hazard type (H.5.)
                               H.5.=="GsEP"~"GsEp",
                               .default = H.5.),
              H.12. = if_else(H.11.>=3&H.11.<=5,3,H.11.)))

#data frames in list into individual data frames, 1 each with coded missing values and with coded missing values changed to NA
for(i in seq_along(clean.list)){
  fa.nam <- paste(names(clean.list)[i], "fa", sep = ".")
  nam <- paste(names(clean.list)[i], "clean", sep = ".")
  assign(fa.nam, clean.list[[i]] %>%
           mutate(across(c(H.1.:H.11., H.12.), 
                         ~ if_else(.x %in% c("77", "88", "99", "Y", "NA"), NA, .x))) %>%
           select(-H.11.) %>%
           relocate(H.12., .after = H.10.)) 
  assign(nam, clean.list[[i]] %>%
           select(-H.11.) %>%
           relocate(H.12., .after = H.10.))
}

hz.clean <- hz.clean%>%
  select(-time)
hz.fa <- hz.fa%>%
  select(-time)

hz.pub <- hz.fa %>%
  dplyr::mutate(H.12. = if_else(H.11.>=3,3,H.11.)) %>%
  select(-(H.11.:years.between))

# write files -------------------------------------------------------------

##data quality
write.csv(dq,"DT-dq1-hz-clean.csv",row.names = FALSE)

##hazard events-not duplicated, not prepared for analysis
write.csv(hz.clean,"DT-hz-event-level-clean.csv",row.names = FALSE)
##hazard events-not duplicated, prepared for analysis
write.csv(hz.fa,"DT-hz-event-level-clean-FA.csv",row.names = FALSE)

##hazard events-duplicated, not prepared for analysis
write.csv(full.clean,"DT-hz-event-level-expanded.csv",row.names = FALSE)
##hazard events-duplicated, prepared for analysis
write.csv(full.fa,"DT-hz-event-level-expanded-FA.csv",row.names = FALSE)

##hazard events FOR PUBLICATIONS
write.csv(hz.pub, "Upstream/DT-hz-event-level.csv", row.names = FALSE)

##metadata
write.csv(meta,"DT-meta-hz-clean.csv",row.names = FALSE)
