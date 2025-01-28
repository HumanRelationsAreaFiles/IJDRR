#revised version of document scraping script (to scrape codesheets from Drive instead of Github. Revisions by Seb, 1/15/24)
#this script should: scrape all the hazards 2023 resolved codesheets
#and all original codesheets that were not recoded
#and codesheets revised in accordance with IBSS coding

library(officer)
library(janitor)
library(plyr)
library(tidyverse)
library(stringr)
library(googledrive)

# Authenticate user's Google Drive account
drive_auth()

# Define the Google Drive folder URL
folder_url <- "https://drive.google.com/drive/folders/1AzbjgZb-4utVtun8vLBoNMyriQ45PWJi"

# Get the list of Word files in the folder
files <- drive_ls(as_id(folder_url)) %>%
  filter(str_detect(name, "\\.docx$"))  # Filter for .docx files

# Download Word files to a temporary directory
temp_dir <- tempdir()  # Create a temporary directory
lapply(files$id, function(file_id) {
  drive_download(as_id(file_id), path = file.path(temp_dir, drive_get(file_id)$name), overwrite = TRUE)
})

# Make a list of the files in the temporary directory 
my_files <- str_subset(
    list.files(
      path = temp_dir,
      pattern = "*[[:upper:]]{2}.docx",
      full.names = TRUE
    ),
    pattern = "HazardsCodes",
    negate = FALSE
  )



# Scrape Word docs 
data_list <- lapply(my_files,function(x) docx_summary(read_docx(x)))


# Make scraped data into readable data frames -----------------------------
# Make vector of IDs for use in naming data frames
## (here the goal was to isolate from the file name the society IDs and coder initials 
## as a vector of uninterrupted character strings)
names <- paste0(str_extract(my_files,"(?<=SCCS)\\d{3}|X{3}"),
                str_extract(my_files,"(?<=PSF|SRS)[:upper:]{2}(\\d{2}|X{2})"),
                str_replace_all(str_extract(my_files,"(?<=\\-)[:upper:]{2,}\\-?[:upper:]{0,2}\\-?[:upper:]{0,2}(?=\\.docx)"),
                                "\\-","\\."))

# Make a list of documents as tibbles
content_list <- lapply(data_list,function(x) as_tibble(x))

# Filter documents in content_list to only tables and get output into a list
for(i in seq_along(content_list)){
  nam<-paste(names[i])
  assign(nam,content_list[[i]]%>%filter(content_type=="table cell"))
}

table_content_list<-mget(ls(pattern = "(\\d|X){3}([[:upper:]]){2}(\\d|X){2}([[:alpha:]])+"))

rm(list=ls(pattern = "(\\d|X){3}([[:upper:]]){2}(\\d|X){2}([[:alpha:]])+"))

# Separate by unique tables into separate data frames and get output into a list
for (i in seq_along(table_content_list)){
  for(j in unique(table_content_list[[i]]$doc_index)){
    nam<-paste(names[i],j,sep =".")
    assign(nam,table_content_list[[i]][table_content_list[[i]]$doc_index==j,])
  }
}

df_list<-mget(ls(pattern = "(\\d|X){3}([[:upper:]]){2}(\\d|X){2}([[:alpha:]])+"))

rm(list=ls(pattern = "(\\d|X){3}([[:upper:]]){2}(\\d|X){2}([[:alpha:]])+"))

# Create vector of IDs with table numbers
table_names <- names(df_list)

# Create vectors of table headers and get list of output header vectors
for(i in seq_along(df_list)){
  nam <- paste(table_names[i],"header",sep = ".")
  df_list[[i]]%>%filter(row_id==1)%>%pull(text)
  assign(nam,df_list[[i]]%>%filter(row_id==1)%>%pull(text))
}

header_list<-mget(ls(pattern = "(\\d|X){3}([[:upper:]]){2}(\\d|X){2}([[:alpha:]])+"))

rm(list=ls(pattern = "(\\d|X){3}([[:upper:]]){2}(\\d|X){2}([[:alpha:]])+"))

# Manipulate individual data frames to include only collected data and not table metadata
# Then get list of output
for(i in seq_along(df_list)){
  nam <- table_names[i]
  assign(nam,
         df_list[[i]]%>%filter(row_id!=1)%>%
           select(text,row_id,cell_id)%>%
           pivot_wider(names_from = cell_id,values_from = text)%>%
           select(-row_id)%>%
           plyr::mutate(across(.cols = -1,.fns = parse_number))%>%
           clean_names())
}

clean_list<-mget(ls(pattern = "(\\d|X){3}([[:upper:]]){2}(\\d|X){2}([[:alpha:]])+"))

rm(list=ls(pattern = "(\\d|X){3}([[:upper:]]){2}(\\d|X){2}([[:alpha:]])+"))

# Reduce to only tables that have data--some societies had no hazards, some tables are empty
clean_list<-clean_list[lapply(clean_list,length)>0]


# Isolate tables of each type and combine into data frames ------------------
# Write function to subset list by number of columns, combine into data frame, and add a header
# (each table from the codesheet has a unique number of columns)
isocat <- function(x){
  list <- list(clean_list[lapply(clean_list,ncol)==as.numeric(x)])
  for (i in seq_along(list[[1]])){
    list[[1]][[i]]$ID <- rep(names[i],nrow(list[[1]][[i]]))
  }
  df <- do.call(rbind,list[[1]])
  df_names <- names(list[[1]])
  df_header <- header_list[[str_which(names(header_list),pattern = paste0(df_names[1],".header"),negate = FALSE)]]
  colnames(df)[1:as.numeric(x)] <- df_header
  df
}

# Isolate and concatenate meta data (10 columns)
meta_df <- isocat(10)

# Isolate and concatenate data quality data (1 column)
# (3 headers were on this table in the codesheet, filter to only include collected data)
dq_df <- isocat(1)%>%
  filter(D.1.!="Data Quality Score for TABLE 1"&D.1.!="DQ1-hz")

# Isolate and concatenate hazards data (15 columns)
# Filtering to only data, same issue as above
hz_df <- isocat(15)%>%
  filter(H.1.!="Hazard  ID"&H.1.!="Hz-xx"&H.1.!="")

# Finally, write files -------------------------------------------------------------
write.csv(meta_df,"Hazards/Datasets/Raw/DT-meta-hz-raw.csv",row.names = FALSE)
write.csv(dq_df,"Hazards/Datasets/Raw/DT-dq1-hz-raw.csv",row.names = FALSE)
write.csv(hz_df,"Hazards/Datasets/Raw/DT-hz-raw.csv",row.names = FALSE)



