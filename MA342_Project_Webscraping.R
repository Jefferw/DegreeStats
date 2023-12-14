library(tidyverse)
library(rvest)
library(xml2)


setwd("C:/Users/crane/Documents/MA342/Project")
links <- read.csv("links.txt")

df <- data.frame("inst_id" = NULL,
                 "pgrm_id" = NULL,
                 "DegreeTitle" = NULL,
                 "Award" = NULL,
                 "RequiredHours" = NULL,
                 "AnnualTuition" = NULL,
                 "AnnualFees" = NULL,
                 "Placement" = NULL,
                 "MedWageEntry" = NULL,
                 "MedWage5yr" = NULL)

for(link in links){
  
#get links from file
  link_first_half <- "https://www.ksdegreestats.org"
  link_second_half <- link
  
  

  
  #combine both parts to create the full link
  full_link <- str_c(link_first_half, link_second_half)
}

for(i in seq_along(full_link)){
  #ficeInstID is the institutionID, and programBnr is the degreeID, use these as primary keys
  split_link <- full_link[i] %>%
    str_split("&")
  
  inst_id <- split_link[[1]][2] %>%
    str_split("=")
  
  inst_id <- inst_id[[1]][2]
  
  pgrm_id <- split_link[[1]][3] %>%
    str_split("=")
  pgrm_id <- pgrm_id[[1]][2]
  
  
  #read webpage
  deg <- read_html(full_link[i])
  
  #TODO: add check to see if data from that page is available, if not, add inst_id and pgrm_id and NA for the rest of the details
  #might be able to check if deg_details = NA
  
  #get degree info (title, award, required hours, description)
  deg_details <- deg %>%
    html_nodes(xpath = '//*[@id="degreestats-results"]') %>%
    html_text2() %>%
    str_trim() %>%
    str_split('\n')
  
  if(length(deg_details) > 0){
    deg_details <- deg_details[[1]] %>%
      str_split(': ')
    deg_details
    
    
    
    #remove the second element with contains the disclaimer at the bottom
    #get detailed degree information, only concerned with costs and wages
    deg_fees <- deg %>% 
      html_elements(".data") %>% 
      html_table()
    
    deg_cost <- deg_fees[[1]][2] %>%
      slice(2:3)
    deg_wages <- deg_fees[[3]][2] %>%
      slice(c(1, 4, 5))
    
    new_df <- data.frame("inst_id" = inst_id,
                         "pgrm_id" = pgrm_id,
                         "DegreeTitle" = deg_details[[1]][2],
                         "Award" = deg_details[[2]][2],
                         "RequiredHours" = deg_details[[3]][2],
                         "AnnualTuition" = deg_cost[[1]][1],
                         "AnnualFees" = deg_cost[[1]][2],
                         "Placement" = deg_wages[[1]][1],
                         "MedWageEntry" = deg_wages[[1]][2],
                         "MedWage5yr" = deg_wages[[1]][3])
  }
    
    else{
      new_df <- data.frame("inst_id" = inst_id,
                           "pgrm_id" = pgrm_id,
                           "DegreeTitle" = NA,
                           "Award" = NA,
                           "RequiredHours" = NA,
                           "AnnualTuition" = NA,
                           "AnnualFees" = NA,
                           "Placement" = NA,
                           "MedWageEntry" = NA,
                           "MedWage5yr" = NA)
    }
    #combine dataframes
    df <- rbind(df, new_df)
  
}

write.csv(df, "C:/Users/crane/Documents/MA342/Project/courses.csv")