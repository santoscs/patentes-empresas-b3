
library("patentsview") 
library(readr)
rm(list = ls())
assignee <- read_delim("data-raw/assignee.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
load("data-raw/empresas-b3.RData")

name_org <- vector('list')
for(i in 1:length(df.reports$company.name)){
  name_org[[i]] <- agrep(df.reports$company.name[i], assignee$organization, value = T)
}

sele <- sapply(name_org, function(x) length(x)==0)
name_org[!sele]

name_org <- df.reports$company.name[!sele][c(1, 3, 4, 12,15, 16, 18, 19)]


uspatentes <- vector('list')
for(i in 1:length(name_org)){
  query <- with_qfuns(
    and(
      contains(assignee_organization = name_org)
    )
  )
  
  data_pv <- search_pv(
    query = query,
    fields = c("patent_number","patent_date","patent_kind","patent_type","app_number","app_date","app_type","location_city","location_country","assignee_id","assignee_organization"), 
    subent_cnts = TRUE,
    endpoint = "assignees"
  )
  uspatentes[[i]] <- data_pv$data$assignees$applications
}

