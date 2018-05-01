
# This script is to fill the FK_tables in the Xcell DB. Usually this is performed before filling the DB with data
# The information to be filled can be found in the xls file Tables_Lists
# Patrick Fonti 15.10.2017

# 1. load libraries and WD -------------------------------------------------------
library(RPostgreSQL)
library(DBI)
library(readxl)
library(tidyverse)

# 2. Create a connection to the DB ----------------------------------------
source('pw.R')
sch <- 'v1' #schema name

# 3. Get a general information about the DB -----------------------------
#--/ List availale tables
db_list_tables(dbcon)
#--/ List columns in the table
dbListFields(dbcon, "institution_fk")

# 4. upload data to the FK-tables ----------------------------------------------
sheetsNameList <- excel_sheets("db_create/xcell_fk_tables.xlsx")
  
for (i in sheetsNameList) {       
  print(i)
  
  #' remove data in db
  db_n <- tbl_x(i) %>% dplyr::summarize(n()) %>% dplyr::pull()
  if(db_n > 0){
    DBI::dbExecute(dbcon, paste0("truncate ", sch, ".", i, ' cascade'))  
  }
  
  #' read new data
  sheet<-read_excel("db_create/xcell_fk_tables.xlsx", sheet = i)
    
  #' write new data
  dbWriteTable(conn = dbcon, 
                 name = c(sch,i), # name of the table that you want to modify
                 value = sheet, # data you want to add
                 overwrite = FALSE, # you don't want to overwrite a table, just append
                 append = TRUE, # since the table already exist you just want to append it
                 row.names=FALSE) # if add the rownames
  }


# 5. Close connection to the DB ----------------------------------------
dbDisconnect(dbcon)
