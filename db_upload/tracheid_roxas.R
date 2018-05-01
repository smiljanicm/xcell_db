
# 0. Libraries ---------------------------------------------------

library(DBI); library(readxl); library(stringr); library(tidyverse)

source('pw.R')
source('db_upload/0_functions.R')


# 0. load the data --------------------------------------------------------
file_dir <- 'db_upload/data/XC_CH_LTS22/'
file_name <- 'XC_CH_LTS22_NEW.xlsm'



# 0. Additional settings --------------------------------------------------
#' info help table
person_info <- read_xlsx('db_upload/info_table.xlsx', 'person') %>% deframe()
site_info <- read_xlsx('db_upload/info_table.xlsx', 'site') %>% deframe()

#' constrains tables (alternative key)
constrains_db <- db_get_constrains(dbcon) 

#' foreign key table
institution_fk_tbl <- tbl_x('institution_fk') %>% select(institution_code, institution_name) %>% collect()
country_fk_tbl <- tbl_x('country_fk') %>% collect()
site_param_fk_tbl <- tbl_x('site_param_fk') %>% select(site_param_id = id, parameter) %>% collect()


# 1. Prepare each table for upload ----------------------------------------


# 1.1 Person --------------------------------------------------------------
read_xlsx(paste0(file_dir,file_name), 'Person') %>%
  filter(!row_number() %in% c(1:2)) %>%
  select(last_name = Last_name, first_name = First_name, email = Email, webpage = Webpage, phone_number = `Phone_number`, institution_name = Institution) %>%
  inner_join(institution_fk_tbl, by = 'institution_name') %>%
  mutate(id = row_number()) %>%
  check_append_db(., 'person', constrains_db = constrains_db) %>%
  append_data('person')



# 1.2 Site ----------------------------------------------------------------
#' prepare the data
site_i <- read_xlsx(paste0(file_dir,file_name), 'Site', n_max = 4) %>% filter(!row_number() %in% c(1:2)) 
site_a <- read_xlsx(paste0(file_dir,file_name), 'Site') %>% filter(!row_number() %in% c(1:10)) %>%
  select(Country, Site_label) %>% spread(Country, Site_label)

#' bind site information
site.df <- bind_cols(site_i, site_a) %>% 
  rename_(.dots = site_info[site_info %in% colnames(.)]) %>%
  inner_join(country_fk_tbl, by = c('country'))
  
# load the data
site.df %>%
  mutate(id = row_number()) %>%
  mutate(sampling_year = as.integer(sampling_year)) %>%
  check_append_db(., 'site', constrains_db = constrains_db) %>%
  append_data('site')


# 1.3 Site parameter table ------------------------------------------------
site.df %>%
  mutate(sampling_year = as.integer(sampling_year)) %>%
  get_id(., 'site', constrains_db = constrains_db) %>%
  select(site_id = id, setdiff(colnames(.), dbListFields(dbcon, c('v1', "site")))) %>%
  gather(parameter, value, -site_id) %>%
  inner_join(site_param_fk_tbl, by = 'parameter') %>%
  check_append_db(., 'site_param', constrains_db = constrains_db) %>%
  append_data('site_param')
