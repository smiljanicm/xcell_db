
# 0. Libraries ---------------------------------------------------

library(raster); library(DBI); library(readxl); library(stringr); library(tidyverse) 

source('pw.R')
source('db_upload/0_functions.R')


# 0. load the data --------------------------------------------------------
file_dir <- 'db_upload/data/XC_CH_LTS22/'
file_name <- 'XC_CH_LTS22_NEW.xlsm'



# 0. Additional settings --------------------------------------------------
#' info help table
person_info <- read_xlsx('db_upload/info/info_table.xlsx', 'person') %>%  deframe()
site_info <- read_xlsx('db_upload/info/info_table.xlsx', 'site') %>% deframe()
tree_info <- read_xlsx('db_upload/info/info_table.xlsx', 'tree') %>% deframe()
sample_info <- read_xlsx('db_upload/info/info_table.xlsx', 'sample') %>% deframe()

#' constrains tables (alternative key)
constrains_db <- db_get_constrains(dbcon) 
constr_short <- constrains_db %>% select(table, constrains) %>% deframe()

#' foreign key table
institution_fk_tbl <- tbl_x('institution_fk') %>% select(institution_code, institution_name) %>% collect()
country_fk_tbl <- tbl_x('country_fk') %>% collect()
site_param_fk_tbl <- tbl_x('site_param_fk') %>% select(site_param_id = id, parameter) %>% collect()
tree_param_fk_tbl <- tbl_x('tree_param_fk') %>% select(tree_param_id = id, parameter) %>% collect()
organ_fk_tbl <- tbl_x('organ_fk') %>%  collect()
sample_param_fk_tbl <- tbl_x('sample_param_fk') %>% select(sample_param_id = id, parameter) %>% collect()

# 1. Prepare each table for upload ----------------------------------------


# 1.1 Person --------------------------------------------------------------
read_file('person') %>%
  inner_join(institution_fk_tbl, by = 'institution_name') %>%
  mutate(id = row_number()) %>%
  check_append_db(., 'person', constrains_db = constrains_db) %>%
  append_data('person')



# 1.2 Site ----------------------------------------------------------------
#' prepare the data
site_i <- read_xlsx(paste0(file_dir,file_name), 'site', n_max = 4) %>% filter(!row_number() %in% c(1:2)) 
site_a <- read_xlsx(paste0(file_dir,file_name), 'site') %>% filter(!row_number() %in% c(1:10)) %>%
  select(Country, Site_label) %>% spread(Country, Site_label)

#' bind site information
site.df <- bind_cols(site_i, site_a) %>% 
  rename_(.dots = site_info[site_info %in% colnames(.)]) %>%
  inner_join(country_fk_tbl, by = c('country')) %>%
  mutate_at(vars(sampling_year, longitude, latitude), funs(as.numeric(.)))

#' add the climate information
site.df <- site.df %>% select(-temp, -prec) %>% bind_cols(extract_clim(site.df$longitude, site.df$latitude))


# load the data
site.df %>%
  mutate(id = row_number()) %>%
  check_append_db(., 'site', constrains_db = constrains_db) %>%
  append_data('site')

# 1.3 Site parameter table ------------------------------------------------
site.df %>%
  get_id(., 'site', constrains_db = constrains_db) %>%
  select(site_id = id, setdiff(colnames(.), dbListFields(dbcon, c('v1', "site")))) %>%
  gather(parameter, value, -site_id) %>%
  inner_join(site_param_fk_tbl, by = 'parameter') %>%
  check_append_db(., 'site_param', constrains_db = constrains_db) %>%
  append_data('site_param')


site_id_d <- get_id(site.df, 'site', constrains_db = constrains_db) %>% pull(id)

# 1.4 Tree ----------------------------------------------------------------
tree.df <- read_file('tree') %>%
  mutate(site_id = site_id_d)


tree.df %>%
  select(site_id, tree_label, species_code) %>%
  mutate(id = row_number()) %>%
  check_append_db(., 'tree',constrains_db = constrains_db) %>%
  append_data('tree')

tree_id_d <- get_id(tree.df, 'tree', constrains_db = constrains_db) %>% select(tree_id = id, constr_name('tree'))


# 1.5 Tree parameters -----------------------------------------------------

tree.df %>%
  select(tree_label, species_code, wood_type, leaf_habit, wood_plane, dbh, height, age, status_code) %>%
  gather(parameter, value, -tree_label, -species_code) %>%
  inner_join(tree_id_d) %>%
  select(tree_id, parameter, value) %>%
  filter(!is.na(value)) %>%
  inner_join(tree_param_fk_tbl, by = 'parameter') %>%
  check_append_db(., 'tree_param',constrains_db = constrains_db) %>%
  append_data('tree_param')

# 1.6 Sample --------------------------------------------------------------
sample.df <- read_file('sample') %>%
  rename(description = organ) %>%
  inner_join(tree_id_d) %>%
  inner_join(organ_fk_tbl)

sample.df %>%
  select(tree_id, sample_label, organ) %>%
  mutate(id = row_number()) %>%
  check_append_db(., 'sample',constrains_db = constrains_db) %>%
  append_data('sample')

sample_id_d <- get_id(sample.df, 'sample', constrains_db = constrains_db) %>% select(sample_id = id, constr_name('sample'))


# 1.7 Sample parameters ---------------------------------------------------

sample.df %>%
  select(sample_label, sample_preparation, cutting_plane, sampling_h, apex_dist, sample_thickness) %>%
  gather(parameter, value, -sample_label) %>%
  inner_join(sample_id_d) %>%
  select(sample_id, parameter, value) %>%
  filter(!is.na(value)) %>%
  inner_join(sample_param_fk_tbl, by = 'parameter') %>%
  check_append_db(., 'sample_param',constrains_db = constrains_db) %>%
  append_data('sample_param')
