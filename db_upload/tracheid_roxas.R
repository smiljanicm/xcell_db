
# 0. Libraries ---------------------------------------------------

library(raster); library(DBI); library(readxl); library(stringr); library(tidyverse) 

source('pw.R')
source('db_upload/0_functions.R')


# 0. load the data --------------------------------------------------------
#file_dir <- 'db_upload/data/XC_RU_Shira/'        # ok!
#file_name <- 'XC_RU_Shira.xlsm'
#file_dir <- 'db_upload/data/XC_CH_LTS22/'        # ok!, 30 double years 
#file_name <- 'XC_CH_LTS22.xlsm'
#file_dir <- 'db_upload/data/XC_CH_LTS19/'        # ok!, 33 double years 
#file_name <- 'XC_CH_LTS19.xlsm'
#file_dir <-  'db_upload/data/XC_CH_LTN13/'       # ok!, 33 double years
#file_name <- 'XC_CH_LTN13.xlsm'
#file_dir <-  'db_upload/data/XC_IT_CRO12/'       # ok!, 157 double years
#file_name <- 'XC_IT_CRO12.xlsm'
#file_dir <-  'db_upload/data/XC_IT_CRO16/'       # ok!, 249 double years
#file_name <- 'XC_IT_CRO16.xlsm'
#file_dir <-  'db_upload/data/XC_IT_CRO21/'       # ok!, 265 double years
#file_name <- 'XC_IT_CRO21.xlsm'
#file_dir <-  'db_upload/data/XC_RU_AKH/'         # ok!, 144 double years
#file_name <- 'XC_RU_AKH.xlsm'
#file_dir <-  'db_upload/data/XC_FI_NFI/'         # ok!, 396 double years # Only the first 10 trees out of 29 are up-loaded
#file_name <- 'XC_FI_NFI.xlsm'
#file_dir <-  'db_upload/data/XC_ES_Zara/'        # ok!, 61 double years
#file_name <- 'XC_ES_Zara.xlsm'
#file_dir <-  'db_upload/data/XC_IT_NAP/'         # ok!, 104 double years
#file_name <- 'XC_IT_NAP.xlsm'
#file_dir <-  'db_upload/data/XC_CH_PFYN/'        #!! Maybe redo, 234 double years 
#file_name <- 'XC_CH_PFYN.xlsm'
#file_dir <-  'db_upload/data/XC_IT_CRO2L/'       # ok!, 282 double years
#file_name <- 'XC_IT_CRO2L.xlsm'


#file_dir <-  'db_upload/data/XC_TR_MURQ/'         #!! NAs introduced by coercion when reading data files
#file_name <- 'XC_TR_MURQ.xlsm'
#file_dir <- 'db_upload/data/XC_TR_MURF/'          # ok!, 38 double years
#file_name <- 'XC_TR_MURF.xlsm'
#file_dir <-  'db_upload/data/XC_CH_ZHUrd/'        # ok!, 96 double years
#file_name <- 'XC_CH_ZHUrd.xlsm'
#file_dir <-  'db_upload/data/XC_CH_ZHShl/'        # ok!
#file_name <- 'XC_CH_ZHShl.xlsm'
#file_dir <-  'db_upload/data/XC_IL_BZR/'          # ok!, 19 double years
#file_name <- 'XC_IL_BZR.xlsm'
#file_dir <-  'db_upload/data/XC_IL_ABA/'          #!! NAs introduced by coercion when reading data files
#file_name <- 'XC_IL_ABA.xlsm'


# 0. Additional settings --------------------------------------------------
#' info help table
person_info <- read_info('person')
site_info <- read_info('site')
tree_info <- read_info('tree')
sample_info <- read_info('sample')
subsample_info <- read_info('subsample')
year_info <- read_info('year')
cell_info <- read_info('cell')
settings_info <- read_info('settings')
publication_info <- read_info('publication')


#' constrains tables (alternative key)
constrains_db <- db_get_constrains(dbcon) 
constr_short <- constrains_db %>% select(table, constrains) %>% deframe()

#' foreign key table
institution_fk_tbl <- tbl_x('institution_fk') %>% select(institution_code, institution_name) %>% collect()
country_fk_tbl <- tbl_x('country_fk') %>% collect()
site_param_fk_tbl <- tbl_x('site_param_fk') %>% select(site_param_id = id, parameter) %>% collect()
tree_param_fk_tbl <- tbl_x('tree_param_fk') %>% select(tree_param_id = id, parameter) %>% collect()
meas_met_param_fk_tbl <- tbl_x('meas_met_param_fk') %>% select(meas_met_param_id = id, parameter) %>% collect()
meas_param_fk_tbl <- tbl_x('meas_param_fk') %>% select(param_id = id, parameter) %>% collect()
organ_fk_tbl <- tbl_x('organ_fk') %>%  collect()
sample_param_fk_tbl <- tbl_x('sample_param_fk') %>% select(sample_param_id = id, parameter) %>% collect()

# 1. Prepare each table for upload ----------------------------------------


# 1.1 Person --------------------------------------------------------------
read_file('person')  %>%
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
  rename_cols(site_info) %>%
  inner_join(country_fk_tbl, by = c('country')) %>%
  mutate_at(vars(sampling_year, longitude, latitude), funs(as.numeric(.)))

#' add the climate information
site.df <- site.df %>% select(-temp, -prec) %>% 
  bind_cols(extract_clim(site.df$longitude, site.df$latitude)) %>% 
  mutate(temp = temp/10)

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
  filter(!is.na(value)) %>%
  check_append_db(., 'site_param', constrains_db = constrains_db) %>%
  append_data('site_param')


site_id_d <- get_id(site.df, 'site', constrains_db = constrains_db) %>% pull(id)

# 2.4. Person_role table --------------------------------------------------
person.df <- read_file('person')  %>%
  inner_join(institution_fk_tbl, by = 'institution_name') 

person_id_d <- get_id(person.df, 'person', constrains_db = constrains_db) %>% select(person_id = id, constr_name('person'))

read_file('person') %>%
  select(role,last_name,first_name) %>%
  inner_join(.,person_id_d, by = c("last_name", "first_name")) %>%
  mutate(site_id = site_id_d) %>%
  mutate(role = case_when(role == 'Contact and Data owner' ~ 1,
                          role == 'Data owner' ~ 2,
                          role == 'Contact' ~ 3)) %>%
  select(-last_name,-first_name,-institution_code) %>%
  mutate(id = row_number()) %>%
  check_append_db(., 'person_role', constrains_db = constrains_db) %>%
  append_data('person_role')


# 2.5. Publication table --------------------------------------------------
publication.df <- read_file('publication') %>%
  filter(!is.na(first_author_last_name)) %>%
  select(-X__1) %>%
  mutate(site_id = site_id_d)

publication.df %>%
  mutate(id = row_number()) %>%
  check_append_db(., 'publication',constrains_db = constrains_db) %>%
  append_data('publication')

# 1.4 Tree ----------------------------------------------------------------
tree.df <- read_file('tree') %>%
  mutate(site_id = site_id_d)


tree.df %>%
  select(site_id, selection("tree", "main")) %>%
  filter(!is.na(tree_label)) %>%
  mutate(id = row_number()) %>%
  check_append_db(., 'tree',constrains_db = constrains_db) %>%
  append_data('tree')

tree_id_d <- get_id(tree.df, 'tree', constrains_db = constrains_db) %>% select(tree_id = id, constr_name('tree'))


# 1.5 Tree parameters -----------------------------------------------------

tree.df %>%
  select(tree_label, species_code, selection("tree", "param")) %>%
  gather(parameter, value, -tree_label, -species_code) %>%
  inner_join(tree_id_d, by = c("tree_label", "species_code")) %>%
  select(tree_id, parameter, value) %>%
  filter(!is.na(value)) %>%
  inner_join(tree_param_fk_tbl, by = 'parameter') %>%
  check_append_db(., 'tree_param',constrains_db = constrains_db) %>%
  append_data('tree_param')

# 1.6 Sample --------------------------------------------------------------
sample.df <- read_file('sample') %>%
  rename(description = organ) %>%
  inner_join(tree_id_d, by = "tree_label") %>%
  inner_join(organ_fk_tbl, by = "description")

sample.df %>%
  select(tree_id, selection("sample", "main")) %>%
  mutate(id = row_number()) %>%
  check_append_db(., 'sample',constrains_db = constrains_db) %>%
  append_data('sample')

sample_id_d <- get_id(sample.df, 'sample', constrains_db = constrains_db) %>% select(sample_id = id, constr_name('sample'))


# 1.7 Sample parameters ---------------------------------------------------

sample.df %>%
  select(sample_label, selection("sample", "param")) %>%
  gather(parameter, value, -sample_label) %>%
  inner_join(sample_id_d, by = "sample_label") %>%
  select(sample_id, parameter, value) %>%
  filter(!is.na(value)) %>%
  inner_join(sample_param_fk_tbl, by = 'parameter') %>%
  check_append_db(., 'sample_param',constrains_db = constrains_db) %>%
  append_data('sample_param')

meas_met_id_d <- get_id(sample.df, 'meas_met_fk', constrains_db = constrains_db) %>% pull(id) %>% unique()


# 1.8 Subsample -----------------------------------------------------------
subsample.df <- read_file('subsample') %>%
  inner_join(sample_id_d, by = "sample_label") %>%
  select(subpiece_label, sample_id, selection("subsample", "main"), selection("subsample", "param")) %>% 
  mutate(meas_date = as.numeric(meas_date),
         meas_date = as.Date(meas_date, origin = "1899-12-30"),
         meas_met_id = meas_met_id_d) 

subsample.df %>%
  mutate( id = row_number()) %>%
  check_append_db(., 'subsample',constrains_db = constrains_db) %>%
  append_data('subsample')

subsample_id_d <- get_id(subsample.df, 'subsample', constrains_db = constrains_db) %>% select(subsample_id = id, data_filename, constr_name('subsample'))


# 1.9 read measurements ---------------------------------------------------

meas_d <- load_roxas_measurements(file_dir = file_dir, subsample_id = subsample_id_d)

meas_d$dontmatch

# 2.0 Subsample parameters ------------------------------------------------
meas_met_rx <- meas_d$setting %>%
  inner_join(subsample_id_d, by = c('data_filename')) %>%
  select(subsample_id, parameter, value)


subsample.df %>%
  inner_join(subsample_id_d, by = c("subpiece_label", "sample_id", "meas_date", "meas_met_id", 'data_filename')) %>%
  select(subsample_id, selection("subsample", "param")) %>%
  gather(parameter, value, -subsample_id) %>%
  filter(!is.na(value))%>%
  anti_join(distinct(meas_met_rx, subsample_id, parameter), by = c('subsample_id', 'parameter')) %>%
  bind_rows(meas_met_rx) %>%
  inner_join(meas_met_param_fk_tbl, by = 'parameter') %>%
  check_append_db(., 'meas_met_set',constrains_db = constrains_db) %>%
  append_data('meas_met_set')


# 2.1 Ring table ----------------------------------------------------------

year.db <- meas_d$year %>%
  inner_join(subsample_id_d, by = c('data_filename')) 

year.db %>%
  distinct(subsample_id, data_filename, year) %>%
  mutate(id = row_number()) %>%
  check_append_db(., 'ring',constrains_db = constrains_db) %>%
  append_data('ring')

ring_id_d <- get_id(distinct(year.db, subsample_id, data_filename, year), 'ring', constrains_db = constrains_db) %>% select(ring_id = id, constr_name('ring'))


# 2.2 Year table ----------------------------------------------------------
year.db %>%
  inner_join(ring_id_d, by = c("year", "subsample_id")) %>%
  inner_join(meas_param_fk_tbl, by = 'parameter') %>%
  check_append_db(., 'year',constrains_db = constrains_db) %>%
  append_data('year')


# 2.3. Cell table ---------------------------------------------------------
meas_d$cell %>%
  filter(value >= -1000000, !is.na(x_cal),!is.na(y_cal))  %>%
  inner_join(subsample_id_d, by = c('data_filename')) %>%
  inner_join(ring_id_d, by = c("year", "subsample_id")) %>%
  inner_join(meas_param_fk_tbl, by = 'parameter') %>%
  check_append_db(., 'cell',constrains_db = constrains_db) %>%
  filter(value!="Inf") %>%
  append_data('cell')




