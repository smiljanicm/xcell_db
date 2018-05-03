
# 0. Libraries ---------------------------------------------------

library(raster); library(DBI); library(readxl); library(stringr); library(tidyverse) 

source('pw.R')
source('db_upload/0_functions.R')


# 0. load the data --------------------------------------------------------
file_dir <- 'db_upload/data/XC_CH_LTS22/'
file_name <- 'XC_CH_LTS22_NEW.xlsm'



# 0. Additional settings --------------------------------------------------
#' info help table
person_info <- read_info('person')
site_info <- read_info('site')
tree_info <- read_info('tree')
sample_info <- read_info('sample')
subsample_info <- read_info('subsample')


#' constrains tables (alternative key)
constrains_db <- db_get_constrains(dbcon) 
constr_short <- constrains_db %>% select(table, constrains) %>% deframe()

#' foreign key table
institution_fk_tbl <- tbl_x('institution_fk') %>% select(institution_code, institution_name) %>% collect()
country_fk_tbl <- tbl_x('country_fk') %>% collect()
site_param_fk_tbl <- tbl_x('site_param_fk') %>% select(site_param_id = id, parameter) %>% collect()
tree_param_fk_tbl <- tbl_x('tree_param_fk') %>% select(tree_param_id = id, parameter) %>% collect()
meas_met_param_fk_tbl <- tbl_x('meas_met_param_fk') %>% select(meas_met_param_id = id, parameter) %>% collect()
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
  select(site_id, selection("tree", "main")) %>%
  mutate(id = row_number()) %>%
  check_append_db(., 'tree',constrains_db = constrains_db) %>%
  append_data('tree')

tree_id_d <- get_id(tree.df, 'tree', constrains_db = constrains_db) %>% select(tree_id = id, constr_name('tree'))


# 1.5 Tree parameters -----------------------------------------------------

tree.df %>%
  select(tree_label, species_code, selection("tree", "param")) %>%
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
  select(tree_id, selection("sample", "main")) %>%
  mutate(id = row_number()) %>%
  check_append_db(., 'sample',constrains_db = constrains_db) %>%
  append_data('sample')

sample_id_d <- get_id(sample.df, 'sample', constrains_db = constrains_db) %>% select(sample_id = id, constr_name('sample'))


# 1.7 Sample parameters ---------------------------------------------------

sample.df %>%
  select(sample_label, selection("sample", "param")) %>%
  gather(parameter, value, -sample_label) %>%
  inner_join(sample_id_d) %>%
  select(sample_id, parameter, value) %>%
  filter(!is.na(value)) %>%
  inner_join(sample_param_fk_tbl, by = 'parameter') %>%
  check_append_db(., 'sample_param',constrains_db = constrains_db) %>%
  append_data('sample_param')


meas_met_id_d <- get_id(sample.df, 'meas_met_fk', constrains_db = constrains_db) %>% pull(id) %>% unique()




# 1.8 Subsample -----------------------------------------------------------
subsample.df <- read_file('subsample') %>%
  inner_join(sample_id_d) %>%
  select(subpiece_label, sample_id, selection("subsample", "main"), selection("subsample", "param")) %>% 
  mutate(meas_date = as.numeric(meas_date),
         meas_date = as.Date(meas_date, origin = "1899-12-30"),
         meas_met_id = meas_met_id_d) 

subsample.df %>%
  mutate( id = row_number()) %>%
  check_append_db(., 'subsample',constrains_db = constrains_db) %>%
  append_data('subsample')

subsample_id_d <- get_id(subsample.df, 'subsample', constrains_db = constrains_db) %>% select(subsample_id = id, constr_name('subsample'))

# 1.9 Subsample parameters ------------------------------------------------
subsample.df %>%
  inner_join(subsample_id_d) %>%
  select(subsample_id, selection("subsample", "param")) %>%
  gather(parameter, value, -subsample_id) %>%
  filter(!is.na(value)) %>%
  inner_join(meas_met_param_fk_tbl, by = 'parameter') %>%
  check_append_db(., 'meas_met_set',constrains_db = constrains_db) %>%
  append_data('meas_met_set')




load_roxas_measurements <- function( file_dir = NULL, sample_id = new.measurement_info ){
  #' @description function load each of the excel file (output of the roxas), combine them together and remove duplicates
  #' @param file_dir - directory where all the files are stored
  #' @param sample_id - a data frame with at least two columns containing sample_label and data_filename. By default is new.measurement_info
  #' @return a list of three tables: annual table, cell table and settings table
  
  if(is.null(file_dir)) stop('please provide the location of the files')
  if(is.null(sample_id)) stop('please provide the measurement information table')
  
  roxas_list <- list.files(file_dir, pattern = 'Output.xlsx', recursive = T, full.names = T)
  
  # output files
  roxas_annual_rings <- tibble()
  roxas_cells <- tibble()
  roxas_settings <- tibble()
  
  for(FL in roxas_list){
    
    rx_image_id <- read_xlsx(FL, "Overall summary", n_max = 1) %>% colnames() %>% gsub('OVERALL SUMMARY  --  ','',.) %>% paste0(.,'_Output.xlsx')
    
    rx_annual <- read_xlsx(FL, "Annual rings", skip = 2) %>% filter(row_number() < which(is.na(Year))[1]) %>% mutate_all(funs(as.numeric)) %>% mutate(data_filename = rx_image_id) 
    
    rx_cells <- read_xlsx(FL, "Cells", skip = 2) %>% dplyr::select(-contains('X__1'),-contains('**********')) %>% mutate_all(funs(as.numeric)) %>% mutate(data_filename = rx_image_id)
    
    rx_settings <- read_xlsx(FL, "ROXAS settings", skip = 2) %>%  set_names(c('id','value', 'variable')) %>%
      filter(!is.na(variable)) %>% mutate(variable = gsub('<- ', '', variable)) %>%
      dplyr::select(-id) %>% spread(variable, value) %>% mutate(data_filename = rx_image_id)
    
    cat('\n', FL, '\n')
    
    roxas_annual_rings <- bind_rows(roxas_annual_rings, rx_annual)
    roxas_cells <- bind_rows(roxas_cells, rx_cells)
    roxas_settings <- bind_rows(roxas_settings, rx_settings)
  }
  
  if(!'Drad' %in% colnames(roxas_cells)) roxas_cells$Drad <- NA
  if(!'Dtan' %in% colnames(roxas_cells)) roxas_cells$Dtan <- NA
  if(!'Bend' %in% colnames(roxas_cells)) roxas_cells$Bend <- NA
  if(!'CWA' %in% colnames(roxas_cells)) roxas_cells$CWA <- NA
  if(!'RWD' %in% colnames(roxas_cells)) roxas_cells$RWD <- NA
  
  sample_id <- sample_id %>% dplyr::select(data_filename, sample_label)
  
  #- files that didn't match
  wrong_file_names <- anti_join(roxas_annual_rings %>% dplyr::select(data_filename) , 
                                sample_id %>% dplyr::select(data_filename) ,
                                by = 'data_filename') %>% distinct(data_filename)
  
  roxas_annual_rings <- roxas_annual_rings %>% 
    dplyr::select(data_filename, year = Year, ring_width = MRW, ring_area = RA, rvgi = RVGI, rvsf = RVSF, rgsgv = RGSGV) %>%
    full_join(sample_id, ., by = 'data_filename')
  
  roxas_cells <- roxas_cells %>% dplyr::select(data_filename, year = Year, x_cal = Xcal, y_cal = Ycal, asp = Asp, dist = RadDistR, rel_dist = RRadDistR, ldrad = Drad, ldtan = Dtan, cwtpi = CWTpi, 
                                               cwtba = CWTba, cwtle = CWTle, cwtri = CWTri, cwttan = CWTtan, cwtrad = CWTrad, lum = CA, cwa = CWA, bend = Bend) %>%
    inner_join(., sample_id, by = 'data_filename')
  
  roxas_settings_clean <- roxas_settings %>% dplyr::select(data_filename, configuration_filename = Configuration, software_version = "ROXAS version") %>%
    inner_join(., sample_id, by = 'data_filename')
  
  
  
  
  #--/ find duplicates
  unique_samples_rx <- roxas_cells %>%
    group_by(sample_label, year, data_filename) %>%
    summarise(n = n()) %>%
    group_by(sample_label, year) %>%
    filter(n == max(n)) %>%
    dplyr::select(-n) %>%
    ungroup()
  
  roxas_annual_rings_clean <- inner_join(roxas_annual_rings, unique_samples_rx, by = c('sample_label', 'data_filename', 'year'))
  roxas_cells_clean <- inner_join(roxas_cells, unique_samples_rx, by = c('sample_label', 'data_filename', 'year'))
  
  roxas_settings_clean <- inner_join(roxas_settings_clean, unique_samples_rx %>% dplyr::select(-year) %>% distinct(data_filename, .keep_all = T), by = c('sample_label', 'data_filename')) %>%
    inner_join(.,
               roxas_annual_rings_clean %>%
                 group_by(data_filename) %>%
                 summarise(from = min(year),
                           to = max(year)),
               by = 'data_filename')  #%>% mutate(subpiece_label = gsub('_Output.xlsx','',data_filename))
  
  
  return(list(roxas_settings = roxas_settings_clean,
              roxas_annual_rings = roxas_annual_rings_clean,
              roxas_cells = roxas_cells_clean,
              roxas_dontmatch = wrong_file_names))
}

