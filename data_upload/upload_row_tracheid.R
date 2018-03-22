
# 0. Libraries ---------------------------------------------------

library(tidyverse); library(DBI); library(readxl); library(stringr)

source('data_upload/0_functions.R')


# 0. load the data --------------------------------------------------------
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_DE_SILH/'
#file_name <- 'XC_DE_SILH.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_DE_SILM/'
#file_name <- 'XC_DE_SILM.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_DE_SILL/'
#file_name <- 'XC_DE_SILL.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_DE_LIL/'
#file_name <- 'XC_DE_LIL.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_DE_SCH/'
#file_name <- 'XC_DE_SCH.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_ALT/'
#file_name <- 'XC_RU_ALT.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_IND/'
#file_name <- 'XC_RU_IND.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_IND2/'
#file_name <- 'XC_RU_IND2.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_NAN/'
#file_name <- 'XC_RU_NAN.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_GOF/'
#file_name <- 'XC_RU_GOF.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_DE_HEI/'
#file_name <- 'XC_DE_HEI.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_IT_REN/'
#file_name <- 'XC_IT_REN.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_SE_FLA/'
#file_name <- 'XC_SE_FLA.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_Tura/'
#file_name <- 'XC_RU_Tura.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_ZOT/'
#file_name <- 'XC_RU_ZOT.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_YAK/'
#file_name <- 'XC_RU_YAK.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_TAY/'
#file_name <- 'XC_RU_TAY.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_KOTL/'
#file_name <- 'XC_RU_KOTL.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_KOTH/'
#file_name <- 'XC_RU_KOTH.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_KAN/'
#file_name <- 'XC_RU_KAN.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_US_CHI/'
#file_name <- 'XC_US_CHI.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_RU_KAK/'
#file_name <- 'XC_RU_KAK.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_ES_SOR/'
#file_name <- 'XC_ES_SOR.xlsm'
#file_dir <- '~/Desktop/xcell/data_upload/data/XC_ES_PAL/'
#file_name <- 'XC_ES_PAL.xlsm'
file_dir <- '~/Desktop/xcell/data_upload/data/XC_ES_ENG/'
file_name <- 'XC_ES_ENG.xlsm'



#- RAW data
general.xls <- read_xlsx(paste0(file_dir,file_name), 'General') %>% .[,1:2] %>% set_names(c('xcel_name', 'value'))
tree.xls <- read_xlsx(paste0(file_dir,file_name), 'TreeTable')
measure.xls <- read_xlsx(paste0(file_dir,file_name), 'MeasuringTable', skip = 1)

#- INFO table
file_info <- '~/Desktop/xcell/data_upload/info_table.xlsx'
institution_info <- read_xlsx(file_info, 'institution')
person_info <- read_xlsx(file_info, 'person')
site_info <- read_xlsx(file_info, 'site')
tree_info <- read_xlsx(file_info, 'tree')
wood_sample_info <- read_xlsx(file_info, 'wood_sample')
measure_info <- read_xlsx(file_info, 'measure')
measure_sys_info <- read_xlsx(file_info, 'measure_sys')

#- Databse
source('pw.R')

#- FK
country_fk <- tbl(dbcon, 'country_fk') %>% select(country, country_code) %>% collect() %>% deframe()
target_proxy_fk <- tbl(dbcon, 'target_proxy_fk') %>% select(description, target_proxy) %>% collect() %>% deframe()
role_fk <- tbl(dbcon, 'role_fk') %>% select(description, id) %>% collect() %>% deframe()
species_fk <- tbl(dbcon, 'species_fk') %>% select(species, species_code) %>% collect() %>% deframe()
institution_fk <- tbl(dbcon, 'institution_fk') %>% pull(institution_code)

#- Constrains
constrains_db <- dbGetConstrains(dbcon)


# 1. Read the raw data from the xcell and measurements -----------------
# 1.1 Institution ---------------------------------------------------------
new.institution <- left_join( institution_info, general.xls, by = 'xcel_name' ) %>%
  group_by(db_name) %>%
  mutate(id = row_number()) %>%
  select(-xcel_name) %>%
  spread(db_name, value) %>%
  filter(!is.na(institution_code)) %>%
  distinct(institution_name, .keep_all = T) %>%
  mutate(country_code = country_fk[country_code]) 

# 1.2 Person --------------------------------------------------------------
new.person <- left_join( person_info, general.xls, by = 'xcel_name' ) %>%
  group_by(db_name) %>%
  mutate(id = row_number()) %>%
  select(-xcel_name) %>%
  spread(db_name, value) %>%
  filter(!is.na(first_name)) %>%
  mutate(person_role = case_when(id == 1 & owner == 'yes' ~ 3,
                                 id == 1 & owner != 'yes' ~ 2,
                                 id >= 2 ~ 1))

# 1.3 Site ----------------------------------------------------------------
new.site <- left_join( site_info, general.xls, by = 'xcel_name' ) %>%
  group_by(db_name) %>%
  mutate(id = row_number()) %>%
  select(-xcel_name) %>%
  spread(db_name, value) %>%
  filter(!is.na(site_code)) %>%
  mutate(country_code = str_sub(country_code, -2),
         sampling_year = as.numeric(sampling_year),
         target_proxy = target_proxy_fk[target_proxy] %>% as.character())

# 1.4 Tree ----------------------------------------------------------------
new.tree <- left_join( tree_info, tree.xls, by = c('xcel_name' = "TREE")) %>%
  select(-xcel_name, -`Unit/remarks`) %>%
  gather(treeid, value, -db_name) %>%
  spread(db_name, value) %>%
  filter(!is.na(tree_label)) %>%
  mutate(species_code = species_fk[species_code] %>% as.character(),
         age = as.integer(age)) %>%
  mutate_at( vars(dbh, height), funs(as.numeric))

# 1.5 Wood sample ---------------------------------------------------------
new.wood_sample <- left_join(wood_sample_info,  tree.xls, by = c('xcel_name' = "TREE") ) %>%
  select(-xcel_name, -`Unit/remarks`) %>%
  group_by(db_name) %>%
  mutate(temp_id = row_number(db_name)) %>%
  gather(treeid, value, -db_name, -temp_id) %>%
  spread(db_name, value) %>%
  filter(!is.na(sample_label)) %>%
  mutate(radius = str_sub(sample_label, -1),
         tree_label = str_sub(sample_label, end = -3))

# 1.6 Measurement info ----------------------------------------------------
new.system <- left_join( measure_sys_info,  measure.xls, by = c('xcel_name' = 'Sample label') ) %>%
  select(db_name, new.wood_sample$sample_label) %>%
  gather(sample_label, value, -db_name) %>%
  spread(db_name, value)

new.measurement_info <- left_join( measure_info, measure.xls, by = c('xcel_name' = 'Sample label') ) %>%
  select(db_name, new.wood_sample$sample_label) %>%
  gather(sample_label, value, -db_name) %>%
  group_by(db_name) %>%
  mutate(temp_id = row_number(db_name),
         software_version = NA_character_) %>%
  spread(db_name, value) %>%
  filter(!is.na(editing_level)) 

# 1.7 Load the Xray data -------------------------------------------------
#xrow_output <- load_tracheid_row_measurements(file_dir = file_dir, sample_id = new.measurement_info)
xrow_output <- load_tracheid_row_measurements_1(file_dir = file_dir, sample_id = new.measurement_info)

#all.sites<-as.data.frame(tbl(dbcon, 'site'))
# 1.8 Extract the climate data -------------------------------------------------
# A Chelsa
chelsa <- extract_climate_chelsa(new.site)
# B Cru 4.1
cru<-extract_climate_cru(new.site) 
# C EU Dirk
new.site$longitude<-as.numeric(new.site$longitude)
new.site$latitude<-as.numeric(new.site$latitude)
if (new.site$longitude > -11 & new.site$longitude < 32 & new.site$latitude >34 & new.site$latitude <72) {
eu<-extract_climate_EuDirk(new.site)
eu$value[which(eu$param=="temp")]<-eu$value[which(eu$param=="temp")]/100
eu$value[which(eu$param=="prec")]<-eu$value[which(eu$param=="prec")]/10
}


# 2. UPLOAD the data ------------------------------------------------------
# Information about the site
sampling.year <- new.site$sampling_year
site.code <- new.site$site_code
country.code <- new.site$country_code
target.proxy <- new.site$target_proxy

# 2.1 Institution ---------------------------------------------------------
dbWriteXcell(table.name =  'institution_fk', df = new.institution)

# 2.2 Person ------------------------------------------------------------------
dbWriteXcell(table.name =  'person', df = new.person)

# 2.3 Site --------------------------------------------------------------------
dbWriteXcell(table.name = 'site', df = new.site)

# collect site id
site.id <- tbl(dbcon, 'site') %>% 
  filter(country_code %in% country.code,
         sampling_year %in% sampling.year,
         site_code %in% site.code,
         target_proxy %in% target.proxy) %>%
  pull(id)

# 2.3.1 Persone role ------------------------------------------------------
tbl(dbcon, 'person') %>%
  inner_join(., new.person %>% select(last_name, first_name, institution_code, person_role), by = c("last_name", "first_name", "institution_code"),  copy = T) %>%
  select(person_id = id, role = person_role) %>%
  collect() %>%
  mutate(site_id = site.id) %>%
  dbWriteXcell(table.name = 'person_role', df = .)

# 2.3.2 Clima ------------------------------------------------------
# A chelsa
chelsa$id<-1:nrow(chelsa)
chelsa$site_id<-site.id
chelsa$value<-round(chelsa$value,2)
chelsa<-chelsa[,c("id","site_id","param","year","month","value","source")]
dbWriteXcell(table.name = 'clima', df = chelsa)
# B cru
cru$site_id<-site.id
dbWriteXcell(table.name = 'clima', df = cru)
# C Eu Dirk
if (new.site$longitude > -11 & new.site$longitude < 32 & new.site$latitude >34 & new.site$latitude <72) {
  eu$site_id<-site.id
  dbWriteXcell(table.name = 'clima', df = eu)
}

# 2.4 Tree ----------------------------------------------------------------
dbWriteXcell(table.name = 'tree', df = new.tree %>% mutate(site_id = site.id))

# collect the tree id
tree.id <- tbl(dbcon, 'tree') %>% filter(site_id %in% site.id) %>% select(tree_id = id, site_id, tree_label, species_code) %>% collect

# 2.5 Wood sample ---------------------------------------------------------
new.wood_sample %>% 
  inner_join(., tree.id %>% select(tree_id, tree_label), by = 'tree_label') %>%
  dbWriteXcell(table.name = 'wood_sample', df = .)

# collect the wood_sample.id
wood_sample.id <- tbl(dbcon, 'wood_sample') %>% filter(tree_id %in% tree.id$tree_id) %>% select(sample_id = id, tree_id, sample_label, radius, organ, cell_type) %>% collect

# 2.6 Measurement info ----------------------------------------------------
inner_join(new.measurement_info, new.system, by = 'sample_label') %>%
  inner_join(., select(wood_sample.id, sample_label, sample_id), by = 'sample_label') %>%
  inner_join(., xrow_output$xrow_data_core, by = c("data_filename")) %>%
  mutate_at(vars(image_size, calibration, from, to), funs(as.numeric)) %>%
  mutate_at(vars(magnification), funs(as.character)) %>%
  mutate_at(vars(magnification, software_version), funs(ifelse(is.na(.),"NA",.))) %>%
  dbWriteXcell(table.name = 'measure_info', df = .)

# collect measure_info.id for specific sample
measure_info.id <- tbl(dbcon, 'measure_info') %>% filter(sample_id %in% wood_sample.id$sample_id) %>% select(subpiece_id = id, sample_id, data_filename, subpiece_label, system, software, software_version, magnification, editing_level) %>% collect

# 2.7 Ring information ----------------------------------------------------
xrow_output$xrow_data_ring %>%
  inner_join(., select(measure_info.id, data_filename, subpiece_id), by = 'data_filename') %>%
  ungroup() %>%
  dbWriteXcell(table.name = 'ring', df = .)

ring.id <- tbl(dbcon, 'ring') %>% filter(subpiece_id %in% measure_info.id$subpiece_id) %>% select(ring_id = id, subpiece_id, year) %>% collect

# 2.8 Xray data ------------------------------------------------------
inner_join(ring.id, measure_info.id, by = 'subpiece_id') %>%
  select(ring_id, data_filename, year) %>%
  inner_join(., xrow_output$xrow_data, by = c('data_filename', 'year')) %>% # filter(year==1949, row==1, position==1) %>% select(ring_id, year, Tree, position, row, TRW, ldrad) 
  dbWriteXcell(table.name = 'tracheid_row', df = .)


## For the case the table is too big
# temp <- inner_join(ring.id, measure_info.id, by = 'subpiece_id') %>%
#   select(ring_id, data_filename, year) %>%
#   inner_join(., roxas_output$roxas_cells, by = c('data_filename', 'year')) %>%
#   mutate_all(funs(ifelse(. == -999, NA, .)))
# 
# dim(temp)
# dbWriteXcell(table.name = 'tracheid_row', df = temp[1:500000,])
# dbWriteXcell(table.name = 'tracheid_row', df = temp[500001:1000000,])
# dbWriteXcell(table.name = 'tracheid_row', df = temp[1000001:1500000,])
# dbWriteXcell(table.name = 'tracheid_row', df = temp[1500001:2000000,])
# dbWriteXcell(table.name = 'tracheid_row', df = temp[2000001:2500000,])
# dbWriteXcell(table.name = 'tracheid_row', df = temp[2500001:3000000,])
# dbWriteXcell(table.name = 'tracheid_row', df = temp[3000001:3500000,])
# dbWriteXcell(table.name = 'tracheid_row', df = temp[3500001:nrow(temp),])
# dbWriteXcell(table.name = 'tracheid_row', df = temp[4000001:4500000,])
# dbWriteXcell(table.name = 'tracheid_row', df = temp[4500001:nrow(temp),])

dbDisconnect(dbcon)
