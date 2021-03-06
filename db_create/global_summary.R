# Connection --------------------------------------------------------------
library(pool)
library(DBI)
library(tidyverse)

source('pw.R')
source('db_upload/0_functions.R')


# Global table ------------------------------------------------------------
tbl_x('year') %>% 
  filter(param_id %in% c(1, 3, 4, 24, 25, 30, 31, 33, 37, 50)) %>%
  right_join( tbl_x('ring'), by = c('ring_id' = 'id')) %>%
  right_join( tbl_x('subsample') %>% select(subsample_id = id, sample_id, meas_met_id), by = c('subsample_id')) %>%
  group_by(sample_id, meas_met_id, param_id) %>%
  summarise(n_rings = n_distinct(ring_id),
            from = min(year, na.rm=TRUE),
            to = max(year, na.rm=TRUE),
            value = ifelse(param_id %in% 50, sum(value, na.rm = T), mean(value, na.rm = T))) %>%
  ungroup() %>%
  right_join( tbl_x('sample') %>% select(sample_id = id, tree_id, organ), by='sample_id') %>%
  right_join( tbl_x('tree') %>% select(tree_id = id, site_id, species_code) , by="tree_id") %>%
  left_join( tbl_x('tree_param') %>%  filter(tree_param_id %in% c(5)) %>% select(tree_id, manip = value), by="tree_id") %>%
  group_by(site_id, organ, meas_met_id, species_code, param_id, manip) %>%
  summarise(value = ifelse(param_id %in% 50, sum(value, na.rm = T), mean(value, na.rm = T)),                    
            from = min(from, na.rm = T),
            to = max(to, na.rm = T),
            n_rings = sum(n_rings, na.rm = T),
            n_radii = n_distinct(sample_id),
            n_trees = n_distinct(tree_id)) %>%
  group_by(site_id, organ, meas_met_id, species_code) %>%
  mutate_at( vars(n_trees, n_radii, n_rings, to), funs(max(., na.rm = T))) %>%
  mutate_at( vars(from), funs(min(., na.rm = T))) %>%
  ungroup() %>%
  mutate(value = round(value, 4)) %>%
  inner_join(tbl_x('meas_param_fk') %>% select(param_id = id, parameter), by = 'param_id') %>%
  select(-param_id) %>%
  spread_db('parameter', 'value') %>%
  inner_join( tbl_x('site') %>% select(site_id = id, year = sampling_year, site_code, site_label, country_code, longitude, latitude, elevation, temp, prec), by='site_id') %>%
  inner_join(tbl_x('species_fk') %>% select(species_code, wood_type, leaf_habit, wood_plane), by="species_code") %>%
  inner_join(tbl_x('meas_met_fk') %>% select(meas_met_id = id,output, hardware, software), by='meas_met_id') %>%
  left_join(tbl_x('person_role') %>% filter(role %in% 1) %>% select(site_id, person_id) , by = 'site_id') %>%
  left_join(tbl_x('person') %>% select(person_id = id, last_name, first_name, email, institution_code), by = 'person_id') %>% 
  ungroup() %>%
  arrange(site_code) %>%
  select(site_id, site_code, year, site_label, manip, country_code, longitude, latitude, elevation, temp, prec, 
         species_code, wood_type, leaf_habit, wood_plane, organ, output, hardware, software, 
         last_name, first_name, email, institution_code, 
         n_trees, n_radii, n_rings, n_obs, from, to, ring_width,la,ldrad,ldtan,cwtrad,cwttan,cwa) ->
  global_tbl

strs <- Sys.time()
  global_tbl
Sys.time() - strs

global_tbl %>% dbplyr::sql_render() %>% write_lines('db_create/global_summary_sql.txt')


# Patrick adjustments -----------------------------------------------------

A[which(A$site_code=="SCH"),"ring_width"]<-A[which(A$site_code=="SCH"),"ring_width"]/5
A[which(A$site_code=="LIL"),"ring_width"]<-A[which(A$site_code=="LIL"),"ring_width"]/5
A[which(A$site_code=="SILH"),"ring_width"]<-A[which(A$site_code=="SILH"),"ring_width"]/5
A[which(A$site_code=="SILM"),"ring_width"]<-A[which(A$site_code=="SILM"),"ring_width"]/5
A[which(A$site_code=="SILL"),"ring_width"]<-A[which(A$site_code=="SILL"),"ring_width"]/5
A[which(A$site_code=="ALT"),"ring_width"]<-A[which(A$site_code=="ALT"),"ring_width"]/5
A[which(A$site_code=="IND"),"ring_width"]<-A[which(A$site_code=="IND"),"ring_width"]/5
A[which(A$site_code=="IND2"),"ring_width"]<-A[which(A$site_code=="IND2"),"ring_width"]/5
A[which(A$site_code=="NAN"),"ring_width"]<-A[which(A$site_code=="NAN"),"ring_width"]/5
A[which(A$site_code=="GOF"),"ring_width"]<-A[which(A$site_code=="GOF"),"ring_width"]/5
A[which(A$site_code=="LAK"),"ring_width"]<-A[which(A$site_code=="LAK"),"ring_width"]/5
A[which(A$site_code=="Hei"),"ring_width"]<-A[which(A$site_code=="Hei"),"ring_width"]/5
A[which(A$site_code=="REN"),"ring_width"]<-A[which(A$site_code=="REN"),"ring_width"]/5
A[which(A$site_code=="FLA"),"ring_width"]<-A[which(A$site_code=="FLA"),"ring_width"]/5
A[which(A$site_code=="Tura"),"ring_width"]<-A[which(A$site_code=="Tura"),"ring_width"]/3
A[which(A$site_code=="ZOT"),"ring_width"]<-A[which(A$site_code=="ZOT"),"ring_width"]/3
A[which(A$site_code=="YAK"),"ring_width"]<-A[which(A$site_code=="YAK"),"ring_width"]/5
A[which(A$site_code=="TAY"),"ring_width"]<-A[which(A$site_code=="TAY"),"ring_width"]/5
A[which(A$site_code=="KOTH"),"ring_width"]<-A[which(A$site_code=="KOTH"),"ring_width"]/5
A[which(A$site_code=="KOTH"),"ring_width"]<-A[which(A$site_code=="KOTH"),"ring_width"]/5
A[which(A$site_code=="KAN"),"ring_width"]<-A[which(A$site_code=="KAN"),"ring_width"]/5
A[which(A$site_code=="CHI"),"ring_width"]<-A[which(A$site_code=="CHI"),"ring_width"]/5
A[which(A$site_code=="SOR"),"ring_width"]<-A[which(A$site_code=="SOR"),"ring_width"]/5
A[which(A$site_code=="KAK"),"ring_width"]<-A[which(A$site_code=="KAK"),"ring_width"]/5

A[11,"wood_type"]<-"gymnosperm"
A[11,"leaf_habit"]<-"evergreen"
A[11,"wood_plane"]<-"conifer"

#dbExecute(dbcon, 'DELETE FROM v1.global_table')

#A %>% append_data(., 'global_table')

#tbl_x('publication') %>% 
#  filter(site_id %in% 2) %>%
#  select(site_id, first_author=first_author_last_name, pub_year=year,journal,doi)


