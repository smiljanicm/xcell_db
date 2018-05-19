# Connection --------------------------------------------------------------
library(pool)
library(DBI)
library(tidyverse)

source('pw.R')
source('db_upload/0_functions.R')

# Global table ------------------------------------------------------------

union(
  tbl_x('cell')  %>% select(-x_cal,-y_cal)  %>%  filter(param_id %in% c(24, 25, 30, 31, 33, 37)), #ldrad,ldtan,cwtrad,cwttan,la,cwa
  tbl_x('year') %>% filter(param_id %in% c(1, 3, 4))
) %>%
  right_join(tbl_x('ring'), by = c('ring_id' = 'id')) %>%
  right_join(tbl_x('subsample') %>% select(subsample_id = id, sample_id, meas_met_id), by = c('subsample_id')) %>%
  group_by(sample_id, meas_met_id, param_id) %>%
  summarise(n_rings=n_distinct(ring_id),
            from=min(year, na.rm=TRUE),
            to=max(year, na.rm=TRUE),
            value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  right_join(tbl_x('sample') %>% select(sample_id = id, tree_id, organ), by='sample_id') %>%
#  right_join(tbl_x('tree') %>% select(tree_id=id,site_id,species_code) , by="tree_id") %>%
  right_join(tbl_x('tree') %>% select(tree_id=id, site_id,species_code) %>% 
            right_join(tbl_x('tree_param') %>% filter(tree_param_id %in% c(5)) %>%  mutate(manip=value) %>% select(tree_id,manip,-value), by="tree_id") , by="tree_id") %>%
  group_by(site_id, organ, meas_met_id, species_code, param_id, manip) %>%
  summarise(value = mean(value, na.rm = T),
            from = min(from, na.rm = T),
            to = max(to, na.rm = T),
            n_rings = sum(n_rings, na.rm = T),
            n_radii = n_distinct(sample_id),
            n_trees = n_distinct(tree_id)) %>%
  group_by(site_id, organ, meas_met_id, species_code) %>%
  mutate(n_rings = max(n_rings, na.rm = T),
         from = min(from, na.rm = T),
         to = max(to, na.rm = T)) %>%
  ungroup() %>%
  inner_join(tbl_x('meas_param_fk') %>% select(param_id = id, parameter), by = 'param_id') %>%
  select(-param_id) %>%
  spread_db('parameter', 'value') %>%
  right_join(tbl_x('site') %>% select(site_id=id,year= sampling_year, site_code,site_label,country_code,longitude,latitude,elevation, temp, prec), by='site_id') %>%
  inner_join(tbl_x('species_fk') %>% select(species_code, wood_type, leaf_habit, wood_plane), by="species_code") %>%
  inner_join(tbl_x('meas_met_fk') %>% select(meas_met_id=id,output, hardware, software), by='meas_met_id') %>%
  left_join(tbl_x('person_role') %>% filter(role %in% 1) %>% select(site_id, person_id) , by = 'site_id') %>%
  left_join(tbl_x('person') %>% select(person_id = id, last_name, first_name, email, institution_code), by = 'person_id') %>% 
  ungroup() %>%
  arrange(site_code) %>%
  select(site_code, year, site_label, manip, country_code, longitude, latitude, elevation, temp, prec, 
         species_code, wood_type, leaf_habit, wood_plane, organ, output, hardware, software, 
         last_name, first_name, email, institution_code, 
         n_trees, n_radii, n_rings, from, to, ring_width,la,ldrad,ldtan,cwtrad,cwttan,cwa) %>% collect()  -> A #eww, lww

A %>%
  append_data(., 'global_table')


#tbl_x('publication') %>% 
#  filter(site_id %in% 2) %>%
#  select(site_id, first_author=first_author_last_name, pub_year=year,journal,doi)


