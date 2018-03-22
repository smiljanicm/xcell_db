
# Connection --------------------------------------------------------------
library(pool)
library(DBI)
library(tidyverse)

dbcon <- dbPool(RPostgreSQL::PostgreSQL(),
                dbname = "Xcell",
                host = "postgresql-wsl.cmxphl52sxwm.us-west-2.rds.amazonaws.com",
                port = 5432,
                user = "xcell_user", 
                password = "Xcel1!con")



# Global table ------------------------------------------------------------

union(tbl(dbcon,'profile') %>% group_by(ring_id) %>% summarise_at(vars(density,ldrad,ldtan,cwtrad,cwttan,lum,cwa), mean),
      tbl(dbcon,'tracheid_full') %>% group_by(ring_id) %>% summarise_at(vars(ldrad,ldtan,cwtrad,cwttan,lum,cwa), mean) %>% mutate(density=NA)
) %>% 
  union(tbl(dbcon,'tracheid_row') %>% group_by(ring_id) %>% summarise_at(vars(ldrad,cwtrad,lum), mean) %>% mutate(density=NA,ldtan=NA,cwttan=NA,cwa=NA)) %>%
  union(tbl(dbcon,'vessel') %>% group_by(ring_id) %>% summarise_at(vars(lum), mean) %>% mutate(density=NA,ldtan=NA,cwttan=NA,cwa=NA,ldrad=NA,cwtrad=NA)) %>% 
  #tbl(dbcon,'profile') %>% group_by(ring_id) %>% summarise_at(vars(density,ldrad,ldtan,cwtrad,cwttan,lum,cwa), funs(mean)) %>% 
  inner_join(tbl(dbcon,'ring') %>% select(ring_id=id,subpiece_id,trw=ring_width,eww,lww), by='ring_id') %>% 
  inner_join(tbl(dbcon,'measure_info') %>% select(subpiece_id=id,sample_id,system, software,from,to), by='subpiece_id') %>%
  inner_join(tbl(dbcon,'wood_sample') %>% select(sample_id=id,tree_id,cell_type,organ), by='sample_id') %>%
  inner_join(tbl(dbcon,'tree') %>% select(tree_id=id,site_id,species_code), by="tree_id") %>%
  group_by(site_id,species_code,cell_type,organ,system,software) %>% 
  summarise(n_trees=n_distinct(tree_id),
            n_radii=n_distinct(sample_id),
            n_rings=n_distinct(ring_id),
            from=min(from),
            to=max(to),
            trw=mean(trw),
            eww=mean(eww),
            lww=mean(lww),
            density=mean(density),
            ldrad=mean(ldrad),
            ldtan=mean(ldtan),
            cwtrad=mean(cwtrad),
            cwttan=mean(cwttan),
            lum=mean(lum),
            cwa=mean(cwa)) %>% 
  inner_join(tbl(dbcon,'site') %>% select(site_id=id,year= sampling_year, site_code,site_label,country_code,longitude,latitude,elevation,target_proxy), by='site_id') %>% 
  left_join(tbl(dbcon,'clima') %>% filter(source=='chelsa', param=='temp') %>% group_by(site_id) %>% summarise(temp=mean(value)), by='site_id')  %>% 
  left_join(tbl(dbcon,'clima') %>% filter(source=='chelsa', param=='prec') %>% group_by(site_id,year) %>% summarise(prec=sum(value)) %>% group_by(site_id) %>% summarise(prec=mean(prec)), by='site_id') %>%
  inner_join(tbl(dbcon, 'person_role') %>% filter(role %in% c(2,3)) %>% select(site_id, person_id) , by = 'site_id') %>%
  inner_join(tbl(dbcon, 'person') %>% select(person_id = id, contact_last_name = last_name, contact_first_name = first_name, contact_email = email, contact_institution_code = institution_code), by = 'person_id') %>%
  inner_join(tbl(dbcon, 'cell_type_fk') %>% rename(cell_type_f = description), by = 'cell_type') %>%
  inner_join(tbl(dbcon, 'organ_fk') %>% rename(organ_f = description), by = 'organ') %>%
  inner_join(tbl(dbcon, 'target_proxy_fk') %>% rename(target_proxy_f = description), by = 'target_proxy') %>%
  ungroup() %>%
  arrange(site_code) %>%
  mutate(target_proxy = target_proxy_f, cell_type = cell_type_f, organ = organ_f,
         id = row_number()) %>%
  select(id, year, site_code, site_label, country_code, longitude, latitude, elevation, temp, prec, 
         species_code, target_proxy, cell_type, organ,system, software, 
         contact_last_name, contact_first_name, contact_email, contact_institution_code, 
         n_trees, n_radii, n_rings, from, to, trw, eww, lww, density,ldrad,ldtan,cwtrad,cwttan,lum,cwa)->
  global.query

global.query %>% dbplyr::sql_render() %>% write_lines('DB/globalsql.txt')

temp.df <- global.query %>% collect
