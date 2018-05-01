#' Get the database constrains, on unique observations
#'
#' @param dbcon a DBI connection
#' @param sch a schema for which constrains shall be returned. Default `level1`
#' @return tibble with tree columns, table name, column name, constrains
#' @importFrom DBI dbSendQuery dbFetch dbClearResult
#' @importFrom dplyr filter mutate select arrange
#' @importFrom tidyr separate gather
#' @export

db_get_constrains <- function(dbcon, sch = 'v1'){
  
  r <- DBI::dbSendQuery(dbcon, "select conrelid::regclass AS table, pg_get_constraintdef(c.oid) AS constrains from   pg_constraint c where  contype in ('p','u') order by contype")
  
  dbConstrains <- DBI::dbFetch(r)
  DBI::dbClearResult(r)
  
  dbConstrains %>%
    tibble::as_tibble() %>%
    #dplyr::filter( substr(constrains, 1, 6) %in% 'UNIQUE') %>%
    dplyr::mutate( constrains = gsub('UNIQUE [(]|[)]|["]|PRIMARY KEY [(]', '', constrains)) %>%
    tidyr::separate(constrains, letters, sep = ',') %>%
    tidyr::gather(lett, constrains, -table) %>%
    dplyr::mutate(constrains = trimws(constrains)) %>%
    dplyr::filter(!is.na(constrains), !constrains %in% 'id') %>%
    dplyr::select(-lett) %>%
    tidyr::separate(table, c('schema', 'table'), sep = '\\.') %>%
    dplyr::filter(schema %in% sch) %>%
    dplyr::arrange(table) ->
    constr.df
  
  return(constr.df)
}



#' Function first check if some id's need to be renamed: check if there are data
#' already in DB and keep id's for that part, and add the new ones
check_append_db <- function(d_table, table.name, sch = 'v1', constrains_db){
  #-- which table and it's constrains
  col_unique <- dplyr::filter(constrains_db, table %in% table.name) %>% dplyr::pull(constrains)
  
  # test if db is non empty
  db_n <- tbl_x(table.name, sch) %>% dplyr::summarize(n()) %>% dplyr::pull()
  
  if(db_n > 0) {
    
    d_table <- dplyr::anti_join(
      d_table,
      dplyr::distinct_(tbl_x(table.name, sch), .dots = col_unique),
      by = col_unique,
      copy = TRUE, auto_index = F
    )
  }
  
  if(any(colnames(d_table) %in% 'id')){
    
    if(db_n > 0){
      # get the last max id
      max.id <- tbl_x(table.name, sch) %>% dplyr::summarize(max(id, na.rm = T)) %>% dplyr::pull()
      
      # compare which data already in db, and replace the id's for all
      d_table %>%
        dplyr::collect() %>%
        mutate(id = row_number() + db_n,
          id = as.integer(id))->
        d_table
    }else{
      d_table$id <- 1:nrow(d_table)
    }
  }
    
return(d_table)
}


append_data <- function(d_table, table.name, sch = 'v1'){
  
  if( nrow(d_table) > 0 ){
    
    # rearange the columns
    colls <- tbl_x(table.name, sch) %>% colnames()
    
    d_table <- dplyr::select(d_table, colls)
    
    print(paste0('Adding ', nrow(d_table), ' entries to ', table.name, ' table.'))
    DBI::dbWriteTable(conn = dbcon, name = c(sch, table.name), value = d_table, overwrite = FALSE, append = TRUE, row.names=FALSE)
    
  }else{
    
    print(paste0('No data to be added to ',  table.name, ' table.'))
    
  }
  
}


get_id <- function(d_table, table.name, sch = 'v1', constrains_db){
  
  col_unique <- dplyr::filter(constrains_db, table %in% table.name) %>% dplyr::pull(constrains)
  
  inner_join(
    dplyr::select(tbl_x(table.name, sch),id, col_unique),
    d_table,
    by = col_unique,
    copy = TRUE, auto_index = F
  ) %>%
    collect()
}


#' extract_climate_chelsa <- function(site_i) {
#'   #' @description extract climate data from the 1km resolution chelsa climate data set 1979-2013
#'   #' @param site.coord - a data frame that include the site information including latitude and longitude in WGS 84
#'   #' @return a table: site_id, param,  year, month, value, source
#'   
#'   latitude<-as.numeric(site_i$Latitude)
#'   longitude<-as.numeric(site_i$Longitude)
#'   
#'     # PREC data
#'     setwd(paste0("~/Desktop/xcell/data_upload/data_climate/Chelsa1979_2013/Climatologies/",'prec'))
#'     tibble(fl = list.files(pattern = '_V1.2_land.tif')) %>%
#'       rowwise() %>%
#'       mutate(value = raster::extract(raster::raster(fl), t(as.data.frame(c(longitude,latitude))), method="bilinear"),
#'              fl   = gsub('CHELSA_|_V1.2_land.tif', '', fl))  %>%
#'       separate(fl, c('param','month'), sep = '_') %>%
#'       mutate_at(vars(month), funs(as.numeric)) ->
#'       site.prec
#'     site_i$Precip<-sum(site.prec$value)
#'     
#'     # Temp data
#'     setwd(paste0("~/Desktop/xcell/data_upload/data_climate/Chelsa1979_2013/Climatologies/",'temp'))
#'     tibble(fl = list.files( pattern = '_V1.2_land.tif')) %>%
#'       rowwise() %>%
#'       mutate( value = raster::extract(raster::raster(fl), t(as.data.frame(c(longitude,latitude))), method="bilinear"),
#'               fl = gsub('CHELSA_|_1979-2013_V1.2_land.tif', '', fl)) %>%
#'       separate(fl, c('param','month'), sep = '_') %>%
#'       mutate_at(vars(month), funs(as.numeric)) ->
#'       site.temp
#'     site_i$Temp<-mean(site.temp$value)
#'     setwd( "/Users/fonti/Desktop/xcell/xcell_db")
#'     # retur results
#'   return(site_i)
#' }


