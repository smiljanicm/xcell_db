
# DATA PREPARATION FUNCTIONS ----------------------------------------------

extract_clim <- function(long, lat, f = '/Users/fonti/Desktop/xcell/xcell_db/db_upload/info/chelsa_clim.tif'){
  #' @description extract the temperature and precipitation from the raster
  #' @param long longiture in wgs84
  #' @param lat latitude in wgs84
  #' @return a data frame with two columns
  raster::extract(raster::brick(f), tibble(long, lat), method="bilinear") %>% 
    as.data.frame() %>%
    set_names(c('temp', 'prec'))
}

constr_name <- function(t){
  #' @description get the columns for the constrains
  #' @param t table to which the constrains are searched 
  constr_short[names(constr_short) %in% t]
  
}

rename_cols <- function(., c_table){
  plyr::rename(., c_table[names(c_table) %in% colnames(.)])
}

read_file <- function(t){
  
  ts <- get(paste0(t, '_info'))
  
  read_xlsx(paste0(file_dir,file_name), t) %>%
    filter(!row_number() %in% c(1:2)) %>%
    rename_cols(ts)
}

read_info <- function(t = 'person', f = 'db_upload/info/info_table.xlsx'){
  
  read_xlsx(f, t) %>% select(xcel_name, db_name) %>%  deframe()
}


selection <- function(table, search) {
  #' @description get the columns for the table from the info file
  #' @param table table to which the columns are searched
  #' @param search main = for the main table ; param for the param table
  xtbl <- read_xlsx('db_upload/info/info_table.xlsx', table)
  sel <- filter(xtbl, tbl %in% search) %>% pull(db_name)
    
  return(sel)
}


# UPLOAD FUNCTIONS --------------------------------------------------------

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
  col_unique <- dplyr::filter(constrains_db, table %in% table.name) %>% dplyr::pull(constrains) %>% unique() # remove unique after db reapload
  
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


# READ MEASUREMENTS -------------------------------------------------------

load_roxas_measurements <- function( file_dir = NULL, subsample_id = subsample_id_d ){
  #' @description function load each of the excel file (output of the roxas), combine them together and remove duplicates
  #' @param file_dir - directory where all the files are stored
  #' @param sample_id - a data frame with at least two columns containing sample_label and data_filename. By default is new.measurement_info
  #' @return a list of three tables: annual table, cell table and settings table
  
  if(is.null(file_dir)) stop('please provide the location of the files')
  if(is.null(subsample_id)) stop('please provide the measurement information table')
  
  f_list <- list.files(file_dir, pattern = 'Output.xlsx', recursive = T, full.names = T)
  
  # output files
  year <- tibble()
  cell <- tibble()
  setting <- tibble()
  
  
  
  for(fl in f_list){
    
    cat(paste0('Proccessing file - ', fl, '\n'))
    
    rx_d_f_id <- read_xlsx(fl, "Overall summary", n_max = 1) %>% 
      colnames() %>% 
      gsub('OVERALL SUMMARY  --  ','',.) %>% 
      paste0(.,'_Output.xlsx')
    
    rx_year <- read_xlsx(fl, "Annual rings", skip = 2) %>% 
      filter(row_number() < which(is.na(Year))[1]) %>% 
      mutate_all(funs(as.numeric)) %>% 
      mutate_all(funs(ifelse(. == -999, NA, .))) %>% 
      rename_cols(year_info) %>%
      select(year = Year, intersect(colnames(.), year_info)) %>%
      mutate(data_filename = rx_d_f_id)
    
    rx_cell <- read_xlsx(fl, "Cells", skip = 2) %>% 
      dplyr::select(-contains('X__1'),-contains('**********')) %>% 
      rename_cols(cell_info) %>%
      select(year = Year, intersect(colnames(.), cell_info)) %>%
      mutate_all(funs(as.numeric)) %>% 
      mutate_all(funs(ifelse(. == -999, NA, .))) %>% 
      mutate(data_filename = rx_d_f_id)
    
    rx_setting <- read_xlsx(fl, "ROXAS settings", skip = 2) %>%  
      set_names(c('id','value', 'variable')) %>%
      filter(!is.na(variable)) %>%
      mutate(variable = gsub('<- ', '', variable)) %>%
      dplyr::select(-id) %>% 
      spread(variable, value) %>% 
      rename_cols(settings_info) %>%
      select(intersect(colnames(.), settings_info)) %>%
      mutate(data_filename = rx_d_f_id)
    
    year <- bind_rows(year, rx_year)
    cell <- bind_rows(cell, rx_cell)
    setting <- bind_rows(setting, rx_setting)
  }
  
  #- files that didn't match
  
  dontmatch <- anti_join(distinct(year, data_filename), subsample_id, by = c('data_filename'))
  
  if(nrow(dontmatch) > 0){
    cat("\n *****CAUTION***** SOME FILES DON'T MATCH!!!")
  }
  
  #--/ find duplicates
  unique_samples_rx <- cell %>%
    distinct(year, data_filename) %>%
    inner_join(subsample_id, by = 'data_filename') %>%
    group_by(sample_id, year, data_filename) %>%
    summarise(n = n()) %>%
    group_by(sample_id, year) %>%
    filter(n == max(n)) %>%
    ungroup() %>%
    dplyr::select(-n, -sample_id) 
  
  
  #--/ select only unique files
  year <- inner_join(year, unique_samples_rx, by = c('data_filename', 'year'))
  cell <- inner_join(cell, unique_samples_rx, by = c('data_filename', 'year'))
  setting <- inner_join(setting, unique_samples_rx %>% distinct(data_filename),  by =c('data_filename'))
  
  #- reshape all the files
  year <- year %>% 
    gather(parameter, value, -year, -data_filename) %>%
    filter(!is.na(value))
  
  cell <- cell %>% 
    mutate(drad = ((4 * cwtrad) / rtsr) + 2 * cwtrad,
           dtan = ((4 * cwttan) / rtsr) + 2 * cwttan) %>%
    gather(parameter, value, -year, -data_filename, -x_cal, -y_cal) %>%
    mutate_at(vars(x_cal, y_cal), funs(round(., 4))) %>%
    filter(!is.na(value))
    
    
  setting <- setting %>% 
    gather(parameter, value, -data_filename) %>%
    filter(!is.na(value))
  
  return(list(setting = setting,
              year = year,
              cell = cell,
              dontmatch = dontmatch))
}

