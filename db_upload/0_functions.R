
# DATA PREPARATION FUNCTIONS ----------------------------------------------

extract_clim <- function(long, lat, f = '/Users/fonti/Desktop/xcell/xcell_db/db_upload/info/chelsa_clim.tif'){
  #' @description extract the temperature and precipitation from the raster
  #' @param long longitude in wgs84
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



spread_db <- function(df, key, value) {
  key_vals <- 
    df %>%
    select_(key) %>%
    distinct() %>%
    collect() %>%
    .[[1]] %>%
    sort()
  
  get_rows <- function(key_val) {
    df %>%
      rename_(.dots= setNames(key, "key")) %>%
      rename_(.dots= setNames(value, key_val)) %>%
      filter(key == key_val) %>%
      select(-key)
  }
  
  join_vars <- setdiff(colnames(df), c(key, value))
  print(join_vars)
  full_join_alt <- function(x, y) {
    full_join(x, y, by=join_vars)
  }
  
  df_new <- lapply(key_vals, get_rows)
  Reduce(full_join_alt, df_new) 
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
  
  if( nrow(d_table) > 0 ){ # if uploading table is not empty
    
    #-- which table and it's constrains
    col_unique <- dplyr::filter(constrains_db, table %in% table.name) %>% dplyr::pull(constrains) %>% unique() # remove unique after db reapload
    
    # test if db is non empty
    db_n <- tbl_x(table.name, sch) %>% head(1) %>% collect() %>% nrow()
    
    if(db_n > 0) {
      
      if(table.name %in% 'cell') {
        
        r_id <- unique(d_table$ring_id)
        
        d_table_t <- tbl_x(table.name, sch) %>% 
          filter(ring_id %in% r_id) %>%
          select(-value) %>%
          collect()
        
        if(nrow(d_table_t) > 0 ){
          d_table_t <- inner_join(d_table_t, d_table, by = col_unique)
        }
          
        
      }else{
        
        # find which already in the database
        d_table_t <- select(d_table, col_unique)
        
        d_table_t <- dplyr::inner_join(
          dplyr::select_(tbl_x(table.name, sch), .dots = col_unique),
          d_table_t,
          by = col_unique,
          copy = T, auto_index = TRUE) %>% 
          collect() 
        
      }

      
      # join with the other data
      if(nrow(d_table_t) > 0){
        d_table <- dplyr::anti_join(d_table, d_table_t, by = col_unique)
      }
      
    }
    
    if(any(colnames(d_table) %in% 'id')){
      
      if(db_n > 0 ){
        # get the last max id
        max.id <- tbl_x(table.name, sch) %>% dplyr::summarize(max(id, na.rm = T)) %>% dplyr::pull()
        
        # compare which data already in db, and replace the id's for all
        d_table %>%
          mutate(id = row_number() + max.id,
                 id = as.integer(id))->
          d_table
      }else{
        d_table$id <- 1:nrow(d_table)
      }
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
  #' @description function to get the id of the d_table data in the table.name
  #' @param d_table - the table from which we want to know the id in the database
  #' @param constrain_db - the table column in table.name that make the unique 
  #' @return a table data from which we want to know the id in the database
  
  col_unique <- dplyr::filter(constrains_db, table %in% table.name) %>% dplyr::pull(constrains)
  
  inner_join(
    dplyr::select(tbl_x(table.name, sch),id, col_unique),
    d_table,
    by = col_unique,
    copy = TRUE, auto_index = F
  ) %>%
    collect()
}

db_delete_cascade <- function( x, table.name = 'site', sch = 'v1', x_col = 'id'){
  #' @description function to delete cascade based on the id of the row
  #' @param x an vector of id's to be removed
  #' @param table.name name of the top table in the database where cascade start
  #' @param sch scheme where table is located
  #' @param x_col a column name based on which id's will be selected
  #' db_delete_cascade(x = c(1000, 2000), table.name = 'ring')
  
  dbExecute(dbcon, paste0('DELETE FROM ', sch, '.', table.name, ' WHERE ', x_col, ' in (', paste(x, collapse = ','), ')') )
}



# READ FILES -------------------------------------------------------

read.TXT_FR<-function(fl=fl, data=data)  {   
  #' @description function to load each of the txt file (output of the roxas) from the UniFreiburg data type
  #' @param fl - directory of the file
  #' @param data - a data table 
  #' @return a table data with a selection of data from the txt.files
  data <- as.tbl(read.table(textConnection(gsub(";", "\t", readLines(fl))),header=TRUE)) %>%
    mutate(subpiece_label = TREE_ID,
           y_cal = TRW,
           x_cal = ROW,
           row = ROW,
           position = CELL_ID, 
           Year = as.numeric(YEAR),
           cwtrad = CWT,
           cwtle = CWT_left, 
           cwtri = CWT_right,
           ldrad = LD,
           rdist = x,
           dist = TRW) %>% 
    dplyr::select(subpiece_label,y_cal,x_cal,row,position,Year,cwtrad,cwtle,cwtri,ldrad,rdist,dist) %>% 
    mutate(data_filename = gsub('.*[/]','',fl)) %>%
    group_by(data_filename, Year, row) %>%
    ungroup()
}

read.TXT_RU<-function(fl=fl, data=data)  {
  #' @description function to load each of the txt file (output of the roxas) from the RUSSIAN (TRACHEID) data type
  #' @param fl - directory of the file
  #' @param data - a data table 
  #' @return a table data with a selection of data from the txt.files
  data <- as.tbl(read.table(fl,header=TRUE,sep="\t")) %>% ## FOR LIL
                      mutate(row = Row,
                             position = Position, 
                             Year = Year,
                             cwtrad = CWT,
                             ldrad = LD,
                             x_cal = Row) %>% 
                      dplyr::select(-Row,-Position,-LD,-CWT,-LUM,-CWA) %>% 
                      mutate(data_filename = gsub('.*[/]','',fl)) %>%
                      group_by(data_filename, Year, row) %>%
                      mutate(y_cal = cumsum(ldrad+2*cwtrad)) %>%
                      ungroup()
}

read.TXT_POT<-function(fl=fl, data=data)  {
  #' @description function to load each of the txt file (output of the roxas) from the RUSSIAN (TRACHEID) data type
  #' @param fl - directory of the file
  #' @param data - a data table 
  #' @return a table data with a selection of data from the txt.files
  data <- as.tbl(read.table(fl,header=TRUE,sep="\t")) %>% ## FOR LIL
    mutate(Year = Year,
           cwtrad = CWT,
           ldrad = LD,
           x_cal = TPos,
           y_cal = RPos) %>% 
    dplyr::select(-Row,-Position,-LD,-CWT,-LUM,-CWA) %>% 
    mutate(data_filename = gsub('.*[/]','',fl))
}


read.TXT_SEC<-function(fl=fl, data=data)  {
  #' @description function to load each of the txt file (output of the roxas) from the RUSSIAN (TRACHEID) data type
  #' @param fl - directory of the file
  #' @param data - a data table 
  #' @return a table data with a selection of data from the txt.files
  data <- as.tbl(read.table(fl,header=TRUE,sep="\t")) %>% ## FOR LIL
    mutate(row = Row,
           sector = BandPos, 
           position = BandPos,
           Year = Year,
           cwtrad = CWT,
           ldrad = LD,
           x_cal = 1) %>% 
    dplyr::select(-Row,-Position,-LD,-CWT,-LUM,-CWA) %>% 
    mutate(data_filename = gsub('.*[/]','',fl)) %>%
    group_by(data_filename, Year, row) %>%
    mutate(y_cal = cumsum(ldrad+2*cwtrad)) %>%
    ungroup()
}

read.TXT_CHI<-function(fl=fl, data=data)  {
  #' @description function to load each of the txt file (output of the roxas) from the RUSSIAN (TRACHEID) data type
  #' @param fl - directory of the file
  #' @param data - a data table 
  #' @return a table data with a selection of data from the txt.files
  data <- as.tbl(read.table(fl,header=TRUE,sep="\t")) %>% ## FOR LIL
    mutate(row = Row,
           position = Position, 
           Year = Year,
           cwtrad = CWT,
           drad = D,
           x_cal = Row) %>% 
    dplyr::select(-Row,-Position,-LD,-CWT,-LUM,-CWA) %>% 
    mutate(data_filename = gsub('.*[/]','',fl)) %>%
    group_by(data_filename, Year, row) %>%
    mutate(y_cal = cumsum(drad)) %>%
    ungroup()
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
      filter(row_number() < which(is.na(Year))[1], .[7] != 0) %>% 
      mutate_all(funs(as.numeric)) %>% 
      mutate_all(funs(ifelse(. == -999 | . == -9999, NA, .))) %>% 
      rename_cols(year_info) %>%
      select(year = Year, intersect(colnames(.), year_info)) %>%
      mutate(data_filename = rx_d_f_id)
    
    rx_cell <- read_xlsx(fl, excel_sheets(fl)[3], skip = 2) %>% 
      dplyr::select(-contains('X__1'),-contains('**********')) %>% 
      rename_cols(cell_info) %>%
      select(year = Year, intersect(colnames(.), cell_info)) %>%
      mutate_all(funs(as.numeric)) %>% 
      mutate_all(funs(ifelse(. == -999 | . == -9999, NA, .))) %>% 
      mutate(data_filename = rx_d_f_id)
    
    rx_setting <- read_xlsx(fl, "ROXAS settings", skip = 2) %>%  
      set_names(c('id','value', 'variable')) %>%
      filter(!is.na(variable)) %>%
      mutate(variable = gsub('<- ', '', variable)) %>%
      dplyr::select(-id)   %>% 
      dplyr::filter(., !grepl('Maximum tangential cell wall thickness of ', variable)) %>% 
      spread(variable, value) %>% 
      rename_cols(settings_info) %>%
      select(intersect(colnames(.), settings_info)) %>%
      mutate(meas_date = paste0(substr(meas_date,7,10),"-",substr(meas_date,4,5),"-",substr(meas_date,1,2))) %>%
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
  
  #--/ find duplicated years
  unique_samples_rx <- cell %>%
    group_by(year, data_filename) %>%
    count() %>%
    inner_join(subsample_id, by = 'data_filename') 
  
  cell.duplicate <- unique_samples_rx %>% group_by(sample_id, year) %>% filter(n() > 1)
  
  if(nrow(cell.duplicate) > 0 ){
    cat("\n *****CAUTION***** SOME YEARS ARE DUPLICATED!!!", distinct(cell.duplicate, sample_id, year) %>% nrow(), ' YEARS REPEATED')
  }  
  
  unique_samples_rx <- unique_samples_rx %>%
    group_by(sample_id, year) %>%
    filter(n == max(n)) %>%
    ungroup() %>%
    distinct(year, data_filename)
  
  #--/ select only unique files
  year <- inner_join(year, unique_samples_rx, by = c('data_filename', 'year'))
  cell <- inner_join(cell, unique_samples_rx, by = c('data_filename', 'year'))
  setting <- inner_join(setting, unique_samples_rx %>% distinct(data_filename),  by =c('data_filename'))
  
  
  #- reshape all the files
  if('cwtrad' %in% colnames(cell)) {
    cell <- cell %>% 
      mutate(drad = ((4 * cwtrad) / rtsr) + 2 * cwtrad,
             dtan = ((4 * cwttan) / rtsr) + 2 * cwttan) 
  }
  
  cell <- cell %>%
    gather(parameter, value, -year, -data_filename, -x_cal, -y_cal) %>%
    mutate_at(vars(x_cal, y_cal), funs(round(., 4))) %>%
    filter(!is.na(value))
  
  # calculate the yearly mean from cell table
  year_cell_param <- cell %>% 
    group_by(data_filename, year, parameter ) %>%
    summarise(value = mean(value, na.rm = T))
  
  n_cell <- cell %>% 
    group_by(data_filename, year) %>%
    summarise(value = n_distinct(paste0(x_cal, y_cal))) %>%
    mutate(parameter = 'n_obs')
  
  # bind year table
  year <- year %>% 
    gather(parameter, value, -year, -data_filename) %>%
    bind_rows( year_cell_param ) %>%
    bind_rows( n_cell ) %>%
    filter(!is.na(value)) %>%
    filter(!parameter %in% c('row', 'position', 'dist', 'rdist'))
  
  setting <- setting %>% 
    gather(parameter, value, -data_filename) %>%
    filter(!is.na(value))
  
  return(list(setting = setting,
              year = year,
              cell = cell,
              dontmatch = dontmatch))
}



load_txt_measurements <- function( file_dir = NULL, subsample_id = subsample_id_d, file_type = 'RU'){
  #' @description function load each of the txt file (output from software with row measurements), combine them together and remove duplicates
  #' @param file_dir - directory where all the files are stored
  #' @param sample_id - a data frame with at least two columns containing sample_label and data_filename. By default is new.measurement_info
  #' @param file_type - specify the function for reading the different roxas file types
  #' @return a list of three tables: annual table, cell table and settings table
  
  if(is.null(file_dir)) stop('please provide the location of the files')
  if(is.null(subsample_id)) stop('please provide the measurement information table')
  
  f_list <- list.files(file_dir, pattern = ".txt", recursive = T, full.names = T)
  
  # output files
  data <- tibble()
  year <- tibble()
  cell <- tibble()
  
  for(fl in f_list){
    
    cat(paste0('Proccessing file - ', fl, '\n'))
    
    data <- switch( file_type, 
                    FR = read.TXT_FR(fl, data),
                    CHI = read.TXT_CHI(fl, data),
                    RU = read.TXT_RU(fl, data),
                    SEC = read.TXT_SEC(fl, data),
                    POT = read.TXT_POT(fl, data) 
                    )

    
    tx_year <- data %>% 
      group_by(data_filename,Year) %>%
      summarise(ring_width = max(y_cal))  %>%
      mutate_all(funs(as.numeric)) %>% 
      mutate_all(funs(ifelse(. %in% c(-999, -9999), NA, .))) %>% 
      rename_cols(year_info) %>%
      select(data_filename, year = Year, intersect(colnames(.), year_info))
    # %>% ggplot() + geom_line(aes(year,ring_width, col=data_filename)) 
    
    tx_cell <- data %>%
      # mutate(x_cal = row*100, y_cal= Dist ) %>%   # here we differentiate the x-coordinates to avoid duplicates between rows 
      group_by(data_filename) %>%
      rename_cols(cell_info) %>%
      select(data_filename, year = Year, intersect(colnames(.), cell_info)) %>%
      mutate_all(funs(as.numeric)) %>% 
      mutate_all(funs(ifelse(. %in% c(-999, -9999), NA, .)))
    
    year <- bind_rows(year, tx_year)
    cell <- bind_rows(cell, tx_cell)
  }
  
  #- files that didn't match
  
  dontmatch <- anti_join(distinct(year, data_filename), subsample_id, by = c('data_filename'))
  
  if(nrow(dontmatch) > 0){
    cat("\n *****CAUTION***** SOME FILES DON'T MATCH!!!")
  }
  
  #--/ find duplicates
  unique_samples_tx <- cell %>%
    distinct(year, data_filename) %>%
    inner_join(subsample_id, by = 'data_filename') %>%
    group_by(sample_id, year, data_filename) %>%
    summarise(n = n()) %>%
    group_by(sample_id, year) %>%
    filter(n == max(n)) %>%
    ungroup() %>%
    dplyr::select(-n, -sample_id) 
  
  #--/ find duplicates
  unique_cells_tx <- cell %>%
    distinct(year, data_filename, row, position) %>%
    inner_join(subsample_id, by = 'data_filename') %>%
    group_by(sample_id, year, data_filename,row, position) %>%
    summarise(n = n()) %>%
    group_by(sample_id, year) %>%
    filter(n == max(n)) %>%
    ungroup() %>%
    dplyr::select(-n, -sample_id)
  
  
  #--/ select only unique files
  year <- inner_join(year, unique_samples_tx, by = c('data_filename', 'year'))
  cell <- inner_join(cell, unique_samples_tx, by = c('data_filename', 'year'))
  
  #- reshape all the files
  cell <- cell %>%
    gather(parameter, value, -year, -data_filename, -x_cal, -y_cal) %>%
    mutate_at(vars(x_cal, y_cal), funs(round(., 4))) %>%
    filter(!is.na(value))
  
  # calculate the yearly mean from cell table
  year_cell_param <- cell %>% 
    group_by(data_filename, year, parameter ) %>%
    summarise(value = mean(value, na.rm = T))
  
  n_cell <- cell %>% 
    group_by(data_filename, year) %>%
    summarise(value = n_distinct(paste0(x_cal, y_cal))) %>%
    mutate(parameter = 'n_obs')
  
  # bind year table
  year <- year %>% 
    gather(parameter, value, -year, -data_filename) %>%
    bind_rows( year_cell_param ) %>%
    bind_rows( n_cell ) %>%
    filter(!is.na(value)) %>%
    filter(!parameter %in% c('row', 'position', 'dist', 'rdist'))
  
  return(list(year = year,
              cell = cell,
              dontmatch = dontmatch))
}




load_xray_measurements <- function( file_dir = NULL, subsample_id = subsample_id_d ){
  #' @description function load each of the txt file (output from software with row measurements), combine them together and remove duplicates
  #' @param file_dir - directory where all the files are stored
  #' @param sample_id - a data frame with at least two columns containing sample_label and data_filename. By default is new.measurement_info
  #' @return a list of three tables: annual table, cell table and settings table
  
  if(is.null(file_dir)) stop('please provide the location of the files')
  if(is.null(subsample_id)) stop('please provide the measurement information table')
  
  f_list <- list.files(file_dir, pattern = "\\.t", recursive = T, full.names = T)
  
  # output files
  data <- tibble()
  year <- tibble()
  cell <- tibble()
  
  for(fl in f_list){
    
    cat(paste0('Proccessing file - ', fl, '\n'))
    
   data<-read_tsv(fl,skip=13, col_names=FALSE) %>% 
          mutate_all(funs(gsub('D:|J:|X:|Y:|,S:e|,S:s|,S:d', '', .))) %>%   
          separate(X1,c('id','density','zero','X','Y','A','year'), sep=',') %>% 
          filter(!is.na(X)) %>% 
          dplyr::select(-zero,-A) %>% 
          mutate_all(as.numeric) %>%
          mutate(x_cal = X, y_cal = Y) %>%
          mutate(data_filename = gsub('.*[/]','',fl)) %>%
          group_by(data_filename, year) %>%
          arrange(id) %>%
          mutate(dist = row_number()*10) %>%
          ungroup()
    
    tx_year <- data %>% 
      group_by(data_filename,year, x_cal, y_cal) %>%
      summarise(ring_width = max(dist))  %>%
      mutate_all(funs(as.numeric)) %>% 
      rename_cols(year_info) %>%
      select(data_filename, year = year, intersect(colnames(.), year_info))
    # %>% ggplot() + geom_line(aes(year,ring_width, col=data_filename)) 
    
    tx_cell <- data %>%
      group_by(data_filename) %>%
#      rename_cols(cell_info) %>%
      select(data_filename, year = year, intersect(colnames(.), cell_info)) %>%
      mutate_all(funs(as.numeric)) %>% 
      mutate_all(funs(ifelse(. %in% c(-999, -9999), NA, .)))
    
    year <- bind_rows(year, tx_year)
    cell <- bind_rows(cell, tx_cell)
  }
  
  #- files that didn't match
  
  dontmatch <- anti_join(distinct(year, data_filename), subsample_id, by = c('data_filename'))
  
  if(nrow(dontmatch) > 0){
    cat("\n *****CAUTION***** SOME FILES DON'T MATCH!!!")
  }
  
  #--/ find duplicates
  unique_samples_tx <- cell %>%
    distinct(year, data_filename,x_cal, y_cal) %>%
    inner_join(subsample_id, by = 'data_filename') %>%
    group_by(sample_id, year, data_filename) %>%
    summarise(n = n()) %>%
    group_by(sample_id, year) %>%
    filter(n == max(n)) %>%
    ungroup() %>%
    dplyr::select(-n, -sample_id) 
  
  #--/ select only unique files
  year <- inner_join(year, unique_samples_tx, by = c('data_filename', 'year'))
  cell <- inner_join(cell, unique_samples_tx, by = c('data_filename', 'year'))
  
  #- reshape all the files
  cell <- cell %>%
    gather(parameter, value, -year, -data_filename, -x_cal, -y_cal) %>%
    filter(!is.na(value))
  
  # calculate the yearly mean from cell table
  year_cell_param <- cell %>% 
    group_by(data_filename, year, parameter ) %>%
    summarise(value = mean(value, na.rm = T))
  
  n_cell <- cell %>% 
    group_by(data_filename, year) %>%
    summarise(value = n_distinct(paste0(x_cal, y_cal))) %>%
    mutate(parameter = 'n_obs')
  
  # bind year table
  year <- year %>% 
    gather(parameter, value, -year, -data_filename) %>%
    bind_rows( year_cell_param ) %>%
    bind_rows( n_cell ) %>%
    filter(!is.na(value)) %>%
    filter(!parameter %in% c('row', 'position', 'dist', 'rdist'))
  
  
  
  return(list(year = year,
              cell = cell,
              dontmatch = dontmatch))
}




