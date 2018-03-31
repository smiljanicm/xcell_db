

dbGetConstrains <- function(dbcon){
  #' @description functions return the list of columnames that shall be unique for each table
  #' @param dbcon a DBI connection
  #' @return tibble with tree columns, table name, column name, constrains
  #' 
  r <- dbSendQuery(dbcon, "select conrelid::regclass AS table, pg_get_constraintdef(c.oid) AS constrains
                   from   pg_constraint c where  contype in ('p','u') order by contype")
  dbConstrains <- dbFetch(r) 
  dbClearResult(r)
  
  dbConstrains <- dbConstrains %>% 
    as.tibble() %>%
    mutate(type = gsub("([ ].*)", "", constrains),
           constrains = gsub("(.*[(])","",constrains),
           constrains = gsub("([)].*)", "", constrains), 
           constrains = gsub('|[(]|[)]|["]', '', constrains)) %>%
    separate(constrains, letters, sep = ',') %>%
    gather(lett, constrains, -table, -type) %>%
    mutate(constrains = trimws(constrains)) %>%
    filter(!is.na(constrains),
           !constrains %in% 'id') %>%
    dplyr::select(-lett) %>%
    arrange(table)
  
  return(dbConstrains)
}


dbWriteXcell <- function(table.name =  NULL, df = NULL){
  #' @description function to write data to the database,
  #' @param table.name name of the table to be added 
  #' @param df a data.frame to be appended
  
  db_tbl <- tbl(dbcon, table.name)
  bc_colnames <- colnames(db_tbl)
  # constrains
  unique_col<- constrains_db %>% filter(table %in% table.name) %>% pull(constrains)
  # data to be excluded

  exclude_df <- inner_join(db_tbl, dplyr::select(df, unique_col),  by = unique_col, copy = TRUE, auto_index = TRUE) %>% collect()

    if(nrow(exclude_df) > 0){
    df <- anti_join(df, exclude_df, by = unique_col)
  }
  
  if( nrow(df) > 0){
    
    #add empty columns
    new_col <- setdiff(bc_colnames, colnames(df))
    if(length(new_col) > 0){
      df[, new_col] <- NA
    }
    
    # add id column
    if(any(bc_colnames %in% 'id')){
      last.id.list <- dbGetQuery(dbcon, paste("SELECT id FROM", table.name, "ORDER BY id DESC LIMIT 1"))
      if (length(last.id.list) == 0) 
        n <- 0
      else n <- last.id.list[[1]]
      df$id <- 1:nrow(df) + n
    }
  
    df <- df %>% dplyr::select(bc_colnames)
  
    print(paste0('Adding ', nrow(df), ' entries.'))
    dbWriteTable(conn = dbcon, name = table.name, value = df, overwrite = FALSE, append = TRUE, row.names=FALSE)
  }else{
    print('No data to be added')
  }
  
}



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




load_roxas_measurements_Vessel <- function( file_dir = NULL, sample_id = new.measurement_info ){
  #' @description function load each of the excel file (output of the roxas), combine them together and remove duplicates
  #' @param file_dir - directory where all the files are stored
  #' @param sample_id - a data frame with at least two columns containing sample_label and data_filename. By default is new.measurement_info
  #' @return a list of three tables: annual table, cell table and settings table
  
  if(is.null(file_dir)) stop('please provide the location of the files')
  if(is.null(sample_id)) stop('please provide the measurement information table')
  
  roxas_list <- list.files(file_dir, pattern = 'Output.xls', recursive = T, full.names = T)
  
  # output files
  roxas_annual_rings <- tibble()
  roxas_cells <- tibble()
  roxas_settings <- tibble()
  
  for(FL in roxas_list){
    
    rx_image_id <- read_xlsx(FL, "Overall summary", n_max = 1) %>% colnames() %>% gsub('OVERALL SUMMARY  --  ','',.) %>% paste0(.,'_Output.xlsx')
    
    rx_annual <- read_xlsx(FL, "Annual rings", skip = 2) %>% filter(row_number() < which(is.na(Year))[1]) %>% mutate_all(funs(as.numeric)) %>% mutate(data_filename = rx_image_id) 
    
    rx_cells <- read_xlsx(FL, 3, skip = 2) %>% dplyr::select(-contains('X__1'),-contains('**********')) %>% mutate_all(funs(as.numeric)) %>% mutate(data_filename = rx_image_id)
#    rx_cells <- read_xlsx(FL, "Vessels", skip = 2) %>% dplyr::select(-contains('X__1'),-contains('**********')) %>% mutate_all(funs(as.numeric)) %>% mutate(data_filename = rx_image_id)
    
    rx_settings <- read_xlsx(FL, "ROXAS settings", skip = 2) %>%  set_names(c('id','value', 'variable')) %>%
      filter(!is.na(variable)) %>% mutate(variable = gsub('<- ', '', variable)) %>%
      dplyr::select(-id) %>% spread(variable, value) %>% mutate(data_filename = rx_image_id)
    
    cat('\n', FL, '\n')
    
    roxas_annual_rings <- bind_rows(roxas_annual_rings, rx_annual)
    roxas_cells <- bind_rows(roxas_cells, rx_cells)
    roxas_settings <- bind_rows(roxas_settings, rx_settings)
  }
  
  if(!'MajAx' %in% colnames(roxas_cells)) roxas_cells$MajAx <- NA
  if(!'NbrNo' %in% colnames(roxas_cells)) roxas_cells$NbrNo <- NA
  if(!'NbrDst' %in% colnames(roxas_cells)) roxas_cells$NbrDst <- NA
  if(!'RVGI' %in% colnames(roxas_annual_rings)) roxas_annual_rings$RVGI <- NA
  if(!'RVSF' %in% colnames(roxas_annual_rings)) roxas_annual_rings$RVSF <- NA
  if(!'RGSGV' %in% colnames(roxas_annual_rings)) roxas_annual_rings$RGSGV <- NA

     
  sample_id <- sample_id %>% dplyr::select(data_filename, sample_label)
  
  #- files that didn't match
  wrong_file_names <- anti_join(roxas_annual_rings %>% dplyr::select(data_filename) , 
                                sample_id %>% dplyr::select(data_filename) ,
                                by = 'data_filename') %>% distinct(data_filename)
  
  roxas_annual_rings <- roxas_annual_rings %>% 
    dplyr::select(data_filename, year = Year, ring_width = MRW, ring_area = RA, rvgi = RVGI, rvsf = RVSF, rgsgv = RGSGV) %>%
    full_join(sample_id, ., by = 'data_filename')
  
  if(length(which(colnames(roxas_cells)=='VA'))>0) {colnames(roxas_cells)[which(colnames(roxas_cells)=='VA')]<-'CA'}
  roxas_cells <- roxas_cells %>% dplyr::select(data_filename, year = Year, x_cal = Xcal, y_cal = Ycal, asp = Asp, majax = MajAx, 
                                        nbrno = NbrNo, nbrdst = NbrDst, dist = RadDistR, rel_dist = RRadDistR, lum = CA) %>%
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







load_xray_measurements <- function( file_dir = NULL, sample_id = new.measurement_info ){
  #' @description function load each of the excel file (output of the roxas), combine them together and remove duplicates
  #' @param file_dir - directory where all the files are stored
  #' @param sample_id - a data frame with at least two columns containing sample_label and data_filename. By default is new.measurement_info
  #' @return a list of three tables: annual table, cell table and settings table
  
  if(is.null(file_dir)) stop('please provide the location of the files')
  if(is.null(sample_id)) stop('please provide the measurement information table')
  
  xray_list <- list.files(file_dir, pattern = "\\.t", recursive = T, full.names = T)
  
  xray_data <- tibble()

  # Create clean txt files
  for(FL in xray_list){
    xray_data<-bind_rows(
      xray_data,
      read_tsv(FL,skip=13, col_names=FALSE) %>% 
       mutate_all(funs(gsub('D:|J:|X:|Y:|,S:e|,S:s|,S:d', '', .))) %>%   
        separate(X1,c('id','density','zero','X','Y','A','year'), sep=',') %>% 
        filter(!is.na(X)) %>% 
        dplyr::select(-zero,-A) %>% 
        mutate_all(as.numeric) %>%
        mutate(data_filename = gsub('.*[/]','',FL)) %>%
        group_by(data_filename, year) %>%
        arrange(id) %>%
        mutate(dist = row_number()*10) %>%
        ungroup()
    )
    }
  
  xray_data_ring <- xray_data %>% 
    group_by(data_filename,year) %>%
    mutate(mxd = max(density), 
           mnd = min(density),
           T_EWLW = if_else(density>=(mxd+mnd)/2, 'lwd', 'ewd')) %>%
    summarise(ring_width = n()*10, 
              mxd = first(mxd),
              mnd= first(mnd),
              minID = min(id),
              maxID = max(id),
              eww = length(id[T_EWLW=='ewd']) *10,
              lww = length(id[T_EWLW=='lwd']) *10,
              ewd = mean(density[T_EWLW=='ewd']),
              lwd = mean(density[T_EWLW=='lwd']),
              ring_area = NA,
              rvgi = NA,
              rvsf = NA,
              rgsgv = NA)
#   ggplot() + geom_line(aes(year,ring_width, col=sample_label)) %>%

  xray_data_core <- xray_data %>%
    group_by(data_filename) %>%
    summarise(from = min(year, na.rm=TRUE),
              to = max(year, na.rm=TRUE))

  if(!'drad' %in% colnames(xray_data)) xray_data$drad <- NA_integer_
  if(!'dtan' %in% colnames(xray_data)) xray_data$dtan <- NA_integer_
  if(!'ldrad' %in% colnames(xray_data)) xray_data$ldrad <- NA_integer_
  if(!'ldtan' %in% colnames(xray_data)) xray_data$ldtan <- NA_integer_
  if(!'cwtrad' %in% colnames(xray_data)) xray_data$cwtrad <- NA_integer_
  if(!'cwttan' %in% colnames(xray_data)) xray_data$cwttan <- NA_integer_
  if(!'lum' %in% colnames(xray_data)) xray_data$lum <- NA_integer_
  if(!'cwa' %in% colnames(xray_data)) xray_data$cwa <- NA_integer_
  
  if(!'drad' %in% colnames(xray_data)) xray_data$drad <- NA_integer_
  return(list(xray_data_core = xray_data_core,
              xray_data_ring = xray_data_ring,
              xray_data = xray_data))

}







load_tracheid_row_measurements <- function( file_dir = NULL, sample_id = new.measurement_info ){
  #' @description function load each of the excel file (output of the roxas), combine them together and remove duplicates
  #' @param file_dir - directory where all the files are stored
  #' @param sample_id - a data frame with at least two columns containing sample_label and data_filename. By default is new.measurement_info
  #' @return a list of three tables: annual table, cell table and settings table
  
  if(is.null(file_dir)) stop('please provide the location of the files')
  if(is.null(sample_id)) stop('please provide the measurement information table')
  
  xrow_list <- list.files(file_dir, pattern = ".txt", recursive = T, full.names = T)
  
  xrow_data <- tibble()
  
  # Create clean txt files
  for(FL in xrow_list){
    xrow_data <- bind_rows(
      xrow_data,
      as.tbl(read.table(FL,header=TRUE,sep=";")) %>% ## FOR SILH/SILM/SILL
      mutate(row = ROW,
             position = CELL_ID, 
             year = YEAR,
             cwtrad = (CWT_left+CWT_right)/2,
             ldrad = LD) %>% 
      dplyr::select(-ROW,-CELL_ID,-YEAR,-LD) %>% 
      mutate(data_filename = gsub('.*[/]','',FL)) %>%
      group_by(data_filename, year) %>%
      ungroup()
    )
  }
      

  xrow_data_ring <- xrow_data %>% 
    group_by(data_filename,year) %>%
    summarise(ring_width = max(TRW),
              ring_area = NA,
              mxd = NA,
              mnd= NA,
              eww = NA,
              lww = NA,
              ewd = NA,
              lwd = NA,
              rvgi = NA,
              rvsf = NA,
              rgsgv = NA) 
  # %>% ggplot() + geom_line(aes(year,ring_width, col=data_filename)) 
  
  xrow_data_core <- xrow_data %>%
    group_by(data_filename) %>%
    summarise(from = min(year, na.rm=TRUE),
              to = max(year, na.rm=TRUE))
  
  if(!'drad' %in% colnames(xrow_data)) xrow_data$drad <- NA_integer_
  if(!'dtan' %in% colnames(xrow_data)) xrow_data$dtan <- NA_integer_
  if(!'ldrad' %in% colnames(xrow_data)) xrow_data$ldrad <- NA_integer_
  if(!'ldtan' %in% colnames(xrow_data)) xrow_data$ldtan <- NA_integer_
  if(!'cwtrad' %in% colnames(xrow_data)) xrow_data$cwtrad <- NA_integer_
  if(!'cwttan' %in% colnames(xrow_data)) xrow_data$cwttan <- NA_integer_
  if(!'lum' %in% colnames(xrow_data)) xrow_data$lum <- NA_integer_
  if(!'cwa' %in% colnames(xrow_data)) xrow_data$cwa <- NA_integer_
  
  
  return(list(xrow_data_core = xrow_data_core,
              xrow_data_ring = xrow_data_ring,
              xrow_data = xrow_data))
  
}






  
  load_tracheid_row_measurements_1 <- function( file_dir = NULL, sample_id = new.measurement_info ){
    #' @description function load each of the excel file (output of the roxas), combine them together and remove duplicates
    #' @param file_dir - directory where all the files are stored
    #' @param sample_id - a data frame with at least two columns containing sample_label and data_filename. By default is new.measurement_info
    #' @return a list of three tables: annual table, cell table and settings table
    
    if(is.null(file_dir)) stop('please provide the location of the files')
    if(is.null(sample_id)) stop('please provide the measurement information table')
    
    xrow_list <- list.files(file_dir, pattern = ".txt", recursive = T, full.names = T)
    if(file_dir=="~/Desktop/xcell/data_upload/data/XC_RU_KAN/") {
      xrow_list <- list.files(file_dir, pattern = "ALLCELL", recursive = T, full.names = T)
    }
    xrow_data <- tibble()
    
    # Create clean txt files
    for(FL in xrow_list){
      xrow_data <- bind_rows(
        xrow_data,
        as.tbl(read.table(FL,header=TRUE,sep="\t")) %>% ## FOR LIL
#       as.tbl(read.table(textConnection(gsub(";", "\t", readLines(FL))),header=TRUE)) %>%
          mutate(row = Row,
                 position = Position, 
                 year = as.numeric(Year),
                 cwtrad = CWT,
                 ldrad = LD) %>% 
          dplyr::select(-Row,-Position,-Year,-LD,-CWT,-LUM,-CWA) %>% 
          mutate(data_filename = gsub('.*[/]','',FL)) %>%
          group_by(data_filename, year) %>%
          ungroup()
      )
    }
    
    
    xrow_data_ring <- xrow_data %>% 
      group_by(data_filename,year) %>%
      summarise(ring_width = max(TRW),
                ring_area = NA,
                mxd = NA,
                mnd= NA,
                eww = NA,
                lww = NA,
                ewd = NA,
                lwd = NA,
                rvgi = NA,
                rvsf = NA,
                rgsgv = NA) 
    # %>% ggplot() + geom_line(aes(year,ring_width, col=data_filename)) 
    
    xrow_data_core <- xrow_data %>%
      group_by(data_filename) %>%
      summarise(from = min(year, na.rm=TRUE),
                to = max(year, na.rm=TRUE))
    
    if(!'drad' %in% colnames(xrow_data)) xrow_data$drad <- NA_integer_
    if(!'dtan' %in% colnames(xrow_data)) xrow_data$dtan <- NA_integer_
    if(!'ldrad' %in% colnames(xrow_data)) xrow_data$ldrad <- NA_integer_
    if(!'ldtan' %in% colnames(xrow_data)) xrow_data$ldtan <- NA_integer_
    if(!'cwtrad' %in% colnames(xrow_data)) xrow_data$cwtrad <- NA_integer_
    if(!'cwttan' %in% colnames(xrow_data)) xrow_data$cwttan <- NA_integer_
    if(!'lum' %in% colnames(xrow_data)) xrow_data$lum <- NA_integer_
    if(!'cwa' %in% colnames(xrow_data)) xrow_data$cwa <- NA_integer_
    
    
    return(list(xrow_data_core = xrow_data_core,
                xrow_data_ring = xrow_data_ring,
                xrow_data = xrow_data))
    
  }

  
  extract_climate_chelsa <- function(site.coord) {
    #' @description extract climate data from the 1km resolution chelsa climate data set 1979-2013
    #' @param site.coord - a data frame that include the site information including latitude and longitude in WGS 84
    #' @return a table: site_id, param,  year, month, value, source
    
    site.coord <- as.data.frame(site.coord[,c('id','latitude', 'longitude')])
    site.coord$latitude<-as.numeric(site.coord$latitude)
    site.coord$longitude<-as.numeric(site.coord$longitude)
    
    allID<-NULL
    for (i in 1:nrow(site.coord)) {
      print(site.coord$id[i])
 
    
    old.wd<-getwd()  
      
      # PREC data
    setwd(paste0("/Volumes/My Passport for Mac/Climate_data/Chelsa1979_2013/timeseries/",'prec'))
    tibble(fl = list.files(pattern = '_V1.2_land.tif')) %>%
      rowwise() %>%
      mutate(value = raster::extract(raster::raster(fl), site.coord[i, c("longitude","latitude")], "bilinear"),
              fl   = gsub('CHELSA_|_V1.2_land.tif', '', fl))  %>%
      separate(fl, c('param','month', 'year'), sep = '_') %>%
      mutate_at(vars(month, year), funs(as.numeric)) ->
      site.prec

    # Temp data
    setwd(paste0("/Volumes/My Passport for Mac/Climate_data/Chelsa1979_2013/timeseries/",'temp'))
    tibble(fl = list.files( pattern = '_V1.2.sdat$', recursive = TRUE,full.names = TRUE)) %>%
      rowwise() %>%
      mutate( value = raster::extract(raster::raster(fl), site.coord[i, c("longitude","latitude")] , "bilinear"),
              fl = gsub('.*CHELSA_|_V1.2.sdat', '', fl)) %>%
      separate(fl, c('param','month', 'year'), sep = '_') %>%
      mutate_at(vars(month, year), funs(as.numeric)) ->
      site.temp
    
    # retur results
    eachID<-bind_rows(site.prec, site.temp) %>%
      mutate(site_id = site.coord$id[i], 
             source = "chelsa")
    allID<-rbind(allID,eachID)
    }
    setwd(old.wd)
    return(allID)
  }
  
#  extract(raster("./CHELSA_temp_12_1979_V1.2.sdat"), site.coord[i, c('latitude', 'longitude')] , "bilinear")
  
  
  

  extract_climate_cru <- function(site.coord) {
    #' @description extract climate data from the 0.5 degree resolution cru climate data set 1901-2016
    #' @param site.coord - a data frame that include the site information including latitude and longitude in WGS 84
    #' @return a table: site_id, param,  year, month, value, source
    # to find CRU data on WEB: 
    # http://data.ceda.ac.uk//badc/cru/data/cru_ts/cru_ts_4.01/data/pre
    # http://data.ceda.ac.uk//badc/cru/data/cru_ts/cru_ts_4.01/data/temp
    
    library(ncdf4)
    library(chron)
    library(reshape2)
    
    longitude <- as.numeric(site.coord$longitude)
    latitude <- as.numeric(site.coord$latitude)
    
    CRU<-NULL
    for (i in c("tmp","pre")) {
    dname <- i
     ifelse(dname=="tmp", ncin<- nc_open("/Volumes/My Passport for Mac/Climate_data/CRU/CRU_4.01/cru_ts4.01.1901.2016.tmp.dat.nc"),
            ncin<- nc_open("/Volumes/My Passport for Mac/Climate_data/CRU/CRU_4.01/cru_ts4.01.1901.2016.pre.dat.nc"))
    
##  Create the empty data-frame for output
      CRU_4.01<-data.frame(year=0,lon=0,lat=0)
      CRU_4.01[,4:15]<-NA
      colnames(CRU_4.01)[4:15]<-1:12   # month.abb
      CRU_4.01<-CRU_4.01[-1,]
      
##  Standardise the latitude and longitude
      longitude<-as.numeric(substr(cut(longitude,breaks=seq(-180,180,0.5)),2,
                                   regexpr(",",cut(longitude,breaks=seq(-180,180,0.5)))-1))+0.25
      latitude<-as.numeric(substr(cut(latitude,breaks=seq(-90,90,0.5)),2,
                                  regexpr(",",cut(latitude,breaks=seq(-90,90,0.5)))-1))+0.25
      for(L in c(1:length(longitude))){
        site_id<-site.coord$id[L]
##  select which number is the Lat and long  
        LonStartIdx <- which( ncin$dim$lon$vals == longitude[L])
        LatStartIdx <- which( ncin$dim$lat$vals == latitude[L])
        Tleng<-length(ncvar_get(ncin, "time"))  #length of the time
        
#### Get the temporary array with the values
        tmp.array <- ncvar_get(ncin,
                               varid=dname,
                               start=c(LonStartIdx,LatStartIdx,1),
                               count=c(1,1,Tleng))

## Get the variable and its attributes, and verify the size of the array.
        fillvalue <- ncatt_get(ncin, dname, "_FillValue")
        tmp.array[tmp.array == fillvalue$value] <- NA
        
        tempDF<-as.data.frame(matrix(tmp.array,ncol=12,byrow=TRUE))
        colnames(tempDF)<-1:12 #month.abb
        
##    Get the year
        t <- ncvar_get(ncin, "time")
        tunits <- ncatt_get(ncin, "time", "units")
        tustr <- strsplit(tunits$value, " ")
        tdstr <- strsplit(unlist(tustr)[3], "-")
        tmonth = as.integer(unlist(tdstr)[2])
        tday = as.integer(unlist(tdstr)[3])
        tyear = as.integer(unlist(tdstr)[1])
        
        tempDF$year<-unique(substr(chron(t,origin=c(tmonth, tday, tyear),format="year-m-d"),1,4))
        
        tempDF$lon<-longitude[L]
        tempDF$lat<-latitude[L]
        tempDF$site_id<-site_id
        
        CRU_4.01<-rbind(CRU_4.01,tempDF)
      }
      
## make the Year as numeric
      CRU_4.01$year<-as.numeric(CRU_4.01$year)
      CRU_4.01.long <- melt(CRU_4.01[,!(colnames(CRU_4.01) %in% c("lon", "lat"))], id.vars = c("site_id","year"),
                variable.name = "month", 
                value.name = "value")
      ifelse(dname=="tmp",CRU_4.01.long$param<-"temp",CRU_4.01.long$param<-"prec")
      CRU_4.01.long$source<-"cru4.01"
      CRU_4.01.long$id<-1:nrow(CRU_4.01.long)
      CRU_4.01.long<-CRU_4.01.long[,c("id","site_id","param","year","month","value","source")]
      CRU_4.01.long$month<-as.numeric(CRU_4.01.long$month)
      
      CRU<-rbind(CRU,CRU_4.01.long)
    }
      
      return(CRU)
      
      # setwd("~/Desktop/xcell/data_upload/data_climate/CRU/Site_cru_Data")
      # write.table(CRU_4.01,
      #             file=paste("~/Desktop/xcell/data_upload/data_climate/CRU/Site_cru_Data/CRU_4.01_",dname, ".csv", sep = ""), 
      #             quote = FALSE, sep = ";") ##save csv file
  }
  
    



  
  extract_climate_EuDirk <- function(site.coord) {
    #' @description extract climate data from the 1 km resolution cru climate data set 1901-2000 (only Europe)
    #' @param site.coord - a data frame that include the site information including latitude and longitude in WGS 84
    #' @return a table: site_id, param,  year, month, value, source
    #   Data can be downloaded at 
    #  /////lsd/lud12/data/europe/clim/wgs84_30sec/cru_obs_ar3_wc/obs_ts_m_1901_2000_b5000/prec/
    ### very usefull
    # http://pakillo.github.io/R-GIS-tutorial/#raster
  
  
  library(rgdal)
  library(plyr)
  library(reshape2)
  
  longitude <- as.numeric(site.coord$longitude)
  latitude <- as.numeric(site.coord$latitude)
    
  EU<-NULL
  for (j in c("temp","prec")) {
    print(j)
    if(j=="temp") {
#      TaverTS<-as.data.frame(t(c(site.coord$id,longitude,latitude)))
      TaverTS<-site.coord[,c("id","longitude","latitude")]
      colnames(TaverTS)<-c("site_id","longitude","latitude")
      ## get the list of the data      ##### Data are on external disc!!!!!!!!!!
      TavList<-list.dirs("/Volumes/My Passport for Mac/Climate_data/Lsd_lud12_Data_europe_clim_wgs84_30sec/cru_obs_ar3_wc/obs_ts_m_1901_2000_b5000/tave",full.name=F)
      TavPath<-"/Volumes/My Passport for Mac/Climate_data/Lsd_lud12_Data_europe_clim_wgs84_30sec/cru_obs_ar3_wc/obs_ts_m_1901_2000_b5000/tave/"
    } 
    if(j=="prec") {    
#      TaverTS<-as.data.frame(t(c(site.coord$id,longitude,latitude)))
      TaverTS<-site.coord[,c("id","longitude","latitude")]
      colnames(TaverTS)<-c("site_id","longitude","latitude")
      ## get the list of the data      ##### Data are on external disc!!!!!!!!!!
      TavList<-list.dirs("/Volumes/My Passport for Mac/Climate_data/Lsd_lud12_Data_europe_clim_wgs84_30sec/cru_obs_ar3_wc/obs_ts_m_1901_2000_b5000/prec",full.name=F)
      TavPath<-"/Volumes/My Passport for Mac/Climate_data/Lsd_lud12_Data_europe_clim_wgs84_30sec/cru_obs_ar3_wc/obs_ts_m_1901_2000_b5000/prec/"
    }
      
  for(Ttime in TavList[-c(1:2)]){ 
      print(Ttime)
    try(TaverTS[,paste(Ttime)] <- raster::extract(raster::raster(paste(TavPath,Ttime,"/w001001.adf",sep="")),
                                         TaverTS[,c("longitude","latitude")],
                                         method="bilinear") ,TRUE)
  }
  
  TaverColumn<-melt(TaverTS[,!colnames(TaverTS) %in% c("longitude","latitude")],
                    id="site_id",na.rm=TRUE)
  colnames(TaverColumn)[2:3]<-c("Time","value")
  
  TaverColumn$month<-as.numeric(substr(as.character(TaverColumn$Time), 10,nchar(as.character(TaverColumn$Time))))
  TaverColumn$year<-as.numeric(substr(as.character(TaverColumn$Time), 5,8))
  TaverColumn$source<-"eu"
  ifelse(j=="temp",TaverColumn$param<-"temp",TaverColumn$param<-"prec")
  TaverColumn<-TaverColumn[,!(colnames(TaverColumn) %in% c("Time"))]
  TaverColumn$id<-1:nrow(TaverColumn)
  TaverColumn<-TaverColumn[,c("id","site_id","param","year","month","value","source")]
  TaverColumn<-TaverColumn[with(TaverColumn, order(site_id, year,month)), ]
  TaverColumn$id<-1:nrow(TaverColumn)
  EU<-rbind(EU,TaverColumn)
  }
  return(EU)
  }
  

 

