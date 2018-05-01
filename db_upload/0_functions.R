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