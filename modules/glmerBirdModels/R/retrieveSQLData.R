retrieveSQLData <- function(SQLtableVersion = sim$SQLtableVersion,
                            SQLServer = sim$SQLServer,
                            SQLDatabase = sim$SQLDatabase,
                            birdSpecies = sim$birdSpecies){
  
  reproducible::Require("DBI")
  reproducible::Require("odbc")
  #reproducible::Require("RODBC") # Not sure it is needed. If anything fails, use it
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "/usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so", # Path for the location of driver for Linux
                        Server   = SQLServer, # Connect to BAM Server
                        # Note: even though the address is set up in Windows as boreal.biology.ualberta.ca\boreal, in R I need to exclude the \boreal
                        Database = SQLDatabase, # Got the names from the Windows ODBC connection
                        UID      = "michelet", #rstudioapi::askForPassword("Database User"),
                        PWD      = "Univ3rs!ty",#studioapi::askForPassword("Database Password"),
                        Port     = 1433)
  
  colXY <- c("PCODE", "SS", "X", "Y") #, "BOR_LOC"
  colPtCount <- c("PCODE", "SS", "PKEY", "SPECIES", "ABUND", "DURATION", "DISTANCE") # Included for making the OFFSETS module
  colPKEY <- c("PCODE", "METHOD", "ROUND", "YYYY", "JULIAN", "SS", "PKEY", "SITE", "STN")
  
  tablesNames <- DBI::dbListTables(con, table_name = paste0("%", SQLtableVersion, "%"))
  
  XYTableName <- tablesNames[grepl(pattern = "XY", x = tablesNames)]
  PtCountTableName <- tablesNames[grepl(pattern = "PtCount", x = tablesNames)]
  PKEYTableName <- tablesNames[grepl(pattern = "PKEY", x = tablesNames)]
  
  message(crayon::yellow(paste0("Fetching ", XYTableName, " SQL table from ",  SQLServer, " :: ", 
                                SQLDatabase)))
  queryXY <- dbSendQuery(con, paste0("SELECT * FROM ", XYTableName)) #, "WHERE BOR_LOC LIKE"
  XY <- Cache(DBI::dbFetch, queryXY) %>% 
    data.table::data.table() %>%
    .[, ..colXY]
  
  message(crayon::yellow(paste0("Fetching ", PtCountTableName, " SQL table from ",  SQLServer, " :: ", 
                                SQLDatabase)))
  queryPtCount <- dbSendQuery(con, paste0("SELECT * FROM ", PtCountTableName))
  PtCount <- Cache(DBI::dbFetch, queryPtCount) %>%
    data.table::data.table() %>%
    .[, ..colPtCount]
  
  message(crayon::yellow(paste0("Fetching ", PKEYTableName, " SQL table from ",  SQLServer, " :: ", 
                                SQLDatabase)))
  queryPKEY <- dbSendQuery(con, paste0("SELECT * FROM ", PKEYTableName))
  PKEY <- Cache(DBI::dbFetch, queryPKEY) %>%
    data.table::data.table() %>%
    .[, ..colPKEY]

  return(list(PKEY = PKEY, PtCount = PtCount, XY = XY))
}