retrieveSQLData <- function(SQLtableVersion,
                            SQLServer,
                            SQLDatabase,
                            birdSpecies){
# FOR Ubuntu 18.04
  # sudo su
  # curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
  #   # Ubuntu 18.04
  #   curl https://packages.microsoft.com/config/ubuntu/18.04/prod.list > /etc/apt/sources.list.d/mssql-release.list
  # exit
  # sudo apt-get update
  # sudo ACCEPT_EULA=Y apt-get install msodbcsql17
  
  pathToDriver <- data.table(odbc::odbcListDrivers())[attribute == "Driver", value]

  con <- DBI::dbConnect(drv = odbc::odbc(), #RMySQL::MySQL(),
                        Driver   = pathToDriver, 
                        Server   = SQLServer, # Connect to BAM Server
                        # Setup = "/usr/lib/x86_64-linux-gnu/odbc/libtdsS.so",
                        # Note: even though the address is set up in Windows as boreal.biology.ualberta.ca\boreal,
                        # in R I need to exclude the \boreal
                        Database = SQLDatabase, # Got the names from the Windows ODBC connection
                        UID      = "michelet", #rstudioapi::askForPassword("Database User"),
                        PWD  = "Univ3rs!ty",#studioapi::askForPassword("Database Password"),
                        port      = 1433) ## 3306 default
  
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
  XY <- Cache(DBI::dbFetch, queryXY, userTags = "objectName:XY") %>% 
    data.table::data.table() %>%
    .[, ..colXY]
  
  message(crayon::yellow(paste0("Fetching ", PtCountTableName, " SQL table from ",  SQLServer, " :: ", 
                                SQLDatabase)))
  queryPtCount <- dbSendQuery(con, paste0("SELECT * FROM ", PtCountTableName))
  PtCount <- Cache(DBI::dbFetch, queryPtCount, userTags = "objectName:PtCount") %>%
    data.table::data.table() %>%
    .[, ..colPtCount]
  
  message(crayon::yellow(paste0("Fetching ", PKEYTableName, " SQL table from ",  SQLServer, " :: ", 
                                SQLDatabase)))
  queryPKEY <- dbSendQuery(con, paste0("SELECT * FROM ", PKEYTableName))
  PKEY <- Cache(DBI::dbFetch, queryPKEY, userTags = "objectName:PKEY") %>%
    data.table::data.table() %>%
    .[, ..colPKEY]

  return(list(PKEY = PKEY, PtCount = PtCount, XY = XY))
}