retrieveSQLData <- function(SQLtableVersion,
                            SQLServer,
                            SQLDatabase,
                            birdSpecies){
  
# Trials to make SQL work on a new Linux machine: from here 
# https://docs.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-2017
# In the terminal:
# 1. Check the version of Ubuntu: lsb_release -a
# 2. Log in as root: sudo -s
# 3. curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
# 4. Install the appropriate version from the website:
# curl https://packages.microsoft.com/config/ubuntu/16.04/prod.list > /etc/apt/sources.list.d/mssql-release.list
# 5. sudo apt-get update
# 6. sudo ACCEPT_EULA=Y apt-get install msodbcsql17
  
  # curl https://packages.microsoft.com/keys/microsoft.asc | sudo apt-key add -
  # curl https://packages.microsoft.com/config/ubuntu/16.04/prod.list | sudo tee /etc/apt/sources.list.d/msprod.list
  # sudo apt-get update 
  # sudo apt-get install mssql-tools unixodbc-dev
  # sudo apt-get update 
  # sudo apt-get install mssql-tools 
  # sudo apt-get install unixodbc unixodbc-dev freetds-dev freetds-bin tdsodbc

  #Copy the odbc folder from .../modules/glmerBirdModels/data/odbc to /usr/lib/x86_64-linux-gnu/odbc
  # in the ~/.../modules/glmerBirdModels/data/ directory
  # sudo mkdir /usr/lib/x86_64-linux-gnu/odbc
  # sudo cp * /usr/lib/x86_64-linux-gnu/odbc
  # 
  # # AS OF 3rd APRIL 19: THIS MODULE IS NOT WORKING IN THE BC!!!
  
  reproducible::Require("DBI")
  reproducible::Require("odbc")
  #reproducible::Require("RODBC") # Not sure it is needed. If anything fails, use it
  browser()
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