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
                        UID      = "michelet",#rstudioapi::askForPassword("Database User"),
                        PWD      = "Univ3rs!ty",#rstudioapi::askForPassword("Database Password"),
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

  # Cleaning as per Alberto ("Steps to create database.docx" located in his Dropbox folder called )
  # 2. Merging counts and XY
  Count_XY <- merge(PtCount, XY, by = "SS", all.x = TRUE, all.y = FALSE) # Obs.: we have several PCODE in XY that are NA's. However, PtCount has the info. 
  names(Count_XY)[names(Count_XY) == "PCODE.x"] <- "PCODE"
  Count_XY <- Count_XY[,!"PCODE.y"]
  
  # 2. Exclude rows without X or Y from Count-XY (n = 80); from  entriees we should end up with . All other columns have all info except BEH (which we don't use now).
  Count_XY <- Count_XY[!is.na(X),]
  
  # There are several cols that have NA (but none matter right now to us): ROUND = 7184; JULIAN = 62194; SITE = 2875; STN = 2877
  # 3. Merge it with PKEY
  Count_XY_PKEY <- merge(Count_XY, PKEY, by = c("PKEY", "SS", "PCODE"), all.x = TRUE, all.y = FALSE) # 2 values are inconsistent in PKEY (PKEY = "CL:F-100-1:4-3:14:0650", "CL:F-100-3:1-1:12:c") Seems as typos
  
  return(Count_XY_PKEY)
}