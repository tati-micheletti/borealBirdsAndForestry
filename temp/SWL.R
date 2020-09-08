# library("DBI")
# library("odbc")
library("data.table")

SQLtableVersion <- "V4_2015" # Data retrieving from SQL: specific versions
SQLServer <- "boreal.biology.ualberta.ca" # Data retrieving from SQL: server
SQLDatabase <- "BAM_National_V4_2015_0206" # Data retrieving from SQL: specific database

pathToDriver <- data.table(odbc::odbcListDrivers())[attribute == "Driver", value]

con <- DBI::dbConnect(odbc::odbc(),
                         Driver = pathToDriver,
                         Server = SQLServer, # Connect to BAM Server
                        port = 1433, # port # 3306 default
                         # Note: even though the address is set up in Windows as boreal.biology.ualberta.ca\boreal,
                         # in R I need to exclude the \boreal
                         Database = SQLDatabase, # Got the names from the Windows ODBC connection
                         UID = "michelet", #rstudioapi::askForPassword("Database User"),
                         PWD = "Univ3rs!ty" #studioapi::askForPassword("Database Password"),
                      ) 


# curl::curl_version() # Gives me which ssl_version I am using. If "OpenSSL/1.1.1", this might be the problem
# sudo apt install libcurl4-gnutls-dev
install.packages("DBI")
install.packages("odbc")


