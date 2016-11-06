library(RMySQL)
library(readr)


db_tables <- list.files(path = 'data-raw/joshua_project/',pattern = '.csv',full.names = TRUE)
db_names <- list.files(path = 'data-raw/joshua_project/',pattern = '.csv',full.names = FALSE)

con <- dbConnect(MySQL(),
                 user = 'indigitous_user',
                 password = 'huajeLgsHzJa',
                 host = 'bridgebuilders.cdm98uj1jenj.us-east-1.rds.amazonaws.com',
                 dbname='joshua_project')

for(i in 1:length(db_tables)){
  db_table <- read_csv(db_tables[i])
  db_name <- gsub(pattern = ".csv",replacement = "",x = db_names[i])
  dbWriteTable(conn = con, name = db_name, value = as.data.frame(db_table,row.names = FALSE),overwrite = FALSE)
}

con <- dbConnect(MySQL(),
                 user = 'RDSUser',
                 password = 'YourPass',
                 host = 'RDS Host',
                 dbname='YourDB')
dbWriteTable(conn = con, name = 'Test', value = as.data.frame(Thurstone))