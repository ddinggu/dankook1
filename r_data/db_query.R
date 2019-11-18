install.packages("RMySQL")
library(RMySQL)

drv = dbDriver("MySQL")
con = dbConnect(drv, 
                host = Sys.getenv('DEPOLY_DB_HOST'), 
                dbname = Sys.getenv('DEPLOY_DATABASE_NAME'),
                user = Sys.getenv('DEPOLY_USERNAME'), 
                pass = Sys.getenv('DEPLOY_PASSWORD')
)

default_field_type <- list(id = "int(11) primary key auto_increment", bookname = "varchar(256)", freq = "int(11)")

# 단과대별 평균 대출기간
dbWriteTable(con, name = "university_mean", value = mm, 
             field.types = list(id = "int(11) primary key auto_increment", university = "varchar(32)", ckdl = "float"),
             row.names=FALSE, overwrite = TRUE
)

# 역대 top10
dbWriteTable(con, name = "top_historic", value = historic_top_10, field.types = default_field_type,
             row.names=FALSE, overwrite = TRUE)


# 연간 top10
dbWriteTable(con, name = "top_yearly", value = yearly_top_10, field.types = default_field_type,
             row.names=FALSE, overwrite = TRUE)

# 사회과학 top10
dbWriteTable(con, name = "top_social", value = gg.data, field.types = default_field_type,
             row.names=FALSE, overwrite = TRUE)

# 종교 top10
dbWriteTable(con, name = "top_religion", value = jj.data, field.types = default_field_type,
             row.names=FALSE, overwrite = TRUE)

# 철학 top10
dbWriteTable(con, name = "top_philosophy", value = hh.data, field.types = default_field_type,
             row.names=FALSE, overwrite = TRUE)

# 역사 top10
dbWriteTable(con, name = "top_history", value = ht.data, field.types = default_field_type,
             row.names=FALSE, overwrite = TRUE)

# 공학 top10
dbWriteTable(con, name = "top_engineering", value = gh.data, field.types = default_field_type,
             row.names=FALSE, overwrite = TRUE)

# 문학 top10
dbWriteTable(con, name = "top_literature", value = mh.data, field.types = default_field_type,
             row.names=FALSE, overwrite = TRUE)

dbDisconnect(con)
