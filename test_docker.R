#test_docker
#==============================================================================
library(RPostgreSQL)
library(DBI)
library(data.table)
library(digest)
library(lubridate)
library(stringr)
setDTthreads(percent = 100)
# DB 접속
drv <- dbDriver("PostgreSQL")

# get environment variables
DATABASE = Sys.getenv('MEDICAL_RECORDS_DATABASE')
USER = Sys.getenv('MEDICAL_RECORDS_USER')
PW = Sys.getenv('MEDICAL_RECORDS_PW')
URL = Sys.getenv('MEDICAL_RECORDS_URL')
DB_HOST=strsplit(URL,':')[[1]][1]
port=strsplit(URL,':')[[1]][2]
schema = Sys.getenv('MEDICAL_RECORDS_SCHEMA')

print(c(DATABASE, USER, PW, URL, DB_HOST, port, schema))

# connect to database 
# host : server name or localhhost
con <- dbConnect(drv, dbname=DATABASE, port=port, user=USER, password=PW, host=DB_HOST)
con
print("0. connect")
# sql<-function(query){
#   temp<-dbGetQuery(con,query)
#   temp<-as.data.table(temp)
#   return(temp)}
  
# Export data
saveTable<-function(data){
  fwrite(data,paste0("/data/results/",deparse(substitute(data)),".csv"))
}

# cohort definition  
# 병원    ip?        대상자 cohort id  
# 아주대 10.5.99.58   3268
# 강동성심            
# -----------------------------------------------------------------------------
# Metformin 
# -----------------------------------------------------------------------------
# (1) metformin single 
# TD:약물 복용 최초, 최후 복용일 표 생성
# TM: ID별 measurement date만 추출
# TM-2: BEFORE/ AFTER 군으로 id, measurement date,필요정보 불러오기. 
# 1-1) TD : 
# sql<-" 
# with TD AS (
#  select distinct a.subject_id as id 
#                  ,b.drug_exposure_start_date AS start
#                  ,b.drug_exposure_end_date AS end
#     from cdmpv532_results_dq_v276.cohort a
#     join cdmpv532.drug_exposure b
#     on a.subject_id = b.person_id
#     where a.cohort_definition_id = 3276
#     and b.drug_source_value like '1915%'
#     order by a.subject_id, b.drug_exposure_start_date
#     )
# select id, row_number() over(order by start)
# "
# sql<-str_replace_all(sql,'cdmpv532',schema)
# TD<-dbGetQuery(con, sql)
# TD<- as.data.frame(TD)

#1-2) TD : FIRST, LAST DATE 
sql<- "WITH TD AS ( select  a.subject_id as id
                 ,b.drug_exposure_start_date AS start_
                 ,b.drug_exposure_end_date AS end_
                 ,b.drug_source_value as drug 
                 ,row_number() over (partition by a.subject_id order by  b.drug_exposure_end_date desc ) as row_desc
                 ,row_number() over (partition by a.subject_id order by  b.drug_exposure_end_date  ) as row_asc
    from cdmpv532_results_dq_v276.cohort a
    join cdmpv532.drug_exposure b
    on a.subject_id = b.person_id
    where a.cohort_definition_id = 3276
   )
  select * from TD LIMIT 30
"
# CREATE TABLE first as select id, start_ from TD WHERE row_asc =1
# CREATE TABLE last as select id, end_ from TD where row_desc =1

# create table target as
#     select a.id, a.start_ as start, (b.end_ +7) as last
#     from first a
#     join last  b
#     on a.id = b.id
#     order by a.id

# drop table TD
# DROP TABLE first
# drop table last

# CREATE TABLE AFTER AS
#     SELECT a.*, b.start, b.last FROM cdmpv532.measurement a
#     JOIN target b
#     ON a.person_id = b.id
#     WHERE b.start <= a.measurement_date and a.measurement_date <= b.last

# DROP TABLE target 
# SELECT * FROM AFTER order by person_id LIMIT 30


sql<-str_replace_all(sql,'cdmpv532',schema)
AFTER<-dbGetQuery(con, sql)
AFTER
#saveTable(AFTER)
# cohort<-as.data.table(cohort)


