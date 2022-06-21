
#=========================================================================#
# RC & DRC 카운트 확인 쿼리
  # analysis_id
    # 401 : Number of condition occurrence records, by condition_concept_id
    # 601 : Number of procedure occurrence records, by procedure_concept_id
    # 701 : Number of drug exposure records, by drug_concept_id
    # 1801 : Number of measurement occurrence records, by measurement_concept_id
    # 2101 : Number of device exposure records, by device_concept_id

# 업데이트:
# MM.DD.YYYY - 
#=========================================================================#

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


# 확인할 ConceptID 설정 - Domain별 설정
##### 도메인에 설정할 ConceptID가 없으면 "(1)"로 설정 (예) device_concept <- "(1)") #####
#==============================================================================
condition_concept <- "(201826,316866)"
drug_concept <- "(1503297,961047)"
measurement_concept <- "(3006923,3013721)"
procedure_concept <- "(46274106,4080499)"
device_concept <- "(1)"


# SQL 쿼리
#==============================================================================
sql <- "
--Condition
with condition as (
    select a.analysis_id,
           b.analysis_name,
           a.stratum_1,
           a.count_value,
           c.descendant_concept_id,
           cast(c.ancestor_concept_id as text) as ancestor_concept_id,
           d.concept_name
    from cdm_hira_2017_results_fnet_v276.achilles_results as a
            inner join cdm_hira_2017_results_fnet_v276.achilles_analysis as b
                       on a.analysis_id = b.analysis_id
            inner join (select cast(descendant_concept_id as text) as descendant_concept_id,
                               ancestor_concept_id
                        from cdm_hira_2017.concept_ancestor
                        where ancestor_concept_id in cd_condition) as c
                        on a.stratum_1 = c.descendant_concept_id
            inner join cdm_hira_2017.concept as d
                        on c.ancestor_concept_id = d.concept_id
    where a.analysis_id = 401 --condition occurrence RC
)

, condition_fin as (
select 'condition' as type
     , a.ancestor_concept_id
     , a.concept_name
     , b.RC
     , a.DRC
from (select ancestor_concept_id, concept_name, sum(count_value) as DRC
      from condition
      group by ancestor_concept_id, concept_name) as a
         left join (select ancestor_concept_id, concept_name, count_value as RC
                    from condition
                    where stratum_1 = ancestor_concept_id) as b
                   on a.ancestor_concept_id = b.ancestor_concept_id
)

--Drug
, drug as (
    select a.analysis_id,
           b.analysis_name,
           a.stratum_1,
           a.count_value,
           c.descendant_concept_id,
           cast(c.ancestor_concept_id as text) as ancestor_concept_id,
           d.concept_name
    from cdm_hira_2017_results_fnet_v276.achilles_results as a
             inner join cdm_hira_2017_results_fnet_v276.achilles_analysis as b
                        on a.analysis_id = b.analysis_id
             inner join (select cast(descendant_concept_id as text) as descendant_concept_id,
                                ancestor_concept_id
                         from cdm_hira_2017.concept_ancestor
                         where ancestor_concept_id in cd_drug) as c
                        on a.stratum_1 = c.descendant_concept_id
            inner join cdm_hira_2017.concept as d
                        on c.ancestor_concept_id = d.concept_id
    where a.analysis_id = 701 or a.analysis_id = 901 --drug exposure RC or drug era (ingredient) RC
)

, drug_fin as (
select 'drug' as type, a.ancestor_concept_id, a.concept_name, b.RC, a.DRC
from (select ancestor_concept_id, concept_name, sum(count_value) as DRC
      from drug
      group by ancestor_concept_id, concept_name) as a
         left join (select ancestor_concept_id, count_value as RC
                    from drug
                    where stratum_1 = ancestor_concept_id) as b
                   on a.ancestor_concept_id = b.ancestor_concept_id
)

--Measurement
, msm as (
    select a.analysis_id,
           b.analysis_name,
           a.stratum_1,
           a.count_value,
           c.descendant_concept_id,
           cast(c.ancestor_concept_id as text) as ancestor_concept_id,
           d.concept_name
    from cdm_hira_2017_results_fnet_v276.achilles_results as a
             inner join cdm_hira_2017_results_fnet_v276.achilles_analysis as b
                        on a.analysis_id = b.analysis_id
             inner join (select cast(descendant_concept_id as text) as descendant_concept_id,
                                ancestor_concept_id
                         from cdm_hira_2017.concept_ancestor
                         where ancestor_concept_id in cd_measurement) as c
                        on a.stratum_1 = c.descendant_concept_id
             inner join cdm_hira_2017.concept as d
                        on c.ancestor_concept_id = d.concept_id
    where a.analysis_id = 1801--measurement occurrence RC
)

, msm_fin as (
    select 'measurement' as type, a.ancestor_concept_id, a.concept_name, b.RC, a.DRC
    from (select ancestor_concept_id, concept_name, sum(count_value) as DRC
          from msm
          group by ancestor_concept_id, concept_name) as a
             left join (select ancestor_concept_id, concept_name, count_value as RC
                        from msm
                        where stratum_1 = ancestor_concept_id) as b
                       on a.ancestor_concept_id = b.ancestor_concept_id
)

--Procedure
, procedure as (
    select a.analysis_id,
           b.analysis_name,
           a.stratum_1,
           a.count_value,
           c.descendant_concept_id,
           cast(c.ancestor_concept_id as text) as ancestor_concept_id,
           d.concept_name
    from cdm_hira_2017_results_fnet_v276.achilles_results as a
             inner join cdm_hira_2017_results_fnet_v276.achilles_analysis as b
                        on a.analysis_id = b.analysis_id
             inner join (select cast(descendant_concept_id as text) as descendant_concept_id,
                                ancestor_concept_id
                         from cdm_hira_2017.concept_ancestor
                         where ancestor_concept_id in cd_procedure) as c
                        on a.stratum_1 = c.descendant_concept_id
             inner join cdm_hira_2017.concept as d
                        on c.ancestor_concept_id = d.concept_id
    where a.analysis_id = 601--procedure occurrence RC
)

, procedure_fin as (
select 'procedure' as type, a.ancestor_concept_id, a.concept_name, b.RC, a.DRC
from (select ancestor_concept_id, concept_name, sum(count_value) as DRC
      from procedure
      group by ancestor_concept_id, concept_name) as a
         left join (select ancestor_concept_id, concept_name, count_value as RC
                    from procedure
                    where stratum_1 = ancestor_concept_id) as b
                   on a.ancestor_concept_id = b.ancestor_concept_id
)

--Device
, device as (
    select a.analysis_id,
           b.analysis_name,
           a.stratum_1,
           a.count_value,
           c.descendant_concept_id,
           cast(c.ancestor_concept_id as text) as ancestor_concept_id,
           d.concept_name
    from cdm_hira_2017_results_fnet_v276.achilles_results as a
             inner join cdm_hira_2017_results_fnet_v276.achilles_analysis as b
                        on a.analysis_id = b.analysis_id
             inner join (select cast(descendant_concept_id as text) as descendant_concept_id,
                                ancestor_concept_id
                         from cdm_hira_2017.concept_ancestor
                         where ancestor_concept_id in cd_device) as c
                        on a.stratum_1 = c.descendant_concept_id
             inner join cdm_hira_2017.concept as d
                        on c.ancestor_concept_id = d.concept_id
    where a.analysis_id = 2101--device occurrence RC
)

, device_fin as (
    select 'device' as type, a.ancestor_concept_id, a.concept_name, b.RC, a.DRC
    from (select ancestor_concept_id, concept_name, sum(count_value) as DRC
          from device
          group by ancestor_concept_id, concept_name) as a
             left join (select ancestor_concept_id, concept_name, count_value as RC
                        from device
                        where stratum_1 = ancestor_concept_id) as b
                       on a.ancestor_concept_id = b.ancestor_concept_id
)

select * from condition_fin
union select * from drug_fin
union select * from msm_fin
union select * from procedure_fin
union select * from device_fin
"

sql <- gsub('cdm_hira_2017_results_fnet_v276', paste0(schema, '_results_dq_v276'), sql)
sql <- gsub('cdm_hira_2017', schema, sql)
sql <- gsub('cd_condition', condition_concept, sql)
sql <- gsub('cd_drug', drug_concept, sql)
sql <- gsub('cd_measurement', measurement_concept, sql)
sql <- gsub('cd_procedure', procedure_concept, sql)
sql <- gsub('cd_device', device_concept, sql)

rc_drc <- dbGetQuery(con, sql)
print("rc_drc")

rc_drc


# csv로 저장
#==============================================================================
# 병원 리스트
hosp_list <- fread(
  "hospital host
강동성심 211.205.78.25
경북본원 192.168.100.239
경북칠곡 192.168.100.227
원광대 172.16.200.1
강원대 172.16.21.26
아주대 10.5.99.58
대구카톨릭 192.168.209.61
부산대 200.2.10.230
강동경희 192.168.50.1
이대목동 172.16.2.110
건보일산 192.168.1.54
동국일산 192.168.62.236
경희의료원 10.1.100.20
한양대 192.168.2.67
분당차 10.16.1.101
인하대 128.1.70.66
전북대 192.1.179.102
전남대 10.1.3.20
전남대화순 10.150.2.130
원주세브란스 192.168.11.159
순천향서울 172.17.61.232
순천향천안 172.17.61.234
순천향부천 172.17.61.233
순천향구미 172.17.61.235
가천길 192.168.248.71
부천세종 10.101.1.193
인천세종 10.101.1.192
단국대병원 172.16.34.140
건국대병원 10.20.250.246
강북삼성병원 116.2.20.86
명지대 192.168.1.232
경상국립대 192.9.35.170
")
hosp_list <- hosp_list[host==DB_HOST]


# 파일명 설정
fwrite(rc_drc, paste0("/data/results/", hosp_list[1,1], "_rc_drc.csv"))


