library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='172.25.187.111', port='5432', dbname='abhinav',
                 user='abhinav', password='qwert$$123')
data<- dbGetQuery(con,"SELECT AVG(ground_speed) as avg_speed,count(*) as count,
    EXTRACT(HOUR FROM to_timestamp(time_stamp/1000)) as hour,
EXTRACT(MINUTE FROM to_timestamp(time_stamp/1000)) as min
from qi_server_week2 where  ground_speed>0 group by hour,min")
summary(data)
attach(data)
hist(avg_speed)
class(min)

plot(count~hour)

