library(readr)
library(tidyverse)
library(skimr)
#process step

resale <- read.csv("car_resale.csv")

summary(resale)
skim_without_charts(resale)
resale <- resale %>% distinct() # checking for duplicates
resale <- resale %>% na.omit()

View(resale)
unique(resale$engine_capacity)
unique(resale$insurance)
resale <- resale %>% filter(!(insurance == "Not Available")) %>% filter(!(insurance == "1")) %>% filter(!(insurance == "2")) # removing not available rows
resale$insurance[resale$insurance == 'Third Party insurance'] <- 'Third Party'
resale1 <- resale
unique(resale1$insurance)
View(resale1)
unique(resale1$transmission_type)
unique(resale1$kms_driven) 
unique(resale1$owner_type)
unique(resale1$fuel_type)
unique(resale1$max_power)
unique(resale1$seats)
unique(resale1$mileage)
unique(resale1$body_type)
resale1 <- resale1 %>% filter(!(body_type == "Audi")) %>% filter(!(body_type == "BMW")) %>% filter(!(body_type == "Chevrolet")) %>% filter(!(body_type == "Datsun")) %>% filter(!(body_type == "Honda")) %>% filter(!(body_type == "Isuzu")) %>% filter(!(body_type == "Jaguar")) %>% filter(!(body_type == "Mahindra")) %>% filter(!(body_type == "Maruti")) %>% filter(!(body_type == "Mercedes-Benz")) %>% filter(!(body_type == "Tata")) %>% filter(!(body_type == "Toyota")) %>% filter(!(body_type == "Volvo")) # removing the company names  
unique(resale1$city)
unique(resale1$full_name)
unique(resale1$resale_price)
unique(resale1$registered_year) 

resale2 <- resale1 %>% filter(grepl('bhp|Bhp',max_power)) # only keeping the bhp rows
unique(resale2$max_power)
resale3 <- resale2 
resale3$kms_driven <-  gsub("Kms","",resale3$kms_driven)
resale3$kms_driven <- as.numeric(gsub(",","",resale3$kms_driven)) #kms converted to numeric
resale3$max_power <- as.numeric(str_extract(resale3$max_power,"[\\d.d]+")) # power converted to numeric
resale3$engine_capacity <- as.numeric(gsub(" cc","",resale3$engine_capacity))
View(resale3)
resale4 <- resale3
resale4 <- resale4 %>% mutate(mileage = case_when(str_detect(mileage,"km/kg")~as.numeric(str_extract(mileage,"[\\d.d]+"))*1.7,str_detect(mileage,"kmpl")~as.numeric(str_extract(mileage,"[\\d.d]+"))))
 # converted km/kg to kmpl and numeric format
resale4$resale_price <- substring(resale4$resale_price,3) 
resale4 <- resale4 %>% mutate(resale_price = case_when(str_detect(resale_price,",")~as.numeric(gsub(",","",resale_price)),str_detect(resale_price,"Lakh")~as.numeric(str_extract(resale_price,"[\\d.d]+")) * 100000,str_detect(resale_price,"Crore")~as.numeric(str_extract(resale_price,"[\\d.d]+")) * 10000000))
View(resale4) # converted resale price to numeric 
resale5 <- resale4
resale5$registered_year <- ifelse(str_length(resale5$registered_year)==6,substr(resale5$registered_year,5,6),resale5$registered_year)
resale5$registered_year <- ifelse(str_length(resale5$registered_year)==2,paste("20",resale5$registered_year,sep=""),resale5$registered_year)
resale5$registered_year <- as.numeric(resale5$registered_year) # converted years to standard 4 digits and type is numeric
resale5 <- resale5 %>% rename("resale_price_rupees" = "resale_price")  %>% rename("mileage_kmpl" = "mileage") %>% rename("max_power_bhp" = "max_power")
View(resale5)


#Analysis and Visualization 
summary(resale5)
skim_without_charts(resale5)
library(ggplot2)
options(scipen = 999)

resale5 %>% group_by(registered_year) %>% summarise(vehicle_count = n())
ggplot(data = resale5) + geom_bar(mapping = aes(x = registered_year))

resale5 %>% group_by(registered_year) %>% summarise(avgrupees = mean(resale_price_rupees))
resale5 %>% group_by(registered_year) %>% summarise(avgrupees = mean(resale_price_rupees)) %>% ggplot(aes(registered_year,avgrupees)) + geom_col()

resale5 %>% group_by(registered_year,insurance) %>% summarise(insurecount = n())
ggplot(data = resale5) + geom_bar(mapping = aes(x = registered_year,fill = insurance))

resale5 %>% group_by(registered_year,transmission_type) %>% summarise(transmiscount = n())
ggplot(data = resale5) + geom_bar(mapping = aes(x = registered_year,fill = transmission_type))

resale5 %>% group_by(registered_year) %>% summarise(kms_mean = mean(kms_driven))
resale5 %>% group_by(registered_year) %>% summarise(kms_mean = mean(kms_driven)) %>% ggplot(aes(registered_year,kms_mean)) + geom_col()

resale5 %>% group_by(registered_year,owner_type) %>% summarise(owncount = n())
ggplot(data = resale5) + geom_bar(mapping = aes(x = registered_year,fill = owner_type))

resale5 %>% group_by(registered_year,fuel_type) %>% summarise(fuelcount = n())
ggplot(data = resale5) + geom_bar(mapping = aes(x = registered_year,fill = fuel_type))

resale5 %>% group_by(registered_year,body_type) %>% summarise(btypecount = n())
ggplot(data = resale5) + geom_bar(mapping = aes(x = registered_year,fill = body_type))

resale5 %>% group_by(registered_year,city) %>% summarise(citycount = n())
ggplot(data = resale5) + geom_bar(mapping = aes(x = registered_year,fill = city))

resale5 %>% group_by(registered_year) %>% summarise(mileagecount = mean(mileage_kmpl))
resale5 %>% group_by(registered_year) %>% summarise(mileagecount = mean(mileage_kmpl)) %>% ggplot(aes(registered_year,mileagecount)) + geom_col()

resale5 %>% group_by(city) %>% summarise(resalemean = mean(resale_price_rupees))
resale5 %>% group_by(city) %>% summarise(resalemean = mean(resale_price_rupees)) %>% ggplot(aes(city,resalemean)) + geom_col()
#ggplot(data = resale5) + geom_col(mapping = aes(x = city,y = mean(kms_driven)))
#ggplot(data = resale5) + geom_col(mapping = aes(x = city,y = mean(mileage_kmpl)))

#ggplot(data = resale5) + geom_point(mapping = aes(x = engine_capacity,y = mileage_kmpl))
#ggplot(data = resale5) + geom_point(mapping = aes(x = engine_capacity,y = max_power_bhp))
ggplot(data = resale5) + geom_point(mapping = aes(x = engine_capacity,y = resale_price_rupees,color = registered_year))

ggplot(data = resale5) + geom_point(mapping = aes(x = max_power_bhp,y = mileage_kmpl))
ggplot(data = resale5) + geom_point(mapping = aes(x = max_power_bhp,y = resale_price_rupees,color = registered_year))

ggplot(data = resale5) + geom_point(mapping = aes(x = mileage_kmpl,y = resale_price_rupees,color = registered_year))

ggplot(data = resale5) + geom_line(mapping = aes(x = engine_capacity,y = max_power_bhp))
ggplot(data = resale5) + geom_line(mapping = aes(x = engine_capacity,y = mileage_kmpl),size = 1.2)



pie(table(resale5$insurance),radius = 1,border = "white",col = c(3,4,5,6,7,8))
pie(table(resale5$owner_type),radius = 1,border = "white",col = c(1,2,3,4,5))
pie(table(resale5$fuel_type),radius = 1,border = "white",col = c(3,4,5,6,7,8))
pie(table(resale5$body_type),radius = 1,border = "white")


resale5 %>% group_by(insurance) %>% count(insurance) %>% 
  ggplot(aes(insurance,n,fill = insurance)) +
  geom_col()+
  coord_polar()+
  scale_fill_ordinal()+
  labs(x = NULL, y = NULL)

resale5 %>% group_by(body_type) %>% count(body_type) %>% 
  ggplot(aes(body_type,n,fill = body_type)) +
  geom_col()+
  coord_polar()+
  scale_fill_ordinal()+
  labs(x = NULL, y = NULL)


resale5 %>% group_by(fuel_type) %>% count(fuel_type) %>% 
  ggplot(aes(fuel_type,n,fill = fuel_type)) +
  geom_col()+
  coord_polar()+
  scale_fill_ordinal()+
  labs(x = NULL, y = NULL)

resale5 %>% group_by(owner_type) %>% count(owner_type) %>% 
  ggplot(aes(owner_type,n,fill = owner_type)) +
  geom_col()+
  coord_polar()+
  scale_fill_ordinal()+
  labs(x = NULL,y = NULL)
