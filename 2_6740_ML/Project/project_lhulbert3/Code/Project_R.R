setwd("C:/Users/lara.hulbert/Dropbox/1.Grad_School/MY_CLASSES/2_6740_ML/Project")
setwd("C:/Users/Lara/Dropbox/1.Grad_School/MY_CLASSES/2_6740_ML/Project")

train = read.csv("train.csv")
View(train)
weather = read.csv("weather.csv", na.strings = c("NA","M","-"))   #weather at each station 
key = read.csv("key.csv")   #relates store number to station number
test = read.csv("test.csv")

test = left_join(test, key, by = "store_nbr")
test = left_join(test, weather, by = c("station_nbr", "date"))

train = left_join(train, key, by = "store_nbr")
train = left_join(train, weather, by = c("station_nbr", "date"))

#-------------------- Abhishek's code --------------------
#left join data1 and weather files 
data1 = merge(x = key, y = weather, by = "station_nbr", all.x = TRUE)

#first transpose the train data for each date and then join
library(reshape)
data2 = cast(train, date+store_nbr~item_nbr)

#------------------ Mess with date column ------------
library(lubridate)
library(dplyr)

train$date_new = ymd(train$date)
days = unique(train$date_new)
days_df = data.frame(1:1034, days)
colnames(days_df) = c("day_num", "date_new")
train = left_join(train, days_df, by="date_new")
train = train[,c(6,5,2,3,4)]

test$date_new = ymd(test$date)
test = left_join(test, days_df, by="date_new")
test = test[,c(5,4,2,3)]

#--------- events column ---------
#set up snow event: 
weather$snow_numeric = weather$snowfall
weather[which(as.character(weather$snowfall) == "  T"), "snow_numeric"] = NA
weather[which(as.character(weather$snowfall) == "M"), "snow_numeric"] = NA
weather$snow_numeric = as.numeric(as.character(weather$snow_numeric))

weather$snow_event = ifelse(weather$snow_numeric >= 2, 1, 0)
summary(as.factor(weather$snow_event))

#set up rain event:
weather$rain_numeric = weather$preciptotal
weather[which(as.character(weather$preciptotal) == "  T"), "rain_numeric"] = NA
weather[which(as.character(weather$preciptotal) == "M"), "rain_numeric"] = NA
weather$rain_numeric = as.numeric(as.character(weather$rain_numeric))

weather$rain_event = ifelse(weather$rain_numeric >= 1, 1, 0)
summary(as.factor(weather$rain_event))

#set up event: 
weather$event = ifelse(weather$rain_event==1 | weather$snow_event==1, 1,0)
summary(as.factor(weather$event))

#look at when were the events for each product:
test = train_nonzero[which(train_nonzero$event!=0),]
View(arrange(test, item_nbr))

#----------- combine weather event with data2 table --------
train_nonzero = train[which(train$units>0),]
train_nonzero = left_join(train_nonzero, key, by= "store_nbr")
train_nonzero = left_join(train_nonzero, weather, by=c("station_nbr", "date"))
train_nonzero[which(is.na(train_nonzero$event)==TRUE), "event"]=0

#------------ product sums------------------
store_vols = c()
for (i in 1:45){
  store_vols = c(store_vols,sum(train[which(train$store_nbr==i), "units"]) )
}

prod_sums = matrix(c(0,0,0), ncol=5)

counter = 0

for (item in 1:111){
  for (store in 1:45){
    entry = sum(data2[which(data2$store_nbr==store), as.character(item)])
    prod_sums = rbind(prod_sums, c(item, store, entry,store_vols[store], entry/store_vols[store]))
    counter= counter + 1
  }}

prod_sums = as.data.frame(prod_sums)
colnames(prod_sums) = c("item_nbr", "store_nbr", "sum_sales_alldays", "store_volume", "percent_sales")
popular_products = prod_sums[which(prod_sums$sum !=0),]

#------------- augment the dataset ---------------------
#add month:
train_nonzero$date = ymd(train_nonzero$date)
train_nonzero$month = month(train_nonzero$date)

#add overall sales numbers: 
train_nonzero = left_join(train_nonzero, popular_products[, c(1,2,4,5)], by= c("item_nbr","store_nbr"))

#add weekend dummy:
library("chron")
train_nonzero$weekend = ifelse(is.weekend(train_nonzero$date), 1, 0)


#add holiday dummy:
dates = unique(train_nonzero$date)
dates = data.frame(dates, year(dates), month(dates), day(dates))
colnames(dates) = c("date", "year", "month", "day")
dates$is_winter = ifelse(dates$month %in% c(1, 2, 11, 12) | (dates$month==3 & dates$day<=15) , 1, 0)
dates$leapday = ifelse(dates$month == 2 & dates$day ==29, 1,0)
dates$before_Valentines = ifelse(dates$month==2 & dates$day > 7 & dates$day<=14, 1,0)
dates$before_4July = ifelse(dates$month==7 & dates$day<=4, 1,0)
dates$before_Xmas = ifelse(dates$month==12 & dates$day>10, 1,0)
dates$before_Halloween = ifelse(dates$month==10 & dates$day > 24, 1,0)
dates$before_Memorial = ifelse(dates$month==5 & ((dates$day>=22 & dates$day<=28 & dates$year==2012) | (dates$day>=21 & dates$day<=27 & dates$year==2013) | (dates$day>=20 & dates$day<=26 & dates$year==2014) ), 1,0)
dates$before_NewYears = ifelse(dates$month==12 & dates$day>28 | (dates$month ==1 & dates$day==1), 1,0)
dates$BlackFriday = ifelse(dates$month==11 & ((dates$day==23 & dates$year==2012) |  (dates$day==29 & dates$year==2013)), 1,0)
dates$before_Thanksgiving = ifelse(dates$month==11 & ((dates$day<=22 & dates$day>=16 & dates$year==2012) |  (dates$day<=29 & dates$day>=23 & dates$year==2013)), 1,0)
dates$before_Easter = ifelse(((dates$month==4 & dates$day<=8 & dates$day>=2 & dates$year==2012) |  (dates$month==3 & dates$day<=31 & dates$day>=25 & dates$year==2013) | (dates$month==4 & dates$day<=14 & dates$day>=8 & dates$year==2014)), 1,0)
#holidays are christmas, new years and easter (it was closed on 2012 xmas though):
dates$is_holiday = ifelse((dates$month==12 & dates$day==25) | (dates$month==1 & dates$day==1) | (dates$month==4 & dates$day==8 & dates$year==2012) |  (dates$month==3 & dates$day==31 & dates$year==2013) | (dates$month==4 & dates$day==20 & dates$year==2014), 1,0)

train_nonzero = left_join(train_nonzero, dates[,-c(2,3,4)], by="date")


###add sales 10 thru 3 days before of that product at that store:
train$date = ymd(train$date)
train$salesbefore = NA

for (store in 1:45){    #1:45
  for(item in 1:111){
    store_df = train[which(train$store_nbr==store & train$item_nbr==item),]
    store_df$salesbefore = NA
    counter = 10
    if (sum(store_df$units)!= 0) {
      for (current_date in store_df$date[10:nrow(store_df)]){  
        date_range = c((current_date - 9),(current_date - 8), (current_date - 7), (current_date - 6), (current_date - 5), (current_date - 4),  (current_date - 3))
        mini = store_df[which(store_df$date %in% date_range), "units"]
        if(length(mini)==7){
          store_df$salesbefore[counter]  = sum(mini)   
        }
        else(store_df$salesbefore[counter]= NA)
        counter = counter + 1
      } 
    }
    train$salesbefore[as.numeric(rownames(store_df))] = store_df$salesbefore
  }
}
train_nonzero = left_join(train_nonzero, train[,c(1,2,3,5)], by = c("date", "store_nbr", "item_nbr"))

#--------------- Plot trends ------------------

color_list = c("red", "blue", "dark green", "black", "purple", "orange")

#enter dates as characters of y/m/d
plotting <- function(item_nbr, start_date="None", end_date="None", store_nbr_list="None"){    
  
  if(store_nbr_list != "None"){
    temp_df = train_nonzero[which(train_nonzero$date<ymd(end_date) &  train_nonzero$date>ymd(start_date) &    
                                    train_nonzero$item_nbr==item_nbr),]
    
    plot(x = temp_df[which(temp_df$store_nbr==store_nbr_list[1]) , "date"], 
         y = temp_df[which(temp_df$store_nbr==store_nbr_list[1]) , "units"],
         xlab = paste("From", start_date,"to",  end_date), ylab="Units of Sales", main=paste("Sales of item #", item_nbr), type="l", col="red")
    
    #for(i in 2:length(store_nbr_list)){
    #    lines(x = temp_df[which(temp_df$store_nbr==store_nbr_list[i]) , "date"], 
    #        y = temp_df[which(temp_df$store_nbr==store_nbr_list[i]) , "units"], type="l", col=color_list[i])
    #}
  }    
  
  #for plotting all stores: 
  if(start_date != "None"){
    temp_df = train_nonzero[which(train_nonzero$date<ymd(end_date) &  train_nonzero$date>ymd(start_date) &    
                                    train_nonzero$item_nbr==item_nbr),]
    
    agg = temp_df %>% group_by(date) %>% summarize(total_units = sum(units))
    agg  = as.data.frame(agg)
    plot(x = agg[ , "date"], 
         y = agg[ , "total_units"],
         xlab = paste("From", start_date,"to",  end_date), ylab="Units of Sales", main=paste("All stores Sales of item #", item_nbr), type="l", col="black")
  }
  
  #for plotting all years:   
  temp_df2 = train_nonzero[which(train_nonzero$item_nbr==item_nbr),]
  temp_df2$day = day(temp_df2$date) 
  agg2 = temp_df2 %>% group_by(month,day) %>% summarize(total_units = sum(units))
  agg2$date = ymd(paste("2004", agg2$month, agg2$day))
  agg2 = agg2[-60, ]   #remove leap year sales
  agg2 = as.data.frame(agg2)
  
  plot(x = agg2[ , "date"], 
       y = agg2[ , "total_units"],
       xlab = "", ylab="Total Units of Sales", main=paste("All years all stores sales of item #", item_nbr), type="l", col="black")
  points(x = agg2[ , "date"], y = agg2[ , "total_units"], pch=16)
  
  #plot all years of data
  agg3 = temp_df2 %>% group_by(date) %>% summarize(total_units = sum(units))
  agg3 = as.data.frame(agg3)
  #joining info on whether there was an event or not:
  new = train_nonzero[which(train_nonzero$item_nbr==item_nbr), c("date", "any_event", "After1")]
  new2 = new[which(new$any_event!=0),]
  new2 = unique(new2[,])
  agg3 = left_join(agg3, new2, by="date")
  plot(x = agg3[,"date"],
       y= agg3[, "total_units"],
       xlab="", ylab = "Total Units of Sales" ,main=paste("Over time sales of item #", item_nbr), type="l",
       col="black")
  points(x = agg3[,"date"], y= agg3[, "total_units"], pch=16, col=ifelse(is.na(agg3$After1)!=TRUE, "red", "black"))
  #abline(lm(total_units ~ date, data = agg3), col="blue")
}

for(prod_id in 63:111){
  plotting(prod_id)
}

store_nbr=14
item_nbr=15
temp_df2 = train_nonzero[which(train_nonzero$item_nbr==item_nbr & train_nonzero$store_nbr==store_nbr),]
##simpler plot:
plot(x = temp_df2[,"date"],
     y= temp_df2[, "units"],
     xlab="", ylab = "Total Units of Sales" ,main=paste("Over time sales of item #", item_nbr), type="l",
     col="black")
points(x = temp_df2[,"date"], y= temp_df2[, "units"], pch=16, col= ifelse(temp_df2$After1 ==1, "red", "black"))


#-------------- Outliers & Interesting things ------------------
View(train_nonzero[which(train_nonzero$store_nbr==37 & train_nonzero$date< ymd("2012-11-18") & train_nonzero$date>ymd("2012-11-13")), ])
#2012-11-15, store 37, item 5 sold 5568 units. No other anomolies with this product or this store around that time 
#and then same store, same product on 2013-11-21 sold 3369 units

#Sales of item 34, 39, 40, 15 are bizzare. 
#Overall sales are very low, but especially low in summer months. 
plotting(item_nbr = 40)

#products even slightly affected by the events column:
#product 39, 32, 30, 26, 23?, 21, 9, 
#The event label is more useful for rarely purchased products

#Product 9,16,5 have a lot of events

#Seems to be more events in earlier years? 
#Is this cuz the events are in the holdout dataset for the later years?

#These products don't have a lot of seasonality (probably they are rarely purchased or they're staple items)
# And most products stay pretty constant level of interest throughout

#----------------- Bring in Tyler's weather variables & re-organize the columns:---------  
tyler = read.csv("train_nonzero_beforeafter.csv")
tyler$date = ymd(tyler$date)
train_nonzero = left_join(train_nonzero, tyler, by = c("date", "store_nbr", "item_nbr"))

train_nonzero = train_nonzero[, -c(45:48)]
train_nonzero = train_nonzero[, c(1, 2, 3, 4, 45, 46, 47, 28, 50, 49, 48, 30, 31, 32, 29, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 5:27)]

train_nonzero[which(train_nonzero$snowfall=="  T"), "snow_numeric"]=0
train_nonzero[which(train_nonzero$preciptotal=="  T"), "rain_numeric"]=0
train_nonzero = train_nonzero[, -c(40,41)]
train_nonzero[which(is.na(train_nonzero$snow_event)==TRUE), "snow_event"]=0
train_nonzero[which(is.na(train_nonzero$rain_event)==TRUE), "rain_event"]=0

write.csv(train_nonzero, "our_train_df.csv")

#-------------------- Do all this shit to this testing data------------------
test = read.csv("test.csv")

#get the weather event info:
test = left_join(test, key, by= "store_nbr")
test = left_join(test, weather, by=c("station_nbr", "date"))
test[which(test$snowfall=="  T"), "snow_numeric"]=0
test[which(test$preciptotal=="  T"), "rain_numeric"]=0
test = test[, -c(16,17)]
test[which(is.na(test$snow_event)==TRUE), "snow_event"]=0
test[which(is.na(test$rain_event)==TRUE), "rain_event"]=0
test[which(is.na(test$event)==TRUE), "event"]=0

#run Tyler's code to get the befores and afters for the events:

#join store volume and percent sales:
test = left_join(test, popular_products[,c(1,2,5)], by=c("item_nbr", "store_nbr"))
test = left_join(test, unique(popular_products[,c("store_nbr", "store_volume")]), by = "store_nbr")

#join date special columns:
test$weekend = ifelse(is.weekend(test$date), 1, 0)
test$date = ymd(test$date)
test = left_join(test, dates[,-c(2,3,4)], by="date")

#can we get previous sales info?????????????????????
###add sales 10 thru 3 days before of that product at that store:
test$salesbefore = NA

for (store in sort(unique(test$store_nbr))){    #product 44 isn't represented in the test set
  for(item in 1:111){
    store_df = train[which(train$store_nbr==store & train$item_nbr==item),]
    store_df$salesbefore = NA
    counter = 10
    if (sum(store_df$units)!= 0) {
      for (current_date in store_df$date[10:nrow(store_df)]){  
        date_range = c((current_date - 9),(current_date - 8), (current_date - 7), (current_date - 6), (current_date - 5), (current_date - 4),  (current_date - 3))
        mini = store_df[which(store_df$date %in% date_range), "units"]
        if(length(mini)==7){
          store_df$salesbefore[counter]  = sum(mini)   
        }
        else(store_df$salesbefore[counter]= NA)
        counter = counter + 1
      } 
    }
    test$salesbefore[as.numeric(rownames(store_df))] = store_df$salesbefore
  }
}
test = left_join(train_nonzero, train[,c(1,2,3,5)], by = c("date", "store_nbr", "item_nbr"))

write.csv(test, "our_test_df.csv")

#-------------- Weather station 5 is entirely missing data---------

weather$sunrise2 = hm("12-59")

for (i in 1:nrow(weather)){
  if(weather$sunrise[i]!= "-"){
    sst <- strsplit(as.character(weather$sunrise[i]), "")[[1]]
    out <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])
    b = paste(out[1], "-", out[2], sep="")
    weather$sunrise2[i] = hm(b)
  }
  else{weather$sunrise2[i] = NA}
  
}
#find a station with similar weather patterns to this:
weather[which(weather$station_nbr==5), "sunrise2"]

sum_diff = 100000000
for(station in c(1:4, 6:20)){
  current_diff = weather[which(weather$station_nbr==5), "sunrise2"] - weather[which(weather$station_nbr==station ), "sunrise2"]
}

#---------------------------- Modeling -------------------------
setwd("C:/Users/lara.hulbert/Dropbox/1.Grad_School/MY_CLASSES/2_6740_ML/Project")
train_nonzero = read.csv("train_df_more_variables.csv", stringsAsFactors = FALSE, na.strings = c("NA", "M", "-"))
train_nonzero = train_nonzero[,-1]
library(lubridate)
train_nonzero$date= ymd(train_nonzero$date)

train_nonzero$store_nbr=as.factor(as.character(train_nonzero$store_nbr))
train_nonzero$item_nbr=as.factor(train_nonzero$item_nbr)
train_nonzero$station_nbr=as.factor(train_nonzero$station_nbr)
train_nonzero$rain_event=as.factor(train_nonzero$rain_event)
train_nonzero$snow_event=as.factor(train_nonzero$snow_event)
train_nonzero$Before3=as.factor(train_nonzero$Before3)
train_nonzero$Before2=as.factor(train_nonzero$Before2)
train_nonzero$Before1=as.factor(train_nonzero$Before1)
train_nonzero$After3=as.factor(train_nonzero$After3)
train_nonzero$After2=as.factor(train_nonzero$After2)
train_nonzero$After1=as.factor(train_nonzero$After1)
train_nonzero$any_event=as.factor(train_nonzero$any_event)
for (i in 15:28){
  train_nonzero[,i] = as.factor(train_nonzero[,i])
}

#check the classes of the columns: 
for(i in 1:ncol(train_nonzero)){print(colnames(train_nonzero)[i]); print( class(train_nonzero[,i])) }

#model with my added variables:
model= lm(units ~ store_nbr + item_nbr + Before3 + Before2 + Before1 + any_event + After1 + After2 + After3+ 
            sales_3_10_days_before+ month+ is_weekend +is_winter + leapday 
          + before_Valentines + before_4July + is_holiday+tmax + tmin +  
            tavg + dewpoint + heat + store_volume + percent_sales + 
            rain_numeric + before_Xmas + before_Memorial + before_NewYears+ 
            BlackFriday + before_Thanksgiving + before_Easter + before_Halloween, 
          data=train_nonzero)   
summary(model)
#took out cool (cuz NAs), rain_event, wetbulb, stnpressure, sealevel, snow_numeric, snow_event, date
#adjusted r^2 of 0.6841 
plot(model)



####Bootstrapped LM:
n = nrow(train_nonzero)[1]  #total number of observations
n1 = round(n/10)  # number of observations randomly selected for testing data

B= 10;   #5-fold bootstrapping 
mse_all = c()
r2_all =c()

for (b in 1:B){
  # randomly select 25 observations as testing data in each loop:
  flag <- sort(sample(1:n, n1));
  xtrain <- train_nonzero[-flag,];
  xtest <- train_nonzero[flag,];
  
  model= lm(units ~ store_nbr + item_nbr + Before3 + Before2 + Before1 + any_event + After1 + After2 + After3+ 
              month+ is_weekend +is_winter + leapday 
            + before_Valentines + before_4July + is_holiday+tmax + tmin +  
              tavg  + store_volume + percent_sales + 
              rain_numeric + before_Xmas + before_Memorial + before_NewYears+ 
              BlackFriday + before_Thanksgiving + before_Easter + before_Halloween, 
            data=train_nonzero)   
  
  pred <- predict(model, xtest)
  pred = pred[which(is.na(pred)==FALSE)]
  xtest= xtest[which(is.na(pred)==FALSE),]
  RMSE_lr_test <- sqrt(mean((pred - xtest$units)^2));
  r2 = summary(model)$r.squared
  
  mse_all = c(mse_all, RMSE_lr_test)
  r2_all = c(r2_all, r2)
}

mean(mse_all)   #28.66
var(mse_all)^.5
mean(r2_all)   #0.607


pred <-  predict(model, newdata = test[,c("store_nbr",  "item_nbr", "Before3", "Before2", "Before1", "any_event", "After1","After2", "After3",
                                          "month", "is_weekend" , "is_winter" , "leapday", 
                                          "before_Valentines" , "before_4July" , "is_holiday" , "tmax" , "tmin" , 
                                          "tavg" ,"dewpoint" , "heat" ,"store_volume" , "percent_sales", 
                                          "before_Xmas" , "before_Memorial",  "before_NewYears", 
                                          "BlackFriday" , "before_Thanksgiving" , "before_Easter" , "before_Halloween") ])



#### model without my variables:
model2=lm(units~ .,  data=train_nonzero[, c(2:11)])
summary(model2)

### model with 1 store and 1 unit:
for(i in 1:ncol(df_temp)){print(colnames(df_temp)[i]); print( summary(df_temp[,i])) }
df_temp = train_nonzero[which(train_nonzero$store_nbr==15 & train_nonzero$item_nbr==45),]
df_temp= df_temp[,-c(2,3, 12, 13, 29, 33, 38, 39, 40, 46, 47, 49)]  #get rid of store_volume, percent_sales, item_nbr, store_nbr, depart, rain & snow events, snow_numeric (too often only 0)

#any of the factor variables only have one level? 
(l <- sapply(df_temp, function(x) is.factor(x)))
m <- df_temp[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")

model3 = lm(units ~ Before3 + Before2 + Before1 + any_event + After1 + After2 + After3+
              sales_3_10_days_before+ month + is_weekend+ is_winter + leapday + before_Valentines + before_4July+
              is_holiday + tmax + tmin+tavg + dewpoint + wetbulb+ heat + cool + stnpressure+sealevel + resultspeed+
              resultdir + avgspeed + rain_numeric + rain_event + before_Xmas + before_Memorial + before_NewYears +
              BlackFriday + before_Thanksgiving + before_Easter + before_Halloween 
            , data=df_temp)    #. -date -snow_numeric - snow_event 

model3 = lm(units~. -date, data = df_temp)
summary(model3)

### another model with 1 store and 1 unit:
df_temp = train_nonzero[which(train_nonzero$store_nbr==3),]
#any of the factor variables only have one level? 
(l <- sapply(df_temp, function(x) is.factor(x)))
m <- df_temp[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")


#Delete Outliers and rerun model
train_nonzero=train_nonzero[-c(42789,84341),]

###Log transform: 
model4= lm(log(units) ~ store_nbr + item_nbr + Before3 + Before2 + Before1 + any_event + After1 + After2 + After3+ 
             sales_3_10_days_before+ month+ is_weekend +is_winter + leapday 
           + before_Valentines + before_4July + is_holiday+tmax + tmin +  
             tavg + dewpoint + heat + resultspeed+ 
             resultdir + avgspeed + rain_numeric + before_Xmas + before_Memorial + before_NewYears+ 
             BlackFriday + before_Thanksgiving + before_Easter + before_Halloween, 
           data=train_nonzero)  
summary(model4)
#adjusted r^2 = 0.8708 
plot(model4)
#residuals look awful!!!

#sqrt transform: 
model5= lm(sqrt(units) ~ store_nbr + item_nbr + Before3 + Before2 + Before1 + any_event + After1 + After2 + After3+ 
             sales_3_10_days_before+ month+ is_weekend +is_winter + leapday 
           + before_Valentines + before_4July + is_holiday+tmax + tmin +  
             tavg + dewpoint + heat + resultspeed+ 
             resultdir + avgspeed + rain_numeric + before_Xmas + before_Memorial + before_NewYears+ 
             BlackFriday + before_Thanksgiving + before_Easter + before_Halloween, 
           data=train_nonzero)  
summary(model4)   #adjusted r^2 = 0.8383
plot(model4)
#residuals look okay with a few outliers


#------------------ Train/test split & cross-val --------------
n<-nrow(train_nonzero)
remaining = 1:n

for (i in 1:10){
  
  if (i <= 10 - n%%10){
    fold_indices=sample(remaining,floor(n/10))
  }
  else{
    fold_indices=sample(remaining,ceiling(n/10))
  }
  remaining =  subset(remaining, !(remaining %in% fold_indices))
  
  #name= paste("fold",i,sep="")
  #assign(name,train_nonzero[fold_indices,])
  
  train = train_nonzero[fold_indices,]
  
  model = lm(units ~ store_nbr + item_nbr + Before3 + Before2 + Before1 + any_event + After1 + After2 + After3+ 
               month+ is_weekend +is_winter + leapday 
             + before_Valentines + before_4July + is_holiday + tmax + tmin +  
               tavg + dewpoint + heat + store_volume + percent_sales + 
               before_Xmas + before_Memorial + before_NewYears+ 
               BlackFriday + before_Thanksgiving + before_Easter + before_Halloween, 
             data=train)   
  
  pred <-  predict(model, newdata = test[,c("store_nbr",  "item_nbr", "Before3", "Before2", "Before1", "any_event", "After1","After2", "After3",
                                            "month", "is_weekend" , "is_winter" , "leapday", 
                                            "before_Valentines" , "before_4July" , "is_holiday" , "tmax" , "tmin" , 
                                            "tavg" ,"dewpoint" , "heat" ,"store_volume" , "percent_sales", 
                                            "before_Xmas" , "before_Memorial",  "before_NewYears", 
                                            "BlackFriday" , "before_Thanksgiving" , "before_Easter" , "before_Halloween") ])
  MSE_current <- mean((pred - test$units)^2);
  r2 = summary(model)$r.squared
  
  acc_list=append(acc_list,MSE_current, r2 )
  
  
}

model5 = lm(units ~ store_nbr + item_nbr + Before3 + Before2 + Before1 + any_event + After1 + After2 + After3+ 
              sales_3_10_days_before+ month+ is_weekend +is_winter + leapday 
            + before_Valentines + before_4July + is_holiday+tmax + tmin +  
              tavg + dewpoint + heat + resultspeed+ 
              resultdir + avgspeed + rain_numeric + before_Xmas + before_Memorial + before_NewYears+ 
              BlackFriday + before_Thanksgiving + before_Easter + before_Halloween, 
            data=train_nonzero) 

#--------------- Impute temperature min/max/avg ---------
current_store = 34

temp = unique(train_nonzero[which((is.na(train_nonzero$tavg)==TRUE  | is.na(train_nonzero$tmin)==TRUE |
                                     is.na(train_nonzero$tmax)==TRUE) & train_nonzero$store_nbr==current_store) ,
                            c("date","store_nbr","tmax", "tmin","tavg" )])
temp

#fix simple average missing but min and max there:
temp$tavg = (temp$tmin + temp$tmax)/2
temp_complete = temp[which(complete.cases(temp)==TRUE),]
temp_complete

for (i in 1:nrow(temp_complete)){
  train_nonzero[which(train_nonzero$date ==temp_complete$date[i] & train_nonzero$store_nbr==temp_complete$store_nbr[i] ), c("tmax", "tmin", "tavg")] = temp_complete[i, c("tmax", "tmin","tavg")]
}

#get the ones that are still missing: 
temp = temp[which(is.na(temp$tavg)==TRUE  | is.na(temp$tmin)==TRUE |is.na(temp$tmax)==TRUE) ,
            c("date","store_nbr","tmax", "tmin","tavg" )]
temp

temptest = unique(train[which(train$store_nbr== current_store),c("date","store_nbr","tmax", "tmin","tavg" )])
#tempTest = unique(test[which(test$store_nbr==current_store),c("date","store_nbr","tmax", "tmin","tavg" )])

#fill in missing averages in temptest
for (i in 1:nrow(temptest)){
  if (is.na(temptest$tavg[i])==TRUE){
    temptest$tavg[i] = (temptest$tmin[i] + temptest$tmax[i])/2
  }
}

#easy ones: 
for (i in 1:nrow(temp)){
  current_date = temp$date[i]
  if (is.na(temp$tmin[i])==TRUE){
    temp$tmin[i] = mean(c(temptest[which(temptest$date == current_date + 1), "tmin"], temptest[which(temptest$date == current_date - 1), "tmin"] ))
  } 
  if (is.na(temp$tmax[i])==TRUE){
    temp$tmax[i] = mean(c(temptest[which(temptest$date == current_date + 1), "tmax"], temptest[which(temptest$date == current_date - 1), "tmax"] ))
  } 
  if (is.na(temp$tavg[i])==TRUE){
    temp$tavg[i] = mean(c(temptest[which(temptest$date == current_date + 1), "tavg"], temptest[which(temptest$date == current_date - 1), "tavg"] ))
  } 
}

#send to main dataframe: 
temp_complete = temp[which(complete.cases(temp)==TRUE),]
temp_complete

for (i in 1:nrow(temp_complete)){
  train_nonzero[which(train_nonzero$date ==temp_complete$date[i] & train_nonzero$store_nbr==temp_complete$store_nbr[i] ), c("tmax", "tmin", "tavg")] = temp_complete[i, c("tmax", "tmin","tavg")]
}


#clean the next round: 
temp = temp[which(is.na(temp$tavg)==TRUE  | is.na(temp$tmin)==TRUE |is.na(temp$tmax)==TRUE) ,
            c("date","store_nbr","tmax", "tmin","tavg" )]
temp


#harder ones:
for (i in 1:nrow(temp)){
  current_date = temp$date[i]
  
  if (is.na(temp$tmin[i])==TRUE){
    
    k=1
    answer1 = NA
    answer2 = NA
    mean_answer = NA
    
    while(is.na(answer1)==TRUE & is.na(answer2)==TRUE & is.na(mean_answer) ==TRUE & k<=4){
      answer1 = temptest[which(temptest$date == current_date + k), "tmin"]
      answer2 = temptest[which(temptest$date == current_date - k), "tmin"]
      mean_answer = mean(c(answer1, answer2))
      k = k+1
    }
    print(k)
    if(is.na(mean_answer)==FALSE){temp$tmin[i] = mean_answer}
    else if (is.na(answer1)==FALSE) {temp$tmin[i] = answer1}
    else {temp$tmin[i] = answer2}
    
  } 
  if (is.na(temp$tmax[i])==TRUE){
    k=1
    answer1 = NA
    answer2 = NA
    mean_answer = NA
    
    while(is.na(answer1)==TRUE & is.na(answer2)==TRUE & is.na(mean_answer) ==TRUE & k<=4){
      answer1 = temptest[which(temptest$date == current_date + k), "tmax"]
      answer2 = temptest[which(temptest$date == current_date - k), "tmax"]
      mean_answer = mean(c(answer1, answer2))
      k = k+1
    }
    print(k)
    if(is.na(mean_answer)==FALSE){temp$tmax[i] = mean_answer}
    else if (is.na(answer1)==FALSE) {temp$tmax[i] = answer1}
    else {temp$tmax[i] = answer2}
    
  } 
  if (is.na(temp$tavg[i])==TRUE){
    k=1
    answer1 = NA
    answer2 = NA
    mean_answer = NA
    
    while(is.na(answer1)==TRUE & is.na(answer2)==TRUE & is.na(mean_answer) ==TRUE & k<=4 ){
      answer1 = temptest[which(temptest$date == current_date + k), "tavg"]
      answer2 = temptest[which(temptest$date == current_date - k), "tavg"]
      mean_answer = mean(c(answer1, answer2))
      k = k+1
    }
    print(k)
    if(is.na(mean_answer)==FALSE){temp$tavg[i] = mean_answer}
    else if (is.na(answer1)==FALSE) {temp$tavg[i] = answer1}
    else {temp$tavg[i] = answer2}
    
  } 
}

#send to main dataframe: 
temp_complete = temp[which(complete.cases(temp)==TRUE),]
temp_complete

for (i in 1:nrow(temp_complete)){
  train_nonzero[which(train_nonzero$date ==temp_complete$date[i] & train_nonzero$store_nbr==temp_complete$store_nbr[i] ), c("tmax", "tmin", "tavg")] = temp_complete[i, c("tmax", "tmin","tavg")]
}

#clean the next round (hardest round):
temp = temp[which(is.na(temp$tavg)==TRUE  | is.na(temp$tmin)==TRUE |is.na(temp$tmax)==TRUE) ,
            c("date","store_nbr","tmax", "tmin","tavg" )]
temp


#for stores 3, 20, 28:
temp$tmax = 60.83333333
temp$tmin = 36.5
temp$tavg = 48.83333333

#send to main dataframe: 
temp_complete = temp[which(complete.cases(temp)==TRUE),]
temp_complete

for (i in 1:nrow(temp_complete)){
  train_nonzero[which(train_nonzero$date ==temp_complete$date[i] & train_nonzero$store_nbr==temp_complete$store_nbr[i] ), c("tmax", "tmin", "tavg")] = temp_complete[i, c("tmax", "tmin","tavg")]
}

#what store next? 
unique(train_nonzero[which((is.na(train_nonzero$tavg)==TRUE  | is.na(train_nonzero$tmin)==TRUE 
                            |is.na(train_nonzero$tmax)==TRUE)) ,c("store_nbr" )])
#make sure all NAs are gone:
temp = unique(train_nonzero[which((is.na(train_nonzero$tavg)==TRUE  | is.na(train_nonzero$tmin)==TRUE |
                                     is.na(train_nonzero$tmax)==TRUE)) ,
                            c("date","store_nbr","tmax", "tmin","tavg" )])

#get rid of store 35:
no35 = train_nonzero[-which(train_nonzero$store_nbr==35),]
write.csv(no35, "train_more_variables_notempNAs_noStore35.csv")


#------------- Descriptives ----------------
blah = train_nonzero %>% group_by(item_nbr, store_nbr) %>%tally()
blah = arrange(blah, store_nbr)
blah2 = blah %>% group_by(store_nbr) %>% count()
colnames(blah2) = c("store_nbr", "num_of_unique_items_sold_there")
blah2$store_nbr = as.numeric(as.character(blah2$store_nbr))
blah2 = arrange(blah2, store_nbr)

popular_products = read.csv("popular_products.csv")

#scatterplot of temperature vs sales:
ggplot(train_nonzero[-c(42303, 82691),], aes(tavg, units))+ geom_point(aes(colour = factor(any_event), alpha = factor(any_event)))+ scale_color_manual(values=alpha(c("#999999", "#56B4E9"), c(.5, .8)))+xlab("Average Temperature")+ylab("Units of Sales")+ggtitle("Sales by Temperature")

#scatterplot of date vs. units sold: 
ggplot(train2[-c(40952, 81017),], aes(date, units))+ 
  geom_point(aes(colour = factor(any_event), alpha = factor(any_event)))+ 
  scale_color_manual(values=alpha(c("#999999", "#56B4E9"), c(.8, .8)))+
  xlab("Date")+ylab("Units of Sales")+ggtitle("Sales by Date")

#Outliers: 
ggplot(train_nonzero[which(train_nonzero$item_nbr==5),], aes(date, units))+ 
  geom_point(alpha=.6, aes(colour = factor(outlier)), size = 2)+ 
  scale_color_manual(values=alpha(c("#999999", "#56B4E9"), c(.8, .8)))+
  xlab("Date")+ylab("Units of Sales")+ggtitle("Sales for Item #5")

#-------------- Abhiskeh's loop for running individual level regressions -------------
#Without test/train split
MSE_op <- data.frame(item= character(0), store_nbr= character(0), MSE = numeric(0), r2 = numeric(0))

i= 110
s=11

newdf = filter(train_nonzero, store_nbr == s)

x = subset(newdf, item_nbr == i)

var_list = c("date", "units")

if(s != 35){
  var_list = c(var_list, "tmax" , "tmin", "tavg")
}

try_list = c("Before3", "Before2", "Before1" , "After1", "any_event", "After2", "After3","month", 
             "is_weekend" , "is_winter" ,"is_holiday" ,"before_Xmas" ,
             "before_Thanksgiving" ,"before_Valentines", "before_Halloween",  "before_Easter",
             "before_Memorial","before_NewYears", "BlackFriday" ,"before_4July", "leapday")

for (var in try_list){
  current_sum = sum(as.numeric(as.character(x[,var]))) 
  
  if (current_sum !=0 & current_sum !=nrow(x)){
    var_list = c(var_list, var)
  }
}

if (sum(is.na(x$heat))==0){
  var_list = c(var_list,  "heat")
}
if (sum(is.na(x$rain_numeric))==0){
  var_list = c(var_list,  "rain_numeric")
}

x = x[, var_list]

lm = lm(units ~  .,data = x[,2:ncol(x)])
summary(lm)
pred <- predict(lm, x)


#RMSE_lr <- sqrt(mean((pred - x$units)^2));
#r2 = summary(lm)$r.squared
#output=cbind(i,s,RMSE_lr,r2)
#MSE_op=rbind(MSE_op,output)


#MSE_op

#colnames(MSE_op)=c("item_nbr","store_nbr","MSE", "r2")
#View(MSE_op)


temp_df2 = train_nonzero[which(train_nonzero$item_nbr==i & train_nonzero$store_nbr==s),]
##simpler plot:
plot(x = temp_df2[,"date"],
     y= temp_df2[, "units"],
     xlab="", ylab = "Total Units of Sales" ,main=paste("Over time sales of item #", i, "store #:", s), type="l",
     col="black")
points(x = temp_df2[,"date"], y= temp_df2[, "units"], pch=16, col= ifelse(temp_df2$After1 ==1, "red", "black"))
#plot predicted points:
points(x=temp_df2[,"date"], y= pred, pch = 17, cex=1.7, col="dark green")


#### With test/train split

MSE_op <- data.frame(item= character(0), store_nbr= character(0), RMSE_mean = numeric(0), RMSE_var = numeric(0),
                     r2_mean = numeric(0), r2_var = numeric(0))

store_nums = c(1:45)
for(s in store_nums) {
  newdf = filter(train_nonzero, store_nbr == s)
  for (i in unique(newdf$item_nbr)){
    
    #all possible rows:
    x = subset(newdf, item_nbr == i)
    
    # bootstrapping to get accurate MSE:
    n = nrow(x)[1]  #total number of observations
    n1 = round(n/10)  # number of observations randomly selected for testing data
    
    B= 1;   #5-fold bootstrapping 
    mse_all = c()
    r2_all =c()
    
    for (b in 1:B){
      # randomly select 25 observations as testing data in each loop:
      flag <- sort(sample(1:n, n1));
      xtrain <- x[-flag,];
      xtest <- x[flag,];
      
      var_list = c("date", "units")
      
      if(s != 35){
        var_list = c(var_list, "tmax" , "tmin", "tavg")
      }
      
      try_list = c("Before3", "Before2", "Before1" , "After1", "any_event", "After2", "After3","month", 
                   "is_weekend" , "is_winter" ,"is_holiday" ,"before_Xmas" ,
                   "before_Thanksgiving" ,"before_Valentines", "before_Halloween",  "before_Easter",
                   "before_Memorial","before_NewYears", "BlackFriday" ,"before_4July", "leapday")
      
      for (variable in try_list){
        current_sum = sum(as.numeric(as.character(xtrain[,variable]))) 
        
        if (current_sum !=0 & current_sum !=nrow(xtrain)){
          var_list = c(var_list, variable)
        }
      }
      
      if (sum(is.na(xtrain$heat))==0){
        var_list = c(var_list,  "heat")
      }
      if (sum(is.na(xtrain$rain_numeric))==0){
        var_list = c(var_list,  "rain_numeric")
      }
      
      xtrain = xtrain[, var_list]
      
      if(identical(sort(unique(xtest$month)), sort(unique(xtrain$month)))==FALSE ){
        xtest = xtest[which(xtest$month %in% unique(xtrain$month) ),]
      }
      
      #full model
      #model_full = lm( brozek ~ ., data = fattrain)
      #pred_full = predict(model_full, fattestx);
      #te1 = mean((pred_full - ytrue)^2)
      
      lm = lm(units ~  .,data = xtrain[,2:ncol(xtrain)])
      pred <- predict(lm, xtest)
      RMSE_lr_test <- sqrt(mean((pred - xtest$units)^2));
      r2 = summary(lm)$r.squared
      
      mse_all = c(mse_all, RMSE_lr_test)
      r2_all = c(r2_all, r2)
    }
    RMSE_mean = mean(mse_all, na.rm=TRUE);
    r2_mean = mean(r2_all, na.rm=TRUE)
    RMSE_var = stats::var(mse_all, na.rm=TRUE);
    r2_var  = stats::var(r2_all, na.rm=TRUE);
    
    output = cbind(i,s,RMSE_mean, RMSE_var, r2_mean, r2_var)
    
    MSE_op=rbind(MSE_op,output)
    
  }
}
colnames(MSE_op)=c("item_nbr","store_nbr","RMSE_test_mean", "RMSE_test_var", "r2_train_mean", "r2_train_var")
MSE_op
MSE_op = as.data.frame(MSE_op)

#join this table:
total_transactions = train_nonzero %>% group_by(store_nbr, item_nbr) %>% tally()
total_units = train_nonzero %>% group_by(store_nbr, item_nbr) %>% summarise(total_units = sum(units) )
colnames(total_transactions) = c("store_nbr", "item_nbr", "total_transactions")
MSE_op = left_join(MSE_op, total_transactions, by = c("store_nbr", "item_nbr"))
MSE_op = left_join(MSE_op, total_units, by = c("store_nbr", "item_nbr"))

write.csv(MSE_op ,"individal_results_AllVars.csv")


temp_df2 = train_nonzero[which(train_nonzero$item_nbr==i & train_nonzero$store_nbr==s),]
##simpler plot:
plot(x = temp_df2[,"date"],
     y= temp_df2[, "units"],
     xlab="", ylab = "Total Units of Sales" ,main=paste("Over time sales of item #", i, "store #:", s), type="l",
     col="black")
points(x = temp_df2[,"date"], y= temp_df2[, "units"], pch=16, col= ifelse(temp_df2$After1 ==1, "red", "black"))
#plot predicted points:
points(x=x[,"date"], y= pred, pch = 17, col="dark green", cex = 1.7)




#---------------------- GAM --------------------------------
library(gam)
train_nonzero = read.csv("train_more_variables_notempNAs_noStore35.csv", stringsAsFactors = FALSE)
train_nonzero = train_nonzero[,-1]

train_nonzero$date= ymd(train_nonzero$date)
for (i in c(2,3, 5:11, 15:29, 47, 49 )){
  train_nonzero[,i] = as.factor(train_nonzero[,i])
}


#remove outliers: 
train_noout = train_nonzero[-c(42303, 82691),]

model_spline = lm(units ~ Before3+Before2
                  +Before1+any_event+After1+After2+After3
                  +s(sales_3_10_days_before)+month+is_weekend+is_winter
                  +leapday+before_Valentines+before_4July+is_holiday
                  +s(tmax)+s(tmin)+s(tavg)+s(dewpoint)+s(heat)+s(resultspeed)
                  +s(resultdir)+s(rain_numeric)+before_Xmas
                  +before_Memorial+before_NewYears+BlackFriday
                  +before_Thanksgiving+before_Easter+before_Halloween,
                  family = gaussian, data=train_noout)
summary(model_spline)
a = summary(model_spline)$coefficients
a = as.data.frame(a)

a$names = rownames(a)
a= arrange(a, `Pr(>|t|)`)
a$names[1] = "Previous_Sales"
a$names[2] = "is_weekend"
a$names[3] = "before_Xmas"
a$names[4] = "December"
a$names[5] = "before_T-giving"
a$names[6] = "May"
a$names[7] = "before 4th of July"
a$names[8] = "September"
a$names[9] = "April"
a$names[10] = "Black Friday"

#`Pr(>|t|)`

a$names<- factor(a$names, levels = a$names[order(a$`Pr(>|t|)`, decreasing=TRUE)])

ggplot(a[1:10,], aes(x=names, y=`t value`)) +
  geom_bar(stat="identity", fill="skyblue4", width=0.5)+ theme_solarized_2() +coord_flip() + xlab("")+ ylab("t-statistic")+
  ggtitle("Overall General Feature Importance")+
  theme(plot.title=element_text(face="bold", size=20))+
  theme(axis.text.y = element_text(face="bold", color="#993333", size=12))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.title = element_text(face="bold", size=15,family="sans"))+
  theme(plot.margin = unit(c(5,5,5,5),"mm"))


ggplot(a[c(1:8, 10),], aes(names))+ 
  geom_bar(aes(weight = `t value`))+ 
  xlab("variables")+
  ylab("t-statistic")+
  ggtitle("General Variable importance")

####Bootstrapped GAM:
n = nrow(train_noout)[1]  #total number of observations
n1 = round(n/10)  # number of observations randomly selected for testing data

B= 10;   #5-fold bootstrapping 
mse_all = c()
r2_all =c()

for (b in 1:B){
  # randomly select 25 observations as testing data in each loop:
  flag <- sort(sample(1:n, n1));
  xtrain <- train_noout[-flag,];
  xtest <- train_noout[flag,];
  
  model_spline = lm(units ~ Before3+Before2
                    +Before1+any_event+After1+After2+After3
                    +s(sales_3_10_days_before)+month+is_weekend+is_winter
                    +leapday+before_Valentines+before_4July+is_holiday
                    +s(tmax)+s(tmin)+s(tavg)+s(dewpoint)+s(heat)+s(resultspeed)
                    +s(resultdir)+s(rain_numeric)+before_Xmas
                    +before_Memorial+before_NewYears+BlackFriday
                    +before_Thanksgiving+before_Easter+before_Halloween,
                    family = gaussian, data=xtrain)
  
  pred <- predict(model_spline, xtest)
  pred = pred[which(is.na(pred)==FALSE)]
  xtest= xtest[which(is.na(pred)==FALSE),]
  RMSE_lr_test <- sqrt(mean((pred - xtest$units)^2));
  r2 = summary(model_spline)$r.squared
  
  mse_all = c(mse_all, RMSE_lr_test)
  r2_all = c(r2_all, r2)
}

mean(mse_all)   #60.202
mean(r2_all)   #0.748

#linear regression models for each cluster

clusters = read.csv("clusters.csv", header = TRUE)
master_data = read.csv("master_data_more_features.csv", header = TRUE)

#left join clustering results with master file
data1=merge(x = master_data, y = clusters, by = "item_nbr", all.x = TRUE)


#Cluster 1
clus2_data=subset(data1, item_cluster1 == 2)

n = dim(clus2_data)[1]; ### total number of observations
n1 = round(n/10); ### number of observations randomly selected for testing data
set.seed(20170201); ### set the random seed
flag = sort(sample(1:n, n1)); #creates a vector flag with indices of test dataset

data2_train = clus2_data[-flag,]; #training dataset
data2_test = clus2_data[flag,]; #test dataset
model2=lm(units~.,data=data2_train)
summary(model2)
pred2 <- predict(model2, data2_test);
MSE_lr2 <- mean((pred2 - data2_test$units)^2);

#Cluster 3
clus3_data=subset(data1, item_cluster1 == 3)

n = dim(clus3_data)[1]; ### total number of observations
n1 = round(n/10); ### number of observations randomly selected for testing data
set.seed(20170201); ### set the random seed
flag = sort(sample(1:n, n1)); #creates a vector flag with indices of test dataset

data3_train = clus3_data[-flag,]; #training dataset
data3_test = clus3_data[flag,]; #test dataset

model3=lm(units~.,data=data3_train)
summary(model3)
pred3 <- predict(model3, data3_test);
MSE_lr3 <- mean((pred3 - data3_test$units)^2);


#Cluster 4
clus4_data=subset(data1, item_cluster1 == 4)

n = dim(clus4_data)[1]; ### total number of observations
n1 = round(n/10); ### number of observations randomly selected for testing data
set.seed(20170201); ### set the random seed
flag = sort(sample(1:n, n1)); #creates a vector flag with indices of test dataset

data4_train = clus4_data[-flag,]; #training dataset
data4_test = clus4_data[flag,]; #test dataset

model4=lm(units~.,data=data4_train)
summary(model4)
pred4 <- predict(model4, data4_test);
MSE_lr4 <- mean((pred4 - data4_test$units)^2);

#Cluster 5
clus5_data=subset(data1, item_cluster1 == 5)

n = dim(clus5_data)[1]; ### total number of observations
n1 = round(n/10); ### number of observations randomly selected for testing data
set.seed(20170201); ### set the random seed
flag = sort(sample(1:n, n1)); #creates a vector flag with indices of test dataset

data5_train = clus5_data[-flag,]; #training dataset
data5_test = clus5_data[flag,]; #test dataset

model5=lm(units~.,data=data5_train)
summary(model5)
pred5 <- predict(model5, data5_test);
MSE_lr5 <- mean((pred5 - data5_test$units)^2);

#Cluster 7
clus7_data=subset(data1, item_cluster1 == 7)
clus7_data = clus7_data[,c(1,4:51)]

n = dim(clus7_data)[1]; ### total number of observations
n1 = round(n/10); ### number of observations randomly selected for testing data
set.seed(20170201); ### set the random seed
flag = sort(sample(1:n, n1)); #creates a vector flag with indices of test dataset

data7_train = clus7_data[-flag,]; #training dataset
data7_test = clus7_data[flag,]; #test dataset

model7=lm(units~.,data=data7_train)
summary(model7)
pred7 <- predict(model7, data7_test);
MSE_lr7 <- mean((pred7 - data7_test$units)^2);

#Random Forest 
library(randomForest)
library(miscTools)
library(ggplot2)


#using cluster number as a proy for item_nbr since RF cannot 
data2=data1[,c(2:11)]
data2$cluster_nbr=as.factor(data2$cluster_nbr)
rf <- randomForest(units ~ ., data=data2, ntree=20)
rf_r2 <- rSquared(data2$units, data2$units - predict(rf, data2))

lr_model=lm(units~., data=data2)
summary(lr_model)

rf_mse <- mean((data2$units - predict(rf, data2[c(1,3:10)]))^2)
lr_mse <- mean((data2$units - predict(lr_model, data2[c(1,3:10)]))^2)


## regression models for each of the items
selected_data=subset(data1, item_nbr == c("1","2","3"))

for (i in 1:45)
{
  mod_data=selected_data[which(selected_data$store_nbr==i),]
  for (j in 1:(unique(selected_data$item_nbr)))
  {
    mod_data1=mod_data[which(selected_data$item_nbr==i),]
    lm_i = lm(mod_data$units ~ . , data = mod_data[,2:10])
    output[i,]=predict(lm_i, mod_data[,c(2,4:10)])
    
  }}


n = dim(data1)[1]; ### total number of observations
n1 = round(n/10); ### number of observations randomly selected for testing data
set.seed(20170201); ### set the random seed
flag = sort(sample(1:n, n1)); #creates a vector flag with indices of test dataset

data1_train = data1[-flag,]; #training dataset
data1_test = data1[flag,]; #test dataset

MSE_op <- data.frame(item= character(0), store_nbr= character(0), MSE = numeric(0), MSE = numeric(0), MSE = numeric(0))

store_nums = c(1:45)
for(s in store_nums) {
  newdf = filter(data1_train, store_nbr == s)
  newdf1= filter(data1_test, store_nbr == s)
  for (i in unique(newdf$item_nbr)){
    x = subset(newdf, item_nbr == i)
    y = subset(newdf1, item_nbr == i)
    lm = lm(units ~ . , data = x[,3:10])
    pred <- predict(lm, x[,4:10]);
    pred1 <- predict(lm, y[,4:10]);
    
    
    MSE_lr <- mean((pred - x$units)^2);
    MSE_lr1 <- mean((pred1 - y$units)^2);
    
    output=cbind(i,s,MSE_lr, MSE_lr1, summary(lm)$r.squared)
    MSE_op=rbind(MSE_op,output)
  }
  
}

colnames(MSE_op)=c("item_nbr","store_nbr","MSE_train", "MSE_test", "R-squar")
View(MSE_op)

details=train_nonzero %>%  group_by(store_nbr, item_nbr) %>% tally()
export=merge(x = MSE_op, y = details, by = c("store_nbr", "item_nbr"), all.x = TRUE)

#RANDOM FOREST

train <- read.csv("Desktop/6740/train_more_variables_notempNAs_noStore35.csv", stringsAsFactors = FALSE, na.strings = c("NA", "M", "-"))
dat <- train[, c("date", "store_nbr", "item_nbr", "units", "tmax", "tmin", "tavg", "snow_numeric", "rain_numeric")]

#dataset with imputed missing temp values and sales > 0

library(dplyr)

dat[is.na(dat)] <- 0

item_nums = sort(unique(dat$item_nbr))
newitems = seq(length(item_nums))

item_key <- data.frame(actual = item_nums, mapped = newitems)

dat$item_nbr = match(dat$item_nbr,item_nums)

#mean of tmax, tavg, tmin

dat$temp <- rowMeans(dat[,c("tmax", "tavg", "tmin")])

dat$date <- as.factor(dat$date)

#get day of week
dat$day <- as.factor(weekdays(as.Date(dat$date, format="%Y-%m-%d")))

summary(dat)

#combine over all stores for that item

dat %>%
  group_by(date, item_nbr) %>%
  summarise(temp = min(temp), snow_numeric = max(snow_numeric), rain_numeric = max(rain_numeric), units = sum(units))

summary(dat)

df <- dat[,c(3, 4, 8:11)]

#normalize prior to clustering

mydata <- scale(df[,c(2:5)])

#get number of clusters

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#kmeans clustering

itemCluster <- kmeans(mydata, 8, nstart = 20)

item_nums = sort(unique(dat$item_nbr))

#assigning clusters - 1st and 2nd most frequent

item_cluster1 = sapply(item_nums, function(x){
  cl <- itemCluster$cluster[dat$item_nbr == x]
  names(sort(table(cl), decreasing = TRUE))[1]
})

clus = data.frame(table(item_cluster1))

item_cluster2 = sapply(item_nums, function(x){
  cl <- itemCluster$cluster[dat$item_nbr == x]
  names(sort(table(cl), decreasing = TRUE))[2]
})



dat$cluster1 = as.factor(item_cluster1[dat$item_nbr])
dat$cluster2 = as.factor(item_cluster2[dat$item_nbr])


df = dat[, c("day", "store_nbr", "cluster1", "cluster2", "snow_numeric", "temp", "rain_numeric", "units")]
df = filter(df, units<500)

cols <- c("day", "store_nbr", "snow_numeric", "temp", "rain_numeric","cluster1", "cluster2")

library(caret)

#parittion data into train and test sets

trainIndex <- createDataPartition(df$units, p = 0.9, list = FALSE)
xtrain <-df[trainIndex, cols]
ytrain <- df[trainIndex, ]$units

xtest <- df[-trainIndex, cols]
ytest<- df[-trainIndex, ]$units

library(randomForest)
library(miscTools)
library(ggplot2)

#random forest

rf <- randomForest(ytrain ~ ., data = xtrain, ntree=15, importance = TRUE)

pred <- predict(rf, xtest)
actuals <- ytest

#variable importance

imp = varImp(rf)

varImpPlot(rf)

r2 = postResample(pred, actuals)[2][[1]]

postResample(pred, actuals)

#plot of predicted vs actual values

p <- ggplot(aes(x=actual, y=pred),
            data=data.frame(actual=actuals, pred=pred))
p + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""))


