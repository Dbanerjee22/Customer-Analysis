#To clear r memory
rm(list=ls(all=TRUE))
#loading data
data=read.delim("purchases.txt",header = FALSE,sep = "\t",dec=".")
#adding column name to the data
colnames(data)=c('customer_id', 'purchase_amount', 'date_of_purchase')
#converting date_of_purchase from string to date format and finding recent visit
data$date_of_purchase=as.Date(data$date_of_purchase,"%Y-%m-%d")
data$year_of_purchase = as.numeric(format(data$date_of_purchase, "%Y"))
data$days_since=as.numeric(difftime("2016-01-01",data$date_of_purchase,
                                    units ="days"))
#viewing data
summary(data)
#loading sql library to filter data
library(sqldf)
#computing recency, frequency, average amount of purchase
customers=sqldf("SELECT customer_id,
               MIN(days_since) as 'recency',
               COUNT(*) as 'frequency', 
               AVG(purchase_amount) as 'amount'
               from data GROUP BY 1")
#exploring the data
hist(customers$recency,main = "How frequently customer visit")
hist(customers$frequency,main = "Frequency of customer visiting")
hist(customers$amount,breaks = 100,main = "Amount bought")
#---------Prepearing and transforming our data--------------------


#copying customers data into new data frame
new_data=customers
head(new_data)
row.names(new_data)=new_data$customer_id
new_data$customer_id=NULL
#take log transformation of the amount and plot 
new_data$amount=log(new_data$amount)
hist(new_data$amount,main = "Log transformation of amount")

#standarize the variables
new_data=scale(new_data)
# --- RUNNING A HIERARCHICAL SEGMENTATION ------------------


# Compute distance metrics on standardized data
# This will likely generate an error on most machines
sample=seq(1,18417,by=10)
head(sample)
customers_sample=customers[sample,]
new_data_sample=new_data[sample,]
# Compute distance metrics on standardized data
d=dist(new_data_sample)
# Perform hierarchical clustering on distance metrics
c=hclust(d,method = "ward.D2")
# Plot de dendogram
plot(c)
# Cut at 9 segments
members=cutree(c,k=9)
# Show 30 first customers, frequency table
members[1:30]
#show which cluster contain how many elements
table(members)
# Show profile of each segment
aggregate(customers_sample[, 2:4], by = list(members), mean)

#customer analysis till 2015
customers_2015 = sqldf("SELECT customer_id,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'amount'
                        FROM data GROUP BY 1")
#explore data of 2015
head(customers_2015)
summary(customers_2015)
hist(customers_2015$recency)
hist(customers_2015$frequency)
hist(customers_2015$amount, breaks = 100)
# --- CODING A MANAGERIAL SEGMENTATION --------------------------
customers_2014 = sqldf("SELECT customer_id,
                               MIN(days_since) - 365 AS 'recency',
                               MAX(days_since) - 365 AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'amount'
                        FROM data
                        WHERE days_since > 365
                        GROUP BY 1")

# Complete segment solution using which, and exploiting previous test as input
customers_2014$segment = "NA"
customers_2014$segment[which(customers_2014$recency > 365*3)] = "inactive"
customers_2014$segment[which(customers_2014$recency <= 365*3 & customers_2014$recency > 365*2)] = "cold"
customers_2014$segment[which(customers_2014$recency <= 365*2 & customers_2014$recency > 365*1)] = "warm"
customers_2014$segment[which(customers_2014$recency <= 365)] = "active"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$first_purchase <= 365*2)] = "new warm"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount < 100)] = "warm low value"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount >= 100)] = "warm high value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$first_purchase <= 365)] = "new active"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount < 100)] = "active low value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount >= 100)] = "active high value"
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))


# Re-order factor in a way that makes sense
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                      "active high value", "active low value", "new active"))
table(customers_2014$segment)
pie(table(customers_2014$segment),edges = 500,radius = 1.5, col = rainbow(24))
aggregate(x=customers_2014[,2:5],by=list(customers_2014$segment),mean)
# Simple 2-segment solution based on recency alone
customers_2015$segment=ifelse(test = customers_2015$recency>365*3,
                              yes = 'inactive',
                              no='NA' )
aggregate(customers_2015[,2:5],by=list(customers_2015$segment),mean)
#this process will cause lot of error when the no of segments increase
customers_2015$segment=ifelse(test = customers_2015$recency>365*3,
                              yes = 'inactive',
                              no=ifelse(test = customers_2015$recency>365*2,
                                        yes="cold",
                                        no="na") )
table(customers_2015$segment)
aggregate(customers_2015[,2:5],by=list(customers_2015$segment),mean)
#creating segments using which statement
customers_2015$segment="NA"
customers_2015$segment[which(customers_2015$recency>365*3)]="inactive"
table(customers_2015$segment)
aggregate(customers_2015[,2:5],by=list(customers_2015$segment),mean)
#creating other segments
customers_2015$segment[which(customers_2015$recency>365*2 & customers_2015$recency<=365*3)]="cold"
customers_2015$segment[which(customers_2015$recency>365*1 & customers_2015$recency<=365*2 )]="warm"
customers_2015$segment[which(customers_2015$recency<=365)]="Active"
table(customers_2015$segment)
aggregate(customers_2015[,2:5],by=list(customers_2015$segment),mean)
#farther segregate the data
customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & customers_2015$recency > 365*2)] = "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & customers_2015$recency > 365*1)] = "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] = "active"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2)] = "new warm"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount < 100)] = "warm low value"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount >= 100)] = "warm high value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$first_purchase <= 365)] = "new active"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount < 100)] = "active low value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount >= 100)] = "active high value"
customers_2015$segment = factor(x = customers_2015$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))

table(customers_2015$segment)
aggregate(customers_2015[,2:5],by=list(customers_2015$segment),mean)
#reordering the data
customers_2015$segment=factor(x=customers_2015$segment,levels = c("inactive", "cold",
                                                                   "warm high value", "warm low value", "new warm",
                                                                   "active high value", "active low value", "new active"))
table(customers_2015$segment)
aggregate(customers_2015[,2:5],by=list(customers_2015$segment),mean)
# --- COMPUTING REVENUE GENERATION PER SEGMENT -------------


# Compute how much revenue is generated by segments
# Notice that people with no revenue in 2015 do NOT appear
revenue_2015= sqldf("SELECT customer_id,
                    SUM(purchase_amount) as 'revenue_2015'
                    FROM data
                    WHERE year_of_purchase = 2015
                    GROUP BY 1")
summary(revenue_2015)
#merge 2015 revenew with 2015 customer
actual=merge(customers_2015,revenue_2015)
#merge statement is excluding all those customer who didn't genartae any revenue in 2015,to include all customer
actual=merge(customers_2015,revenue_2015,all.x = TRUE)
actual$revenue_2015[is.na(actual$revenue_2015)]=0
#show average revenue per customer per segment
aggregate(x=actual$revenue_2015,by=list(actual$segment),mean)
#forward 2014 customers to 2015 revenue
forward=merge(customers_2014,revenue_2015,all.x = TRUE)
forward$revenue_2015[is.na(forward$revenue_2015)]=0
#average revenue genartaed on 2015 based on the segments of 2014
r=aggregate(forward$revenue_2015,by=list(forward$segment),mean)
#reordering the data
r=r[order(r$x,decreasing = TRUE),]
barplot(r$x,names.arg = r$Group.1)
#-------------Create a model for prediction---------------------
#Merge data customer_2014 with revenue_2015
in_sample=merge(customers_2014,revenue_2015,all.x = TRUE)
in_sample$revenue_2015[is.na(in_sample$revenue_2015)]=0
in_sample$active_2015=as.numeric(in_sample$revenue_2015>0)
# Display calibration (in-sample) data
head(in_sample)
summary(in_sample)
# --- CALIBRATE THE MODELS ---------------------------------
# Calibrate probability model
library(nnet)
prob.model=multinom(formula = active_2015~recency+first_purchase+frequency+avg_amount+max_amount,
                      data=in_sample)
coef=summary(prob.model)$coefficients
std=summary(prob.model)$standard.errors
print(coef)
print(std)
print(coef / std)
# For the monetary model, select only those who made a purchase
z=which(in_sample$active_2015==1)
head(in_sample[z, ])
summary(in_sample[z, ])
# Calibrate the monetary model (version 1)
amount.model=lm(log(revenue_2015)~log(avg_amount)+log(max_amount),data = in_sample[z,])
summary(amount.model)
# Plot the results of this new monetary model
plot(x=log(in_sample[z,]$revenue_2015),y=amount.model$fitted.values)
# --- APPLY THE MODELS TO TODAY'S DATA ---------------------
# Compute RFM variables as of today
# Predict the target variables based on today's data
customers_2015$prob_predicted=predict(object = prob.model,newdata = customers_2015,type = "prob")
customers_2015$revenue_predicted=exp(predict(object = amount.model,newdata = customers_2015))
customers_2015$score_predicted=customers_2015$prob_predicted*customers_2015$revenue_predicted
summary(customers_2015$prob_predicted)
summary(customers_2015$revenue_predicted)
summary(customers_2015$score_predicted)
hist(customers_2015$score_predicted)
# How many customers have an expected revenue of more than $50
z=which(customers_2015$score_predicted>50)
#------------------Customer Lifetime Value-------------------------
#-----Compute Transition Matrix----------------
new_data=merge(x=customers_2014,y=customers_2015,by="customer_id",all.x = TRUE)
transition=table(new_data$segment.x,new_data$segment.y)
print(transition)
#divide each row with its sum
transition=transition / rowSums(transition)
print(transition)
#---------using transition matrix to make prediction-----------------
# Initialize a matrix with the number of customers in each segment today and after 10 periods
segments=matrix(nrow = 8,ncol = 11)
segments[,1]=table(customers_2015$segment)
colnames(segments)=2015:2025
row.names(segments)=levels(customers_2015$segment)
# Compute for each an every period
for (i in 2:11) {
  segments[,i]=segments[,i-1] %*% transition
}
# Plot inactive, active high value customers over time
barplot(segments[1, ])
barplot(segments[2, ])
# Display how segments will evolve over time
print(round(segments))
# --- COMPUTE THE (DISCOUNTED) CLV OF A DATABASE -----------


# Yearly revenue per segment
# This comes directly from module 2, lines 160-161
# Compute revenue per segment
yearly_revenue = c(0, 0, 0, 0, 0, 323.57, 52.31, 79.17)
print(segments)
revenue_per_segment=yearly_revenue*segments
print(revenue_per_segment)
# Compute yearly revenue
yearly_revenue=colSums(revenue_per_segment)
print(yearly_revenue)
barplot(yearly_revenue)
# Compute cumulated revenue
cumulated_revenue=cumsum(yearly_revenue)
print(round(cumulated_revenue))
barplot(cumulated_revenue)
#create discount rate
discount_rate=0.10
discount=1/((1+discount_rate)^((1:11)-1))
print(discount)
# Compute discounted yearly revenue
disc_yearly_revenue=yearly_revenue*discount
print(disc_yearly_revenue)
barplot(disc_yearly_revenue)
lines(yearly_revenue)
# Compute discounted cumulated revenue
disc_cumulated_revenue=cumsum(disc_yearly_revenue)
print(disc_cumulated_revenue)
print(round(disc_cumulated_revenue))
barplot(disc_cumulated_revenue)
# What is the database worth?
print(disc_cumulated_revenue[11]-yearly_revenue[1])
