library(lubridate)
library(tidyverse)
library(plotrix)
library(scales)

#Question1
table1 <- SalesData %>% select(Region, Sales2015, Sales2016)
table2 <- gather(table1,"Year","Sales",2:3)
comb_data <- table2 %>% group_by(Region,Year) %>% summarise(total_sales=sum(Sales))
ggplot(comb_data, aes(x=Region, y=total_sales, fill=Year)) + geom_bar(position=position_dodge(), stat="identity")+geom_text(aes(label=round(total_sales)), position=position_dodge(width=0.9), vjust=-0.25)+scale_y_continuous(labels=comma) 

#Question2
data2016 <- SalesData %>% group_by(Region) %>% summarise(TotalSales=sum(Sales2016))
data2016 <- mutate(data2016, percent=round(TotalSales/sum(TotalSales)*100,1))

pie(data2016$percent, labels = data2016$Region)

slices <- data2016$TotalSales
pct <- data2016$percent
lbls <- data2016$Region
lbls <- paste0(pct,"%"," :",lbls)

#Combining Pie Charts
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
pie(data2016$percent, labels = data2016$Region)
pie(slices,labels = lbls,col=rainbow(length(lbls)))
pie3D(slices,labels=lbls,explode = 0.1)
mtext("Pie Chart of Sales 2016", outer=TRUE,  cex=1.7, line=-1.95)

#Question3
table3 <- mutate(table1, Tier = SalesData$Tier)
table4 <- gather(table3, "Year", "Sales", 2:3)
comb_data1 <- table4 %>% group_by(Region,Tier,Year) %>% summarise(total_sales=sum(Sales))
ggplot(comb_data1, aes(fill=Year, y=total_sales, x=Tier)) + geom_bar(position="dodge", stat="identity") + facet_grid(. ~ Region)+scale_y_continuous(labels=comma)

#Question4
table5 <- SalesData %>% select(Region, State, Sales2015, Sales2016) %>% filter(Region=="East")
table6 <- gather(table5,"Year","Sales",3:4)
comb_data2 <- table6 %>% group_by(State,Year) %>% summarise(total_sales=sum(Sales))
ggplot(comb_data2, aes(fill=Year, y=total_sales, x=State)) + geom_bar(position=position_dodge(), stat="identity")

#Question5.
table7 <- SalesData %>% select(Division, Tier, Units2015, Units2016) %>% filter(Tier=="High")
table8 <- gather(table7, "Year", "Units",3:4)
comb_data3 <- table8 %>% group_by(Division,Year) %>% summarise(Total_Units=sum(Units))
ggplot(comb_data3, aes(fill=Year, y=Total_Units, x=Division)) + geom_bar(position=position_dodge(), stat="identity") + scale_x_discrete(labels=abbreviate)

#Question6.
x <- SalesData$Month #Vector of months

vector1<-c("Jan","Feb","Mar")
vector2<-c("Apr","May","Jun")
vector3<-c("Jul","Aug","Sep")
vector4<-c("Oct","Nov","Dec")

for(i in 1:length(x))
{
  for(j in 1:length(vector1))
  {
  if(x[i]==vector1[j])
  {
    x[i]="Qtr1"
  }
  }
}
for(k in 1:length(x))
{
  for(l in 1:length(vector2))
  {
  if(x[k]==vector2[l])
  {
      x[k]="Qtr2"
  }
  }
}
for(m in 1:length(x))
{
  for(n in 1:length(vector3))
  {
    if(x[m]==vector3[n])
    {
      x[m]="Qtr3"
    }
  }
}
for(o in 1:length(x))
{
  for(p in 1:length(vector4))
  {
    if(x[o]==vector4[p])
    {
      x[o]="Qtr4"
    }
  }
}
SalesDataMod <- mutate(SalesData, Quarter=x) #Adding Quarter column

#Question7.
table9 <- SalesDataMod %>% select(Quarter, Sales2015, Sales2016)
table10 <- gather(table9,"Year","Sales",2:3)
comb_data4 <- table10 %>% group_by(Quarter,Year) %>% summarise(TotalSales=sum(Sales))
ggplot(comb_data4, aes(fill=Year, y=TotalSales, x=Quarter)) + geom_bar(position=position_dodge(), stat="identity")+scale_y_continuous(labels=comma)

#Question8.
FilterData <- SalesDataMod %>% select(Tier,Quarter,Sales2015)

Q1 <- FilterData %>% group_by(Quarter,Tier) %>% summarise(Sales=sum(Sales2015)) %>% filter(Quarter=="Qtr1")
Q1$Tier <- as.factor(Q1$Tier) 

Q2 <- FilterData %>% group_by(Quarter,Tier) %>% summarise(Sales=sum(Sales2015)) %>% filter(Quarter=="Qtr2")
Q2$Tier <- as.factor(Q2$Tier) 

Q3 <- FilterData %>% group_by(Quarter,Tier) %>% summarise(Sales=sum(Sales2015)) %>% filter(Quarter=="Qtr3")
Q3$Tier <- as.factor(Q3$Tier) 

Q4 <- FilterData %>% group_by(Quarter,Tier) %>% summarise(Sales=sum(Sales2015)) %>% filter(Quarter=="Qtr4")
Q4$Tier <- as.factor(Q4$Tier) 

#Combining Pie charts
par(mfrow=c(2,2))
pie(Q1$Sales, main = "Qtr1" ,labels = Q1$Tier)
pie(Q2$Sales, main = "Qtr2" ,labels = Q2$Tier)
pie(Q3$Sales, main = "Qtr3" ,labels = Q3$Tier)
pie(Q4$Sales, main = "Qtr4" ,labels = Q4$Tier)
 