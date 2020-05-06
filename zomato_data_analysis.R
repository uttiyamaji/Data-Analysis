library(dplyr)
library(stringr)
library(ggplot2)
library(magrittr)

## Data pre-processing 

zomato1=zomato_india[zomato_india$City%in%unique(zomato_india$City)[-32],]
zomato2=zomato_india[zomato_india$City%in%unique(zomato_india$City)[32],]

zomato2$Restaurant.Name=as.character(zomato2$Restaurant.Name)
zomato2$Address=as.character(zomato2$Address)
zomato2$Locality=as.character(zomato2$Locality)
zomato2$Locality.Verbose=as.character(zomato2$Locality.Verbose)
zomato2$Cuisines=as.character(zomato2$Cuisines)
zomato2a=zomato2[,c(2,5,6,10,11,13,14,17,18,20,21)]

## EDA

## an indiviual will loook for the price range initially before choosing a rest.
## how the price range depends on other factors such as cuisines and location??

ggplot(zomato2a,aes(x=zomato2a$Average.Cost.for.two))+
  geom_histogram(aes(fill=factor(zomato2a$Price.range)))

zomato3=zomato2a[zomato2a$Price.range!=4,] #removing high priced restaurents as outliers

##Locality

str(zomato3$Locality)
length(unique(zomato3$Locality)) #unique localities


# counting no of rests per locality
# how does the average cost change with Locality?
# how does the cost vary within a locality?/How dispersed is the cost per locality?

locality=zomato3%>%
  group_by(Locality)%>%
  summarise(count=length(Average.Cost.for.two),avg=mean(Average.Cost.for.two),disp=sd(Average.Cost.for.two))

ggplot(locality,aes(y=disp,x=avg))+
  geom_point(aes(size=count),alpha=.25)+
  geom_smooth()

ggplot(locality,aes(x=avg))+
  geom_bar(stat="identity") # what's the error in this code?



##{ Rough

length(str_subset(unique(zomato2a$Locality),'Mall'))
# for rests Locality is 'Aerocity', for other 31, Locality='<...>Aerocity', so convert  all of them as 'Aerocity'
length(str_subset(unique(zomato2a$Locality),'Aerocity'))
length(str_subset((zomato2a$Locality),'Aerocity'))
length(str_subset(zomato2a$Locality,'^Aerocity$'))
zomato2a[zomato2a$Locality=='Aerocity',1]
places_in_aerocity=(str_subset(unique(zomato2a$Locality),'Aerocity'))
zomato2a$Locality2=zomato2a$Locality
zomato2a$Locality2[zomato2a$Locality%in%rests_in_aerocity]='Aerocity'

t=table(factor(zomato2a$Locality2))
t2=as.numeric(t)
summary(t2)

n1=as.character(unlist(attr(t[which(t2<=3)],'dimnames')))
d1=zomato2a[zomato2a$Locality2%in%n1,1]
n2=as.character(unlist(attr(t[which(t2>3 & t2<=10)],'dimnames')))
d2=zomato2a[zomato2a$Locality2%in%n2,1]
n3=as.character(unlist(attr(t[which(t2>10 & t2<=36)],'dimnames')))
d3=zomato2a[zomato2a$Locality2%in%n3,1]
n4=as.character(unlist(attr(t[which(t2>36)],'dimnames')))
d4=zomato2a[zomato2a$Locality2%in%n4,1]

zomato2a$density=rep(1,5473)
zomato2a$density[zomato2a$Restaurant.Name%in%d1]=1
zomato2a$density[zomato2a$Restaurant.Name%in%d2]=2 
zomato2a$density[zomato2a$Restaurant.Name%in%d3]=3
zomato2a$density[zomato2a$Restaurant.Name%in%d4]=4
  
zomato2a$density=as.factor(zomato2a$density)  
 
# many rest has multiple outlets
# how to treat them?
# extracting those which have multiple outlets
names=table(as.factor(zomato2a$Restaurant.Name))
names_n= as.numeric(names)
length(names[which(names_n>1)])
names_rep_table=(names[which(names_n>1)]) # freq table 
#names of those rests
names_rep=names_rep_table%>%
 attr('dimnames')%>%
  unlist()%>%
  as.character()

#question: Do same rests at different outlets differ in cost?

names_with_2_outlets=names_rep[names_rep_table==2]

#price=zomato2a[zomato2a[,1]%in%names_with_2_outlets,"Average.Cost.for.two"] 
# is wrong way to extract price. Think why its wrong :)


price=matrix(ncol=2,nrow=269)
for(i in 1:269){
  price[i,]=zomato2a[zomato2a[,1]==names_with_2_outlets[i],"Average.Cost.for.two"]
}
price_diff=price[,1]-price[,2]
plot(price_diff,col='dodgerblue',pch=20)

# for the 269 rests which has two outlets, only 68 has price differences.

ggplot(zomato2a[zomato2a$Price.range!=4,],aes(x=Average.Cost.for.two))+
  geom_histogram(aes(fill=factor(Price.range)))


##}


## Cuisines

str(zomato2a$Cuisines)

cuisines2=str_split(zomato2a$Cuisines,',')  
cuisines2=lapply(cuisines2, str_trim)
#no of cuisines it serves
no_of_cuisines=unlist(lapply(lapply(cuisines2,unique),length)) 

ggplot(data.frame(zomato2a,no_of_cuisines),aes(x=no_of_cuisines,y=Average.Cost.for.two,))+
  geom_point(alpha=0.5)

multi_cuisine=ifelse(no_of_cuisines>1,1,0)

plot(zomato2a$Average.Cost.for.two~factor(multi_cuisine))

zomato3$no_of_cuisines=no_of_cuisines[zomato2a$Price.range!=4]
zomato3$multi_cuisine=ifelse(zomato3$no_of_cuisines>1,1,0)

ggplot(zomato3,aes(x=zomato3$no_of_cuisines,y=zomato3$Average.Cost.for.two,))+
  geom_point(alpha=0.5)

plot(zomato3$Average.Cost.for.two~factor(zomato3$multi_cuisine))

# freq distn of cuisines
table(factor(unlist(cuisines2)))
# American 116
# Asian  94
# Bakery 465
# Beverages 123
# Biriyani 99
# Burger 90
# Cafe 325
# Chinese 1638
# Continental 373
# Desserts 383
# Fast food 1304
# Ice cream 148
# Italian 376
# Mithai 282
# Mughlai 628
# North Indian 2425
# Pizza 198
# South indian 411
# Street food 411
# Thai 112
cuisines3=cuisines2
library(rebus)
cuisines3=lapply(cuisines3,str_replace,pattern='Sushi',replacement='Japanese')
cuisines3=lapply(cuisines3,str_replace,pattern='Afghani',replacement='Asian')
cuisines3=lapply(cuisines3,str_replace,pattern=or('African','American','British','European','French','Mediterranean','Mexican','Middle Eastern','Portuguese','South American','Spanish'),replacement='NonAsian')
cuisines3=lapply(cuisines3,str_replace,pattern=or('Andhra','Assamese','Awadhi','Bengali','Bihari','Goan','Gujarati','Hyderabadi','Indian','Kashmiri','Kerala','Lucknowi','Maharashtrian','Modern Indian','Naga','North Eastern','Oriya','Rajasthani'),replacement='Indian')
cuisines3=lapply(cuisines3,str_replace,pattern=or('Arabian','Armenian','Burmese','Chettinad','Cuisine Varies','Deli','Drinks Only','Finger Food','Healthy Food','Indonesian','Iranian','Japanese','Juices','Korean','Lebanese','Malaysian','Mangalorean','Moroccan','Nepalese','Pakistani','Parsi','Raw Meats','Salad','Sandwich','Seafood','Sri Lankan','Tea','Tex-Mex','Tibetan','Turkish','Vietnamese'),replacement='others')

cuisines4=lapply(cuisines3,unique)

tf=lapply(lapply(cuisines2,str_detect,pattern='North Indian'),sum)


cuisine_extractor=function(x){
  unlist(lapply(lapply(cuisines4,str_detect,pattern=START %R% x %R% END),sum))
}


North_Indian=cuisine_extractor('North Indian')
Asian=cuisine_extractor('Asian')
Bakery=cuisine_extractor('Bakery')
Beverages=cuisine_extractor('Beverages')
Biryani=cuisine_extractor('Biryani')
Burger=cuisine_extractor('Burger')
Cafe=cuisine_extractor('Cafe')
Chinese=cuisine_extractor('Chinese')
Continental=cuisine_extractor('Continental')
Desserts=cuisine_extractor('Desserts')
Fast_food=cuisine_extractor('Fast Food')
IceCream=cuisine_extractor('Ice Cream')
Indian=cuisine_extractor('Indian')
Italian=cuisine_extractor('Italian')
Mithai=cuisine_extractor('Mithai')
Mughlai=cuisine_extractor('Mughlai')
NonAsian=cuisine_extractor('NonAsian')
others=cuisine_extractor('others')
Pizza=cuisine_extractor('Pizza')
SouthIndian=cuisine_extractor('South Indian')
StreetFood=cuisine_extractor('Street Food')
Thai=cuisine_extractor('Thai')

cuisines_df=data.frame(Asian,Bakery,Beverages,Biryani,Burger,Cafe,Chinese,Continental,
                       Desserts,Fast_food,IceCream,Indian,Italian,Mithai,
                       Mughlai,NonAsian,North_Indian,others,Pizza, SouthIndian,
                       StreetFood,Thai)


# understand the price structure wrt cuisines
# 1. 5 pt summary
# 2. histogram
# 3. ...
CuisinesNames=names(table(factor(unlist(cuisines4))))
AvgByCuisine=function(x)
  mean(zomato3$Average.Cost.for.two[which(cuisines_df[,x]==1)],na.rm=T)

avg=numeric(0)

for(x in 1:22)avg[x]=AvgByCuisine(x) # avg price for each cuisine in zomato3

summary=list()

PriceSummaryByCuisine=
summary[[1]]=summary(zomato3$Average.Cost.for.two[which(cuisines_df[,x]==1)],na.rm=T)


## Ratings









