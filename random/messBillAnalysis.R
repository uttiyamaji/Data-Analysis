# Data Prep
mess=read.csv('C:/Users/Uttiya Maji/Documents/Mess_Bill.csv')
mess=mess[-1,]
mess=mess[,-1] # sl no is of no use
mess$Name.of.Students=as.character(mess$Name.of.Students)
mess=mess[,-6]
mess=mess[,-6]
mess$Total=as.character(mess$Total)
mess$Total=as.numeric(mess$Total)
mess$Roll.No.=as.character(mess$Roll.No.)

## Feature Engg

# Extract batch/year from Roll No
library(stringr)
batch=str_sub(mess$Roll.No.,2,2) # only 13 are not from 16,17,18, remove them.
mess$batch=batch
ext=batch%in%c('1','2','3','4','X')
mess=mess[!ext,]
mess$batch=as.factor(mess$batch)

library(dplyr)
meanByBatch=mess%>%
  group_by(batch)%>%
  summarise(avg=mean(Total))


library(ggplot2)

#barplot
ggplot(meanByBatch,aes(x=batch,y=avg))+
  geom_bar(stat='identity')

#scatterplot
ggplot(mess,aes(x=Roll.No.,y=Total,col=batch))+
  geom_point()

floor=str_sub(mess$Room.No,1,1) #floor
mess$floor=as.factor(floor)

meanByFloor=mess%>%
  group_by(floor)%>%
  summarise(avg=mean(Total))

ggplot(meanByFloor,aes(x=floor,y=avg))+
  geom_bar(stat='identity')

ggplot(mess,aes(x=Roll.No.,y=Total,col=Block))+
  geom_point()+
  geom_smooth(aes(col=floor))


ggplot(mess,aes(x=Total))+
  geom_histogram(aes(fill=floor))

plot(mess$Total~mess$floor)

#floor has no effect on Total

ggplot(mess,aes(x=Total))+
  geom_histogram(aes(fill=batch))
#  total for batch 16,17 are more dispersed, whereas for batch 18 its mostly 


getRoomMate=function(x){
  mess[mess$Room.No ==x,]
}









opted_out_RollNo=mess[mess$Total==0,'Roll.No.']

mess=mess[!mess$Roll.No.%in%opted_out_RollNo,]

ggplot(mess,aes(x=mess$Current.Month.Dues))+
  geom_histogram(aes(fill=batch),bins=30)


# what is measure of consumption pattern?
# relation of current with arrear
ggplot(mess,aes(x=mess$Current.Month.Dues,y=mess$ARREAR.OTHER.DUES,col=batch))+
  geom_point()+
  geom_smooth()

cor(mess$Current.Month.Dues,mess$ARREAR.OTHER.DUES)
arrear0=mess$Roll.No.[which(mess$ARREAR.OTHER.DUES==0)]
cor(mess$Current.Month.Dues[!mess$Roll.No.%in%arrear0],mess$ARREAR.OTHER.DUES[!mess$Roll.No.%in%arrear0])
x=mess$Current.Month.Dues[!mess$Roll.No.%in%arrear0]
y=mess$ARREAR.OTHER.DUES[!mess$Roll.No.%in%arrear0]
m2=mess[!mess$Roll.No.%in%arrear0,]
ggplot(m2,aes(x=m2$Current.Month.Dues,y=m2$ARREAR.OTHER.DUES))+
  geom_point()+
  geom_smooth()

## food prefernce of roommates are same or diff?
t=table(as.factor(mess$Room.No))
length(which(t==2))
Double_rooms=as.numeric(names(t[t==2]))

roommates=mess[mess$Room.No%in%Double_rooms,]

roommates$Room.No=as.factor(roommates$Room.No)
room_mates=roommates%>%
  group_by(Room.No)%>%
  summarise(diff=diff(Total))

ggplot(room_mates,aes(Room.No,y=diff))+geom_point()

# difference bw the room mates is random which says that rooms are distributed randomly
  