#Residence index commands
#created by Easton White
#last edited 29-Apr-2014

#Script currently calculates the Residence index (RI) for Jupiter sharks (see Kessel et al 2014)
# RI= number of days on each station / number of days detected somewhere in array






det_shark=with(JupiterData,table(Transponder_Number,cut(PingTime, breaks="year")))





#################################################
#trying with different data set
#################################################

#call in data file with correct column classes
setClass("t_class3_",representation("character"))
setAs("character", "t_class3_", function(from)
as.POSIXct(strptime(from,format="%Y-%m-%d")))

coltype=c('character','numeric','t_class3_','numeric','character','character',rep('numeric',times=6))
main=read.table("presence_absence_data_sep07_sep11_noNA.txt",header=T,colClasses = coltype,sep=',')

#main=na.omit(main) #removes 21 days for each shark b/c of missing temp data (is this okay?)
#I think this is fine b/c it doesn't affect residency index. How does this affect the model?

#Make presence-ansence data
main$Pres_Abs[main$Pres_Abs>0]=1


zed=table(main$Shark_ID)

#calculate which sharks are active for at least 20 days in array (56 total active sharks at the end)
active_sharks=NULL
for (i in 1:nrow(zed)){
	if (sum(subset(main$Pres_Abs,main$Shark_ID==row.names(zed)[i]))>20){
		active_sharks=c(active_sharks,rownames(zed)[i])
	}
}


#########################################
##Call in original data file
#call in data file with correct column classes
setClass("t_class2_",representation("character"))
setAs("character", "t_class2_", function(from)
as.POSIXct(strptime(from,format="%m/%d/%Y %H:%M:%S")))

coltype=c('numeric','character','character','character','t_class2_','numeric','numeric')

#FINAL_Jupiter_Sharks_Jun13_twotag_as_one.txt is a post-filtered (it accounts for false detections) data set of all records and for all years in which the sharks with two tags is only labeled with one unique tag identifier 
JupiterData=read.table('FINAL_Jupiter_Sharks_Jun13_twotag_as_one.txt',header=T,colClasses = coltype,sep=',')


#Filter by date (Sep-12-2007 to Sep-12-2011)
JupiterData=subset(JupiterData,(as.Date(JupiterData$PingTime,format="%Y-%m-%d %H:%M:%S")>as.Date('2007-09-12 00:00:00',format="%Y-%m-%d %H:%M:%S")) & (as.Date(JupiterData$PingTime,format="%Y-%m-%d %H:%M:%S")<as.Date('2011-09-13 00:00:00',format="%Y-%m-%d %H:%M:%S")) )

#finds the days that sharks were present at each station
JupiterData=JupiterData[,c(2:4,6:8)]
JupiterData=unique(JupiterData)


#Create RESIDENCY_INDEX matrix for all stations and all active sharks
RESIDENCY_INDEX=matrix(0,nrow=72,ncol=56)
RESIDENCY_INDEX=as.data.frame(RESIDENCY_INDEX)
row.names(RESIDENCY_INDEX)=names(table(JupiterData$Station_Code))
names(RESIDENCY_INDEX)=active_sharks



for (j in 1:length(active_sharks)){ #loop through active sharks (n=56)
	SHARK=subset(JupiterData,JupiterData$Transponder_Number==active_sharks[j]) #subset data for each active shark (n=56)
	for (i in 1:length(names(table(JupiterData$Station_Code)))){   #loop through stations (n=72)
		RESIDENCY_INDEX[i,j]=nrow(subset(SHARK,SHARK$Station_Code==names(table(JupiterData$Station_Code))[i])) / length(table(SHARK$StudyYear)) #calculate # of days a SHARK was seen on station i / # of days in array
	}
}









aaa=with(JupiterData,table(Transponder_Number[Transponder_Number %in% active_sharks],Station_Code[Transponder_Number %in% active_sharks]))







