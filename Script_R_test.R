if(!require(RODBC)){
  install.packages("RODBC")
  library(RODBC)
}
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(mice)){
  install.packages("mice")
  library(mice)
}

udm <- odbcDriverConnect('driver={SQL Server Native Client 11.0};
                         server=dkvbrusrvm139;
                         database=UDM;
                         uid="DKV_ERGOSERVICE\\elmallah";
                         trusted_connection=yes;')


Fees_16_17 <- sqlQuery(udm," select eL.ApprovalCode, eL.[Authorisation_NBR], 
                       cast(sum(SupplementalAmount) as float) as Sum_SupplementalAmount, 
                       cast(sum(MutualityAmount) as float) as Sum_MutualityAmount, 
                       cast(sum(CostSharingAmount) as float) as Sum_CostSharingAmount
                       from   [eBillingLine] eL
                       where eL.ApprovalCode in ('0108', '0332', '0409','0152', '0158', '0403')
                       and eL.Policy_NBR not like '4%'
                       and  eL.ReceivedDate>= '2016-07-01' and eL.ReceivedDate < '2017-07-01'
                       and eL.TableSource = 'Services'
                       and eL.Procedure_NBR <> '0'
                       and eL.Procedure_NBR in (
                       select distinct Procedure_NBR
                       FROM  [Procedure]
                       where Article in 
                       ('11','12','13','14a','14b','14c','14d','14e','14f','14g','14h','14i','14j','14k','14l','14m','15','16','17','17bis','17quater','17ter','18','2','20','21','22','25','26','3','34','5','7','8','9','24','24bis','32','33','33bis','36')
                       )
                       group by eL.ApprovalCode, eL.[Authorisation_NBR]")


Fees_17_18 <- sqlQuery(udm," select eL.ApprovalCode,eL.[Authorisation_NBR],
                       cast(sum(SupplementalAmount) as float) as Sum_SupplementalAmount, 
                       cast(sum(MutualityAmount) as float) as Sum_MutualityAmount, 
                       cast(sum(CostSharingAmount) as float) as Sum_CostSharingAmount
                       from   [eBillingLine] eL
                       where eL.ApprovalCode in ('0108', '0332', '0409','0152', '0158', '0403')
                       and eL.Policy_NBR not like '4%'
                       and  eL.ReceivedDate>= '2017-07-01' and eL.ReceivedDate < '2018-07-01'
                       and eL.TableSource = 'Services'
                       and eL.Procedure_NBR <> '0'
                       and eL.Procedure_NBR in (
                       select distinct Procedure_NBR
                       FROM  [Procedure]
                       where Article in 
                       ('11','12','13','14a','14b','14c','14d','14e','14f','14g','14h','14i','14j','14k','14l','14m','15','16','17','17bis','17quater','17ter','18','2','20','21','22','25','26','3','34','5','7','8','9','24','24bis','32','33','33bis','36')
                       )
                       group by eL.ApprovalCode, eL.[Authorisation_NBR]")

Room_16_17 <- sqlQuery(udm," select eL.ApprovalCode,eL.[Authorisation_NBR], 
                       sum(eL.SupplementalAmount) as Room_Sum_SupplAmount,
                       sum(eL.RoomDurationDays) as Room_Sum_Nb_Days
                       from [eBillingLine] eL 
                       where eL.ApprovalCode in ('0108', '0332', '0409','0152', '0158', '0403')
                       and eL.Policy_NBR not like '4%'
                       and eL.ReceivedDate>= '2016-07-01' and eL.ReceivedDate < '2017-07-01'
                       and (eL.Pseudocode=0761633 or eL.Pseudocode=0761644)
                       and eL.TableSource = 'NursingDays'
                       and eL.SupplementalAmount <> 0
                       group by eL.ApprovalCode, eL.[Authorisation_NBR]")

Room_17_18 <- sqlQuery(udm," select eL.ApprovalCode,eL.[Authorisation_NBR], 
                       sum(eL.SupplementalAmount) as Room_Sum_SupplAmount,
                       sum(eL.RoomDurationDays) as Room_Sum_Nb_Days
                       from [eBillingLine] eL 
                       where eL.ApprovalCode in ('0108', '0332', '0409','0152', '0158', '0403')
                       and eL.Policy_NBR not like '4%'
                       and eL.ReceivedDate>= '2017-07-01' and eL.ReceivedDate < '2018-07-01'
                       and (eL.Pseudocode=0761633 or eL.Pseudocode=0761644)
                       and eL.TableSource = 'NursingDays'
                       and eL.SupplementalAmount <> 0
                       group by eL.ApprovalCode, eL.[Authorisation_NBR]")


#Select Provider
Provider_Selected = c(152, 158)

Fees_16_17_Provider = Fees_16_17[Fees_16_17$ApprovalCode %in% Provider_Selected, ]
Fees_17_18_Provider = Fees_17_18[Fees_17_18$ApprovalCode %in% Provider_Selected, ]
Room_16_17_Provider = Room_16_17[Room_16_17$ApprovalCode %in% Provider_Selected, ]
Room_17_18_Provider = Room_17_18[Room_17_18$ApprovalCode %in% Provider_Selected, ]



#Compute KPI for both period
############################
#Fees_KPI
sum(Fees_16_17_Provider$Sum_SupplementalAmount)/(sum(Fees_16_17_Provider$Sum_MutualityAmount) + sum(Fees_16_17_Provider$Sum_CostSharingAmount))*100
sum(Fees_17_18_Provider$Sum_SupplementalAmount)/(sum(Fees_17_18_Provider$Sum_MutualityAmount) + sum(Fees_17_18_Provider$Sum_CostSharingAmount))*100
#Room_KPI
sum(Room_16_17_Provider$Room_Sum_SupplAmount)/sum(Room_16_17_Provider$Room_Sum_Nb_Days)
sum(Room_17_18_Provider$Room_Sum_SupplAmount)/sum(Room_17_18_Provider$Room_Sum_Nb_Days)



#Add KPi on each lines
######################
#Fees
Fees_16_17_Provider$Fees_KPI <- (Fees_16_17_Provider$Sum_SupplementalAmount/(Fees_16_17_Provider$Sum_MutualityAmount + Fees_16_17_Provider$Sum_CostSharingAmount))*100
Fees_17_18_Provider$Fees_KPI <- (Fees_17_18_Provider$Sum_SupplementalAmount/(Fees_17_18_Provider$Sum_MutualityAmount + Fees_17_18_Provider$Sum_CostSharingAmount))*100
#Room
Room_16_17_Provider$Room_KPI <- Room_16_17_Provider$Room_Sum_SupplAmount/Room_16_17_Provider$Room_Sum_Nb_Days
Room_17_18_Provider$Room_KPI <- Room_17_18_Provider$Room_Sum_SupplAmount/Room_17_18_Provider$Room_Sum_Nb_Days

#remove Inf values (divide by zeros)
Fees_16_17_Provider <- Fees_16_17_Provider[is.finite(rowSums(Fees_16_17_Provider)),]
Fees_17_18_Provider <- Fees_17_18_Provider[is.finite(rowSums(Fees_17_18_Provider)),]
Fees_16_17_Provider <- Fees_16_17_Provider[Fees_16_17_Provider$Fees_KPI<500,]
Fees_17_18_Provider <- Fees_17_18_Provider[Fees_17_18_Provider$Fees_KPI<500,]


#histogram
#All splits
par(mfrow=c(2,2))
hist(Fees_16_17_Provider$Fees_KPI, breaks=seq(0,1000,5), col=rgb(0.2,0.8,0.5,0.5), xlab="Fees KPI", ylab="Frequency (Nbr of Admission)", xlim=c(0,300), ylim = c(0,1000), main = 'Supplementary Fees - Period July16-June17')
hist(Room_16_17_Provider$Room_KPI, breaks=seq(0,1000,5), col=rgb(0.2,0.8,0.5,0.5), xlab="Fees KPI", ylab="Frequency (Nbr of Admission)", xlim=c(0,300), ylim = c(0,1000), main = 'Room Supplement - Period July16-June17')
hist(Fees_17_18_Provider$Fees_KPI, breaks=seq(0,1000,5), col=rgb(0.2,0.8,0.5,0.5), xlab="Fees KPI", ylab="Frequency (Nbr of Admission)", xlim=c(0,300), ylim = c(0,1000), main = 'Supplementary Fees - Period July17-June18')
hist(Room_17_18_Provider$Room_KPI, breaks=seq(0,1000,5), col=rgb(0.2,0.8,0.5,0.5), xlab="Fees KPI", ylab="Frequency (Nbr of Admission)", xlim=c(0,300), ylim = c(0,1000), main = 'Room Supplement - Period July17-June18')

#Overlapping
par(mfrow=c(2,1))
hist(Fees_16_17_Provider$Fees_KPI, breaks=seq(0,1000,5), col=rgb(1,0,0,0.5), xlab="Fees KPI", ylab="Frequency (Nbr of Admission)", xlim=c(0,300), ylim = c(0,2000), main = 'Supplementary Fees')
hist(Fees_17_18_Provider$Fees_KPI, breaks=seq(0,1000,5), col=rgb(0,0,1,0.5), add=T)
legend("topright", c("Fees_16_17", "Fees_17_18"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), cex = 0.75, bty = "n")

hist(Room_16_17_Provider$Room_KPI, breaks=seq(0,1000,5), col=rgb(1,0,0,0.5), xlab="Fees KPI", ylab="Frequency (Nbr of Admission)", xlim=c(0,300), ylim = c(0,2000), main = 'Room Supplement')
hist(Room_17_18_Provider$Room_KPI, breaks=seq(0,1000,5), col=rgb(0,0,1,0.5), add=T)
legend("topright", c("Room_16_17", "Room_17_18"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), cex = 0.75, bty = "n")

##########
#Analysis#
##########

#proportion of double room in both period
length(Fees_16_17_Provider$Fees_KPI[Fees_16_17_Provider$Fees_KPI==0])/nrow(Fees_16_17_Provider)      
length(Fees_17_18_Provider$Fees_KPI[Fees_17_18_Provider$Fees_KPI==0])/nrow(Fees_17_18_Provider)

#Compute comparison between each histogram bucket

#Fees_16_17
hist_Fees_16_17 <- hist(Fees_16_17_Provider$Fees_KPI, breaks=seq(0,1000,5), col=rgb(1,0,0,0.5), xlab="Fees KPI", ylab="Frequency (Nbr of Admission)", xlim=c(0,300), ylim = c(0,1500), main = 'Supplementary Fees')
rm(hist_Fees_16_17_df)
hist_Fees_16_17_df = data.frame(hist_Fees_16_17$counts)
hist_Fees_16_17_df$Bin1 = as.numeric(unlist(data.frame(hist_Fees_16_17$breaks[-length(hist_Fees_16_17$breaks)])))
hist_Fees_16_17_df$Bin2 = as.numeric(unlist(data.frame(hist_Fees_16_17$breaks[-1])))
hist_Fees_16_17_df$Bin1_new <- sprintf("%03d", hist_Fees_16_17_df$Bin1)
hist_Fees_16_17_df$Bin2_new <- sprintf("%03d", hist_Fees_16_17_df$Bin2)
hist_Fees_16_17_df$Bin = paste(hist_Fees_16_17_df$Bin1_new, hist_Fees_16_17_df$Bin2_new, sep="-")
hist_Fees_16_17_df <- hist_Fees_16_17_df[c(6,1)]
colnames(hist_Fees_16_17_df) = c("Bins", "Frequency")

#Fees_17_18
hist_Fees_17_18 <- hist(Fees_17_18_Provider$Fees_KPI, breaks=seq(0,1000,5), col=rgb(1,0,0,0.5), xlab="Fees KPI", ylab="Frequency (Nbr of Admission)", xlim=c(0,300), ylim = c(0,1500), main = 'Supplementary Fees')
rm(hist_Fees_17_18_df)
hist_Fees_17_18_df = data.frame(hist_Fees_17_18$counts)
hist_Fees_17_18_df$Bin1 = as.numeric(unlist(data.frame(hist_Fees_17_18$breaks[-length(hist_Fees_17_18$breaks)])))
hist_Fees_17_18_df$Bin2 = as.numeric(unlist(data.frame(hist_Fees_17_18$breaks[-1])))
hist_Fees_17_18_df$Bin1_new <- sprintf("%03d", hist_Fees_17_18_df$Bin1)
hist_Fees_17_18_df$Bin2_new <- sprintf("%03d", hist_Fees_17_18_df$Bin2)

hist_Fees_17_18_df$Bin = paste(hist_Fees_17_18_df$Bin1_new, hist_Fees_17_18_df$Bin2_new, sep="-")

hist_Fees_17_18_df <- hist_Fees_17_18_df[c(6,1)]
colnames(hist_Fees_17_18_df) = c("Bins", "Frequency")



