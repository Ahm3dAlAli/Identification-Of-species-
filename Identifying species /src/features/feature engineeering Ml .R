install.packages("data.table")
library(data.table)
library(readr)


ML_spec<- data.table(fread("project/volume/data/raw/data.csv"))

#############################################################################################
#Exploring data#
################

#Strucutre
##########
str(ML_spec)

#Summary
########
summary(ML_spec)

#Unique Values
##############
lapply(ML_spec,unique)

#create binary for each entry of a cloumn 
########################################

#locus_1
ML_spec$locus1_125<-ifelse(ML_spec$locus_1==125 ,yes=1,no=0)
ML_spec$locus1_123=ifelse(ML_spec$locus_1==123 ,yes=1,no=0) 
ML_spec$locus1_127=ifelse(ML_spec$locus_1==127 ,yes=1,no=0) 
ML_spec$locus1_129=ifelse(ML_spec$locus_1==129 ,yes=1,no=0) 
ML_spec$locus1_119=ifelse(ML_spec$locus_1==119 ,yes=1,no=0) 
ML_spec$locus1_121=ifelse(ML_spec$locus_1==121 ,yes=1,no=0)

#locus 2
ML_spec$locus2_239<-ifelse(ML_spec$locus_2==239,yes=1,no=0) 
ML_spec$locus2_236<-ifelse(ML_spec$locus_2==236,yes=1,no=0) 
ML_spec$locus2_242<-ifelse(ML_spec$locus_2==242,yes=1,no=0) 
ML_spec$locus2_233<-ifelse(ML_spec$locus_2==233 ,yes=1,no=0) 
ML_spec$locus2_245<-ifelse(ML_spec$locus_2==245,yes=1,no=0)

#locus_3
ML_spec$locus3_109<-ifelse(ML_spec$locus_3==109,yes=1,no=0) 
ML_spec$locus3_101<-ifelse(ML_spec$locus_3==101,yes=1,no=0)  
ML_spec$locus3_105<-ifelse(ML_spec$locus_3==105,yes=1,no=0)  
ML_spec$locus3_113<-ifelse(ML_spec$locus_3==113,yes=1,no=0) 

#locus_4
ML_spec$locus4_372<-ifelse(ML_spec$locus_4==372,yes=1,no=0) 
ML_spec$locus4_366<-ifelse(ML_spec$locus_4==366,yes=1,no=0) 
ML_spec$locus4_370<-ifelse(ML_spec$locus_4==370,yes=1,no=0) 
ML_spec$locus4_364<-ifelse(ML_spec$locus_4==364,yes=1,no=0) 
ML_spec$locus4_362<-ifelse(ML_spec$locus_4==362,yes=1,no=0) 
ML_spec$locus4_368<-ifelse(ML_spec$locus_4==368,yes=1,no=0) 
ML_spec$locus4_361<-ifelse(ML_spec$locus_4==361,yes=1,no=0) 
#locus_5
ML_spec$locus5_424<-ifelse(ML_spec$locus_4==424,yes=1,no=0) 
ML_spec$locus5_426<-ifelse(ML_spec$locus_5==426,yes=1,no=0)  
ML_spec$locus5_428<-ifelse(ML_spec$locus_5==428,yes=1,no=0)  
ML_spec$locus5_422<-ifelse(ML_spec$locus_5==422,yes=1,no=0) 
#locus_6
ML_spec$locus6_101<-ifelse(ML_spec$locus_6==101,yes=1,no=0) 
ML_spec$locus6_110<-ifelse(ML_spec$locus_6==110,yes=1,no=0) 
ML_spec$locus6_104<-ifelse(ML_spec$locus_6==104,yes=1,no=0) 
ML_spec$locus6_107<-ifelse(ML_spec$locus_6==107,yes=1,no=0) 
ML_spec$locus6_98<-ifelse(ML_spec$locus_6==98,yes=1,no=0) 
#locus_7
ML_spec$locus7_215<-ifelse(ML_spec$locus_7==215,yes=1,no=0) 
ML_spec$locus7_210<-ifelse(ML_spec$locus_7==210,yes=1,no=0) 
#locus_8
ML_spec$locus8_164<-ifelse(ML_spec$locus_7==98,yes=1,no=0)  
ML_spec$locus8_172<-ifelse(ML_spec$locus_7==98,yes=1,no=0)  
ML_spec$locus8_176<-ifelse(ML_spec$locus_7==98,yes=1,no=0)  
ML_spec$locus8_159<-ifelse(ML_spec$locus_7==98,yes=1,no=0)  
ML_spec$locus8_168<-ifelse(ML_spec$locus_7==98,yes=1,no=0) 
#locus_9
ML_spec$locus9_100<-ifelse(ML_spec$locus_7==100,yes=1,no=0)  
ML_spec$locus9_109<-ifelse(ML_spec$locus_7==109,yes=1,no=0)  
ML_spec$locus9_112<-ifelse(ML_spec$locus_7==112,yes=1,no=0)  
ML_spec$locus9_106<-ifelse(ML_spec$locus_7==106,yes=1,no=0)   
ML_spec$locus9_97<-ifelse(ML_spec$locus_7==97,yes=1,no=0)  
ML_spec$locus8_103<-ifelse(ML_spec$locus_7==103,yes=1,no=0) 
#locus_10
ML_spec$locus10_82<-ifelse(ML_spec$locus_7==82,yes=1,no=0) 
ML_spec$locus10_86<-ifelse(ML_spec$locus_7==86,yes=1,no=0)  
ML_spec$locus10_78<-ifelse(ML_spec$locus_7==78,yes=1,no=0)  
ML_spec$locus10_80<-ifelse(ML_spec$locus_7==80,yes=1,no=0)  
ML_spec$locus10_84<-ifelse(ML_spec$locus_7==84,yes=1,no=0) 
#locus_11
ML_spec$locus11_125<-ifelse(ML_spec$locus_7==125,yes=1,no=0) 
ML_spec$locus11_123<-ifelse(ML_spec$locus_7==123,yes=1,no=0) 
ML_spec$locus11_127<-ifelse(ML_spec$locus_7==127,yes=1,no=0)  
ML_spec$locus11_129<-ifelse(ML_spec$locus_7==129,yes=1,no=0)  
ML_spec$locus11_121<-ifelse(ML_spec$locus_7==121,yes=1,no=0)  
ML_spec$locus11_119<-ifelse(ML_spec$locus_7==119,yes=1,no=0) 
#locus_12
ML_spec$locus12_239<-ifelse(ML_spec$locus_7==239,yes=1,no=0) 
ML_spec$locus12_233<-ifelse(ML_spec$locus_7==233,yes=1,no=0) 
ML_spec$locus12_245<-ifelse(ML_spec$locus_7==245,yes=1,no=0)  
ML_spec$locus12_236<-ifelse(ML_spec$locus_7==236,yes=1,no=0)  
ML_spec$locus12_242<-ifelse(ML_spec$locus_7==242,yes=1,no=0) 
#locus_13
ML_spec$locus13_209<-ifelse(ML_spec$locus_7==209,yes=1,no=0) 
ML_spec$locus13_205<-ifelse(ML_spec$locus_7==205,yes=1,no=0)  
ML_spec$locus13_213<-ifelse(ML_spec$locus_7==213,yes=1,no=0) 
ML_spec$locus13_201<-ifelse(ML_spec$locus_7==201,yes=1,no=0) 
#locus_14
ML_spec$locus14_261<-ifelse(ML_spec$locus_7==261,yes=1,no=0) 
ML_spec$locus14_268<-ifelse(ML_spec$locus_7==268,yes=1,no=0)  
ML_spec$locus14_272<-ifelse(ML_spec$locus_7==272,yes=1,no=0)  
ML_spec$locus14_262<-ifelse(ML_spec$locus_7==262,yes=1,no=0) 
ML_spec$locus14_266<-ifelse(ML_spec$locus_7==266,yes=1,no=0)  
ML_spec$locus14_264<-ifelse(ML_spec$locus_7==264,yes=1,no=0)  
ML_spec$locus14_270<-ifelse(ML_spec$locus_7==270,yes=1,no=0) 
#locus_15
ML_spec$locus15_124<-ifelse(ML_spec$locus_7==124,yes=1,no=0)  
ML_spec$locus15_122<-ifelse(ML_spec$locus_7==122,yes=1,no=0) 
ML_spec[,2:16]<-NULL
#########################################################################################################
##############
#Process file#
##############

write_csv(ML_spec,"project/volume/data/processed/cleaned_data.csv")


