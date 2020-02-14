rm(list=ls())
hd <- read.csv("Hunter Douglas Quality Data.csv", na.strings = c("NULL",""))
df <- as.data.frame(hd)
sil <- df[df$PRODUCT_CATEGORY == '02 Silhouette/Nantucket' & df$ORIGINAL_PLANT == 'G',]
sil <- sil[sil$ORDER_REASON_ID!='PAR',] #get rid of PAR
sil <- sil[!is.na(sil$ORIGINAL_ORDER),] #get rid of NULL original orders
sil <- sil[!is.na(sil$FABRIC_ID),] #get rid of NULL fabric IDs
sil <- sil[sil$ORDER_REASON_ID=="STD" | sil$ORDER_REASON_ID=="REM" | sil$ORDER_REASON_ID=="REP" |sil$ORDER_REASON_ID=="CON" ,]
sil <- sil[!is.na(sil$OPERATING_SYSTEM_ID),]
sil <- sil[!is.na(sil$SOLD_TO_ID),]
sil <- sil[!(sil$ORIGINAL_ORDER== 230066675421 | sil$SALES_ORDER== 230066675421),]
sil <- sil[!sil$ORIGINAL_ORDER== 230054119481,]
sil <- sil[!sil$ORIGINAL_ORDER== 230052926688,]
sil <- sil[!sil$ORIGINAL_ORDER== 230053010643,]
sil <- sil[!sil$ORIGINAL_ORDER== 230066612777,]
sil$ORDER_REASON_ID <- factor(sil$ORDER_REASON_ID)
sil$RESPONSIBILITY_CODE_ID <- as.character(sil$RESPONSIBILITY_CODE_ID)
sil$RESPONSIBILITY_CODE_ID[is.na(sil$RESPONSIBILITY_CODE_ID)] <- "NULL"
sil$REASON_CODE_ID <- as.character(sil$REASON_CODE_ID)
sil$REASON_CODE_ID[is.na(sil$REASON_CODE_ID)] <- "NULL"
sil$REASON_CODE <- as.character(sil$REASON_CODE)
sil$REASON_CODE[is.na(sil$REASON_CODE)] <- "NULL"
sil$SLAT_SIZE <- NULL

sil$ORIGINAL_ORDER1 <- paste0(sil$ORIGINAL_ORDER,sil$ORIGINAL_ORDER_LINE)
#combine sales order and sales order line
sil$SALES_ORDER1 <- paste0(sil$SALES_ORDER,sil$SALES_ORDER_LINE)
sil <- sil[(sil$ORIGINAL_ORDER %in% sil$SALES_ORDER),] # drop rep/rem/crr orders with no og order
sil <- sil[!sil$NET_SALES_UNITS<0,]
dupes <- sil[duplicated(sil$SALES_ORDER1),]
good_stuff <- sil[!(sil$SALES_ORDER1 %in% dupes$SALES_ORDER1 & sil$NET_SALES_UNITS == 0),]
sil <- good_stuff[!duplicated(good_stuff$SALES_ORDER1),]
sil.cp <- sil
sil1<-merge(sil,sil.cp,by.x='ORIGINAL_ORDER1',by.y='SALES_ORDER1')
sil[(sil$ORDER_REASON_ID=="REP" | sil$ORDER_REASON_ID=="REM")&!(sil$ORIGINAL_ORDER1 %in% sil$SALES_ORDER1),]
# ^ checks for orders marked REP or REM that are not in our merged df (i.e. dont have OG order # associated)
sil$ORIGINAL_ORDER <- as.factor(sil$ORIGINAL_ORDER)
nrow(sil[sil$ORDER_REASON_ID=="REP" | sil$ORDER_REASON_ID=="REM",])

sil1$lag <- as.Date(as.character(sil1$SO_CREATED_DATE.x), "%Y%m%d") - as.Date(as.character(sil1$SO_CREATED_DATE.y), "%Y%m%d")
lagdfToInsert <- data.frame(sil1$ORIGINAL_ORDER1,sil1$SALES_ORDER1,sil1$lag)
lagdfToInsert$sil1.ORIGINAL_ORDER1 <- as.character(lagdfToInsert$sil1.ORIGINAL_ORDER1)
sil$lag <- NULL

silFinal <- merge(sil, lagdfToInsert, by.x = 'SALES_ORDER1', by.y='sil1.SALES_ORDER1', all.x = TRUE)
names(silFinal)[names(silFinal) == 'sil1.lag'] <- 'lagDays'
silFinal$lagDays[is.na(silFinal$lagDays)] <- 0
silFinal$failure <- ifelse(silFinal$ORDER_REASON_ID=="REP" | silFinal$ORDER_REASON_ID=="REM",1,0)
sum(silFinal$failure)
silFinal[,is.factor(silFinal)] <- factor(silFinal[,is.factor(silFinal)])
unique(silFinal$PRODUCT_CATEGORY)

library('plyr')
#reduce cfirst top 50 colors
COLOR_ID_COUNT<- count(silFinal$COLOR_ID)
color_sort<-COLOR_ID_COUNT[order(COLOR_ID_COUNT$freq, decreasing = T),]
top50<- head(color_sort, 50)
top50_colors <- silFinal
top50_colors$COLOR_ID_50<- ifelse(top50_colors$COLOR_ID %in% top50$x, 1, 0)
silFinal <- top50_colors[top50_colors$COLOR_ID_50 == 1,]

#reduce cfirst top 50 fabricIDs
FABRIC_ID_COUNT<- count(silFinal$FABRIC_ID)
FABRIC_ID_sort<-FABRIC_ID_COUNT[order(FABRIC_ID_COUNT$freq, decreasing = T),]
top50<- head(FABRIC_ID_sort, 50)
top50_FABRIC_ID <- silFinal
top50_FABRIC_ID$FABRIC_ID_50<- ifelse(top50_FABRIC_ID$FABRIC_ID %in% top50$x, 1, 0)
silFinal <- top50_FABRIC_ID[top50_FABRIC_ID$FABRIC_ID_50 == 1,]

#reduce cfirst top 50 operating system id
ID_COUNT<- count(silFinal$OPERATING_SYSTEM_ID)
ID_sort<-ID_COUNT[order(ID_COUNT$freq, decreasing = T),]
top50<- head(ID_sort, 50)
top50_ID <- silFinal
top50_ID$ID_50<- ifelse(top50_ID$OPERATING_SYSTEM_ID %in% top50$x, 1, 0)
silFinal <- top50_ID[top50_ID$ID_50 == 1,]

#reduce cfirst top 50 original material id
ID_COUNT<- count(silFinal$ORIGINAL_MATERIAL_ID)
ID_sort<-ID_COUNT[order(ID_COUNT$freq, decreasing = T),]
top50<- head(ID_sort, 50)
top50_ID <- silFinal
top50_ID$ID_50<- ifelse(top50_ID$ORIGINAL_MATERIAL_ID %in% top50$x, 1, 0)
silFinal <- top50_ID[top50_ID$ID_50 == 1,]

#reduce cfirst top 50 operating sys opt id
ID_COUNT<- count(silFinal$OPERATING_SYS_OPT_ID)
ID_sort<-ID_COUNT[order(ID_COUNT$freq, decreasing = T),]
top50<- head(ID_sort, 50)
top50_ID <- silFinal
top50_ID$ID_50<- ifelse(top50_ID$OPERATING_SYS_OPT_ID %in% top50$x, 1, 0)
silFinal <- top50_ID[top50_ID$ID_50 == 1,]

silFinal <- silFinal[!((silFinal$SALES_ORDER1 %in% silFinal$sil1.ORIGINAL_ORDER1) & (silFinal$ORDER_REASON_ID == "STD" | silFinal$ORDER_REASON_ID == "CON")),]

silFinal$sil1.ORIGINAL_ORDER1 <- NULL
silFinal$PRODUCT_CATEGORY <- NULL
silFinal$ORIGINAL_PLANT <- NULL
silFinal$COLOR_ID_50 <- NULL
silFinal$FABRIC_ID_50 <- NULL
silFinal$ID_50 <- NULL
names(silFinal)[names(silFinal) == 'ORIGINAL_ORDER1'] <- 'ORIGINAL_ORDER_COMBINED'
names(silFinal)[names(silFinal) == 'SALES_ORDER1'] <- 'SALES_ORDER_COMBINED'
save(silFinal, file="silFinal.RData")
