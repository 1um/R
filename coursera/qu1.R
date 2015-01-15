acs = read.csv('http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv')
sum( acs$VAL==24,na.rm = TRUE)
#53

require(xlsx)
file = tempfile()
download.file('http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx',file)
dat = read.xlsx(file,sheetIndex = 1,rowIndex = 18:23,colIndex = 7:15)
sum(dat$Zip*dat$Ext,na.rm=T)
# 0 <- bad

library(XML)
doc <- xmlTreeParse('http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml',useInternalNodes = TRUE)
root <- xmlRoot(doc)
sum(xpathSApply(root,'//zipcode',xmlValue)=="21231")
#127

library(data.table)
file = tempfile()
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv',file,method = 'curl')
DT = fread(file)
system.time(DT[,mean(pwgtp15),by=SEX]) # winner?
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
system.time({rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]})
system.time({mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)})
system.time(mean(DT$pwgtp15,by=DT$SEX))
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
