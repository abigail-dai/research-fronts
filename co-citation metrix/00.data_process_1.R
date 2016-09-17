rm(list=ls())  #清除R工作环境中的全部东西，

### trying to process those data in DB of papers

library(RODBC)
 
# Set project path
path <- "E:/AbigailDai/Graduate_first/TRY_PacificVis/code/co-citation metrix/"
setwd(path)
# Load functions
source("./01.function_use.R")

#build.tables.eachyear()#每年的数据建立一个表
#extract.corepapers#从每年表中抽取10%作为核心数据建成一个表

cr.generate() #选取某年的论文，摘取可用的cr并编码存入crpapers数据库中



#############+++++++++++++++++++++生成原始矩阵++++++++++++++++++++++++++++###########
dbcon <- odbcConnect("mysqlodbc", uid="root", pwd="088027")

year <- 2016 
sql <- paste("select p.id from corepapers p where p.publishyear = ",year)
corepapers <- sqlQuery(dbcon,sql)
sql <- paste("select p.id from papers p where p.publishyear = ",year)
initialpapers <- sqlQuery(dbcon,sql)
initialM <- matrix(data=0,nrow=length(initialpapers[[1]]),ncol=length(corepapers[[1]]),byrow=T,dimnames=list(initialpapers[[1]],corepapers[[1]]))

sql <- "select p.paperid, p.crid from crpapers p "
all.cr <- sqlQuery(dbcon,sql)
count = 1

while(count <= nrow(all.cr)){
	eachrow <- all.cr[count,]
if(count < 15){
print(eachrow)
count <- count + 1
#	rwname <- paste("'",eachrow$paperid,"'",sep = "")
#	clname <- paste("'",eachrow$crid,"'",sep = "")
#				
#	print(paste("rwname",rwname))
#	print(paste("clname",clname))
	if(clname == "0" || !(clname %in% corepapers[[1]])){
		print(0)
		next
	}
	initialM[as.character(rwname)][as.character(clname)] <- 1
	print(1)
	}
else{
break 
}
}




