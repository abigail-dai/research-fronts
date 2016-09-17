### trying to process those data in DB of papers

library(RODBC)
 
# Set project path
path <- "E:/AbigailDai/Graduate_first/TRY_PacificVis/code/co-citation metrix/"
setwd(path)

#############+++++++++++++++++++++++++++++将每年的论文按照被引次数排序，单独列一个表2016papers+++++++++++++++++++++++++++++++++++++###
build.tables.eachyear <- function(){           

	dbcon <- odbcConnect("mysqlodbc", uid="root", pwd="088027")

	year <- 2016 
	while(year >= 2006){
		sql <- paste("select p.ID,p.AuthorFullName,p.Title,p.Keywords,p.KeywordsPlus,p.Abstract,p.CitedReference,p.TimesCited,
		 	p.PublishYear from papers p where PublishYear =", year, "order by TimesCited desc")
		somePapers <- sqlQuery(dbcon,sql)
		name <- paste(year,"papers",sep = "")
		sqlSave(dbcon ,somePapers ,name ,append = TRUE)
	 	year <- year - 1
	}

	#close the connection of ODBC;
	odbcClose(dbcon)
}


#############+++++++++++++++++++++抽取每年前百分之10的文章作为核心文献，建表corepapers:10%只是作为一个样例，可以是15%或者100%+++++++++++++++###阈值需确定,数据准确率需确定
extract.corepapers <- function(){
	dbcon <- odbcConnect("mysqlodbc", uid="root", pwd="088027")
	
	year <- 2016 
	while(year >= 2006){
		sql <- paste("select count(*) from ",year,"papers",sep = "")
		PapersNumber <- sqlQuery(dbcon,sql)
		TakePercent <- PapersNumber * 0.1
	
		cutsql <- paste("select p.ID,p.AuthorFullName,p.Title,p.Keywords,p.KeywordsPlus,p.Abstract,
			p.CitedReference,p.TimesCited,p.PublishYear from ",year,"papers p where rownames <= ",TakePercent[1],sep = "")
		records <- sqlQuery(dbcon,cutsql )

		sqlSave(dbcon ,records ,"corePapers",append = TRUE)
		year <- year - 1
	}
	odbcClose(dbcon)
}

#############+++++++++++++++++++++++++++++选取6年10-15年的论文，摘取可用的cr并编码存入citedreference数据库中+++++++++++++++++++++++++++++++++++++###
cr.generate <- function(){

dbcon <- odbcConnect("mysqlodbc", uid="root", pwd="088027")
year <- 2015 
#while(year >= 2010)
sql <- paste("select p.ID, p.CitedReference from papers p where p.PublishYear = ",year,sep = "")
all.cr <- sqlQuery(dbcon,sql)  #读取数据库返回一个data.frame

# split co-cited references from all papers,generate the dataframe cr
count <- 1
for(i in all.cr[,1]){
#将每篇文章的多个引用，按分号区分开。
	k <- unlist(strsplit(as.character(all.cr$CitedReference[count]),"; ",fixed=FALSE))

	if(length(k) != 0){
#根据doi找出引文的文章ID
		for(each in k){
			remain <- strsplit(each,"DOI ",fixed = FALSE)
			citationid <- 0
			remainleft <- character(0)
			citationmark <- ""
			citationVolume <- ""
			citationPage <- ""
			citationyear = 0
			
			if(length(remain[[1]]) == 2){

				sql <-  paste("select p.ID from papers p where p.DOI = \"",remain[[1]][[2]],"\"",sep = "")
				id <- sqlQuery(dbcon,sql)
				if(length(id[[1]]) != 0){
					citationid <- id[[1]] 
					citationmark <- "DOI"
					part <- strsplit(remain[[1]][[1]],", ",fixed = FALSE)
					if(substr(part[[1]][[2]], 1, 1) == "1" || substr(part[[1]][[2]],1,2) == "20"){
						citationyear <- as.numeric(part[[1]][[2]])
					}
				}
				else{
					remainleft <- substr(remain[[1]][[1]],1,nchar(remain[[1]][[1]])-2)
				}
			}
			else if(length(remain[[1]]) == 3){

				sql <-  paste("select p.ID from papers p where p.DOI = \"",remain[[1]][[3]],"\"",sep = "")
				id <- sqlQuery(dbcon,sql) 
				if(length(id[[1]]) != 0){
					citationid <- id[[1]] 
					citationmark <- "DOI"
					part <- strsplit(remain[[1]][[1]],", ",fixed = FALSE)
					if(substr(part[[1]][[2]], 1, 1) == "1" || substr(part[[1]][[2]],1,2) == "20"){
						citationyear <- as.numeric(part[[1]][[2]])
					}
				}
				else{
					remainleft <- substr(remain[[1]][[1]],1,nchar(remain[[1]][[1]])-2)
				}
			}
			else{
				remainleft <- remain[[1]]
			}
			
			if(citationid == 0){
				part <- strsplit(remainleft,", ",fixed = FALSE)
				citationyear = 0
				if(length(part[[1]]) >= 3){
#去除包含‘的引用。
					Pnum <- length(part[[1]])
					every <- 1
					while(every <= Pnum){
						part[[1]][[every]] <- chartr("'", " ", part[[1]][[every]])
						every <- every + 1
					}
					if(substr(part[[1]][[2]], 1, 1) == "1" || substr(part[[1]][[2]],1,2) == "20"){
						citationyear <- as.numeric(part[[1]][[2]])
					}
					
					author <- strsplit(part[[1]][[1]]," ",fixed = FALSE)
#有误差，有的引文作者是全名，但是大部分还是缩写。比如Shai Shalev-Shwartz;Shalev-Shwartz S.,
					author <- author[[1]][[1]]
					Pcount <- 4
#判断是否包含卷和页的存在
					while(Pcount <= Pnum){
						if(length(grep("V",part[[1]][[Pcount]])) > 0 && grep("V",part[[1]][[Pcount]]) == 1){
							citationVolume = substr(part[[1]][[Pcount]],2,nchar(part[[1]][[Pcount]]))
						}
						
						if(length(grep("P",part[[1]][[Pcount]])) > 0 && grep("P",part[[1]][[Pcount]]) == 1){
							citationPage = substr(part[[1]][[Pcount]],2,nchar(part[[1]][[Pcount]]))
						}
						Pcount <- Pcount + 1
					}

					if(citationVolume != "" && citationPage != ""){
						sql <- paste("select p.ID from papers p where p.Author LIKE '",author,"%\' and p.PublishYear = ",citationyear," and p.J9 = '",part[[1]][[3]],"'"," and p.Volume = '",citationVolume,"' and p.BeginPage = '",citationPage,"'",sep = "")
					}
					else if(citationVolume != "" && citationPage == ""){
						sql <- paste("select p.ID from papers p where p.Author LIKE '",author,"%\' and p.PublishYear = ",citationyear," and p.J9 = '",part[[1]][[3]],"'"," and p.Volume = '",citationVolume,"'",sep = "")
					}
					else if(citationVolume == "" && citationPage != ""){
						sql <- paste("select p.ID from papers p where p.Author LIKE '",author,"%\' and p.PublishYear = ",citationyear," and p.J9 = '",part[[1]][[3]],"' and p.BeginPage = '",citationPage,"'",sep = "")
					}
					else if(citationVolume == "" && citationPage == ""){
						sql <- paste("select p.ID from papers p where p.Author LIKE '",author,"%\' and p.PublishYear = ",citationyear," and p.J9 = '",part[[1]][[3]],"'",sep = "")
					}
					id <- sqlQuery(dbcon,sql)
					if(length(id[[1]]) != 0){
						citationid <- id[[1]] 
						citationmark <- "match"
					}
				}
				else{
					temp <- data.frame(paperid = i,citedreference = each, crid = citationid, year = 0,mark = citationmark,stringsAsFactors=FALSE)
					print(temp)
					sqlSave(dbcon ,temp ,"citedreference",append = TRUE)
					next
				}
			}
			temp <- data.frame(paperid = i,citedreference = each, crid = citationid, year = citationyear,mark = citationmark,stringsAsFactors=FALSE)
			print(temp)
			sqlSave(dbcon ,temp ,"citedreference",append = TRUE)
			print("seccess")
		}
	}
	count <- count + 1
}

#	year <- year - 1
#}
	odbcClose(dbcon)
}


