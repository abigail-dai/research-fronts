### trying to process those data in DB of papers

library(RODBC)
 
# Set project path
path <- "E:/AbigailDai/Graduate_first/TRY_PacificVis/code/co-citation metrix/"
setwd(path)

#############+++++++++++++++++++++++++++++��ÿ������İ��ձ����������򣬵�����һ����2016papers+++++++++++++++++++++++++++++++++++++###
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


#############+++++++++++++++++++++��ȡÿ��ǰ�ٷ�֮10��������Ϊ�������ף�����corepapers:10%ֻ����Ϊһ��������������15%����100%+++++++++++++++###��ֵ��ȷ��,����׼ȷ����ȷ��
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

#############+++++++++++++++++++++++++++++ѡȡ6��10-15������ģ�ժȡ���õ�cr���������citedreference���ݿ���+++++++++++++++++++++++++++++++++++++###
cr.generate <- function(){

dbcon <- odbcConnect("mysqlodbc", uid="root", pwd="088027")
year <- 2015 
#while(year >= 2010)
sql <- paste("select p.ID, p.CitedReference from papers p where p.PublishYear = ",year,sep = "")
all.cr <- sqlQuery(dbcon,sql)  #��ȡ���ݿⷵ��һ��data.frame

# split co-cited references from all papers,generate the dataframe cr
count <- 1
for(i in all.cr[,1]){
#��ÿƪ���µĶ�����ã����ֺ����ֿ���
	k <- unlist(strsplit(as.character(all.cr$CitedReference[count]),"; ",fixed=FALSE))

	if(length(k) != 0){
#����doi�ҳ����ĵ�����ID
		for(each in k){
			remain <- strsplit(each,"DOI ",fixed = FALSE)
			citationid <- 0
			citationyear <- numeric(0)
			remainleft <- character(0)
			citationmark <- ""
			citationVolume <- ""
			citationPage <- ""
			if(length(remain[[1]]) == 2){

				sql <-  paste("select p.ID from papers p where p.DOI = \"",remain[[1]][[2]],"\"",sep = "")
				id <- sqlQuery(dbcon,sql)
				if(length(id[[1]]) != 0){
					citationid <- id[[1]] 
					citationmark <- "DOI"
					part <- strsplit(remain[[1]][[1]],", ",fixed = FALSE)
					citationyear <- as.numeric(part[[1]][[2]])
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
					citationyear <- as.numeric(part[[1]][[2]])
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
#ȥ�������������á�
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
#�����е�����������ȫ�������Ǵ󲿷ֻ�����д������Shai Shalev-Shwartz;Shalev-Shwartz S.,
					author <- author[[1]][[1]]
					Pcount <- 4
#�ж��Ƿ��������ҳ�Ĵ���
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
						sql <- paste("select p.ID from papers p where p.Author LIKE '",author,"%\' and p.PublishYear = ",as.numeric(part[[1]][[2]])," and p.J9 = '",part[[1]][[3]],"'"," and p.Volume = '",citationVolume,"' and p.BeginPage = '",citationPage,"'",sep = "")
					}
					else if(citationVolume != "" && citationPage == ""){
						sql <- paste("select p.ID from papers p where p.Author LIKE '",author,"%\' and p.PublishYear = ",as.numeric(part[[1]][[2]])," and p.J9 = '",part[[1]][[3]],"'"," and p.Volume = '",citationVolume,"'",sep = "")
					}
					else if(citationVolume == "" && citationPage != ""){
						sql <- paste("select p.ID from papers p where p.Author LIKE '",author,"%\' and p.PublishYear = ",as.numeric(part[[1]][[2]])," and p.J9 = '",part[[1]][[3]],"' and p.BeginPage = '",citationPage,"'",sep = "")
					}
					else if(citationVolume == "" && citationPage == ""){
						sql <- paste("select p.ID from papers p where p.Author LIKE '",author,"%\' and p.PublishYear = ",as.numeric(part[[1]][[2]])," and p.J9 = '",part[[1]][[3]],"'",sep = "")
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

