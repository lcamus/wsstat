Sys.setlocale("LC_TIME", "English")
fData <- "data/access.170501"

setDatetime <- function(df) {
  
  #split date-time
  dt <- strsplit(df$dt,":")
  df$date <- sapply(dt,"[",1)
  df$hour <- sapply(dt,"[",2)
  df$min <- sapply(dt,"[",3)
  df$sec <- sapply(dt,"[",4)
  df$dt <- NULL
  rm(dt)
  
  #split dates
  ud <- unique(df$date)
  mud <- match(df$date,ud)
  ud <- as.Date(ud,"%d/%b/%Y")
  
  for (i in 1:length(ud))
    df[which(mud==i),]$date <- as.character(ud[i])
  
  return(df)

}

setReqHttp <- function(df) {
  
  req <- strsplit(df$req," ")

  df$req_method <- gsub("\"","",noquote(sapply(req,"[",1)))
  df$req_content <- sapply(req,"[",2)
  df$req_protocol <- gsub("\"","",noquote(sapply(req,"[",3)))
  
  return(df)
  
}

getData <- function(fData,n) {
  
  if (missing(n)) n=-1
  
  v <- readLines(con=fData,n=n)
  v <- v[-1]
  v <-strsplit(v," (?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)",perl=T)
  # filter regular logs (begins with @IP):
  v <- v[grepl("^(\\d{1,3}\\.){3}\\d{1,3}$",lapply(v,"[",1))]
  # remove unuseful logs:
  v <- v[!grepl("\\.css|\\.js|\\.ico|\\.png|\\.gif",lapply(v,"[",6))]
  
  v <- noquote(v)
  v <- lapply(v,function(x){
      x[4] <- substring(x[4],2)
      noquote(x[-5])
  })
  
  print(length(v))
  
  df <- setNames(
    as.data.frame(
      t(as.data.frame(v, stringsAsFactors=F)),
      stringsAsFactors=F
    )
    ,c("ip","v2","v3","dt","req","status","size","referrer","useragent")
  )
  
  lapply(c("req","referrer","useragent"),function(x){df[,c(x)]<-noquote(df[,c(x)])})
  rownames(df) <- NULL
  
  return(df)
  
}

#d <- getData(fData)