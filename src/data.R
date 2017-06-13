go <- function() {

  Sys.setlocale("LC_TIME", "English")
  
  fData <- "data/access.170501"
  f_d <- "data/d.RData"
  f_d_out <- "data/d_out.RData"
  
  if (!exists("d")) {
    if (file.exists(f_d))
      load(f_d)
    else
      d <- getData(fData)
  }
  
  if (!exists("d_out"))
    if (file.exists(f_d_out))
      load(f_d_out)
  else
    d_out <- head(d[,c("ip","dt","req","status","referrer","useragent")],0)
  
}

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
  df$req_content <- sub("http://webstat.banque-france.fr","",sapply(req,"[",2),fixed=T)
  df$req_content <- sub("//","/",df$req_content,fixed=T)
  df$req_protocol <- gsub("\"","",noquote(sapply(req,"[",3)))
  
  l <- splitReqHttp(df)
  
  return(l)
  
}

splitReqHttp <- function(df) {
  
  keep <- c("","fr","en","unavailable.html")
  
  req <- sapply(strsplit(df$req_content,"/"),"[",-1)
  r <- sapply(req,"[",1)
  
  df_out <- df[!(r %in% keep) & !is.na(r),]
  #out <- rbind(out,df[which(is.na(r)),])
  #df_out <- rbind(d_out,out)
  
  df <- df[(r %in% keep) | is.na(r),]
  
  return(list(df,df_out))
  
}

setLanguage <- function(df) {
  
  cond <- grep("^/(fr|en)",df$req_content)
  df$req_language <- ""
  df[cond,]$req_language <- substr(df[cond,]$req_content,2,3)
  
  return(df)
  
}

setAction <- function(df) {
  
  df$req_action <- ""
  
  # .do actions
  pat <- "/([a-z,A-Z]+)\\.do"
  m_v <- regmatches(df$req_content,regexpr(pat,df$req_content))
  m_v <- sub("/","",m_v)
  m_i <- grepl(pat,df$req_content)
  
  df[m_i,]$req_action <- m_v
  
  # api actions
  pat <- "(?<=/)api/[a-z,A-Z]+(?=\\?|/)"
  m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=T))
  m_v <- sub("/","",m_v)
  m_i <- grepl(pat,df$req_content,perl=T)
  
  df[m_i,]$req_action <- m_v
  
  # lang no action
  pat <- "^/(fr|en)/$"
  m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=F))
  #m_v <- sub("/(fr|en)","",m_v)
  m_v <- sub("/","",m_v)
  m_i <- grepl(pat,df$req_content,perl=F)
  
  df[m_i,]$req_action <- m_v
  
  # no action
  pat <- "^/$"
  m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=F))
  #m_v <- sub("/","",m_v)
  m_i <- grepl(pat,df$req_content,perl=F)
  
  df[m_i,]$req_action <- m_v
  
  return(df)
    
}

getData <- function(fData,n) {
  
  if (missing(n)) n=-1
  
  v <- readLines(con=fData,n=n)
  v <- v[-1]
  v <-strsplit(v," (?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)",perl=T)
  # remove headers (keep records beginning with @IP):
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
  
  df <- setDatetime(df)
  l <- setReqHttp(df)
  
  if (exists("d"))
    d <<- rbind(d,l[[1]])
  else
    d <<- l[[1]]
  
  if (exists("d_out"))
    d_out <<- rbind(d_out,l[[2]])
  else
    d_out <<- l[[2]]
  
}

#d <- getData(fData)