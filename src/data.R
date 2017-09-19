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

getData <- function(fData,n) {
  
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
    
    splitReqHttp <- function(df) {
      
      keep <- c("","fr","en","unavailable.html")
      
      req <- sapply(strsplit(df$req_content,"/"),"[",-1)
      r <- sapply(req,"[",1)
      
      df_out <- df[!(r %in% keep) & !is.na(r),]
      df <- df[(r %in% keep) | is.na(r),]
      
      return(list(df,df_out))
      
    }
    
    req <- strsplit(df$req," ")
    
    df$req_method <- gsub("\"","",noquote(sapply(req,"[",1)))
    df$req_content <- sub("http://webstat.banque-france.fr","",sapply(req,"[",2),fixed=T)
    df$req_content <- sub("//","/",df$req_content,fixed=T)
    df$req_protocol <- gsub("\"","",noquote(sapply(req,"[",3)))
    
    l <- splitReqHttp(df)
    
    return(l)
    
  }
  
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

setData <- function(df) {
  
  setLanguage <- function(df) {
    
    cond <- grep("^/(fr|en)",df$req_content)
    df$req_language <- ""
    df[cond,]$req_language <- substr(df[cond,]$req_content,2,3)
    
    return(df)
    
  }
  
  setAction <- function(df) {
    
    #req_action
    
    df$req_action <- ""
    
    # .do actions
    pat <- "/([a-z,A-Z]+)\\.do"
    m_v <- regmatches(df$req_content,regexpr(pat,df$req_content))
    m_v <- sub("/","",m_v)
    m_i <- grepl(pat,df$req_content)
    
    df[m_i,]$req_action <- m_v
    
    # api actions
    pat <- "(?<=^/(fr|en)/)api/[a-z,A-Z]+(?=(\\?|/)?)"
    m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=T))
    m_i <- grepl(pat,df$req_content,perl=T)
    
    df[m_i,]$req_action <- m_v
    
    # lang no action
    pat <- "^/(fr|en)/$"
    m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=F))
    m_v <- sub("/","",m_v)
    m_i <- grepl(pat,df$req_content,perl=F)
    
    df[m_i,]$req_action <- m_v
    
    # no action
    pat <- "^/$"
    m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=F))
    m_i <- grepl(pat,df$req_content,perl=F)
    
    df[m_i,]$req_action <- m_v
    
    # Modint
    pat <- "(?<=/(fr|en)/ws/)getLastUpdateCalendar"
    m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=T))
    m_i <- grepl(pat,df$req_content,perl=T)
    
    df[m_i,]$req_action <- m_v
    
    # IMF
    pat <- "(?<=/(fr|en)/)fmi(?=/)"
    m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=T))
    m_i <- grepl(pat,df$req_content,perl=T)
    
    df[m_i,]$req_action <- m_v
    
    # unavailable
    pat <- "(?<=/)unavailable(?=\\.html)"
    m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=T))
    m_i <- grepl(pat,df$req_content,perl=T)
    
    df[m_i,]$req_action <- m_v
    
    # /lang
    pat <- "(?<=^/)(fr|en)(?=$)"
    m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=T))
    m_i <- grepl(pat,df$req_content,perl=T)
    
    df[m_i,]$req_action <- m_v
    
    # publication
    pat <- "(?<=/)publication(?=/)"
    m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=T))
    m_i <- grepl(pat,df$req_content,perl=T)
    
    df[m_i,]$req_action <- m_v
    
    # App mobile
    pat <- "(?<=/(fr|en)/series/sdmx/)[a-z,A-Z]+(?=/)"
    m_v <- paste0("mob_",regmatches(df$req_content,regexpr(pat,df$req_content,perl=T)))
    m_i <- grepl(pat,df$req_content,perl=T)
    
    df[m_i,]$req_action <- m_v
    
    # quickviewChart
    pat <- "(?<=/(fr|en)/servlet/)quickviewChart(?=\\?)"
    m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=T))
    m_i <- grepl(pat,df$req_content,perl=T)
    
    df[m_i,]$req_action <- m_v  
    
    # dataTables.locale.txt
    pat <- "(?<=^/(fr|en)/jsp/)dataTables\\.locale(?=\\.txt)"
    m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=T))
    m_i <- grepl(pat,df$req_content,perl=T)
    
    df[m_i,]$req_action <- m_v
    
    # browse
    pat <- "(?<=^/(fr|en)/)browse(?=$)"
    m_v <- regmatches(df$req_content,regexpr(pat,df$req_content,perl=T))
    m_i <- grepl(pat,df$req_content,perl=T)
    
    df[m_i,]$req_action <- m_v
    
    #action
    
    df$action <- ""
    
    # df[df$req_action=="unavailable",]$action <- "ERROR"
    # df[df$req_action=="api/seriesMetadata",]$action <- "AUTO"
    map <- list(
      c("unavailable","ERROR","none","none"),
      c("api/seriesMetadata","INTERNAL","META","SERIES"),
      c("quickviewexport.do","DATA","EXPORT_INTER","SERIES"),
      c("browse.do","PORTAL","BROWSE","NODE"),
      c("quickview.do","DATA","QUICKVIEW","SERIES"),
      c("downloadFile.do","DATA","EXPORT_STATIC","SERIES_LIST"),
      c("searchExport.do","DATA","EXPORT_INTER","SERIES_LIST"),
      c("export.do","DATA","EXPORT_INTER","SERIES_LIST"),
      c("api/dimension","INTERNAL","STRUCTURE","DIMENSION"),
      c("fr/","ENTRY","none","none"),
      c("print.do","PORTAL","PRINT","MULTI"),
      c("en/","ENTRY","none","none"),
      c("connect.do","PORTAL","CONNECT","none"),
      c("search.do","PORTAL","SEARCH","SERIES_LIST"),
      c("/","INTERNAL","none","none"),
      c("browseSelection.do","PORTAL","BROWSE","NODE"),
      c("api/series","INTERNAL","?","?"),
      c("api/dataset","INTERNAL","?","?"),
      c("browseTable.do","PORTAL","BROWSE","NODE"),
      c("api/node","INTERNAL","?","?"),
      c("api/seriesWithInfo","INTERNAL","?","?"),
      c("searchAutoComplete.do","INTERNAL","SEARCH","none"),
      c("home.do","PORTAL","HOME","none"),
      c("fillDatatables.do","INTERNAL","BROWSE","SERIES_LIST"),
      c("myBasket.do","PORTAL","BROWSE","SERIES_LIST"),
      c("updates.do","PORTAL","BROWSE","SERIES_LIST"),
      c("calendarFilter.do","PORTAL","CALENDAR_FILTER","none"),
      c("calendarPublication.do","PORTAL","?","?"),
      c("mob_data","DATA","MOBILE","?"),
      c("api/seriebox","INTERNAL","BROWSE","BOXES"),
      c("getLastUpdateCalendar","DISSEMINATION","MODINT","CALENDAR_UPDATE"),
      c("publication","PORTAL","PUBLICATION","SERIES_LIST"),
      c("en","ENTRY","none","none"),
      c("downloading.do","PORTAL","BROWSE","DOWNLOAD"),
      c("concepts.do","PORTAL","BROWSE","CONCEPT"),
      c("browseChart.do","PORTAL","BROWSE_CHART","NODE"),
      c("fmi","DISSEMINATION","IMF","NODE"),
      c("fr","ENTRY","",""),
      c("")
      
    )
    
    
    
    return(df)
    
  }
  
  setParameters <- function(df) {
    
    # if (!require(stringr)) install.packages("stringr")
    
    setParams <- function(df,action,pat) {
      data <- df[df$req_action==action,]$req_content
      m_v <- regmatches(data,regexpr(pat,data,perl=T))
      m_i <- grepl(pat,data,perl=T)
      df[df$req_action==action,][m_i,]$req_parameters <- m_v
      return(df)
    }
    
    df$req_parameters <- NA
    
    lapply(unique(d$req_action),function(x){
      eval(parse(text=paste0("df <- setParams(df,'",x,"','(?<=^/(fr|en)/",x,").+(?=$)')")),
           envir=parent.frame(2))
    })
    
    return(df)
    
  }
  
  setLevelData <- function(df) {
    
  }
  
  df <- setLanguage(df)
  df <- setAction(df)
  df <- setParameters(df)
  
}
