#Required packages
require(rvest)
require(XBRL)
require(pdftools)
require(syuzhet)

#Set working directory
setwd("C:/Users/cob.vkrotov/Google Drive/Research/JETA/Code")

#Store strings as strings, rather than factors, inside data frames
options(stringsAsFactors=FALSE)

#Set filters
type <- "10-K"    #read annual statements
numStmts <- 8     #number of statements to be read (the most-recent statements will be read)

#Build the URL to get the list of reports
baseQueryUrl <- "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=0001067983&type="
queryUrl <- paste(baseQueryUrl, type, sep="")

#Get and parse the HTML for the list of reports
reportListHtml <- read_html(html_session(queryUrl))
documentLinks <- html_nodes(reportListHtml, "a[id=\"documentsbutton\"]")
documentLinkUrls <- data.frame(xml_attrs(documentLinks))

#Create frame in which data results will be stored
all_data <- data.frame(Year=character(0),
                       NetIncome=character(0),
                       ShareholdersEquity=character(0),
                       TotalAssets=character(0),
                       ROE=character(0),
                       ROA=character(0),
                       SentimentSyuzhet=character(0),
                       SentimentBing=character(0),
                       SentimentAfinn=character(0),
                       SentimentNRC=character(0))

#Create columns for the data frame
colnames(all_data) <- c("Year",
                        "NetIncome",
                        "ShareholdersEquity",
                        "TotalAssets",
                        "ROE",
                        "ROA",
                        "SentimentSyuzhet",
                        "SentimentBing",
                        "SentimentAfinn",
                        "SentimentNRC")

#This will store the "Total Assets" value for the previous year (used in ROA calculation)
prevTA <- -1

#Read each statement one at a time
for(stmt in numStmts:1)
{
  #Read HTML for the current statement
  filingDetails <- toString(documentLinkUrls[1,stmt])
  filingDetailsUrl <- paste("https://www.sec.gov", filingDetails, sep="")
  filingDetailsHtml <- read_html(html_session(filingDetailsUrl))
  
  #Read the Data Files html table which should contain the XBRL document
  dataFilesTable <- html_nodes(filingDetailsHtml, "table[summary=\"Data Files\"]")
  tableRows <- html_nodes(dataFilesTable, "tr")
  
  for(row in 1:length(tableRows))
  {
    #Specifically find the XBRL document from among the Data Files
    if(grepl("XBRL INSTANCE DOCUMENT", tableRows[row]))
    {
      #Get the URL of the XBRL document itself
      link <- html_node(tableRows[row], "a")
      linkUrl <- paste("https://www.sec.gov", xml_attr(link, "href"), sep="")
      writeLines(linkUrl) #print the url of the XBRL document
      
      #Read the data from the XBRL instance file
      xbrl.vars <- xbrlDoAll(linkUrl, cache.dir="XBRLcache", prefix.out="out", verbose=FALSE)
      
      #Determine the date of the annual statement based on the name of the XBRL file
      year  <- substr(linkUrl, nchar(linkUrl)-11, nchar(linkUrl)-8)
      month <- substr(linkUrl, nchar(linkUrl)-7,  nchar(linkUrl)-6)
      day   <- substr(linkUrl, nchar(linkUrl)-5,  nchar(linkUrl)-4)
      
      #Retreve the "facts" from the XBRL
      values <- xbrl.vars$fact
      
      #Find the positions of Net Income (NI), Shareholders Equity (SE), and Total Assets (TA) in the facts data frame
      regex <- paste(year,"-?",month,"-?",day,"(_0)?","$",sep="")
      indexNI <- which(values$elementId=="us-gaap_NetIncomeLoss" & grepl(regex,values$contextId))
      indexSE <- which(values$elementId=="us-gaap_StockholdersEquity" & grepl(regex,values$contextId))
      indexTA <- which(values$elementId=="us-gaap_Assets" & grepl(regex,values$contextId))
      
      #Obtaining NI, SE, and TA values
      NI <- as.numeric(values$fact[indexNI]) #NI contains income for both the year & the 4th quarter (we only use the annual value)
      SE <- as.numeric(values$fact[indexSE])
      TA <- as.numeric(values$fact[indexTA])
      
      #print key values to output
      writeLines(paste("Net Income: ", NI[1]))
      writeLines(paste("Shareholders Equity: ", SE))
      writeLines(paste("Total Assets: ", TA))
      writeLines("")
      
      #Download a corresponding Letter to Shareholders and read into a charvector
      txt <- pdf_text(paste("http://www.berkshirehathaway.com/letters/",year,"ltr.pdf",sep=""))
      all_txt <- paste(txt, sep="", collapse="")
      
      #Calculate sentiment score using four different methods
      sentimentSyuzhet <- get_sentiment(all_txt, method="syuzhet")
      sentimentBing <- get_sentiment(all_txt, method="bing")
      sentimentAfinn <- get_sentiment(all_txt, method="afinn")
      sentimentNRC <- get_sentiment(all_txt, method="nrc")
      
      #If available, use the previous Total Assets to calculate ROA
      if(prevTA!=-1)
        ROA <- 2*NI[1]/(TA+prevTA)
      else
        ROA <- "NA"
      
      #Consolidate all the information about this statement into a single frame
      stmt_info <- data.frame("Year"=year,
                              "NetIncome"=NI[1],
                              "ShareholdersEquity"=SE,
                              "TotalAssets"=TA,
                              "ROE"=NI[1]/SE,
                              "ROA"=ROA,
                              "SentimentSyuzhet"=sentimentSyuzhet,
                              "SentimentBing"=sentimentBing,
                              "SentimentAfinn"=sentimentAfinn,
                              "SentimentNRC"=sentimentNRC)
      
      #Write the current statement info to the frame where all data is stored
      all_data <- rbind(all_data, stmt_info)
      
      #this year's TA is stored as the previous year's TA to use in the next loop iteration
      prevTA <- TA
    }
  }
}

#Finally, write all the collected data to an output file
write.table(all_data, file="output_data.csv", sep=",", col.names=TRUE, row.names=FALSE)


