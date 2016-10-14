#!/usr/bin/Rscript
require(rvest)
library(methods)

url <- 
  "https://it.wikipedia.org/wiki/Sondaggi_sul_referendum_costituzionale_del_2016_in_Italia"

getPollTable <- function(xpath, drop_last_row) {
  
  col_names <- 
    c('date', 'pollster', 'client', 'sample', 
      'yes_wtout_undecided', 'no_wtout_undecided',
      'yes_wt_undecided', 'no_wt_undecided',
      'novote', 'undecided', 'affluence', 'error')
  
  tab <- url %>%
    read_html() %>%
    html_nodes(xpath=xpath) %>%
    html_table(fill = TRUE)
  
  tab <- tab[[1]]
  tab <- tab[-1,]
  if (drop_last_row == TRUE) {
    tab <- tab[-nrow(tab),]
  }
  
  colnames(tab) <- col_names
  
  # Dates
  returnAsDate <- function(str) {
    library(stringr)
    it_month <- 
      c('gennaio', 'febbraio', 'marzo', 'aprile', 'maggio', 'giugno', 'luglio', 
        'agosto', 'settembre', 'ottobre', 'novembre', 'dicembre')
    names(it_month) <- sprintf("%02d", 1:12)
    it_month_regex <- paste(it_month, collapse = "|")
    date_str <- 
      str_extract(str, paste0("^(\\d{1,2})?(-\\d{1,2})?( )?", "(", it_month_regex, ") \\d{4}"))
    
    if (is.na(date_str)) {
      # Double month
      date_str <- 
        str_extract(str, paste0("^(.*)\\d{4}"))
      date_str <- gsub("-(.*) ", " ", date_str)
      date_str <- gsub("gen(\\.)?", "gennaio", date_str)
      date_str <- gsub("feb(\\.)?", "febbraio", date_str)
      date_str <- gsub("mar(\\.)?", "marzo", date_str)
      date_str <- gsub("apr(\\.)?", "aprile", date_str)
      date_str <- gsub("mag(\\.)?", "maggio", date_str)
      date_str <- gsub("giu(\\.)?", "giugno", date_str)
      date_str <- gsub("lug(\\.)?", "luglio", date_str)
      date_str <- gsub("ago(\\.)?", "agosto", date_str)
      date_str <- gsub("set(\\.)?", "settembre", date_str)
      date_str <- gsub("ott(\\.)?", "ottobre", date_str)
      date_str <- gsub("nov(\\.)?", "novembre", date_str)
      date_str <- gsub("dic(\\.)?", "dicembre", date_str)
    }
    
    day <- str_extract(date_str, "^\\d{1,2}")
    if(is.na(day)) {
      day <- "15"
    }
    day <- sprintf("%02s", day)
    month <- str_extract(date_str, it_month_regex)
    month <- names(it_month)[which(month == it_month)]
    year <-  str_extract(date_str, "\\d{4}$")
    
    
    return(paste(year, month, day, sep = "-"))
  }
  tab$date <- as.Date(sapply(tab$date, returnAsDate))
  
  # Sample conversion
  tab$sample <- as.numeric(gsub("[^0-9]", "", tab$sample))
  tab$sample[!grepl("\\d", tab$sample)] <- NA
  
  # Percentage conversion
  formatPerc <- function(x) {
    x <- gsub("\\[(.*)\\]", "", x)
    x <- gsub("%", "", x)
    x <- gsub(",", ".", x)
    x <- gsub("±", "", x)
    x <- gsub("\\(|\\)", "", x)
    if(!grepl("\\d", x)) {
      x <- NA
    }
    return(x)
  }
  for (i in 5:ncol(tab)) {
    tab[[i]] <- as.numeric(sapply(tab[[i]], formatPerc))
  }
  
  return(tab)
}

tab1 <- getPollTable('//*[@id="mw-content-text"]/table[1]', drop_last_row = TRUE)
tab2 <- getPollTable('//*[@id="mw-content-text"]/table[2]', drop_last_row = TRUE)
tab3 <- getPollTable('//*[@id="mw-content-text"]/table[3]', drop_last_row = FALSE)

polls <- rbind(tab1, tab2, tab3)

setwd('~/public_git/referendum_ita_2016')
save(polls, file = 'data/polls.RData')