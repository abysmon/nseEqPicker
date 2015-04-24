'http://www.nseindia.com/content/historical/EQUITIES/2015/APR/cm01APR2015bhav.csv.zip'

require(httr)

cmbasedir = 'D:/dataset/CM/'
dirlist  = paste0(cmbase, 2000:2015)
for(jimbo in seq_len(length(dirlist)))
  dir.create(path = dirlist[jimbo])

baseurl = 'http://www.nseindia.com/content/historical/EQUITIES/'
# x = expand.grid(sprintf(fmt = "%02d", 1:31), toupper(month.abb))
# x[,1] = as.character(x[,1])
# x[,2] = as.character(x[,2])

cmDates = timeDate::timeSequence("01JAN2015", "31DEC2015", format = "%d%b%Y")
cmDates = cmDates[timeDate::isWeekday(cmDates)]
cmDates = as.POSIXct(strptime(cmDates, format = "%Y-%m-%d"))

holidays = unique(read.csv('D:/dataset/nseall/holidays.csv', as.is = T)[ ,2])
holidays = as.POSIXct(strptime(holidays, format = "%Y-%m-%d"))
cmDates = cmDates[!cmDates %in% holidays]

cmDates = toupper(as.character.Date(cmDates, "%d%b%Y"))
date.splot = cbind(substr(cmDates, 1, 2), substr(cmDates, 3, 5), substr(cmDates, 6, 9)) 
url.list = paste0(baseurl, date.splot[ ,3], '/', date.splot[ ,2], '/cm', 
                  date.splot[ ,1], date.splot[ ,2], date.splot[ ,3], 'bhav.csv.zip')

# write.table(url.list, file = 'cmlinks2015.txt', row.names = F, quote = F, col.names = F)
for(year in 2000:2015){
  start = min(grep(pattern = year, date.splot[ ,3]))
  end = max(grep(pattern = year, date.splot[ ,3]))
  url.list = paste0(baseurl, date.splot[start:end, 3], '/', date.splot[start:end, 2], 
                    '/cm', date.splot[start:end, 1], date.splot[start:end, 2], 
                    date.splot[start:end, 3], 'bhav.csv.zip')
  write.table(url.list, file = paste0('cmlinks', year, '.txt'), 
              row.names = F, quote = F, col.names = F)
  
  # it is easier to use the url list to dld instead of using httr in a for loop
  for(jimbo in 1:length(url.list)){
    fname = paste0(cmbase, date.splot[start+jimbo-1, 3], '/cm', 
                   date.splot[start+jimbo-1, 1], date.splot[start+jimbo-1, 2], 
                   date.splot[start+jimbo-1, 3], 'bhav.csv.zip')
    
    httr::GET(url, user_agent("PillarsOfEternity"), write_disk(fname, overwrite = T))
  }
}


# some addl trading on weekends
cmbase = 'D:/dataset/nseCM/'
for(year in 2000:2015){
  if(year != 2011){
    listcm = substring(list.files(paste0(cmbase, year), "*.csv$"), 3)
    listcm = substring(list.files(paste0(cmbase, year), "*.csv$"), 3)
  } else {
    listcm = substring(list.files(paste0(cmbase, year), "*.csv$"), 3)
    listcm = substring(c(list.files('D:/dataset/nseCM/2011/part1/', "*.csv$"), 
                         list.files('D:/dataset/nseCM/2011/part2/', "*.csv$")), 3)
  }
  listcm[ !listcm %in% listcm]
  listcm[ !listcm %in% listcm]
  listcm[ !listcm %in% listcm]
  listcm[ !listcm %in% listcm]
}

# "02JUL2007bhav.csv"
# "28MAY2008bhav.csv" "31JUL2008bhav.csv"
# "14OCT2010bhav.csv"
# "26OCT2011bhav.csv"
# "09OCT2013bhav.csv"
# "22MAR2014bhav.csv"
