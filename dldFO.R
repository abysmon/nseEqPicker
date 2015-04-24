'http://www.nseindia.com/content/historical/DERIVATIVES/2000/SEP/fo11SEP2000bhav.csv.zip'
'http://www.nseindia.com/content/historical/DERIVATIVES/2014/SEP/fo19SEP2014bhav.csv.zip'

require(httr)

fobase = 'D:/dataset/nseFO/'
dirlist  = paste0(fobase, 2000:2015)
for(jimbo in seq_len(length(dirlist)))
  dir.create(path = dirlist[jimbo])

baseurl = 'http://www.nseindia.com/content/historical/DERIVATIVES/'
# x = expand.grid(sprintf(fmt = "%02d", 1:31), toupper(month.abb))
# x[,1] = as.character(x[,1])
# x[,2] = as.character(x[,2])

# foDates = timeDate::timeSequence("11SEP2000", "31MAR2015", format = "%d%b%Y")
foDates = timeDate::timeSequence("01JAN2015", "31DEC2015", format = "%d%b%Y")
foDates = foDates[timeDate::isWeekday(foDates)]
foDates = as.POSIXct(strptime(foDates, format = "%Y-%m-%d"))

holidays = unique(read.csv('D:/dataset/nseall/holidays.csv', as.is = T)[ ,2])
holidays = as.POSIXct(strptime(holidays, format = "%Y-%m-%d"))
foDates = foDates[!foDates %in% holidays]

foDates = toupper(as.character.Date(foDates, "%d%b%Y"))
date.splot = cbind(substr(foDates, 1, 2), substr(foDates, 3, 5), substr(foDates, 6, 9)) 
url.list = paste0(baseurl, date.splot[ ,3], '/', date.splot[ ,2], '/fo', 
                  date.splot[ ,1], date.splot[ ,2], date.splot[ ,3], 'bhav.csv.zip')

# write.table(url.list, file = 'folinks2015.txt', row.names = F, quote = F, col.names = F)

for(year in 2000:2015){
  start = min(grep(pattern = year, date.splot[ ,3]))
  end = max(grep(pattern = year, date.splot[ ,3]))
  url.list = paste0(baseurl, date.splot[start:end, 3], '/', date.splot[start:end, 2], 
                    '/fo', date.splot[start:end, 1], date.splot[start:end, 2], 
                    date.splot[start:end, 3], 'bhav.csv.zip')
  write.table(url.list, file = paste0('folinks', year, '.txt'), 
              row.names = F, quote = F, col.names = F)
  
  # it is easier to use the url list to dld instead of using httr in a for loop
  for(jimbo in 1:length(url.list)){
    fname = paste0(fobase, date.splot[start+jimbo-1, 3], '/fo', 
                   date.splot[start+jimbo-1, 1], date.splot[start+jimbo-1, 2], 
                   date.splot[start+jimbo-1, 3], 'bhav.csv.zip')

    httr::GET(url, user_agent("PillarsOfEternity"), write_disk(fname, overwrite = T))
  }
}


# some addl trading on weekends
cmbase = 'D:/dataset/nsebhavcopy/'
for(year in 2000:2015){
  if(year != 2011){
    listfo = substring(list.files(paste0(fobase, year), "*.csv$"), 3)
    listcm = substring(list.files(paste0(cmbase, year), "*.csv$"), 3)
  } else {
    listfo = substring(list.files(paste0(fobase, year), "*.csv$"), 3)
    listcm = substring(c(list.files('D:/dataset/nsebhavcopy/2011/part1/', "*.csv$"), 
                         list.files('D:/dataset/nsebhavcopy/2011/part2/', "*.csv$")), 3)
  }
  listcm[ !listfo %in% listcm]
  listcm[ !listcm %in% listfo]
  listfo[ !listfo %in% listcm]
  listfo[ !listcm %in% listfo]
}

# "02JUL2007bhav.csv"
# "28MAY2008bhav.csv" "31JUL2008bhav.csv"
# "14OCT2010bhav.csv"
# "26OCT2011bhav.csv"
# "09OCT2013bhav.csv"
# "22MAR2014bhav.csv"
