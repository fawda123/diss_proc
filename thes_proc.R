# obtaining and parsing theses (masters) data

# starting URL to search
url.in <- 'http://conservancy.umn.edu/handle/11299/53656/browse?type=author&order=ASC&rpp=20&offset=0'

# output object
dat <- list()

# stopping criteria for search loop
max_rec <- '1157'
stp.txt <- paste(max_rec, 'of', max_rec)
str.chk <- 'foo'

# initiate search loop
while(!grepl(stp.txt, str.chk)){
  
  # get html from url.in
  html <- htmlTreeParse(url.in, useInternalNodes=T)
  
  # find record numbers on page, loop stops of txt containts stp.txt
  str.chk <- xpathSApply(html, '//div', xmlValue)
  str.chk <- str.chk[grep('1157', str.chk)][1]
  
  # get author names
  names.tmp <- xpathSApply(html, "//table", xmlValue)[9]
  names.tmp <- gsub("^\\s+|\\[1]", "", strsplit(names.tmp,'\n')[[1]])
  names.tmp <- names.tmp[nchar(names.tmp)>0]
  names.tmp <- gsub("[[:space:]]*$", "", names.tmp)
  
  # go through author names to find unique url
  url.txt <- strsplit(names.tmp,', ')
  url.txt <- lapply(
    url.txt,
    function(x){
      
      cat(x,'\n')
      flush.console()
      
      # get permanent handle by using author search
      url.tmp <- gsub(' ','+',x)
      url.tmp <- paste(
        'http://conservancy.umn.edu/handle/11299/53656/browse?type=author&order=ASC&rpp=20&value=',
        paste(url.tmp,collapse='%2C+'),
        sep=''
      )
      html.tmp <- readLines(url.tmp)
      str.tmp <- rev(html.tmp[grep('handle',html.tmp)])[1]
      str.tmp <- strsplit(str.tmp,'\"')[[1]]
      str.tmp <- str.tmp[grep('handle',str.tmp)] # permanent URL
      
      # parse permanent handle
      perm.tmp <- htmlTreeParse(
        paste('http://conservancy.umn.edu',str.tmp,sep=''),useInternalNodes=T
      )
      perm.tmp <- xpathSApply(perm.tmp, "//td", xmlValue)
      perm.tmp <- perm.tmp[grep('Major|pages',perm.tmp)]
      perm.tmp <- c(str.tmp,rev(perm.tmp)[1])
      
    }
  )
  
  # append data to list
  dat <- c(dat, url.txt)
  
  # reinitiate url search with next twenty recs
  url.in <- strsplit(url.in, 'offset=')[[1]]
  url.in <- paste(url.in[1], as.numeric(url.in[2]) + 20, sep = 'offset=')
  
}

# remove duplicates
dat <- unique(dat)

thes_dat <- dat
save(thes_dat, file = 'thes_dat.RData')

######
# parse the data

load('thes_dat.RData')

# function for parsing text from website
get.txt <- function(str.in){
  
  # separate string by spaces
  str.in <- strsplit(gsub(',',' ',str.in,fixed=T),' ')[[1]]
  str.in <- gsub('.','',str.in,fixed=T)
  
  # get page number
  pages <- str.in[grep('page',str.in)[1]-1]
  if(grepl('appendices|appendix|:',pages)) pages <- NA
  
  # get major, exception for error
  if(class(try({
    major <- str.in[c(
      grep(':|;',str.in)[1]:(grep(':|;',str.in)[2]-1)
    )]
    major <- gsub('.','',gsub('Major|Mayor|;|:','',major),fixed=T)
    major <- paste(major[nchar(major)>0],collapse=' ')
    major <- gsub(' 1 computer file', '', major)
    major <- gsub('Bussiness', 'Business', major)
    major <- gsub('&', 'and', major)
  }))=='try-error') major <- NA
  
  # get year of graduation
  yrs <- seq(2006,2014)
  yr <- str.in[grep(paste(yrs,collapse='|'),str.in)[1]]
  yr <- gsub('Major|:','',yr)
  if(!length(yr)>0) yr <- NA
  
  # get month of graduation
  months <- c('January','February','March','April','May','June','July','August',
              'September','October','November','December')
  month <- str.in[grep(paste(months,collapse='|'),str.in)[1]]
  month <- gsub('theis|\r\n|:','',month)
  if(!length(month)>0) month <- NA
  
  # get advisor, exception for error
  if(class(try({
    advis <- str.in[(grep('Advis',str.in)+1):(grep('computer',str.in)-2)]
    advis <- paste(advis,collapse=' ')
  }))=='try-error') advis <- NA
  
  # output text
  c(pages,major,yr,month,advis)
  
}

# handles from thes_dat
handles <- c(ldply(thes_dat, function(x) x[[1]]))[[1]]

# get data using function, ran on 'dat'
thes_parse <- ldply(thes_dat,
                    function(x){
                      cat(x[1], which(x[1] == handles), '\n')
                      c(x[1], get.txt(x[2]))
                    }
)
names(thes_parse) <- c('Handle', 'Pages', 'Major', 'Year', 'Month', 'Advisor')

# reformat some vectors for analysis
thes_parse$Pages <- as.numeric(as.character(thes_parse$Pages))
thes_parse <- na.omit(thes_parse[, c('Pages', 'Major', 'Year')])
thes_parse$Major <- tolower(thes_parse$Major)

# table of number of theses per major, minimum of five
pop.maj <- sort(table(thes_parse$Major), decreasing = T)
pop.maj <- data.frame(num_thes = pop.maj[pop.maj >= 5])
pop.maj$Major <- row.names(pop.maj)

# subset data by majors with at least five
thes_parse <- thes_parse[thes_parse$Major %in% pop.maj$Major, ]

# save parsed thesis data
save(thes_parse, file = 'thes_parse.RData')
