library(dplyr)
library(data.table)
setwd("C:/Users/Jack/Desktop/School Stuff/VT Baseball/BP Reports/Trackman/Scrimmages-2020")
myfiles <- list.files(pattern="*.csv")
data = rbindlist(lapply(myfiles, fread, header = F, fill = T), fill = T)
names(data) <- as.matrix(data[1, ])
data <- data[-1, ]
data[] <- lapply(data, function(x) type.convert(as.character(x)))
scrimmages = subset(data, select = -c(76))

pitchers = unique(scrimmages$Pitcher)

for (p in pitchers) {
  rmarkdown::render('C:/Users/Jack/Desktop/School Stuff/VT Baseball/Pitcher-Ranges.Rmd',  # file 2
                    output_file =  paste("report-", p, ".pdf", sep=''), 
                    output_dir = 'C:/Users/Jack/Desktop/School Stuff/VT Baseball/Pitcher Averages',
                    params=list(player = p))
}