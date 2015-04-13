#reading data from the US NEWS and REVIEW website
library(XML)
fileUrl <- "http://www.usnews.com/education/blogs/college-rankings-blog/2013/06/18/top-ranked-universities-that-grant-the-most-stem-degrees"
table = readHTMLTable(fileUrl, header = TRUE)
names(table)