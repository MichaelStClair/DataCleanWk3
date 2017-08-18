## Lecture notes

# Video 1: A quick review of subsetting

set.seed(13435) # choose a seed number for RNG consistency
X <- data.frame("var1" = sample(1:5), "var2" = sample(6:10), "var3" = sample(11:15)) # create a data frame of 5 rows 3 cols
X <- X[sample(1:5),]; X$var2[c(1,3)] = NA # scramble the order then insert two explicit NAs
X

# sample results from above
#    var1 var2 var3
#1    2   NA   15
#4    1   10   11
#2    3   NA   12
#3    5    6   14
#5    4    9   13

# examples of subsetting this data frame

X[,1]         # subset the first column
# [1] 2 1 3 5 4

X[,"var1"]    # subset the first column by name
# [1] 2 1 3 5 4

X[1:2,"var2"] # subset the first two rows of second column by name
# [1] NA 10

X[(X$var1 <= 3 & X$var3 >11),] # all rows meeting two range criteria (AND)
#    var1 var2 var3
# 1    2   NA   15
# 2    3   NA   12

X[(X$var1 <=3 | X$var3 > 15),] # all rows meeting one criteria or another (OR)
#    var1 var2 var3
# 1    2   NA   15
# 4    1   10   11
# 2    3   NA   12

X[which(X$var2 > 8),] # using WHICH to eliminate NAs
#     var1 var2 var3
#  4    1   10   11
#  5    4    9   13

sort(X$var2,decreasing = TRUE, na.last = TRUE) # sort second column, in decreasing order, with NA last
# [1] NA NA 10  9  6

X[order(X$var1,X$var3),] # order the data frame by column 1 then column 3
#   var1 var2 var3
#4    1   10   11
#1    2   NA   15
#2    3   NA   12
#5    4    9   13
#3    5    6   14

# Ordering with plyr 
# install.packages("plyr")
library(plyr)

arrange(X,var1) # ascending
arrange(X,desc(var1)) # descending

# Add a column to the data frame.  Here, a random normal vector of the same dimension
X$var4 <- rnorm(5)
X
#   var1 var2 var3   var4
#1    2   NA   15  0.1875960
#4    1   10   11  1.7869764
#2    3   NA   12  0.4966936
#3    5    6   14  0.0631830
#5    4    9   13 -0.5361329

# using cbind (column bind) to bind another column to a copy of the frame
Y <- cbind(X,rnorm(5)) # args are left to right order what to bind
Y
#  var1 var2 var3       var4    rnorm(5)
#1    2   NA   15  0.1875960  0.62578490
#4    1   10   11  1.7869764 -2.45083750
#2    3   NA   12  0.4966936  0.08909424
#3    5    6   14  0.0631830  0.47838570
#5    4    9   13 -0.5361329  1.00053336

# Video 2: Summarizing data (I cleared the workspace here)
        
# Get the CSV file from the web
if(!file.exists("./data")){dir.create("./data")}
fileURL <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileURL,destfile="./data/restaurants.csv")
restData <- read.csv("./data/restaurants.csv")

# take a quick look
head(restData, n=3) # show top 3, 6 is the default
tail(restData, n=3)

# get summary information including some basic statistics
summary(restData)

# get some information about the structure
str(restData)

# Quantiles of quantitative variables
quantile(restData$councilDistrict,na.rm=TRUE)
#   0%  25%  50%  75% 100% 
#    1    2    9   11   14 
# smallest value is 1, largest is 14, median is 9

# Look at different probabilities.  Different quantiles of the distribution.
# here, the quantiles for the percentiles ranging from 50% to 90% with a median of 75%
quantile(restData$councilDistrict,probs=c(0.5,0.75,0.9))
# 50% 75% 90% 
#   9  11  12 
# I need to study quantiles!

# Make a table of zip codes
table(restData$zipCode,useNA = "ifany")
# -21226  21201  21202  21205  21206  etc etc 
#     1    136    201     27     30
# Each column is a zip, the singular rows are a count

# two dimensional table of count of Council Districts by zip code
table(restData$councilDistrict,restData$zipCode)
#     -21226 21201 21202 21205 21206 21207 etc etc
#1       0     0    37     0     0     0              
#2       0     0     0     3    27     0
#3       0     0     0     0     0 

# count number of NAs in a column of the table
sum(is.na(restData$councilDistrict))

# See if there are any NAs in a column
any(is.na(restData$councilDistrict))

# Oh no, we have a negative zip code!  All function
all(restData$zipCode > 0)

# Easy way to see if any NAs in any column
colSums(is.na(restData))

# Even easier, don't have to look at the sum of each column!
all(colSums(is.na(restData)) == 0)

# find how many records have zip codes of 21212 and 21213
table(restData$zipCode %in% c("21212", "21213"))
# FALSE  TRUE 
# 1268    59 

# get all the restaurants that match that criteria
restData[restData$zipCode %in% c("21212", "21213"),]

# Cross tabs (summaries)
data("UCBAdmissions")
df = as.data.frame(UCBAdmissions)
summary(df)
# Admit       Gender   Dept       Freq      
# Admitted:12   Male  :12   A:4   Min.   :  8.0  
# Rejected:12   Female:12   B:4   1st Qu.: 80.0  
#                           C:4   Median :170.0  
#                           D:4   Mean   :188.6  
#                           E:4   3rd Qu.:302.5  
#                           F:4   Max.   :512.0 

# crosstab 
xt <- xtabs(Freq ~ Gender + Admit,data=df)
xt
# Admit
# Gender   Admitted Rejected
# Male       1198     1493
# Female      557     1278

# Flat tables.  I should revisit this.  It's at the end of the Summarizing Data video

# Size of a dataset
fakeData = rnorm(1e5)
object.size(fakeData)
# 800040 bytes
print(object.size(fakeData),units="Mb")
# 0.8 Mb

# Video 3: Creating new variables (I cleared the workspace)



















































