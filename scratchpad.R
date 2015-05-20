#---- benchmarking ave, data.table and plyr ----
library(ggplot2)
library(data.table)
library(plyr)

theme_set(theme_bw())
datsize = c(10e4, 10e5)
noClasses = c(10, 50, 100, 200, 500, 1000, 2500, 10e3)
comb = expand.grid(datsize = datsize, noClasses = noClasses)
res = ddply(comb, .(datsize, noClasses), function(x) {
  expdata = data.frame(value = runif(x$datsize),
                       cat = round(runif(x$datsize, min = 0, max = x$noClasses)))
  expdataDT = data.table(expdata)
  
  t1 = system.time(res1 <- with(expdata, ave(value, cat)))
  t2 = system.time(res2 <- ddply(expdata, .(cat), mean))
  t3 = system.time(res3 <- expdataDT[, sum(value), by = cat])
  return(data.frame(tave = t1[3], tddply = t2[3], tdata.table = t3[3]))
}, .progress = 'text')

res

ggplot(aes(x = noClasses, y = log(value), color = variable), 
       data = melt(res, id.vars = c("datsize","noClasses"))) + 
  geom_line() + facet_wrap(~ datsize)



#---- benchmarking ave, dplyr, data.table and plyr ----
set.seed(42)

types <- c("A", "B", "C", "D", "E", "F")
obs <- 4e+07
one <- data.frame(id = as.factor(seq(from = 1, to = 80000, by = 1)), 
                  percent = round(runif(obs, min = 0, max = 1), digits = 2), 
                  type = as.factor(sample(types, obs, replace = TRUE)))

print(object.size(one), units = "GB")
summary(one)

library(plyr)

## Test 1 (plyr): Use ddply and subset one with [ ] style indexing from
## within the ddply call.

typeSubset <- c("A", "C", "E")
system.time(test1 <- ddply(one[one$type %in% typeSubset, ], .(id), summarise,
                           percent_total = sum(percent)))

two <- subset(one, type %in% typeSubset)
system.time(test2 <- ddply(two, .(id), summarise, percent_total = sum(percent)))

system.time(test3 <- count(two, "id", "percent"))


library(data.table)
three <- data.table(two, key = c("id"))

tables()
system.time(test4 <- three[, list(percent_total = sum(percent)), by = key(three)])


library(dplyr)

## Test 5 (dplyr): Speed test for package dplyr
fourDf <- group_by(two, id)
system.time(test5 <- summarise(fourDf, percent_total = sum(percent)))



#---- Arrows in plots ----
library(ggplot2)
library(scales)
library(grid)

df3 <- structure(list(value1 = c(51L, 57L, 59L, 57L, 56L, 56L, 60L,66L, 61L, 61L), 
                      value2 = c(56L, 60L, 66L, 61L, 61L, 59L, 61L,66L, 63L, 63L), 
                      group = c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E"), 
                      time = c("1999", "1999", "1999", "1999", "1999", 
                               "2004", "2004","2004", "2004", "2004"), 
                      y_position = c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L)), 
                 .Names = c("value1", "value2", "group", "time", "y_position"), 
                 row.names = c(NA, -10L), class = "data.frame") 

# question
p = ggplot(df3, aes(x = value1, y = y_position, group = time, color = time)) +
  geom_segment(x = min(df3$value1, df3$value2), xend = max(df3$value1, df3$value2),
                aes(yend = y_position), color = "lightgrey", size = 19) +
  scale_y_continuous(labels = df3$group, breaks = df3$y_position) + 
  theme_classic() + theme(axis.line = element_blank(), axis.title = element_blank()) + 
  geom_segment(aes(yend = y_position, xend = value2, color = time, group = time), 
               size = 19, alpha = 0.9, 
               arrow = arrow(length = unit(40, "points"),type = "closed", angle = 40))

print(p)

# answer 1
g <-  ggplotGrob(p)
idx <- grep("panel", g$layout$name)
nms <- sapply(g$grobs[[idx]]$children[[3]]$children , '[[', "name")
for(i in nms) {
  g$grobs[[idx]]$children[[3]] <- 
    editGrob(g$grobs[[idx]]$children[[3]], nms[i], 
             gp=gpar(linejoin ='mitre'), grep=TRUE)
}
grid.newpage()
grid.draw(g)


# answer 2
p0 <- ggplot(df3) + coord_flip()
p1 <- p0 + geom_bar(aes(x = group,y = max(c(value1,value2))*1.1),width = 0.2, 
                   stat = "identity",position = "identity",alpha = 0.2)
df1 <- subset(df3, time == "1999")
p1 + geom_segment(data = df1, aes(x = group, xend = group, y = value1, yend = value2),
                  color = "blue", size = 8, arrow = arrow(
                    angle = 20, type = "closed", ends = "last", length = unit(1,"cm")))

p2 <- p1 + geom_segment(data = df1, color = "blue", 
                        aes(x = group, xend = group, y = value1, yend = value2), 
                        arrow = arrow(angle = 20, type = "closed", ends = "last",
                                      length = unit(1,"cm")))

p2 + geom_segment(data = df1, colour = "green", size = 8, alpha = 0.25,
                  aes(x = group, xend = group, y = value1, yend = value2))




#---- Gaussian copula ----
require(mvtnorm)
S <- matrix(c(1,.8,.8,1),2,2) #Correlation matrix
AB <- rmvnorm(mean=c(0,0),sig=S,n=1000) #Our gaussian variables
hist(AB[,1])
hist(AB[,2])
cor(AB[,1],AB[,2])

U <- pnorm(AB) #Now U is uniform - check using hist(U[,1]) or hist(U[,2])
hist(U[,1])
hist(U[,2])
cor(U[,1],U[,2])

x <- qgamma(U[,1],2) #x is gamma distributed
y <- qbeta(U[,2],1,2) #y is beta distributed
plot(x,y)
cor(x,y) #They correlate!
