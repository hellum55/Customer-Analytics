#------------------------------
# Choice-based conjoint analysis
#------------------------------
# For the exercises in this chapter, we use a simulated conjoint 
# data set where we observe respondents choices from among sets 
# of sportscars. The attributes of # sportscars in this data are 
        # number of seats
        # convertible top
        # transmission type 
        # price
# The data also includes a segment variable that indicates which 
# sportscar segment each respondent belongs to.


# data
sportscar <- read.csv("https://goo.gl/8g7vtT")
str(sportscar)
sportscar$segment  = as.factor(sportscar$segment)
sportscar$seat  = as.factor(sportscar$seat)
sportscar$trans  = as.factor(sportscar$trans)
sportscar$convert  = as.factor(sportscar$convert)
sportscar$price  = as.factor(sportscar$price)


# ex. 1
summary(sportscar)
# I simply re-order the data and for last resp_id 200, the is price = 40
xtabs(choice~trans,data=sportscar)
# trans
# auto manual 
# 1328    672
# consumers mostly prefer auto transmission

# ex. 2
# convert the data to an mlogit.data
library(dfidx)
# add a column with unique question numbers, as needed in mlogit 1.1+
sportscar$chid <- rep(1:(nrow(sportscar)/3), each=3)
# shape the data for mlogit
sportscar.mlogit <- dfidx(sportscar, 
                          choice="choice", 
                    idx=list(c("chid", "resp_id"), "alt" ))
# fitting a nmlogit model
library(mlogit)
m1 <- mlogit(choice ~ 0 + 
               seat + 
               convert + 
               trans + 
               price, 
             data = sportscar.mlogit)
summary(m1)

# What is the ideal sportscar for the respondents based on this model
# ideal car: seat5, convert_yes, trans_auto, price_30

# Which coefficient is the most precisely estimated?
# standard error for convert_yes is the smallest


# treating price as continuous (instead of a factor with levels)
m3 <- mlogit(choice ~ 0 + 
               seat + 
               convert + 
               trans +
               as.numeric(as.character(price)), 
             data = sportscar.mlogit)
summary(m3)
coef(m3)["convertyes"]/(-coef(m3)["as.numeric(as.character(price))"]/1000)
# 1053.01 is the WTP or the price the customer become indifferent between 
# convert being yes versus no; so it is not reasonable to charge 5000.


# set up a function to predict for new data
predict.mnl<-function(model,data){
  data.model<-model.matrix(update(model$formula,0~.),data=data)[,-1]
  utility<-data.model%*%model$coef
  share<-exp(utility)/sum(exp(utility))
  cbind(share,data)
}
# create some new data
newcars <- data.frame(seat=factor(c("2","4", "5")),
                      trans=factor(c("manual", "automatic", "automatic")),
                      convert=factor(c("no", "yes", "no")),
                      price=c(40, 37, 35))
newcars
predict.mnl(m3,newcars)


