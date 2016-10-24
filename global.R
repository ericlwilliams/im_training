library(data.table)
library(lubridate)
library(data.table)
library(ggplot2)

train.data <- data.table(read.csv("./training_data.csv"))
train.data[,
           `:=`(Swim=as.POSIXct(Swim,format="%T"),
                Bike=as.POSIXct(Bike,format="%T"),
                Run=as.POSIXct(Run,format="%T"),
                Total=as.POSIXct(Total,format="%T"))]

train.data[,
           `:=`(swim_time=hour(Swim)+minute(Swim)/60,
                bike_time=hour(Bike)+minute(Bike)/60,
                run_time=hour(Run)+minute(Run)/60,
                total_time=hour(Total)+minute(Total)/60)]

### Remove TP

train.data <- train.data[!is.na(Week)]
levels(train.data$Program)[levels(train.data$Program) %in% c("TrainingPeaks","","TTB-600ATP")] <- NA
train.data <- train.data[!is.na(Program)]


train.data[,race_week:=max(Week)-Week,Program]
p <- ggplot(train.data,aes(x=-race_week,y=total_time,color=Program)) + geom_line() +
    geom_point() +
        theme_bw() +
        ggtitle("Weekly IM Training") +
            ylab("Weekly Total Training (SBR) Hours") +
                xlab("Weeks Until IM") +
                scale_x_continuous(breaks=seq(-40,0,by=5),
                                   labels=seq(40,0,by=-5)) +
                                       scale_y_continuous(breaks=seq(0,21,by=3),
                                                          labels=seq(0,21,by=3),
                                                          limits=c(3,21))


ggsave("./figures/latest/gr_tot_sbr_time_to_im.png",plot=p)
print(p)

train.data[,`:=`(c_total_time=cumsum(total_time),
                 c_swim_time=cumsum(swim_time),
                 c_bike_time=cumsum(bike_time),
                 c_run_time=cumsum(run_time)),Program]


p <- ggplot(train.data,aes(x=-race_week,y=c_total_time,color=Program)) + geom_line() +
    geom_point() +
        theme_bw() +
        ggtitle("Cumulative IM Training") +
            ylab("Cumulative Training (SBR) Hours") +
                xlab("Weeks Until IM") +
                    scale_x_continuous(breaks=seq(-40,0,by=5),
                                       labels=seq(40,0,by=-5)) +
                                           scale_y_continuous(breaks=seq(0,475,by=25),
                                                              labels=seq(0,475,by=25)) +
                                               theme(legend.position = "top")

ggsave("./figures/latest/gr_c_tot_sbr_time_to_im.png",plot=p)
print(p)

## add prep phase for rest of year

atp.data <- train.data[,list(race_week = 0:51),Program]

atp.data <- merge(atp.data,train.data[,list(total_time),by=c("Program","race_week")],by=c("Program","race_week"),all=TRUE)

atp.data[is.na(total_time),total_time:=7]


atp.data <- atp.data[order(-race_week)]

atp.data[,`:=`(c_total_time=cumsum(total_time)),Program]

p <- ggplot(atp.data,aes(x=-race_week,y=c_total_time,color=Program)) + geom_line() +
    geom_point() +
        theme_bw() +
        ggtitle("Cumulative IM Training") +
            ylab("Cumulative Training (SBR) Hours") +
                xlab("Weeks Until IM") +
                    scale_x_continuous(breaks=seq(-51,0,by=5),
                                       labels=seq(51,0,by=-5)) +
                                           scale_y_continuous(breaks=seq(0,600,by=25),
                                                              labels=seq(0,600,by=25)) +
                                                                  theme(legend.position = "top")

ggsave("./figures/latest/gr_atp_c_tot_sbr_time_to_im.png",plot=p)
print(p)




train.data[,`:=`(run_frac=c_run_time/c_total_time,
                 bike_frac=c_bike_time/c_total_time,
                 swim_frac=c_swim_time/c_total_time),
           Program]

frac.data <- melt(train.data[,list(race_week,run_frac,bike_frac,swim_frac),Program],id.vars = c('race_week','Program'))

avg.data <- frac.data[,list(avg_frac = mean(value),race_week=mean(race_week+1)),by=c("Program","variable")]

avg.data$variable <- ordered(avg.data$variable)

avg.data <- avg.data[order(variable)]
avg.data[,pos_frac := cumsum(avg_frac)-(avg_frac/3),Program]


p <- ggplot(frac.data,aes(x=-race_week,y=value,fill=variable)) +
    geom_bar(stat='identity',position='stack') +
        theme_bw() +
        facet_wrap(~Program) +
            geom_text(data=avg.data,aes(y=pos_frac,label=paste0(round(100*avg_frac,digits=1),"%")),size=10) +
            theme(legend.position = "top") +
                ylab("") +
                    xlab("Weeks Until IM")
ggsave("./figures/latest/gr_fractional_sbr_vs_program.png",plot=p)
print(p)




browser();browser();

## Swimming
p <- ggplot(train.data,aes(x=-race_week,y=swim_time,color=Program)) + geom_line() +
    geom_point() +
        theme_bw() +
        ggtitle("Swim Hours") +
            ylab("Weekly Swim Hours")
print(p)

p <- ggplot(train.data,aes(x=-race_week,y=c_swim_time,color=Program)) + geom_line() +
    geom_point() +
        theme_bw() +
        ggtitle("Cumulative Swim Hours") +
            ylab("Cumulative Swim Hours")
print(p)


## Bike
p <- ggplot(train.data,aes(x=-race_week,y=bike_time,color=Program)) + geom_line() +
    geom_point() +
        ggtitle("Bike Hours") +
            theme_bw() +
            ylab("Weekly Bike Hours")
print(p)

p <- ggplot(train.data,aes(x=-race_week,y=c_bike_time,color=Program)) + geom_line() +
    geom_point() +
        ggtitle("Cumulative Bike Hours") +
            theme_bw() +
                ylab("Cumulative Bike Hours")
print(p)

## Run
p <- ggplot(train.data,aes(x=-race_week,y=run_time,color=Program)) + geom_line() +
    geom_point() +
        ggtitle("Run Hours") +
            theme_bw() +
            ylab("Weekly Run Hours")
print(p)

p <- ggplot(train.data,aes(x=-race_week,y=c_run_time,color=Program)) + geom_line() +
    geom_point() +
        ggtitle("Cumulative Run Hours") +
            theme_bw() +
                ylab("Cumulative Run Hours")
print(p)
