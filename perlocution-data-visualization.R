### set working directory

setwd("/Users/sunwoojeong/Desktop/perlocution_experiment/")

### Load the data

dat <- read.csv('perlocution_combined_data_exp1_exp2.csv')

### Clean the labels for the data

dat$sentence_type <- as.character(dat$sentence_type)
dat$sentence_type[dat$sentence_type == "polar_Q"] <- "Polar-Q"
dat$sentence_type[dat$sentence_type == "Wh_Q"] <- "Wh-Q"
dat$sentence_type <- as.factor(dat$sentence_type)

dat$illocution <- as.character(dat$illocution)
dat$illocution[dat$illocution == "info_g"] <- "info-g"
dat$illocution[dat$illocution == "info_s"] <- "info-s"
dat$illocution <- as.factor(dat$illocution)

### Subdivide the data

dat_pq <- dat[dat$sentence_type == 'Polar-Q', ]
dat_wh <- dat[dat$sentence_type == 'Wh-Q', ]
dat_dec <- dat[dat$sentence_type == 'Declarative', ]
dat_imp <- dat[dat$sentence_type == 'Imperative', ]

dat_dec <- dat[dat$sentence.type == 'Decl', ]
dat_imp <- dat[dat$sentence.type == 'Impr', ]

### Counts on illocutions

## polar interrogatives

summary(dat_pq)

dat_pq$illocution <- as.character(dat_pq$illocution)
dat_pq$illocution[dat_pq$illocution == "accusation"] <- "accuse"
dat_pq$illocution <- as.factor(dat_pq$illocution)

# data grouped according to sentence content (precoded illocutionary biases)

dat_pq_s <- dat_pq[dat_pq$bias == 'info_s', ]
dat_pq_r <- dat_pq[dat_pq$bias == 'request', ]
dat_pq_i <- dat_pq[dat_pq$bias == 'invitation', ]
dat_pq_pro <- dat_pq[dat_pq$question == 'Problem', ]
dat_pq_lan <- dat_pq[dat_pq$question == 'Laundry', ]

# plots on illocution (in percentage)

ggplot(dat_pq_s, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
ggplot(dat_pq_r, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
ggplot(dat_pq_i, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
ggplot(dat_pq_pro, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)/sum(..count..)), position = 'dodge', colour='black')
ggplot(dat_pq_lan, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')

# plots on illocution (in raw counts)

# info-seeking bias sentences

dat_pq_s <- dat_pq_s[dat_pq_s$illocution != 'invitation', ]
dat_pq_s$illocution <- factor(dat_pq_s$illocution, levels= c("info-s", "request", "accuse"))
dat_pq_s$illocution <- factor(dat_pq_s$illocution, levels= c("accuse", "request", "info-s"))
s <- ggplot(dat_pq_s, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
p1 <- s + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + labs(y='illocution count') + ggtitle("info-seeking bias") + theme(plot.title=element_text(size=23)) + scale_y_continuous(limits=c(0,160)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p1
ggsave(p1, file="pq-il-s-050816.pdf",width=4.85,height=3.48)

# request bias sentences

dat_pq_r <- dat_pq_r[dat_pq_r$illocution != 'accuse', ]
dat_pq_r$illocution <- factor(dat_pq_r$illocution, levels= c("info-s", "request", "invitation"))
dat_pq_r$illocution <- factor(dat_pq_r$illocution, levels= c("invitation", "request", "info-s"))
r <- ggplot(dat_pq_r, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
p2 <- r + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + labs(y='illocution count') + ggtitle("request bias") + theme(plot.title=element_text(size=23)) + scale_y_continuous(limits=c(0,160)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p2
ggsave(p2, file="pq-il-req-050816.pdf",width=4.85,height=3.48)

# invitation bias sentences

dat_pq_i <- dat_pq_i[dat_pq_i$illocution != 'accuse', ]
dat_pq_i$illocution <- factor(dat_pq_i$illocution, levels= c("info-s", "request", "invitation"))
i <- ggplot(dat_pq_i, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
p2 <- i + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + labs(y='illocution count') + ggtitle("invitation bias") + theme(plot.title=element_text(size=23)) + scale_y_continuous(limits=c(0,160)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p2
ggsave(p2, file="pq-il-inv-050816.pdf",width=4.85,height=3.48)

# 'Do you have a problem?' - illocution

dat_pq_pro$illocution <- factor(dat_pq_pro$illocution, levels= c("info-s", "request", "accuse"))
dat_pq_pro$illocution <- factor(dat_pq_pro$illocution, levels= c("accuse", "request", "info-s"))
pro <- ggplot(dat_pq_pro, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
p3 <- pro + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + labs(y='illocution count') + ggtitle("'Do you have a problem?'") + theme(plot.title=element_text(size=21)) + scale_y_continuous(limits=c(0,30)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p3
ggsave(p3, file="pq-il-problem-050816.pdf",width=4.85,height=3.48)

# 'Do you want to do the laundry?' - illocution

dat_pq_lan <- dat_pq_lan[dat_pq_lan$illocution != 'accuse', ]
dat_pq_lan$illocution <- factor(dat_pq_lan$illocution, levels= c("info-s", "request", "invitation"))
dat_pq_lan$illocution <- factor(dat_pq_lan$illocution, levels= c("invitation", "request", "info-s"))
lan <- ggplot(dat_pq_lan, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
p4 <- lan + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + labs(y='illocution count') + ggtitle("'Do you want to do the laundry?'") + theme(plot.title=element_text(size=21)) + scale_y_continuous(limits=c(0,30)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p4
ggsave(p4, file="pq-il-laundry-050816.pdf",width=4.85,height=3.48)


## Declaratives

dat_dec <- dat[dat$sentence_type == 'Declarative', ]

# Not using the low rising intonation data for now

dat_dec <- dat_dec[dat_dec$intonation != 'L_rise', ]

dat_dec$illocution <- as.character(dat_dec$illocution)
dat_dec$illocution[dat_dec$illocution == "accusation"] <- "accuse"
dat_dec$illocution <- as.factor(dat_dec$illocution)

dat_dec$intonation <- as.character(dat_dec$intonation)

# Rename the H_rise data
dat_dec$intonation[dat_dec$intonation == "H_rise"] <- "Rise"

dat_dec$intonation[dat_dec$intonation == "L_rise"] <- "L-rise"
dat_dec$intonation <- as.factor(dat_dec$intonation)
dat_dec$intonation <- factor(dat_dec$intonation, levels= c("Fall", "Level", "L-rise", "Rise"))
summary(dat_dec$question)

# Group according to sentence content

dat_dec_man <- dat_dec[dat_dec$question == 'Manatee', ]
dat_dec_hip <- dat_dec[dat_dec$question == 'Hippo', ]
dat_dec_g <- rbind(dat_dec_man, dat_dec_hip)

dat_dec_bee <- dat_dec[dat_dec$question == 'Beer', ]
dat_dec_dan <- dat_dec[dat_dec$question == 'Dancing', ]
dat_dec_i <- rbind(dat_dec_bee, dat_dec_dan)

dat_dec_box <- dat_dec[dat_dec$question == 'Box', ]
dat_dec_win <- dat_dec[dat_dec$question == 'Close', ]
dat_dec_r <- rbind(dat_dec_box, dat_dec_win)

dat_dec_cal <- dat_dec[dat_dec$question == 'Call', ]

# plots on illocution (in percentage)

ggplot(dat_dec_bee, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
ggplot(dat_dec_man, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
ggplot(dat_dec_box, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')

ggplot(dat_dec_cal, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
ggplot(dat_dec_g, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
ggplot(dat_dec_i, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
ggplot(dat_dec_r, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')

# plots on illocution (in raw counts)

# info-giving bias sentences

dat_dec_g <- rbind(dat_dec_g, cbind(expand.grid(illocution=levels(dat_dec$illocution), intonation=levels(dat_dec$intonation)), subject=NA, sentence_type=NA, question=NA, speaker=NA, annoyance=NA, authority=NA, polite=NA,stance=NA,illoc_2=NA,perloc_2=NA, gender=NA, age=NA, race=NA, region=NA, bias=NA, exp_date=NA, speaker_gen=NA, X=NA))
dat_dec_g <- dat_dec_g[dat_dec_g$illocution != 'request', ]
dat_dec_g <- dat_dec_g[dat_dec_g$illocution != 'invitation', ]
dat_dec_g$illocution <- factor(dat_dec_g$illocution, levels= c("info-s", "accuse", "info-g"))
dat_dec_g$illocution <- factor(dat_dec_g$illocution, levels= c("info-s", "info-g", "accuse"))
g <- ggplot(dat_dec_g, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
p1 <- g + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + ggtitle("info-giving bias") + theme(plot.title=element_text(size=23)) + scale_y_continuous(limits=c(0,70)) + labs(y='illocution count') + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p1
ggsave(p1, file="dec-il-infog-050816.pdf",width=4.85,height=3.48)

# invitation bias sentences

dat_dec_i <- rbind(dat_dec_i, cbind(expand.grid(illocution=levels(dat_dec$illocution), intonation=levels(dat_dec$intonation)), subject=NA, sentence_type=NA, question=NA, speaker=NA, annoyance=NA, authority=NA, polite=NA,stance=NA,illoc_2=NA,perloc_2=NA, gender=NA, age=NA, race=NA, region=NA, bias=NA, exp_date=NA, speaker_gen=NA, X=NA))
dat_dec_i <- dat_dec_i[dat_dec_i$illocution != 'accuse', ]
dat_dec_i <- dat_dec_i[dat_dec_i$illocution != 'info-g', ]
dat_dec_i$illocution <- factor(dat_dec_i$illocution, levels= c("info-s", "request", "invitation"))
dat_dec_i$illocution <- factor(dat_dec_i$illocution, levels= c("invitation", "request", "info-s"))
i <- ggplot(dat_dec_i, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
p2 <- i + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + ggtitle("invitation bias") + theme(plot.title=element_text(size=23)) + scale_y_continuous(limits=c(0,70)) + labs(y='illocution count') + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p2
ggsave(p2, file="dec-il-inv-050816.pdf",width=4.85,height=3.48)

# request bias sentences

dat_dec_r <- rbind(dat_dec_r, cbind(expand.grid(illocution=levels(dat_dec$illocution), intonation=levels(dat_dec$intonation)), subject=NA, sentence_type=NA, question=NA, speaker=NA, annoyance=NA, authority=NA, polite=NA,stance=NA,illoc_2=NA,perloc_2=NA, gender=NA, age=NA, race=NA, region=NA, bias=NA, exp_date=NA, speaker_gen=NA, X=NA))
dat_dec_r <- dat_dec_r[dat_dec_r$illocution != 'accuse', ]
dat_dec_r <- dat_dec_r[dat_dec_r$illocution != 'invitation', ]
dat_dec_r$illocution <- factor(dat_dec_r$illocution, levels= c("info-s", "request", "info-g"))
r <- ggplot(dat_dec_r, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
p3 <- r + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + ggtitle("request bias") + theme(plot.title=element_text(size=23)) + scale_y_continuous(limits=c(0,70)) + labs(y='illocution count') + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p3
ggsave(p3, file="dec-il-req-050816.pdf",width=4.85,height=3.48)

# 'You didn't call me back.' - illocution

dat_dec_cal <- rbind(dat_dec_cal, cbind(expand.grid(illocution=levels(dat_dec$illocution), intonation=levels(dat_dec$intonation)), subject=NA, sentence_type=NA, question=NA, speaker=NA, annoyance=NA, authority=NA, polite=NA,stance=NA,illoc_2=NA,perloc_2=NA, gender=NA, age=NA, race=NA, region=NA, bias=NA, exp_date=NA, speaker_gen=NA, X=NA))
dat_dec_cal <- dat_dec_cal[dat_dec_cal$illocution != 'request', ]
dat_dec_cal <- dat_dec_cal[dat_dec_cal$illocution != 'invitation', ]
dat_dec_cal$illocution <- factor(dat_dec_cal$illocution, levels= c("info-s", "info-g", "accuse"))
dat_dec_cal$illocution <- factor(dat_dec_cal$illocution, levels= c("info-s", "accuse", "info-g"))
c <- ggplot(dat_dec_cal, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
p4 <- c + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + labs(y='illocution count') + ggtitle("'You didn't call me back.'") + theme(plot.title=element_text(size=21)) + scale_y_continuous(limits=c(0,35)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p4
ggsave(p4, file="dec-il-call-050816.pdf",width=4.85,height=3.48)


## Imperatives

dat_imp <- dat[dat$sentence_type == 'Imperative', ]

# not using high level intonation for now

dat_imp <- dat_imp[dat_imp$intonation != "H_Level", ]
dat_imp <- dat_imp[dat_imp$intonation != "L_rise", ]
dat_imp$intonation <- as.character(dat_imp$intonation)
dat_imp$intonation[dat_imp$intonation == "H_rise"] <- "Rise"
dat_imp$intonation <- as.factor(dat_imp$intonation)
summary(dat_imp$bias)
dat_imp_r <- dat_imp[dat_imp$bias == 'command', ]
dat_imp_w <- dat_imp[dat_imp$bias == 'wish', ]

# plot illocutions (in percentage)

ggplot(dat_imp_r, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
ggplot(dat_imp_w, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')

# plot illocution (in raw counts)

# request

dat_imp_r <- rbind(dat_imp_r, cbind(expand.grid(illocution=levels(dat_imp_r$illocution), intonation=levels(dat_imp_r$intonation)), subject=NA, sentence_type=NA, question=NA, speaker=NA, annoyance=NA, authority=NA, polite=NA,stance=NA,illoc_2=NA,perloc_2=NA, gender=NA, age=NA, race=NA, region=NA, bias=NA, exp_date=NA, speaker_gen=NA, X=NA))
dat_imp_r <- dat_imp_r[dat_imp_r$illocution != 'wish', ]
dat_imp_r <- dat_imp_r[dat_imp_r$illocution != 'info-g', ]
dat_imp_r$illocution <- factor(dat_imp_r$illocution, levels= c("request", "suggestion", "info-s"))
r <- ggplot(dat_imp_r, aes(x=illocution, fill=intonation)) + geom_bar(aes(y=(..count..)), position = 'dodge', colour='black')
p1 <- r + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + labs(y='illocution count') + ggtitle("request bias") + theme(plot.title=element_text(size=21)) + scale_y_continuous(limits=c(0,35)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p1
ggsave(p1, file="imp-il-req-033016.pdf",width=4.85,height=3.48)

### Visualize perlocution data
## First retrieve a summary of the data

## Summarize means, standard deviations, standard errors, etc.
## Use the summarySE function below.

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
## End of summarySE function

## summarize all sentence types

dat <- dat[dat$intonation != 'L_rise', ]
dat <- dat[dat$intonation != 'H_Level', ]
dat$intonation <- as.character(dat$intonation)
dat$intonation[dat$intonation == "H_rise"] <- "Rise"
dat$intonation <- as.factor(dat$intonation)

dat$sentence_type <- as.character(dat$sentence_type)
dat$sentence_type[dat$sentence_type == "Declarative"] <- "Decl"
dat$sentence_type[dat$sentence_type == "Imperative"] <- "Impr"
dat$sentence_type <- as.factor(dat$sentence_type)

dat$sentence_type <- factor(dat$sentence_type, levels= c("Polar-Q", "Impr", "Wh-Q", "Decl"))

annoyance_sum <- summarySE(dat, measurevar="annoyance", groupvars=c("sentence.type", "intonation"))
authority_sum <- summarySE(dat, measurevar="authority", groupvars=c("sentence.type", "intonation"))
polite_sum <- summarySE(dat, measurevar="polite", groupvars=c("sentence.type", "intonation"))
stance_sum <- summarySE(dat, measurevar="stance", groupvars=c("sentence.type", "intonation"))

# all sentence type: annoyance

annoyance_plot = ggplot(annoyance_sum, aes(x=sentence.type, y=annoyance, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=annoyance-se, ymax=annoyance+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p1 <- annoyance_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,70)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p1
ggsave(p1, file="nst-annoyance-032516.pdf",width=4.86,height=3.45)

# all sentence type: authority

authority_plot = ggplot(authority_sum, aes(x=sentence.type, y=authority, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=authority-se, ymax=authority+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p2 <- authority_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,70)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p2
ggsave(p2, file="nst-authority-032516.pdf",width=4.86,height=3.45)

# all sentence type: politeness 

polite_plot = ggplot(polite_sum, aes(x=sentence.type, y=polite, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=polite-se, ymax=polite+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p3 <- polite_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + labs(y='politeness') + scale_y_continuous(limits=c(0,70)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p3
ggsave(p3, file="nst-polite-032516.pdf",width=4.86,height=3.45)

# all sentence type: stance

stance_plot = ggplot(stance_sum, aes(x=sentence.type, y=stance, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=stance-se, ymax=stance+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p4 <- stance_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,70)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p4
ggsave(p4, file="nst-stance-032516.pdf",width=4.86,height=3.45)

## summarize polar-interrogative data

dat_pq <- dat_pq[dat_pq$illocution != 'accusation', ]

annoyance_sum <- summarySE(dat_pq, measurevar="annoyance", groupvars=c("illocution", "intonation"))
authority_sum <- summarySE(dat_pq, measurevar="authority", groupvars=c("illocution", "intonation"))
polite_sum <- summarySE(dat_pq, measurevar="polite", groupvars=c("illocution", "intonation"))
stance_sum <- summarySE(dat_pq, measurevar="stance", groupvars=c("illocution", "intonation"))

# polar-Q annoyance

annoyance_plot = ggplot(annoyance_sum, aes(x=illocution, y=annoyance, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=annoyance-se, ymax=annoyance+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p1 <- annoyance_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,65)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p1
ggsave(p1, file="npolarq-annoyance2-032516.pdf",width=4.86,height=3.45)

# polar-Q authority

authority_plot = ggplot(authority_sum, aes(x=illocution, y=authority, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=authority-se, ymax=authority+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p2 <- authority_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,65)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p2
ggsave(p2, file="npolarq-authority2-032516.pdf",width=4.86,height=3.45)

# polar-Q politeness 

polite_plot = ggplot(polite_sum, aes(x=illocution, y=polite, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=polite-se, ymax=polite+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p3 <- polite_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + labs(y='politeness') + scale_y_continuous(limits=c(0,85)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p3
ggsave(p3, file="npolarq-polite2-032516.pdf",width=4.86,height=3.45)

# polar-Q stance

stance_plot = ggplot(stance_sum, aes(x=illocution, y=stance, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=stance-se, ymax=stance+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p4 <- stance_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,85)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p4
ggsave(p4, file="npolarq-stance2-032516.pdf",width=4.86,height=3.45)


## summarize wh-interrogative data

dat_wh <- dat_wh[dat_wh$illocution != 'accusation', ]

annoyance_sum <- summarySE(dat_wh, measurevar="annoyance", groupvars=c("illocution", "intonation"))
authority_sum <- summarySE(dat_wh, measurevar="authority", groupvars=c("illocution", "intonation"))
polite_sum <- summarySE(dat_wh, measurevar="polite", groupvars=c("illocution", "intonation"))
stance_sum <- summarySE(dat_wh, measurevar="stance", groupvars=c("illocution", "intonation"))

# wh-Q annoyance

annoyance_plot = ggplot(annoyance_sum, aes(x=illocution, y=annoyance, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=annoyance-se, ymax=annoyance+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p1 <- annoyance_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,65)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p1
ggsave(p1, file="nwhq-annoyance-032516.pdf",width=4.86,height=3.45)

# wh-Q authority

authority_plot = ggplot(authority_sum, aes(x=illocution, y=authority, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=authority-se, ymax=authority+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p2 <- authority_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,65)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p2
ggsave(p2, file="nwhq-authority-032516.pdf",width=4.86,height=3.45)

# wh-Q politeness 

polite_plot = ggplot(polite_sum, aes(x=illocution, y=polite, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=polite-se, ymax=polite+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p3 <- polite_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + labs(y='politeness') + scale_y_continuous(limits=c(0,85)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p3
ggsave(p3, file="nwhq-polite-032516.pdf",width=4.86,height=3.45)

# wh-Q stance

stance_plot = ggplot(stance_sum, aes(x=illocution, y=stance, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=stance-se, ymax=stance+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p4 <- stance_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,85)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p4
ggsave(p4, file="nwhq-stance-032516.pdf",width=4.86,height=3.45)


## summarize imperative data

dat_imp <- dat_imp[dat_imp$illocution != 'accusation', ]
dat_imp <- dat_imp[dat_imp$illocution != 'info-g', ]
dat_imp <- dat_imp[dat_imp$illocution != 'info-s', ]
dat_imp <- dat_imp[dat_imp$illocution != 'invitation', ]
dat_imp <- dat_imp[dat_imp$intonation != 'H_Level', ]

dat_imp$illocution <- as.character(dat_imp$illocution)
dat_imp$illocution[dat_imp$illocution == "suggestion"] <- "info-g"
dat_imp$illocution[dat_imp$illocution == "suggestion"] <- "sugg"
dat_imp$illocution <- as.factor(dat_imp$illocution)
dat_imp$illocution <- factor(dat_imp$illocution, levels= c("request", "wish", "info-g"))
dat_imp$illocution <- factor(dat_imp$illocution, levels= c("sugg", "wish", "request"))

annoyance_sum <- summarySE(dat_imp, measurevar="annoyance", groupvars=c("illocution", "intonation"))
authority_sum <- summarySE(dat_imp, measurevar="authority", groupvars=c("illocution", "intonation"))
polite_sum <- summarySE(dat_imp, measurevar="polite", groupvars=c("illocution", "intonation"))
stance_sum <- summarySE(dat_imp, measurevar="stance", groupvars=c("illocution", "intonation"))

# imperative annoyance

annoyance_plot = ggplot(annoyance_sum, aes(x=illocution, y=annoyance, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=annoyance-se, ymax=annoyance+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p1 <- annoyance_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,65)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p1
ggsave(p1, file="imp-annoyance-050816.pdf",width=4.86,height=3.45)

# imperative authority

authority_plot = ggplot(authority_sum, aes(x=illocution, y=authority, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=authority-se, ymax=authority+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p2 <- authority_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,85)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p2
ggsave(p2, file="imp-authority-050816.pdf",width=4.86,height=3.45)

# imperative politeness 

polite_plot = ggplot(polite_sum, aes(x=illocution, y=polite, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=polite-se, ymax=polite+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p3 <- polite_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + labs(y='politeness') + scale_y_continuous(limits=c(0,85)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p3
ggsave(p3, file="imp-polite-050816.pdf",width=4.86,height=3.45)

# imperative stance

stance_plot = ggplot(stance_sum, aes(x=illocution, y=stance, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=stance-se, ymax=stance+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p4 <- stance_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=23), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,85)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p4
ggsave(p4, file="imp-stance-050816.pdf",width=4.86,height=3.45)


## summarize declarative data

dat_dec <- dat_dec[dat_dec$illocution != 'accusation', ]
dat_dec <- dat_dec[dat_dec$illocution != 'invitation', ]
dat_dec <- dat_dec[dat_dec$illocution != 'info-s', ]
dat_dec <- dat_dec[dat_dec$illocution != 'wish', ]
dat_dec <- dat_dec[dat_dec$illocution != 'suggestion', ]

dat_dec <- dat_dec[dat_dec$intonation != 'L_rise', ]
dat_dec <- dat_dec[dat_dec$intonation != 'H_Level', ]
dat_dec$intonation <- as.character(dat_dec$intonation)
dat_dec$intonation[dat_dec$intonation == "H_rise"] <- "Rise"
dat_dec$intonation <- as.factor(dat_dec$intonation)

dat_dec$illocution <- factor(dat_dec$illocution, levels= c("info-g", "invitation", "request", "info-s"))
dat_dec$illocution <- factor(dat_dec$illocution, levels= c("info-g", "invitation", "request"))

annoyance_sum <- summarySE(dat_dec, measurevar="annoyance", groupvars=c("illocution", "intonation"))
authority_sum <- summarySE(dat_dec, measurevar="authority", groupvars=c("illocution", "intonation"))
polite_sum <- summarySE(dat_dec, measurevar="polite", groupvars=c("illocution", "intonation"))
stance_sum <- summarySE(dat_dec, measurevar="stance", groupvars=c("illocution", "intonation"))

annoyance_sum <- rbind(annoyance_sum, cbind(expand.grid(illocution=levels(annoyance_sum$illocution), intonation=levels(annoyance_sum$intonation)), annoyance=NA, sd=NA, se=NA, ci=NA, N=NA))
authority_sum <- rbind(authority_sum, cbind(expand.grid(illocution=levels(authority_sum$illocution), intonation=levels(authority_sum$intonation)), authority=NA, sd=NA, se=NA, ci=NA, N=NA))
polite_sum <- rbind(polite_sum, cbind(expand.grid(illocution=levels(polite_sum$illocution), intonation=levels(polite_sum$intonation)), polite=NA, sd=NA, se=NA, ci=NA, N=NA))
stance_sum <- rbind(stance_sum, cbind(expand.grid(illocution=levels(stance_sum$illocution), intonation=levels(stance_sum$intonation)), stance=NA, sd=NA, se=NA, ci=NA, N=NA))

# declarative annoyance

annoyance_plot = ggplot(annoyance_sum, aes(x=illocution, y=annoyance, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=annoyance-se, ymax=annoyance+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p1 <- annoyance_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,65)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p1
ggsave(p1, file="dec-annoyance-0505516.pdf",width=4.86,height=3.45)

# declarative authority

authority_plot = ggplot(authority_sum, aes(x=illocution, y=authority, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=authority-se, ymax=authority+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p2 <- authority_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,85)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p2
ggsave(p2, file="dec-authority-050516.pdf",width=4.86,height=3.45)

# declarative politeness 

polite_plot = ggplot(polite_sum, aes(x=illocution, y=polite, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=polite-se, ymax=polite+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p3 <- polite_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + labs(y='politeness') + scale_y_continuous(limits=c(0,85)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p3
ggsave(p3, file="dec-polite-050516.pdf",width=4.86,height=3.45)

# declarative stance

stance_plot = ggplot(stance_sum, aes(x=illocution, y=stance, fill=intonation)) + 
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(aes(ymin=stance-se, ymax=stance+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p4 <- stance_plot + theme_bw() + theme(legend.position="none") + theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=20), axis.title.y=element_text(size=28), axis.title.x=element_blank(), plot.margin=unit(c(1,1,2,0), "mm")) + scale_y_continuous(limits=c(0,85)) + scale_fill_manual(values=c("#D55E00", "#009E73", "#56B4E9"))
p4
ggsave(p4, file="dec-stance-050516.pdf",width=4.86,height=3.45)



