W = c(8, 21, 15, 21, 21, 22, 14)
L = c(5, 10, 12, 14, 17, 14, 19)

Win.Pct = W*100/(W+L)
Year = seq(1946, 1952)
Year = 1946:1952
Age = Year - 1921

plot(Age, Win.Pct)

mean(Win.Pct)
sd(Win.Pct)
length(Win.Pct)
max(Win.Pct)
sort(Win.Pct)
order(Win.Pct)

sum(W)
cumsum(W)
summary(Win.Pct)

W[c(1,2,5)]
W[1:3]
W[-c(1,5)]

Win.Pct>60
Win.Pct[Win.Pct>60]
(W>20) & (Win.Pct>60)
Win.Pct==max(Win.Pct)
Year[Win.Pct==max(Win.Pct)]
Year[(W+L)>30]

NL=c('FLA','STL','HOU','STL','COL','PHI','PHI','SFG','STL','SFG')
AL=c('NYY','BOS','CHW','DET','BOS','TBR','NYY','TEX','TEX','DET')
Winner=c('NL','AL','AL','NL','NL','NL','AL','NL','NL','NL')
N.Games=c(6,4,4,5,4,5,6,5,7,4)
Year=2003:2012

results=matrix(c(NL,AL),10,2)
results
dimnames(results)[[1]]=Year
dimnames(results)[[2]]=c('NL Teams','AL Teams')
results
table(Winner)
barplot(table(Winner))

table(NL)
NL2=factor(NL, levels=c('FLA','PHI','STL','HOU','SFG','COL'))
str(NL2)
table(NL2)

World.Series=list(Winner=Winner, Number.Games=N.Games, Seasons='2003 to 2012')
World.Series$Number.Games
World.Series[[2]]
World.Series['Number.Games']
World.Series[2]

table(Winner)
barplot(table(Winner))
by(N.Games, Winner, summary)

hr.rates=function(age, hr, ab){
  rates=round(100*hr/ab,1)
  list(x=age, y=rates)
}
Age=19:29
Hr=c(33,45,22,45,46,54,44,46,46,55,54)
AB=c(350,455,340,444,562,541,453,541,451,552,455)
HR.Rates=hr.rates(Age, Hr, AB)
plot(hr.rates(Age,Hr,AB))

getwd()
setwd("C:/Users/allen/Desktop/R/baseballdatabank-master/core")
batting=read.csv("Batting.csv")
batting

Mantle=cbind(Age, Hr, AB, Rates=HR.Rates$y)
write.csv(Mantle, 'mantle.csv', row.names=FALSE)
write.csv(Mantle, 'mantles.csv', row.names=TRUE)

batting[1:5, 1:4]
batting[1,]
batting[1:4, c('playerID','stint')]
batting['RBI']
batting$RBI
summary(batting$RBI)
mean(batting$yearID[batting['stint']==min(batting['stint'])])

pitching=read.csv("Pitching.csv")
pitching$FIP=with(pitching, (13*HR+3*BB-2*SO)/(IPouts/3))
pitching[1,]
pos=order(pitching$FIP)
head(pitching[pos, c('ER','FIP')])

pcb=subset(pitching, teamID=='PH1'| teamID=='CL1'| teamID=='BS1')
pcb$teamID=factor(pcb$teamID, levels=c('PH1','CL1'))
by(pcb[,c('W','L','G')], pcb$teamID, summary)
pcb.10=subset(pcb, W>10)

rbind(pitching, batting)
merge(pitching, batting, by='teamID')

install.packages('Lahman')
library(Lahman)
?Batting

Batting.60=subset(Batting, yearID>=1960 & yearID<=1969)
compute.hr=function(pid){
  d=subset(Batting.60, playerID==pid)
  sum(d$HR)
}
players=unique(Batting.60$playerID)
s=sapply(players, compute.hr)
R=data.frame(Player=players, HR=s)
R=R[order(R$HR, decreasing=TRUE), ]
head(R)

library(plyr)
dataframe.AB=ddply(Batting, .(playerID), summarize, Career.AB=sum(AB, na.rm=TRUE))
dataframe.AB
Batting=merge(Batting, dataframe.AB, by='playerID')
Batting.5000=subset(Batting, Career.AB>=5000)

ab.hr.so=function(d){
  c.AB=sum(d$AB, na.rm=TRUE)
  c.HR=sum(d$HR, na.rm=TRUE)
  c.SO=sum(d$SO, na.rm=TRUE)
  data.frame(AB=c.AB, HR=c.HR, SO=c.SO)
}
d.5000=ddply(Batting.5000, .(playerID), ab.hr.so)
head(d.5000)
with(d.5000, plot(HR/AB, SO/AB))
with(d.5000, lines(lowess(HR/AB, SO/AB)))

Batting$Era=cut(Batting$yearID,
                breaks=c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
                labels=c('19th Century','Dead Ball','Lively Ball','Integration','Expansion','Free Agency','Long Ball'))
T.Era=table(Batting$Era)
T.Era
barplot(T.Era)
barplot(T.Era, xlab='Era', ylab='Frequency', main='Era of the Batters')
plot(T.Era)
pie(T.Era)
dotchart(as.numeric(T.Era), labels=names(T.Era), xlab='Frequency')

png('Era of the Batters')
barplot(T.Era, xlab='Era', ylab='Frequency', main='Era of the Batters')
dev.off()
getwd()

dataframe.HR=ddply(Batting, .(playerID), summarize, Career.HR=sum(HR, na.rm=TRUE))
Batting=merge(Batting, dataframe.HR, by='playerID')
HR.500=subset(Batting, Career.HR>=500)
HR.500=HR.500[order(HR.500$Career.HR),]

windows(width=7, height=3.5)
dotchart(HR.500$Career.HR, labels=HR.500$playerID, xlab='HR')
stripchart(HR.500$Career.HR, pch=1, xlab='HR')
stripchart(HR.500$Career.HR, method='jitter', pch=2, xlab='HR')
hist(HR.500$Career.HR, xlab='HR', main='', breaks=seq(500, 800, by=50))

with(HR.500, plot(AB, HR))
with(HR.500, lines(lowess(AB, HR, f=0.3))) 
with(HR.500, identify(AB, HR, labels=playerID, n=3))

with(HR.500, plot(AB, HR, xlim=c(0,650), ylim=c(0,60),
                  pch=19, xlab='At Bat', ylab='Home Run'))
curve(150-x, add=TRUE)
curve(300-x, add=TRUE)
curve(600-x, add=TRUE)
text(100, 40, 'ABHR=100')
text(280, 40, 'ABHR=300')
#with(HR.500, identify(AB, HR, playerID, n=1))

Batting$HR.Rate=with(Batting, HR/AB)
stripchart(HR.Rate~Era, data=Batting)

par(plt=c(0.2, 0.94, 0.145, 0.883))
stripchart(HR.Rate~Era, data=Batting, method='jitter', pch=1, las=2)

par(plt=c(0.2, 0.94, 0.145, 0.883))
with(Batting, boxplot(HR.Rate~Era, las=2, horizontal=TRUE, xlab='HR Rate'))

Master=Master
getinfo=function(firstname, lastname){
  playerline=subset(Master, nameFirst==firstname & nameLast==lastname)
  name.code=as.character(playerline$playerID)
  birthyear=playerline$birthYear
  birthmonth=playerline$birthMonth
  byear=ifelse(birthmonth<=6, birthyear, birthyear+1)
  list(name.code=name.code, byear=byear)
}
ruth.info=getinfo('Babe','Ruth')
aaron.info=getinfo('Hank','Aaron')
bonds.info=getinfo('Barry','Bonds')
arod.info=getinfo('Alex','Rodriguez')
ruth.info
ruth.data=subset(Batting, playerID==ruth.info$name.code)
ruth.data$Age=ruth.data$yearID-ruth.info$byear
aaron.data=subset(Batting, playerID==aaron.info$name.code)
aaron.data$Age=aaron.data$yearID-aaron.info$byear
bonds.data=subset(Batting, playerID==bonds.info$name.code)
bonds.data$Age=bonds.data$yearID-bonds.info$byear
arod.data=subset(Batting, playerID==arod.info$name.code)
arod.data$Age=arod.data$yearID-arod.info$byear

with(ruth.data[order(ruth.data$Age),], plot(Age, cumsum(HR), type='l', lty=3, lwd=2, xlab='Age'
                     , ylab='Career HR', xlim=c(18,45), ylim=c(0,800)))
with(aaron.data[order(aaron.data$Age),], lines(Age, cumsum(HR), lty=2, lwd=2))
with(bonds.data[order(bonds.data$Age),], lines(Age, cumsum(HR), lty=1, lwd=2))
with(arod.data[order(arod.data$Age),], lines(Age, cumsum(HR), lty=4, lwd=2))
legend(20, 800, legend=c('Bonds', 'Aaron', 'Ruth', 'Arod'), lty=1:4, lwd=2)

data1998=read.csv('all1998.csv', header=FALSE)
fields=read.csv('fields.csv')
names(data1998)=fields[,'Header']
retro.ids=read.csv('retrosheetIDs.csv')
sosa.id=as.character(subset(retro.ids, FIRST=='Sammy'& LAST=='Sosa')$ID)
mac.id=as.character(subset(retro.ids, FIRST=='Mark'& LAST=='McGwire')$ID)
sosa.data=subset(data1998, BAT_ID==sosa.id)
mac.data=subset(data1998, BAT_ID==mac.id)
createdata=function(d){
  d$Date=as.Date(substr(d$GAME_ID, 4, 11), format='%Y%m%d')
  d=d[order(d$Date),]
  d$HR=ifelse(d$EVENT_CD==23, 1, 0)
  d$cumHR=cumsum(d$HR)
  d[,c('Date','cumHR')]
}
sosa.hr=createdata(sosa.data)
mac.hr=createdata(mac.data)
head(sosa.hr)
plot(mac.hr, type='l', lwd=2, ylab='Home Runs in the Season')
lines(sosa.hr, lwd=2, col='grey')
abline(h=62, lty=3)
text(10440, 65, '62')
legend(10440, 30, legend=c('McGwire(70)', 'Sosa(66)'), 
       lwd=2, col=c('black','grey'))



teams=Teams
tail(teams)
myteams=subset(teams, yearID>=2000)[, c('teamID','yearID','lgID','G','W','L','R','RA')]
tail(myteams)
myteams$RD=with(myteams, R-RA)
myteams$Wpct=with(myteams, W/(W+L))
with(myteams, plot(RD, Wpct, xlab='run differential', ylab='winning percentage'))
linfit=lm(Wpct~RD, data=myteams)
coef(linfit)
abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

myteams$linWpct=predict(linfit)
myteams$linResiduals=residuals(linfit)
with(myteams, plot(RD, linResiduals, xlab='run differential', ylab='residual'))
abline(h=0, lty=3)
points(x=c(68, 88), y=c(0.0749, -0.0733), pch=19)
text(68, 0.0749, "LAA'08", pos=4, cex=0.8)
text(88, -0.0733, "CLE'06", pos=4, cex=0.8)

mean(myteams$linResiduals)
linRMSE=sqrt(mean(myteams$linResiduals^2))
nrow(subset(myteams, abs(linResiduals)<linRMSE))/nrow(myteams)
nrow(subset(myteams, abs(linResiduals)<2*linRMSE))/nrow(myteams)

myteams$pytWpct=with(myteams, R^2/(R^2+RA^2))
myteams$pytResiduals=myteams$Wpct-myteams$pytWpct
sqrt(mean(myteams$pytResiduals^2))

myteams$logWratio=log(myteams$W/myteams$L)
myteams$logRratio=log(myteams$R/myteams$RA)
pytFit=lm(logWratio~0+logRratio, data=myteams)
pytFit



setwd('C:/Users/allen/Desktop/R/gl1871_2020')
gl2011=read.table('GL2011.txt', sep=',')
setwd('C:/Users/allen/Desktop/R')
glheaders=read.csv('game_log_header.csv')
names(gl2011)=names(glheaders)
BOS2011=subset(gl2011, HomeTeam=='BOS'| VisitingTeam=='BOS')[,c('VisitingTeam',
                                                                'HomeTeam',
                                                                'VisitorRunsScored',
                                                                'HomeRunsScore')]
head(BOS2011)
BOS2011$ScoreDiff=with(BOS2011, ifelse(HomeTeam=='BOS', HomeRunsScore-VisitorRunsScored,
                                       VisitorRunsScored-HomeRunsScore))
BOS2011$W=BOS2011$ScoreDiff>0
aggregate(abs(BOS2011$ScoreDiff), list(W=BOS2011$W), summary)

results=gl2011[,c('VisitingTeam','HomeTeam','VisitorRunsScored','HomeRunsScore')]
results$winner=ifelse(results$VisitorRunsScored>results$HomeRunsScore, 
                      as.character(results$VisitingTeam), as.character(results$HomeTeam))
results$diff=abs(results$VisitorRunsScored-results$HomeRunsScore)
onerungames=subset(results, diff==1)
onerunwins=as.data.frame(table(onerungames$winner))
names(onerunwins)=c('teamID','onerunW')
teams2011=subset(myteams, yearID==2011)
teams2011[teams2011$teamID=='LAA','teamID']='ANA'
teams2011=merge(teams2011, onerunwins)
plot(teams2011$onerunW, teams2011$pytResiduals,
     xlab='one run wins',
     ylab='Pythagorean Residuals')
#identify(teams2011$onerunW, teams2011$pytResiduals, labels=teams2011$teamID, n=2)



pit=Pitching
top_closers=subset(pit, GF>50 & ERA<2.5)[,c('playerID', 'yearID', 'teamID')]
teams_top_closers=merge(myteams, top_closers)
summary(teams_top_closers$pytResiduals)
IR=function(RS=5, RA=5){
  round((RS^2+RA^2)^2/(2*RS*RA^2), 1)
}
IRtable=expand.grid(RS=seq(3, 6, 0.5), RA=seq(3, 6, 0.5))
rbind(head(IRtable), tail(IRtable))
IRtable$IRW=IR(IRtable$RS, IRtable$RA)
xtabs(IRW~RS+RA, data=IRtable)
xtabs(IRW~RA+RS, data=IRtable)



data2011=read.csv('all2011.csv', header=FALSE)
fields=read.csv('fields.csv')
names(data2011)=fields[,'Header']
data2011$RUNS=with(data2011, AWAY_SCORE_CT+HOME_SCORE_CT)
data2011$HALF.INNING=with(data2011, paste(GAME_ID, INN_CT, BAT_HOME_ID))
data2011$RUNS.SCORED=with(data2011, (BAT_DEST_ID>3)+(RUN1_DEST_ID>3)+(RUN2_DEST_ID>3)+
                          (RUN3_DEST_ID>3))
RUNS.SCORED.INNING=aggregate(data2011$RUNS.SCORED, list(HALF.INNING=data2011$HALF.INNING), 
                             sum)
RUNS.SCORED.START=aggregate(data2011$RUNS, list(HALF.INNING=data2011$HALF.INNING), '[', 1)
MAX=data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
MAX$x=RUNS.SCORED.START$x+RUNS.SCORED.INNING$x
data2011=merge(data2011, MAX)
N=ncol(data2011)
names(data2011)[N]='MAX.RUNS'
data2011$RUNS.ROI=with(data2011, MAX.RUNS-RUNS)
RUNNER1=ifelse(as.character(data2011[,'BASE1_RUN_ID'])=='', 0, 1)
RUNNER2=ifelse(as.character(data2011[,'BASE2_RUN_ID'])=='', 0, 1)
RUNNER3=ifelse(as.character(data2011[,'BASE3_RUN_ID'])=='', 0, 1)
get.state=function(runner1, runner2, runner3, outs){
  runners=paste(runner1, runner2, runner3, sep='')
  paste(runners, outs)
}
data2011$STATE=get.state(RUNNER1, RUNNER2, RUNNER3, data2011$OUTS_CT)
data2011$STATE
NRUNNER1=with(data2011, as.numeric(RUN1_DEST_ID==1|BAT_DEST_ID==1))
NRUNNER2=with(data2011, as.numeric(RUN2_DEST_ID==2|RUN1_DEST_ID==2|BAT_DEST_ID==2))
NRUNNER3=with(data2011, as.numeric(RUN3_DEST_ID==3|RUN2_DEST_ID==3|RUN1_DEST_ID==3|BAT_DEST_ID==3))
NOUTS=with(data2011, OUTS_CT+EVENT_OUTS_CT)
data2011$NEW.STATE=get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
data2011=subset(data2011, (STATE!=NEW.STATE)|(RUNS.SCORED>0))       
data2011=subset(data2011, select=-NEWSTATE)
library(plyr)
data.outs=ddply(data2011, .(HALF.INNING), summarize, Outs.Inning=sum(EVENT_OUTS_CT))
data2011=merge(data2011, data.outs)
data2011C=subset(data2011, Outs.Inning==3)
RUNS=with(data2011C, aggregate(RUNS.ROI, list(STATE), mean))
RUNS$Outs=substr(RUNS$Group.1, 5, 5)
RUNS=RUNS[order(RUNS$Outs),]
RUNS.out=matrix(round(RUNS$x, 2), 8, 3)
dimnames(RUNS.out)[[2]]=c('0 outs', '1 out', '2 outs')
dimnames(RUNS.out)[[1]]=c('000', '001', '010', '011', '100', '101', '110', '111')
RUNS.2002=matrix(c(.51, 1.40, 1.14, 1.96, .90, 1.84, 1.51, 2.33,
                   .27, .94, .68, 1.36, .54, 1.18, .94, 1.51,
                   .10, .36, .32, .63, .23, .52, .45, .78), 8, 3)
dimnames(RUNS.2002)=dimnames(RUNS.out)
cbind(RUNS.out, RUNS.2002)



RUNS.POTENTIAL=matrix(c(RUNS$x, rep(0, 8)), 32, 1)
dimnames(RUNS.POTENTIAL)[[1]]=c(RUNS$Group.1, '000 3', '001 3', '010 3', '011 3',
                                '100 3', '101 3', '110 3', '111 3')
data2011$RUNS.STATE=RUNS.POTENTIAL[data2011$STATE, ]
data2011$RUNS.NEW.STATE=RUNS.POTENTIAL[data2011$NEW.STATE, ]
data2011$RUNS.VALUE=data2011$RUNS.NEW.STATE-data2011$RUNS.STATE+data2011$RUNS.SCORED



Roster=read.csv('roster2011.csv')
albert.id=subset(Roster, Last.Name=='Pujols' & First.Name=='Albert')$Player.ID
albert.id=as.character(albert.id)
albert=subset(data2011, BAT_ID==albert.id)
albert=subset(albert, BAT_EVENT_FL=TRUE)
albert[1:2, c('STATE', "NEW.STATE", "RUNS.VALUE")]
albert$RUNNERS=substr(albert$STATE, 1, 3)
table(albert$RUNNERS)
with(albert, stripchart(RUNS.VALUE~RUNNERS, vertical=TRUE, jitter=0.2, method='jitter',
                        xlab='RUNNERS',pch=1, cex=0.8))
abline(h=0)
A.runs=aggregate(albert$RUNS.VALUE, list(albert$RUNNERS), sum)
colnames(A.runs)[2]='RUNS'
A.PA=aggregate(albert$RUNS.VALUE, list(albert$RUNNERS), length)
colnames(A.PA)[2]='PA'
A=merge(A.PA, A.runs)
A
sum(A$RUNS)



data2011b=subset(data2011, BAT_EVENT_FL=TRUE)
runs.sums=aggregate(data2011b$RUNS.VALUE, list(data2011b$BAT_ID), sum)
runs.pa=aggregate(data2011b$RUNS.VALUE, list(data2011b$BAT_ID), length)
runs.start=aggregate(data2011b$RUNS.STATE, list(data2011b$BAT_ID), sum)
colnames(runs.sums)=c('Batter', 'Runs')
colnames(runs.pa)=c('Batter', 'PA')
colnames(runs.start)=c('Batter', 'Runs.Start')
runs=merge(runs.sums, runs.pa)
runs=merge(runs, runs.start)
runs400=subset(runs, PA>=400)
with(runs400, plot(Runs.Start, Runs))
with(runs400, lines(lowess(Runs.Start, Runs)))
abline(h=0)
runs400.top=subset(runs400, Runs>=40)
roster2011=read.csv('roster2011.csv')
runs400.top=merge(runs400.top, roster2011, by.x='Batter', by.y='Player.ID')
with(runs400.top, text(Runs.Start, Runs, Last.Name, pos=1))



get.batting.pos=function(batter){
    TB=table(subset(data2011, BAT_ID==batter)$BAT_LINEUP_ID)
    names(TB)[TB==max(TB)][1]}
position=sapply(runs400$Batter, get.batting.pos)
with(runs400, plot(Runs.Start, Runs, type='n'))
with(runs400, lines(lowess(Runs.Start, Runs)))
abline(h=0)
with(runs400, text(Runs.Start, Runs, position))
