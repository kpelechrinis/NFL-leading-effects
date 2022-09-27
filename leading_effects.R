library(nflfastR)
library(tidyr)
library(lme4)

pbp <- nflfastR::load_pbp(c(2000:2022))

pbp <- pbp[!is.na(pbp$down),] # remove the kickoff downs that have "down == NA"
pbp$drive_id = paste(as.character(pbp$fixed_drive),"-",pbp$game_id)
pbp$defense = paste0(pbp$defteam,"-",pbp$season)
pbp$offense = paste0(pbp$posteam,"-",pbp$season)
dr = unique(pbp$drive_id)

data.df <- data.frame(off=c(),def=c(),startline=c(),ptsdiff=c(),ptsscored=c())
for (d in dr){
    tmp = pbp[pbp$drive_id==d,]
    data.df <- rbind(data.df,data.frame(off=tmp[1,]$offense,def=tmp[1,]$defense,startline=tmp[1,]$yardline_100,ptsdiff=tmp[1,]$score_differential,ptsscored=tmp[1,]$fixed_drive_result))
}

data.df$pts = rep(0,dim(data.df)[1])
data.df = data.df[-which(data.df$ptsscored=="End of half"),]
data.df[which(data.df$ptsscored=="Field goal"),]$pts = 3
data.df[which(data.df$ptsscored=="Opp touchdown"),]$pts = -6
data.df[which(data.df$ptsscored=="Safety"),]$pts = -2
data.df[which(data.df$ptsscored=="Touchdown"),]$pts = 6

tmp = separate(data = data.df, col = off, into = c("o", "season"), sep = "-")
tmp$o = paste0(tmp$o,"-", tmp$season)
colnames(tmp)[1]="off"

b = c() #here we will keep the coefficients
for (y in 2010:2021){
  lmm <- lme4::lmer(pts~ptsdiff+startline+(1|off)+(1|def),data = tmp[which(tmp$season<=y & tmp$season> y-10),])
  b <- append(b,coef(lmm)$def$ptsdiff[1])
}

b.df = data.frame(b=b, season=2010:2021)
ggplot(b.df,aes(x=season,y=b))+geom_point()+geom_smooth()+labs(x="Season",y="Point Differential Fixed Effects")+ylim(c(-0.011,0))+theme_bw(base_size=16)
