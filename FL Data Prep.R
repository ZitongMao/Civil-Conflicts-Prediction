# FL Data Prep

# This program takes the original FL replication data and imputes the missing data:

setwd()
library(foreign)
options(show.coef.Pvalues=F,scipen=200,show.signif.stars=F)
#
# Load the data, and change the case where onset=4 to onset=1:
fl<-read.dta("repdata.dta")
attach(fl)
table(onset)
fl$onset[fl$onset==4]<-1
#
# Now replicate Fearon & Laitin's regression in Table 1, Column 1 of their article:
## replication of Table 1 Model 1 ##
t1m1 <- glm(as.factor(onset) ~ 
        warl + gdpenl + lpopl1 + lmtnest + ncontig + 
        Oil + nwstate + instab + polity2l
        + ethfrac + relfrac, family = binomial(link = logit),
        data = fl,model=TRUE)
summary(t1m1)


#
# Now impute the missing data, using the procedure from Ward & Bakke (2005):
#
#
# first we have to impute the missing data for this problem
# non missing vectors
#  onset, warl, lmtnest, ncontig, Oil, nwstate
# missing vectors
# lpopl1 (25), instab (14), polity21 (69), gdpenl (255)
# now imputation on gdpenl1, r2=.99
# first take it from the lags:
#lm(formula = fl$gdpenl ~ fl$gdpen)
#              Estimate Std. Error t value
#(Intercept) -0.0753973  0.0058227  -12.95
#fl$gdpen     1.0048375  0.0009982 1006.67
bad<- is.na(fl$gdpenl)
fl$gdpenl[bad]<- -.0753973 + 1.0048375*fl$gdpen[bad]
# this gets imputes 55, leaving 200 missing values.
# deal with polity2l
# deal with polity21, the regime score
fl$polity2l[1857]<- -10
fl[1980 ,49]<- -10
fl[2269 ,49]<- -10
fl[2270 ,49]<- -10
fl[2271 ,49]<- -10
fl[2272 ,49]<- -10
fl[3319 ,49]<- -8
fl[3320 ,49]<- -8
fl[3321 ,49]<- -8
fl[3322 ,49]<- -8
fl[3700 ,49]<- -10
fl[4083 ,49]<- 4
fl[4084 ,49]<- 4
fl[4085 ,49]<- 4
fl[4086 ,49]<- 4
fl[4087 ,49]<- 4
fl[4088 ,49]<- 4
fl[4473 ,49]<- -9
fl[4474 ,49]<- -9
fl[4475 ,49]<- -9
fl[4476 ,49]<- -9
fl[4843 ,49]<- -10
fl[4844 ,49]<- -10
fl[4845 ,49]<- -10
fl[4929 ,49]<- -10
fl[4930 ,49]<- -10
fl[4931 ,49]<- -10
fl[4932 ,49]<- -10
fl[4933 ,49]<- -10
fl[4934 ,49]<- -10
fl[4935 ,49]<- -10
fl[4936 ,49]<- -10
fl[4937 ,49]<- -10
fl[5179 ,49]<- -8
fl[5180 ,49]<- -8
fl[5181 ,49]<- -8
fl[5209 ,49]<- -10
fl[5405 ,49]<- -10
fl[5406 ,49]<- -10
fl[5724 ,49]<- 9
fl[5725 ,49]<- 9
fl[5726 ,49]<- 9
fl[5727 ,49]<- 9
fl[6128 ,49]<- -10
fl[6129 ,49]<- -10
fl[6130 ,49]<- -10
fl[6131 ,49]<- -10
fl[6132 ,49]<- -10
fl[6133 ,49]<- -10
fl[6134 ,49]<- -10
fl[6135 ,49]<- -10
fl[6136 ,49]<- -10
fl[6149 ,49]<- -10
fl[6241 ,49]<- -10
fl[6242 ,49]<- -10
bad<-is.na(fl$polity2l)   
fl[bad,1:5]  
# now deal with instability 
bad<-is.na(fl$instab)
fl[bad,]  
fl[1765 ,50] <- 1 #   260 GERMANYFED. REP. GERMANYF     0 1949 
fl[3321 ,50] <- 1 #   452            GHANA    GHANA     0 1959 
fl[3322 ,50] <- 1 #   452            GHANA    GHANA     0 1960 
fl[4085 ,50] <- 1 #    552         ZIMBABWE ZIMBABWE     0 1967 
fl[4086 ,50] <- 1 #    552         ZIMBABWE ZIMBABWE     0 1968 
fl[4087 ,50] <- 1 #    552         ZIMBABWE ZIMBABWE     0 1969 
fl[4088 ,50] <- 1 #    552         ZIMBABWE ZIMBABWE     0 1970 
fl[4475 ,50] <- 1 #    616          TUNISIA  TUNISIA     0 1958 
fl[4476 ,50] <- 1 #    616          TUNISIA  TUNISIA     0 1959 
fl[4845 ,50] <- 1 #    652            SYRIA    SYRIA     0 1961 
fl[5181 ,50] <- 1 #    690           KUWAIT   KUWAIT     0 1963 
fl[5676 ,50] <- 1 #    740            JAPAN    JAPAN     0 1952 
fl[5726 ,50] <- 1 #    750            INDIA    INDIA     0 1949 
fl[5727 ,50] <- 1 #    750            INDIA    INDIA     0 1950 
bad<-is.na(fl$instab)
fl[bad,] 
# lpopl1 is dealt with as follows:
# 1. Exclude the data on the Federal Republic of German (260) before 1951
#    since it was not really independent before that point and prewar
#    data were for the Third Reich and post 1951 data are for BRD only.
#    68,377; 68,879;  for 1950 and 1951
# 2. The same is essentially true fro the GDR as well. For the 1951 to 1953
#    data, 
#    18,338; 18,351; 18,328; 18,178 for 1950 .. 1953
# for one and two, data were obtained from
# http://www.gesis.org/en/social_monitoring/social_indicators/Data/System/keyindic/population.pdf
#  which collages data from the Federal Statistical Office, Statistical Yearbook
# 3. Austria was occupied until 1955, when it regained its independence.
#    As a result, it is excluded until 1955.
# 4. Libya was not independent until 1952, so it is excluded until
#    1953, owing to lags.
# 5. Interpolate for the 1960 data in Syria, using 4434.
# 6. Japan did not regain independence before 1953(April 28 1952), but
#    www.stat.go.jp has data starting with the 1947 census:
#    81,773; 84,115; for 1949 and 1950.
# 7. Laos was independent in 1953, use 2103 for 1953 lags.
# subset(fl, ccode==260 & year < 1951) # 1761:1766
# subset(fl, ccode==265 & year < 1951) # 1816, 1817
# subset(fl, ccode==305 & year < 1951) # 1913:1918
# subset(fl, ccode==620 & year < 1953) # 4517, 4518
# subset(fl, ccode==812 & year < 1954) # 6148
# subset(fl, ccode==740 & year < 1953) # 5669:5676
omits<-c(1761:1766,1816,1817,1913:1918,4517,4518,6148,5669:5676)
fl.one <- fl[-omits,]
dim(fl.one)
detach(fl)
# fix GDR for 51l, 52l and 53l 
fl.one$lpopl1[fl.one$ccode=="265"][1]<- log(18338) # 51L
fl.one$lpopl1[fl.one$ccode=="265"][2]<- log(18351) # 52L
fl.one$lpopl1[fl.one$ccode=="265"][3]<- log(18328) # 53L
fl.one$lpopl1[fl.one$ccode=="265"][4]<- log(18178) # 54L
# fix Syria for 1960
fl.one$lpopl1[fl.one$ccode=="652"][15]<- log(4434)
# fix Laos for 1954
fl.one$lpopl1[fl.one$ccode=="812"]<- log (2103)
fl.two <-subset(fl.one, select=c(country, year, onset, instab,  warl , gdpenl,lpopl1 , lmtnest , 
                                ncontig ,Oil , nwstate , polity2l, ethfrac , relfrac))  
fl.three<-na.omit(fl.two)
#
# Now perform an equivalent regression using the updated data set:
#
t1.culled <- glm(as.factor(onset) ~ 
        warl + gdpenl + lpopl1 + lmtnest + ncontig + 
        Oil + nwstate + instab + polity2l
        + ethfrac + relfrac, family = binomial(link = logit),
        data = fl.three)
summary(t1.culled)   

# Now save the fl.three dataframe:
save(fl.three,file="fl.three.RData")
