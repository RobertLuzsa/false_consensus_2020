# ==== 1. Read data ====

x <- read.table("data_experiment1.csv", sep="\t", quote="\"", header=T)

# ==== 2. Clean data ====

x <- x[!(x$CASE==6),]   # Case left from pretesting

# Exclude cases with too quick or slow completion times

x$time_sum_real <- rowSums(
  subset(x, 
         select = c("TIME002", "TIME003", "TIME004",  
                    "TIME006", "TIME007", "TIME008", "TIME009", "TIME010",
                    "TIME011", "TIME012", "TIME013", "TIME014",  
                    "TIME017", "TIME018",  "TIME020",
                    "TIME022", "TIME024", 
                    "TIME026",  "TIME028",  "TIME030",
                     "TIME032",  "TIME034", 
                    "TIME036", "TIME037", "TIME038", "TIME039", "TIME040"))
)

library(psych)

x <- subset(x,
            x$time_sum_real < 3600 &
              x$time_sum_real > 300)


describe(x$time_sum_real)

real_time_sd = sd(x$time_sum_real)
real_time_mean = mean(x$time_sum_real)

x2 <- subset(x, 
             (x$time_sum_real <= real_time_mean + 2 * real_time_sd) &
               (x$time_sum_real >=real_time_mean - 2 * real_time_sd))
nrow(x2)



# Exclude all older than 35 who are very few and might distort the results while being non-representative

table(x2$FB04_01)
x3 <- subset(x2,
             x2$FB04_01 <= 35)
nrow(x3)



# ==== 3. Setting factors and basic calculations ====

x3$FB03 <- factor(x3$FB03)


# Determine condition Attitudinal congruency bed_anz 1=congruent, 2=incongruent


x3$t1_anz_p <- NA
x3$t1_t<-NA
for (p in 1:nrow(x3)) {
  n <- 0
  for (i in list("V101_02","V102_02","V103_02","V104_02")) {
    if (x3[[p,i]]==1) {n <- n + 1}
  }
  x3$t1_anz_p[p] <- n
  x3$t1_t[p] <- x3$V101_05[p]
}

meinI <- c("E005_01", "E006_01", "E007_01","E008_01", "E009_01",
           "E010_01", "E011_01", "E012_01","E013_01")

x3$t1_mein <- NA
for (p in 1:nrow(x3)) {x3$t1_mein[p] <- x3[[p,meinI[x3$t1_t[p]]]]}

x3$t1_mein_d <- NA
x3$t1_mein_d[x3$t1_mein <=3] <- 2
x3$t1_mein_d[x3$t1_mein > 3] <- 1

x3$bed_anz <- NA
x3$bed_anz[x3$t1_mein_d==1 & x3$t1_anz_p ==3] <- 1 # congruent
x3$bed_anz[x3$t1_mein_d==2 & x3$t1_anz_p ==1] <- 1 # congruent
x3$bed_anz[x3$t1_mein_d==1 & x3$t1_anz_p ==1] <- 2 # incongruent
x3$bed_anz[x3$t1_mein_d==2 & x3$t1_anz_p ==3] <- 2 # incongruent

# Determine condition Endorsement bed_like 1=most likes for congruent, 2=most for incongruent

x3$t1_beimein <- x3$V101_02
x3$t1_beilike <- x3$V101_03

x3$bed_like <- NA
x3$bed_like[x3$t1_beimein==x3$t1_mein_d & x3$t1_beilike > 1000] <- 1 # most for congruent
x3$bed_like[x3$t1_beimein==x3$t1_mein_d & x3$t1_beilike < 1000] <- 2 # most for incongruent
x3$bed_like[x3$t1_beimein!=x3$t1_mein_d & x3$t1_beilike > 1000] <- 2 # most for incongruent
x3$bed_like[x3$t1_beimein!=x3$t1_mein_d & x3$t1_beilike < 1000] <- 1 # most for congruent

x3$bedingung <- NA
x3$bedingung[x3$bed_anz == 1 & x3$bed_like ==1] <- 3
x3$bedingung[x3$bed_anz == 2 & x3$bed_like ==1] <- 2
x3$bedingung[x3$bed_anz == 1 & x3$bed_like ==2] <- 1
x3$bedingung[x3$bed_anz == 2 & x3$bed_like ==2] <- 0


# ==== 4. Descriptive Statistics ====

# Experimental groups
table(x3$bed_like, x3$bed_anz)

# Sociodemographics

describe(x3$FB04_01)
describe(x3$time_sum_real)

table(x3$FB03)
prop.table(table(x3$FB03))

table(x3$FB05)
prop.table(table(x3$FB05))

table(x3$FB06)
prop.table(table(x3$FB06))

# Table with attitudes, population estimates for pro- and contra-attitude-participants, and attitude-estimate correlations
# --> See point 6

# ==== 5. Main analysis of False Consensus Effect ====

library(lme4)
library(lmerTest)

x4a <- reshape(x3,
               varying=list(
                 c("E005_01", "E006_01", "E007_01","E008_01", "E009_01",
                   "E010_01", "E011_01", "E012_01","E013_01"),
                 c("E503_01", "E503_02", "E503_03","E503_04","E503_05",
                   "E503_06", "E503_07", "E503_08","E503_09")),
               direction="long",
               v.names=c("einst_v","fc"),
               timevar="thema",
               idvar="CASE")

# Variables for presentation order

x3$reihe1 <- NA
x3$reihe2 <- NA
x3$reihe3 <- NA
x3$reihe4 <- NA
x3$reihe5 <- NA
x3$reihe6 <- NA
x3$reihe7 <- NA
x3$reihe8 <- NA
x3$reihe9 <- NA

for(i in 1:nrow(x3)){

if (x3$V001_06[i] == 1) {x3$reihe1[i] <- 1}
if (x3$V002_06[i] == 1) {x3$reihe1[i] <- 2}
if (x3$V003_06[i] == 1) {x3$reihe1[i] <- 3}
if (x3$V004_06[i] == 1) {x3$reihe1[i] <- 4}
if (x3$V005_06[i] == 1) {x3$reihe1[i] <- 5}
if (x3$V006_06[i] == 1) {x3$reihe1[i] <- 6}
if (x3$V007_06[i] == 1) {x3$reihe1[i] <- 7}
if (x3$V008_06[i] == 1) {x3$reihe1[i] <- 8}
if (x3$V009_06[i] == 1) {x3$reihe1[i] <- 9}

if (x3$V001_06[i] == 2) {x3$reihe2[i] <- 1}
if (x3$V002_06[i] == 2) {x3$reihe2[i] <- 2}
if (x3$V003_06[i] == 2) {x3$reihe2[i] <- 3}
if (x3$V004_06[i] == 2) {x3$reihe2[i] <- 4}
if (x3$V005_06[i] == 2) {x3$reihe2[i] <- 5}
if (x3$V006_06[i] == 2) {x3$reihe2[i] <- 6}
if (x3$V007_06[i] == 2) {x3$reihe2[i] <- 7}
if (x3$V008_06[i] == 2) {x3$reihe2[i] <- 8}
if (x3$V009_06[i] == 2) {x3$reihe2[i] <- 9}

if (x3$V001_06[i] == 3) {x3$reihe3[i] <- 1}
if (x3$V002_06[i] == 3) {x3$reihe3[i] <- 2}
if (x3$V003_06[i] == 3) {x3$reihe3[i] <- 3}
if (x3$V004_06[i] == 3) {x3$reihe3[i] <- 4}
if (x3$V005_06[i] == 3) {x3$reihe3[i] <- 5}
if (x3$V006_06[i] == 3) {x3$reihe3[i] <- 6}
if (x3$V007_06[i] == 3) {x3$reihe3[i]<- 7}
if (x3$V008_06[i] == 3) {x3$reihe3[i] <- 8}
if (x3$V009_06[i] == 3) {x3$reihe3[i] <- 9}

if (x3$V001_06[i] == 4) {x3$reihe4[i] <- 1}
if (x3$V002_06[i] == 4) {x3$reihe4[i] <- 2}
if (x3$V003_06[i] == 4) {x3$reihe4[i] <- 3}
if (x3$V004_06[i] == 4) {x3$reihe4[i] <- 4}
if (x3$V005_06[i] == 4) {x3$reihe4[i] <- 5}
if (x3$V006_06[i] == 4) {x3$reihe4[i] <- 6}
if (x3$V007_06[i] == 4) {x3$reihe4[i] <- 7}
if (x3$V008_06[i] == 4) {x3$reihe4[i] <- 8}
if (x3$V009_06[i] == 4) {x3$reihe4[i] <- 9}

if (x3$V001_06[i] == 5) {x3$reihe5[i] <- 1}
if (x3$V002_06[i] == 5) {x3$reihe5[i] <- 2}
if (x3$V003_06[i] == 5) {x3$reihe5[i] <- 3}
if (x3$V004_06[i] == 5) {x3$reihe5[i] <- 4}
if (x3$V005_06[i] == 5) {x3$reihe5[i] <- 5}
if (x3$V006_06[i] == 5) {x3$reihe5[i] <- 6}
if (x3$V007_06[i] == 5) {x3$reihe5[i] <- 7}
if (x3$V008_06[i] == 5) {x3$reihe5[i] <- 8}
if (x3$V009_06[i] == 5) {x3$reihe5[i] <- 9}

if (x3$V001_06[i] == 6) {x3$reihe6[i] <- 1}
if (x3$V002_06[i] == 6) {x3$reihe6[i] <- 2}
if (x3$V003_06[i] == 6) {x3$reihe6[i] <- 3}
if (x3$V004_06[i] == 6) {x3$reihe6[i] <- 4}
if (x3$V005_06[i] == 6) {x3$reihe6[i] <- 5}
if (x3$V006_06[i] == 6) {x3$reihe6[i] <- 6}
if (x3$V007_06[i] == 6) {x3$reihe6[i] <- 7}
if (x3$V008_06[i] == 6) {x3$reihe6[i] <- 8}
if (x3$V009_06[i] == 6) {x3$reihe6[i] <- 9}

if (x3$V001_06[i] == 7) {x3$reihe7[i] <- 1}
if (x3$V002_06[i] == 7) {x3$reihe7[i] <- 2}
if (x3$V003_06[i] == 7) {x3$reihe7[i] <- 3}
if (x3$V004_06[i] == 7) {x3$reihe7[i] <- 4}
if (x3$V005_06[i] == 7) {x3$reihe7[i] <- 5}
if (x3$V006_06[i] == 7) {x3$reihe7[i] <- 6}
if (x3$V007_06[i] == 7) {x3$reihe7[i] <- 7}
if (x3$V008_06[i] == 7) {x3$reihe7[i] <- 8}
if (x3$V009_06[i] == 7) {x3$reihe7[i] <- 9}

if (x3$V001_06[i] == 8) {x3$reihe8[i] <- 1}
if (x3$V002_06[i] == 8) {x3$reihe8[i] <- 2}
if (x3$V003_06[i] == 8) {x3$reihe8[i] <- 3}
if (x3$V004_06[i] == 8) {x3$reihe8[i] <- 4}
if (x3$V005_06[i] == 8) {x3$reihe8[i] <- 5}
if (x3$V006_06[i] == 8) {x3$reihe8[i] <- 6}
if (x3$V007_06[i] == 8) {x3$reihe8[i] <- 7}
if (x3$V008_06[i] == 8) {x3$reihe8[i] <- 8}
if (x3$V009_06[i] == 8) {x3$reihe8[i] <- 9}

if (x3$V001_06[i] == 9) {x3$reihe9[i] <- 1}
if (x3$V002_06[i] == 9) {x3$reihe9[i] <- 2}
if (x3$V003_06[i] == 9) {x3$reihe9[i] <- 3}
if (x3$V004_06[i] == 9) {x3$reihe9[i] <- 4}
if (x3$V005_06[i] == 9) {x3$reihe9[i] <- 5}
if (x3$V006_06[i] == 9) {x3$reihe9[i] <- 6}
if (x3$V007_06[i] == 9) {x3$reihe9[i] <- 7}
if (x3$V008_06[i] == 9) {x3$reihe9[i] <- 8}
if (x3$V009_06[i] == 9) {x3$reihe9[i] <- 9}
}

# Attitude prior to exposure

x3$evMar <- x3$E005_01 - 1
x3$evFam <- x3$E006_01 - 1 
x3$evKri <- x3$E007_01 - 1
x3$evEU  <- x3$E008_01 - 1
x3$evVid <- x3$E009_01 - 1
x3$evKul <- x3$E010_01 - 1
x3$evTie <- x3$E011_01 - 1
x3$evPas <- x3$E012_01 - 1
x3$evDie <- x3$E013_01 - 1


# Attitude after exposure

x3$enMar <- (x3$E501_01 +     x3$E504_01) / 2 - 1
x3$enFam <- (x3$E501_02 + 7 - x3$E504_02) / 2 - 1
x3$enKri <- (x3$E501_03 +     x3$E504_03) / 2 - 1
x3$enEU  <- (7 - x3$E501_04 + x3$E504_04) / 2 - 1
x3$enVid <- (x3$E501_05 +     x3$E504_05) / 2 - 1
x3$enKul <- (x3$E501_06 +     x3$E504_06) / 2 - 1
x3$enTie <- (7 - x3$E501_07 + x3$E504_07) / 2 - 1
x3$enPas <- (7 - x3$E501_08 + x3$E504_08) / 2 - 1
x3$enDie <- (x3$E501_09 + x3$E504_09) / 2 - 1


# Put table in long format

x4a <- reshape(x3,
               varying=list(
                 c("E005_01", "E006_01", "E007_01","E008_01", "E009_01",
                   "E010_01", "E011_01", "E012_01","E013_01"),
                 c("E503_01", "E503_02", "E503_03","E503_04","E503_05",
                   "E503_06", "E503_07", "E503_08","E503_09"),
                 c("E005_02", "E006_02", "E007_02","E008_02", "E009_02",
                   "E010_02", "E011_02", "E012_02","E013_02"),
                 c("E005_01a", "E006_01a", "E007_01a","E008_01a", "E009_01a",
                   "E010_01a", "E011_01a", "E012_01a","E013_01a"),
                 c("reihe1","reihe2","reihe3","reihe4","reihe5","reihe6",
                   "reihe7","reihe8","reihe9"),
                 c("enMar","enFam",
                   "enKri","enEU",
                   "enVid","enKul",
                   "enTie","enPas","enDie")),
               direction="long",
               v.names=c("einst_v","fc","int","rt","reihe","einst_n"),
               timevar="thema",
               idvar="CASE")


x4a$thema <- factor(x4a$thema)
x4a$bedingung <- factor(x4a$bedingung)

x4a$bed_anz2 <- NA
x4a$bed_anz2[x4a$bed_anz==2] <- 0
x4a$bed_anz2[x4a$bed_anz==1] <- 1
x4a$bed_anz2 <- factor(x4a$bed_anz2)

x4a$bed_like2 <- NA
x4a$bed_like2[x4a$bed_like==2] <- 0
x4a$bed_like2[x4a$bed_like==1] <- 1
x4a$bed_like2 <- factor(x4a$bed_like2)

contrasts(x4a$bed_like2) <- contr.treatment(2, base=1)
contrasts(x4a$bed_anz2) <- contr.treatment(2, base=1)

x4a$bed_like2 <- relevel(x4a$bed_like2, "1")
x4a$bed_anz2 <- relevel(x4a$bed_anz2, "1")

contrasts(x4a$bed_like2) <- contr.sum(2)
contrasts(x4a$bed_anz2) <- contr.sum(2)

# Modell 0 - Empty

mod0 <- lmer(fc ~ (1|CASE) + (1|thema) , data=x4a, REML=F)
summary(mod0)

# Modell 1 - Only controls

moda <- lmer(fc ~ (1|CASE) + (1|thema) + scale(einst_v, scale=F)*(scale(FB04_01, scale=F) + factor(FB03) + scale(reihe, scale=F)), data=x4a, REML=F)

summary(moda)
anova(mod0, moda)

# Modell 2 - Full model

modd <- lmer(fc ~ (1|CASE) + (1|thema) + scale(einst_v, scale=F)*(bed_like2*bed_anz2*scale(int, scale=F)+ scale(FB04_01, scale=F) + factor(FB03) + scale(reihe, scale=F)), data=x4a, REML=F)
summary(modd)
anova(moda, modd)


# ==== 6. Table with descriptive values of attitudes and FCE indicators ====

x4 <- x3
x4$B001_01 <- factor(x4$bed_anz, levels=c("2","1"),labels=c("Inkongruent","Kongruent"))
x4$B001_02 <- factor(x4$bed_like,levels=c("2","1"),labels=c("Likes -","Likes +"))

tabFCgruppen <- data.frame(
  thema = c(1,2,3,4,5,6,7,8,9),
  einst_mw = c( mean(x4a$einst_v[x4a$thema==1]),
             mean(x4a$einst_v[x4a$thema==2]),
             mean(x4a$einst_v[x4a$thema==3]),
             mean(x4a$einst_v[x4a$thema==4]),
             mean(x4a$einst_v[x4a$thema==5]),
             mean(x4a$einst_v[x4a$thema==6]),
             mean(x4a$einst_v[x4a$thema==7]),
             mean(x4a$einst_v[x4a$thema==8]),
             mean(x4a$einst_v[x4a$thema==9])),
  einst_sd = c( sd(x4a$einst_v[x4a$thema==1]),
                sd(x4a$einst_v[x4a$thema==2]),
                sd(x4a$einst_v[x4a$thema==3]),
                sd(x4a$einst_v[x4a$thema==4]),
                sd(x4a$einst_v[x4a$thema==5]),
                sd(x4a$einst_v[x4a$thema==6]),
                sd(x4a$einst_v[x4a$thema==7]),
                sd(x4a$einst_v[x4a$thema==8]),
                sd(x4a$einst_v[x4a$thema==9])),
  ja_mw = c(mean(x4$E503_01[x4$E005_01 >= 4]), mean(x4$E503_02[x4$E006_01 >= 4]),
         mean(x4$E503_03[x4$E007_01 >= 4]), mean(x4$E503_04[x4$E008_01 >= 4]),
         mean(x4$E503_05[x4$E009_01 >= 4]), mean(x4$E503_06[x4$E010_01 >= 4]),
         mean(x4$E503_07[x4$E011_01 >= 4]), mean(x4$E503_08[x4$E012_01 >= 4]),
         mean(x4$E503_09[x4$E013_01 >= 4])),
  ja_sd = c(sd(x4$E503_01[x4$E005_01 >= 4]), sd(x4$E503_02[x4$E006_01 >= 4]),
            sd(x4$E503_03[x4$E007_01 >= 4]), sd(x4$E503_04[x4$E008_01 >= 4]),
            sd(x4$E503_05[x4$E009_01 >= 4]), sd(x4$E503_06[x4$E010_01 >= 4]),
            sd(x4$E503_07[x4$E011_01 >= 4]), sd(x4$E503_08[x4$E012_01 >= 4]),
            sd(x4$E503_09[x4$E013_01 >= 4])),
  nein_mw = c(mean(x4$E503_01[x4$E005_01 <= 3]), mean(x4$E503_02[x4$E006_01 <= 3]),
           mean(x4$E503_03[x4$E007_01 <= 3]), mean(x4$E503_04[x4$E008_01 <= 3]),
           mean(x4$E503_05[x4$E009_01 <= 3]), mean(x4$E503_06[x4$E010_01 <= 3]),
           mean(x4$E503_07[x4$E011_01 <= 3]), mean(x4$E503_08[x4$E012_01 <= 3]),
           mean(x4$E503_09[x4$E013_01 <= 3])),
  nein_sd = c(sd(x4$E503_01[x4$E005_01 <= 3]), sd(x4$E503_02[x4$E006_01 <= 3]),
              sd(x4$E503_03[x4$E007_01 <= 3]), sd(x4$E503_04[x4$E008_01 <= 3]),
              sd(x4$E503_05[x4$E009_01 <= 3]), sd(x4$E503_06[x4$E010_01 <= 3]),
              sd(x4$E503_07[x4$E011_01 <= 3]), sd(x4$E503_08[x4$E012_01 <= 3]),
              sd(x4$E503_09[x4$E013_01 <= 3])),
  t = c(t.test(x4$E503_01 ~ (x4$E005_01 >= 4))$statistic,
        t.test(x4$E503_02 ~ (x4$E006_01 >= 4))$statistic,
        t.test(x4$E503_03 ~ (x4$E007_01 >= 4))$statistic,
        t.test(x4$E503_04 ~ (x4$E008_01 >= 4))$statistic,
        t.test(x4$E503_05 ~ (x4$E009_01 >= 4))$statistic,
        t.test(x4$E503_06 ~ (x4$E010_01 >= 4))$statistic,
        t.test(x4$E503_07 ~ (x4$E011_01 >= 4))$statistic,
        t.test(x4$E503_08 ~ (x4$E012_01 >= 4))$statistic,
        t.test(x4$E503_09 ~ (x4$E013_01 >= 4))$statistic
  ),
  df = c(t.test(x4$E503_01 ~ (x4$E005_01 >= 4))$parameter,
         t.test(x4$E503_02 ~ (x4$E006_01 >= 4))$parameter,
         t.test(x4$E503_03 ~ (x4$E007_01 >= 4))$parameter,
         t.test(x4$E503_04 ~ (x4$E008_01 >= 4))$parameter,
         t.test(x4$E503_05 ~ (x4$E009_01 >= 4))$parameter,
         t.test(x4$E503_06 ~ (x4$E010_01 >= 4))$parameter,
         t.test(x4$E503_07 ~ (x4$E011_01 >= 4))$parameter,
         t.test(x4$E503_08 ~ (x4$E012_01 >= 4))$parameter,
         t.test(x4$E503_09 ~ (x4$E013_01 >= 4))$parameter
  ),
  p =c(t.test(x4$E503_01 ~ (x4$E005_01 >= 4))$p.value,
       t.test(x4$E503_02 ~ (x4$E006_01 >= 4))$p.value,
       t.test(x4$E503_03 ~ (x4$E007_01 >= 4))$p.value,
       t.test(x4$E503_04 ~ (x4$E008_01 >= 4))$p.value,
       t.test(x4$E503_05 ~ (x4$E009_01 >= 4))$p.value,
       t.test(x4$E503_06 ~ (x4$E010_01 >= 4))$p.value,
       t.test(x4$E503_07 ~ (x4$E011_01 >= 4))$p.value,
       t.test(x4$E503_08 ~ (x4$E012_01 >= 4))$p.value,
       t.test(x4$E503_09 ~ (x4$E013_01 >= 4))$p.value
  ),
  cor=c(cor.test(x4a$fc[x4a$thema==1], x4a$einst_v[x4a$thema==1], method="pearson")$estimate,
        cor.test(x4a$fc[x4a$thema==2], x4a$einst_v[x4a$thema==2], method="pearson")$estimate,
        cor.test(x4a$fc[x4a$thema==3], x4a$einst_v[x4a$thema==3], method="pearson")$estimate,
        cor.test(x4a$fc[x4a$thema==4], x4a$einst_v[x4a$thema==4], method="pearson")$estimate,
        cor.test(x4a$fc[x4a$thema==5], x4a$einst_v[x4a$thema==5], method="pearson")$estimate,
        cor.test(x4a$fc[x4a$thema==6], x4a$einst_v[x4a$thema==6], method="pearson")$estimate,
        cor.test(x4a$fc[x4a$thema==7], x4a$einst_v[x4a$thema==7], method="pearson")$estimate,
        cor.test(x4a$fc[x4a$thema==8], x4a$einst_v[x4a$thema==8], method="pearson")$estimate,
        cor.test(x4a$fc[x4a$thema==9], x4a$einst_v[x4a$thema==9], method="pearson")$estimate),
  cor_p=c(cor.test(x4a$fc[x4a$thema==1], x4a$einst_v[x4a$thema==1], method="pearson")$p.value,
        cor.test(x4a$fc[x4a$thema==2], x4a$einst_v[x4a$thema==2], method="pearson")$p.value,
        cor.test(x4a$fc[x4a$thema==3], x4a$einst_v[x4a$thema==3], method="pearson")$p.value,
        cor.test(x4a$fc[x4a$thema==4], x4a$einst_v[x4a$thema==4], method="pearson")$p.value,
        cor.test(x4a$fc[x4a$thema==5], x4a$einst_v[x4a$thema==5], method="pearson")$p.value,
        cor.test(x4a$fc[x4a$thema==6], x4a$einst_v[x4a$thema==6], method="pearson")$p.value,
        cor.test(x4a$fc[x4a$thema==7], x4a$einst_v[x4a$thema==7], method="pearson")$p.value,
        cor.test(x4a$fc[x4a$thema==8], x4a$einst_v[x4a$thema==8], method="pearson")$p.value,
        cor.test(x4a$fc[x4a$thema==9], x4a$einst_v[x4a$thema==9], method="pearson")$p.value)
)

tabFCgruppen$p_kor <- p.adjust(tabFCgruppen$p, method="holm")

print(round(tabFCgruppen,2))


# ==== 7. Stability of attitudes ====

x4b <- x3

# Attitude before exposure

x4b$evMar <- x4b$E005_01 - 1
x4b$evFam <- x4b$E006_01 - 1 
x4b$evKri <- x4b$E007_01 - 1
x4b$evEU  <- x4b$E008_01 - 1
x4b$evVid <- x4b$E009_01 - 1
x4b$evKul <- x4b$E010_01 - 1
x4b$evTie <- x4b$E011_01 - 1
x4b$evPas <- x4b$E012_01 - 1
x4b$evDie <- x4b$E013_01 - 1


# Attitude after exposure

x4b$enMar <- (x4b$E501_01 +     x4b$E504_01) / 2 - 1
x4b$enFam <- (x4b$E501_02 + 7 - x4b$E504_02) / 2 - 1
x4b$enKri <- (x4b$E501_03 +     x4b$E504_03) / 2 - 1
x4b$enEU  <- (7 - x4b$E501_04 + x4b$E504_04) / 2 - 1
x4b$enVid <- (x4b$E501_05 +     x4b$E504_05) / 2 - 1
x4b$enKul <- (x4b$E501_06 +     x4b$E504_06) / 2 - 1
x4b$enTie <- (7 - x4b$E501_07 + x4b$E504_07) / 2 - 1
x4b$enPas <- (7 - x4b$E501_08 + x4b$E504_08) / 2 - 1
x4b$enDie <- (x4b$E501_09 + x4b$E504_09) / 2 - 1


options(scipen=999)
cor.test(x4b$evMar, x4b$enMar, method="pearson")
cor.test(x4b$evVid, x4b$enVid, method="pearson")
cor.test(x4b$evFam, x4b$enFam, method="pearson")
cor.test(x4b$evKri, x4b$enKri, method="pearson")
cor.test(x4b$evEU, x4b$enEU, method="pearson")
cor.test(x4b$evKul, x4b$enKul, method="pearson")
cor.test(x4b$evTie, x4b$enTie, method="pearson")
cor.test(x4b$evPas, x4b$enPas, method="pearson")
cor.test(x4b$evDie, x4b$enDie, method="pearson")




# ==== 8. Message selection ====

varThemaNr <- c("V001_06","V002_06","V003_06","V004_06","V005_06","V006_06",
                "V007_06","V008_06","V009_06")
varMeinung <- c("V001_02","V002_02","V003_02","V004_02","V005_02","V006_02",
                "V007_02","V008_02","V009_02")
varLikes   <- c("V001_03","V002_03","V003_03","V004_03","V005_03","V006_03",
                "V007_03","V008_03","V009_03")
varPos     <- c("V001_04","V002_04","V003_04","V004_04","V005_04","V006_04",
                "V007_04","V008_04","V009_04")

varInt <- c("E005_02","E006_02","E007_02","E008_02","E009_02","E010_02","E011_02","E012_02","E013_02")

# Attitudinal direction of chosen message
# # 1 = pro, 2 = contra
namenWahl <- c("t1_wm","t2_wm","t3_wm","t4_wm","t5_wm","t6_wm","t7_wm","t8_wm","t9_wm")

# Numbers of likes of chosen message
namenAnzahlLikesGew <- c("t1_wl","t2_wl","t3_wl","t4_wl","t5_wl","t6_wl","t7_wl","t8_wl","t9_wl")

# Position of chosen message in news feed
namenPos <- c("t1_po","t2_po","t3_po","t4_po","t5_po","t6_po","t7_po","t8_po","t9_po")

# Reported interest in topic of chosen message (not used for analysis)
namenIntr <- c("t1_i","t2_i","t3_i","t4_i","t5_i","t6_i","t7_i","t8_i","t9_i")

for (i in 1:length(varThemaNr)) {
  x3[[namenWahl[i]]] <- NA
  x3[[namenAnzahlLikesGew[i]]] <- NA
  x3[[namenPos[i]]] <- NA
  x3[[namenIntr[i]]] <- NA
}
for (i in 1:length(varThemaNr)) {
  for (p in 1:nrow(x3)){
      x3[[p,namenWahl[x3[[p,varThemaNr[i]]]]]] <- 1-(x3[[p,varMeinung[i]]]-1)
      x3[[p,namenAnzahlLikesGew[x3[[p,varThemaNr[i]]]]]] <- x3[[p,varLikes[i]]]
      x3[[p,namenPos[x3[[p,varThemaNr[i]]]]]] <- x3[[p,varPos[i]]]
      x3[[p,namenIntr[x3[[p,varThemaNr[i]]]]]] <- x3[[p, varInt[i]]]
  }
}

x3$bed_anz <- factor(x3$bed_anz)
levels(x3$bed_anz) <- c("+","-")

x3$bed_like <- factor(x3$bed_like)
levels(x3$bed_like) <- c("+","-")

x3$text_wahl_kons <- rowSums(subset(x3,
                                   select=c("t1_wm","t2_wm","t3_wm","t4_wm","t5_wm","t6_wm","t7_wm","t8_wm","t9_wm")))


x3$text_wahl_kons_rek <- NA
x3$text_wahl_kons_rek[x3$bed_anz=="+"] <- x3$text_wahl_kons[x3$bed_anz=="+"] - (9*(3/4))
x3$text_wahl_kons_rek[x3$bed_anz=="-"] <- x3$text_wahl_kons[x3$bed_anz=="-"] - (9*(1/4))

describeBy(x3$text_wahl_kons_rek, group=interaction(x3$bed_anz, x3$bed_like))

Anova(lm(x3$text_wahl_kons_rek ~ x3$bed_anz*x3$bed_like), type="III")
