# ==== 1. Read data ====

x <- read.table("data_experiment2.csv", sep="\t", quote="\"", header=T)

# ==== 2. Clean data ====

# Exclude cases with to quick or slow completion times and those who refused to indicate sex

x$time_sum_real <- rowSums(
  subset(x, 
         select = c("TIME002", "TIME003", "TIME004",  
                    "TIME006", "TIME007", "TIME008", "TIME009", "TIME010",
                    "TIME011", "TIME012", "TIME013", "TIME014",  
                    "TIME018", "TIME019", "TIME020", "TIME021",
                    "TIME022", "TIME023", "TIME024", "TIME025", "TIME027",
                    "TIME029",  "TIME030", "TIME031",
                     "TIME033",  "TIME034", "TIME035"))
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
               (x$time_sum_real >=real_time_mean - 2 * real_time_sd) & (x$FB03 %in% c(1,2)))
nrow(x2)


# Exclude all older than 35 who are very few and might distort the results while being non-representative

table(x2$FB04_01)
x3 <- subset(x2,
             x2$FB04_01 <= 35)
nrow(x3)

describe(x3$FB04_01)
describe(x3$time_sum_real)

x3$FB03 <- factor(x3$FB03)
table(x3$FB03)

prop.table(table(x3$FB03))
table(x3$FB05)
prop.table(table(x3$FB05))
table(x3$FB06)
prop.table(table(x3$FB06))

# Exclude all who indicated that they have participated in the first experiment

table(x3$CH01)
x4 <- subset(x3,
             x3$CH01 ==2)

# ==== 3. Setting factors and basic calculations ====

# Condition Attitudinal congruency
x4$B001_01 <- factor(x4$B001_01, labels=c("Inkongruent", "Kongruent")) 

# Condition Endorsement - More likes (+) for congruent articles or incongruent (-)
x4$B001_02 <- factor(x4$B001_02, labels=c("Likes-", "Likes+")) # 

# B001_04 indicates if participants saw messages with numerically high or low numbers of likes, regardless of attitudinal congruency;
# only used for analysis of clicking behaviro; irrelevant for main analysis

x4$B001_04 <- NA
x4$B001_04[
  (x4$B001_01=="Kongruent" & x4$B001_02 =="Likes+") | 
    (x4$B001_01=="Inkongruent" & x4$B001_02 =="Likes-")] <- "High"
x4$B001_04[
  (x4$B001_01=="Kongruent" & x4$B001_02 =="Likes-") | 
    (x4$B001_01=="Inkongruent" & x4$B001_02 =="Likes+")] <- "Low"
x4$B001_04 <- factor(x4$B001_04)

# Interest

x4$mwInteresse <- rowMeans(subset(x4, select=c(
  "E005_02","E006_02","E007_02","E008_02","E009_02",
  "E010_02","E011_02","E012_02","E013_02"
)))

hist(x4$mwInteresse)

# Mean clicking probabilities for articles

x4$mwKlickNeutral <- rowMeans(subset(x4, select = c(
  "SE13_01","SE13_02","SE13_03","SE13_04"
)))


x4$mwKlickStim <- rowMeans(subset(x4, select = c(
  "SE04_01","SE04_02","SE04_03","SE04_04",
  "SE04_05","SE04_06","SE14_01","SE14_02",
  "SE14_03","SE14_04","SE14_05","SE14_06",
  "SE15_01","SE15_02","SE15_03","SE15_04",
  "SE15_05","SE15_06","SE16_01","SE16_02",
  "SE16_03","SE16_04","SE16_05","SE16_06",
  "SE17_01","SE17_02","SE17_03","SE17_04",
  "SE17_05","SE17_06","SE18_01","SE18_02",
  "SE18_03","SE18_04","SE18_05","SE18_06"
)))

x4$mwKlickThema1 <- rowMeans(subset(x4,select=c("SE04_01","SE04_02","SE04_03","SE04_04")))
x4$mwKlickThema2 <- rowMeans(subset(x4,select=c("SE04_05","SE04_06","SE14_01","SE14_02")))
x4$mwKlickThema3 <- rowMeans(subset(x4,select=c("SE14_03","SE14_04","SE14_05","SE14_06")))
x4$mwKlickThema4 <- rowMeans(subset(x4,select=c("SE15_01","SE15_02","SE15_03","SE15_04")))
x4$mwKlickThema5 <- rowMeans(subset(x4,select=c("SE15_05","SE15_06","SE16_01","SE16_02")))
x4$mwKlickThema6 <- rowMeans(subset(x4,select=c("SE16_03","SE16_04","SE16_05","SE16_06")))
x4$mwKlickThema7 <- rowMeans(subset(x4,select=c("SE17_01","SE17_02","SE17_03","SE17_04")))
x4$mwKlickThema8 <- rowMeans(subset(x4,select=c("SE17_05","SE17_06","SE18_01","SE18_02")))
x4$mwKlickThema9 <- rowMeans(subset(x4,select=c("SE18_03","SE18_04","SE18_05","SE18_06")))




# ==== 4. Descriptive Statistics ====

# Sociodemographics
describe(x4$FB04_01)
table(x4$FB03)

# Size of experimental groups
print(table(x4$B001_01, x4$B001_02))

# Attitudes
print(describe(subset(x4, select=c("E005_01", "E006_01","E007_01","E008_01","E009_01","E010_01","E011_01","E012_01","E013_01"))))

# Interest 
print(describe(subset(x4, select=c("E005_02", "E006_02","E007_02","E008_02","E009_02","E010_02","E011_02","E012_02","E013_02"))))

# Table with attitudes, population estimates for pro- and contra-attitude-participants, and attitude-estimate correlations

tabFCgruppen <- data.frame(
  thema = c(1,2,3,4,5,6,7,8,9),
  einst_mw = c( mean(x4$E005_01),
                mean(x4$E006_01),
                mean(x4$E007_01),
                mean(x4$E008_01),
                mean(x4$E009_01),
                mean(x4$E010_01),
                mean(x4$E011_01),
                mean(x4$E012_01),
                mean(x4$E013_01)),
  einst_sd = c( sd(x4$E005_01),
                sd(x4$E006_01),
                sd(x4$E007_01),
                sd(x4$E008_01),
                sd(x4$E009_01),
                sd(x4$E010_01),
                sd(x4$E011_01),
                sd(x4$E012_01),
                sd(x4$E013_01)),
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
  cor=c(cor.test(x4$E503_01, x4$E005_01, method="pearson")$estimate,
        cor.test(x4$E503_02, x4$E006_01, method="pearson")$estimate,
        cor.test(x4$E503_03, x4$E007_01, method="pearson")$estimate,
        cor.test(x4$E503_04, x4$E008_01, method="pearson")$estimate,
        cor.test(x4$E503_05, x4$E009_01, method="pearson")$estimate,
        cor.test(x4$E503_06, x4$E010_01, method="pearson")$estimate,
        cor.test(x4$E503_07, x4$E011_01, method="pearson")$estimate,
        cor.test(x4$E503_08, x4$E012_01, method="pearson")$estimate,
        cor.test(x4$E503_09, x4$E013_01, method="pearson")$estimate),
  cor_p=c(cor.test(x4$E503_01, x4$E005_01, method="pearson")$p.value,
          cor.test(x4$E503_02, x4$E006_01, method="pearson")$p.value,
          cor.test(x4$E503_03, x4$E007_01, method="pearson")$p.value,
          cor.test(x4$E503_04, x4$E008_01, method="pearson")$p.value,
          cor.test(x4$E503_05, x4$E009_01, method="pearson")$p.value,
          cor.test(x4$E503_06, x4$E010_01, method="pearson")$p.value,
          cor.test(x4$E503_07, x4$E011_01, method="pearson")$p.value,
          cor.test(x4$E503_08, x4$E012_01, method="pearson")$p.value,
          cor.test(x4$E503_09, x4$E013_01, method="pearson")$p.value)
  )

tabFCgruppen$p_kor <- p.adjust(tabFCgruppen$p, method="holm")

tabFCgruppen <- round(tabFCgruppen, 2)

tabFCgruppen

# ==== 5. Main analysis of False Consensus Effect ====

# Variables for presentation order

x4$reihe1 <- NA
x4$reihe2 <- NA
x4$reihe3 <- NA
x4$reihe4 <- NA
x4$reihe5 <- NA
x4$reihe6 <- NA
x4$reihe7 <- NA
x4$reihe8 <- NA
x4$reihe9 <- NA

for(i in 1:nrow(x4)){
  
  if (x4$V101_05[i] == 1) {x4$reihe1[i] <- 1}
  if (x4$V105_05[i] == 1) {x4$reihe1[i] <- 2}
  if (x4$V109_05[i] == 1) {x4$reihe1[i] <- 3}
  if (x4$V113_05[i] == 1) {x4$reihe1[i] <- 4}
  if (x4$V117_05[i] == 1) {x4$reihe1[i] <- 5}
  if (x4$V121_05[i] == 1) {x4$reihe1[i] <- 6}
  if (x4$V125_05[i] == 1) {x4$reihe1[i] <- 7}
  if (x4$V129_05[i] == 1) {x4$reihe1[i] <- 8}
  if (x4$V133_05[i] == 1) {x4$reihe1[i] <- 9}
  
  if (x4$V101_05[i] == 2) {x4$reihe2[i] <- 1}
  if (x4$V105_05[i] == 2) {x4$reihe2[i] <- 2}
  if (x4$V109_05[i] == 2) {x4$reihe2[i] <- 3}
  if (x4$V113_05[i] == 2) {x4$reihe2[i] <- 4}
  if (x4$V117_05[i] == 2) {x4$reihe2[i] <- 5}
  if (x4$V121_05[i] == 2) {x4$reihe2[i] <- 6}
  if (x4$V125_05[i] == 2) {x4$reihe2[i] <- 7}
  if (x4$V129_05[i] == 2) {x4$reihe2[i] <- 8}
  if (x4$V133_05[i] == 2) {x4$reihe2[i] <- 9}
  
  if (x4$V101_05[i] == 3) {x4$reihe3[i] <- 1}
  if (x4$V105_05[i] == 3) {x4$reihe3[i] <- 2}
  if (x4$V109_05[i] == 3) {x4$reihe3[i] <- 3}
  if (x4$V113_05[i] == 3) {x4$reihe3[i] <- 4}
  if (x4$V117_05[i] == 3) {x4$reihe3[i] <- 5}
  if (x4$V121_05[i] == 3) {x4$reihe3[i] <- 6}
  if (x4$V125_05[i] == 3) {x4$reihe3[i] <- 7}
  if (x4$V129_05[i] == 3) {x4$reihe3[i] <- 8}
  if (x4$V133_05[i] == 3) {x4$reihe3[i] <- 9}
  
  
  if (x4$V101_05[i] == 4) {x4$reihe4[i] <- 1}
  if (x4$V105_05[i] == 4) {x4$reihe4[i] <- 2}
  if (x4$V109_05[i] == 4) {x4$reihe4[i] <- 3}
  if (x4$V113_05[i] == 4) {x4$reihe4[i] <- 4}
  if (x4$V117_05[i] == 4) {x4$reihe4[i] <- 5}
  if (x4$V121_05[i] == 4) {x4$reihe4[i] <- 6}
  if (x4$V125_05[i] == 4) {x4$reihe4[i] <- 7}
  if (x4$V129_05[i] == 4) {x4$reihe4[i] <- 8}
  if (x4$V133_05[i] == 4) {x4$reihe4[i] <- 9}
  
  if (x4$V101_05[i] == 5) {x4$reihe5[i] <- 1}
  if (x4$V105_05[i] == 5) {x4$reihe5[i] <- 2}
  if (x4$V109_05[i] == 5) {x4$reihe5[i] <- 3}
  if (x4$V113_05[i] == 5) {x4$reihe5[i] <- 4}
  if (x4$V117_05[i] == 5) {x4$reihe5[i] <- 5}
  if (x4$V121_05[i] == 5) {x4$reihe5[i] <- 6}
  if (x4$V125_05[i] == 5) {x4$reihe5[i] <- 7}
  if (x4$V129_05[i] == 5) {x4$reihe5[i] <- 8}
  if (x4$V133_05[i] == 5) {x4$reihe5[i] <- 9}
  
  if (x4$V101_05[i] == 6) {x4$reihe6[i] <- 1}
  if (x4$V105_05[i] == 6) {x4$reihe6[i] <- 2}
  if (x4$V109_05[i] == 6) {x4$reihe6[i] <- 3}
  if (x4$V113_05[i] == 6) {x4$reihe6[i] <- 4}
  if (x4$V117_05[i] == 6) {x4$reihe6[i] <- 5}
  if (x4$V121_05[i] == 6) {x4$reihe6[i] <- 6}
  if (x4$V125_05[i] == 6) {x4$reihe6[i] <- 7}
  if (x4$V129_05[i] == 6) {x4$reihe6[i] <- 8}
  if (x4$V133_05[i] == 6) {x4$reihe6[i] <- 9}

  if (x4$V101_05[i] == 7) {x4$reihe7[i] <- 1}
  if (x4$V105_05[i] == 7) {x4$reihe7[i] <- 2}
  if (x4$V109_05[i] == 7) {x4$reihe7[i] <- 3}
  if (x4$V113_05[i] == 7) {x4$reihe7[i] <- 4}
  if (x4$V117_05[i] == 7) {x4$reihe7[i] <- 5}
  if (x4$V121_05[i] == 7) {x4$reihe7[i] <- 6}
  if (x4$V125_05[i] == 7) {x4$reihe7[i] <- 7}
  if (x4$V129_05[i] == 7) {x4$reihe7[i] <- 8}
  if (x4$V133_05[i] == 7) {x4$reihe7[i] <- 9}
  
  if (x4$V101_05[i] == 8) {x4$reihe8[i] <- 1}
  if (x4$V105_05[i] == 8) {x4$reihe8[i] <- 2}
  if (x4$V109_05[i] == 8) {x4$reihe8[i] <- 3}
  if (x4$V113_05[i] == 8) {x4$reihe8[i] <- 4}
  if (x4$V117_05[i] == 8) {x4$reihe8[i] <- 5}
  if (x4$V121_05[i] == 8) {x4$reihe8[i] <- 6}
  if (x4$V125_05[i] == 8) {x4$reihe8[i] <- 7}
  if (x4$V129_05[i] == 8) {x4$reihe8[i] <- 8}
  if (x4$V133_05[i] == 8) {x4$reihe8[i] <- 9}
  
  if (x4$V101_05[i] == 9) {x4$reihe9[i] <- 1}
  if (x4$V105_05[i] == 9) {x4$reihe9[i] <- 2}
  if (x4$V109_05[i] == 9) {x4$reihe9[i] <- 3}
  if (x4$V113_05[i] == 9) {x4$reihe9[i] <- 4}
  if (x4$V117_05[i] == 9) {x4$reihe9[i] <- 5}
  if (x4$V121_05[i] == 9) {x4$reihe9[i] <- 6}
  if (x4$V125_05[i] == 9) {x4$reihe9[i] <- 7}
  if (x4$V129_05[i] == 9) {x4$reihe9[i] <- 8}
  if (x4$V133_05[i] == 9) {x4$reihe9[i] <- 9}
 

  
}

# Put table in long format

x5 <- reshape(x4,
              varying=list(
                c("E503_01","E503_02","E503_03","E503_04","E503_05","E503_06",
                  "E503_07","E503_08","E503_09"),
                c("E005_01","E006_01","E007_01","E008_01","E009_01","E010_01",
                  "E011_01","E012_01","E013_01"),
                c("E005_02","E006_02","E007_02","E008_02","E009_02","E010_02",
                  "E011_02","E012_02","E013_02"),
                c("mwKlickThema1","mwKlickThema2","mwKlickThema3",
                  "mwKlickThema4","mwKlickThema5","mwKlickThema6",
                  "mwKlickThema7","mwKlickThema8","mwKlickThema9"),
                c(
                  "E504_01","E504_02","E504_03","E504_04","E504_05",
                  "E505_01","E505_02","E505_03","E505_04"
                ),
                c("reihe1","reihe2","reihe3","reihe4","reihe5","reihe6",
                  "reihe7","reihe8","reihe9")
              ),
              v.names=c("estimate","opinion","interest","klicken","sharen","reihe"),
              timevar="topic",
              times=c(1,2,3,4,5,6,7,8,9),
              idvar=c("CASE"),
              direction="long"
)

# Select only relevant variables

x5b <- subset(x5, topic %in% c(1,2,3,4,5,6,7,8,9), select=c("CASE","topic","B001_01","B001_02","B001_03","B001_04","estimate","opinion","interest","klicken","sharen","FB03","FB04_01","reihe"))

# Make factors, correctly level and set contrasts

x5b$FB03 <- factor(x5b$FB03)
contrasts(x5b$FB03)<- contr.sum(2)

x5b$B001_02 <- factor(x5b$B001_02)

x5b$B001_01 <- relevel(x5b$B001_01, "Kongruent")
x5b$B001_02 <- relevel(x5b$B001_02, "Likes+")
x5b$B001_04 <- relevel(x5b$B001_04, "High")


contrasts(x5b$B001_01) <- contr.sum(2)
contrasts(x5b$B001_02) <- contr.sum(2)
contrasts(x5b$B001_04) <- contr.sum(2)

library(lme4)
library(lmerTest)

# Modell 0 - Empty

mod_lmer_fc_a <- lmer(
  estimate ~ ( 1 | CASE) + (1 | topic)  , 
  data = x5b,
  REML=F
)
summary(mod_lmer_fc_a)

# Modell 1 - Control variables

mod_lmer_fc_b <- lmer(
  estimate ~ ( 1 | CASE) + (1 | topic) + scale(opinion, scale=F)*(FB03 + scale(FB04_01,scale=F)+scale(reihe, scale=F)) , 
  data = x5b,
  REML=F
)
summary(mod_lmer_fc_b)

anova(mod_lmer_fc_a, mod_lmer_fc_b)

# Modell 2- Full 

# Centered but not standardized predictors
mod_lmer_fc_c <- lmer(
  estimate ~ ( 1 | CASE) + (1 | topic) + scale(opinion, scale=F)*(B001_01*B001_02*scale(interest, scale=F) + FB03 + scale(FB04_01,scale=F)+scale(reihe, scale=F)) , 
  data = x5b,
  REML=F
)
summary(mod_lmer_fc_c)

anova(mod_lmer_fc_c, mod_lmer_fc_b)


# ==== 6. Sidenote: Selection behavior ====

mod_lmer_klick <- lmer(
  klicken ~ ( 1 | CASE) + (1 | topic) + B001_01+B001_04+scale(interest, scale=F) + FB03 + scale(FB04_01, scale=F) , 
  data = x5b,
  REML=F
)

summary(mod_lmer_klick)