install.packages("cquad")
library(cquad)

####Dataset ingame 
acrossgame = read.csv(file = "acrossgameomit.csv", header = TRUE, stringsAsFactors = FALSE)
attach(acrossgameomit)



###

pdata1 <- pdata.frame(acrossgame, c("id", "Time"))

####summary staistics
describe(pdata1)
stat.desc(pdata1)

###chamberlain 1980 model 
out1000 = cquad(YITmedian ~ home, pdata1, model = "basic")
summary(out1000)

out2000 = cquad(YITmedian ~ home + Diffinrank + diffinwin,pdata1, model = "basic")
summary(out2000)

out3000 = cquad(YITmedian~ home + Diffinrank + diffinwin + DifferenceinRebounds + DifferenceinBlocks, pdata1, model = "basic")
summary(out3000)

out4000 = cquad(YITmedian~ home + Diffinrank + diffinwin + DifferenceinRebounds + DifferenceinBlocks + Differenceinpoints, pdata1, model = "basic")
summary(out4000)

out5000 = cquad(YITmedian~ home + Diffinrank + diffinwin + DifferenceinRebounds + DifferenceinBlocks + Differenceinpoints + QuarteroftheSeason, pdata1, model = "basic")
summary(out5000)


#### Bartolucci and Nigro (2010). The OG model 
out1001 = cquad(YITmedian ~ home, pdata1, model = "extended")
summary(out1001)

out2001 = cquad(YITmedian ~ home + Diffinrank + diffinwin,pdata1, model = "extended")
summary(out2001)

out3001 = cquad(YITmedian~ home + Diffinrank + diffinwin + DifferenceinRebounds + DifferenceinBlocks, pdata1, model = "extended")
summary(out3001)

out4001 = cquad(YITmedian~ home + Diffinrank + diffinwin + DifferenceinRebounds + DifferenceinBlocks + Differenceinpoints, pdata1, model = "extended")
summary(out4001)

out5001 = cquad(YITmedian~ home + Diffinrank + diffinwin + DifferenceinRebounds + DifferenceinBlocks + Differenceinpoints + QuarteroftheSeason, pdata1, model = "extended")
summary(out5001)

#### Bartolucci and Nigro (2012). pseduo
out1011 = cquad(YITmedian ~ home, pdata1, model = "pseudo")
summary(out1011)



out2011 = cquad(YITmedian ~ home + Diffinrank + diffinwin,pdata1, model = "pseudo")
summary(out2011)

out3011 = cquad(YITmedian~ home + Diffinrank + diffinwin + DifferenceinRebounds + DifferenceinBlocks, pdata1, model = "pseudo")
summary(out3011)

out4011 = cquad(YITmedian~ home + Diffinrank + diffinwin + DifferenceinRebounds + DifferenceinBlocks + Differenceinpoints, pdata1, model = "pseudo")
summary(out4011)

out5011 = cquad(YITmedian~ home + Diffinrank + diffinwin + DifferenceinRebounds + DifferenceinBlocks + Differenceinpoints + QuarteroftheSeason, pdata1, model = "extended")
summary(out5011)


