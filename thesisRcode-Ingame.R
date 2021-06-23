install.packages("cquad")
library(cquad)


####Dataset ingame 
ingame = read.csv(file = "ingame.csv", header = TRUE, stringsAsFactors = FALSE)


###
pdata <- pdata.frame(ingame, c("id", "time"))
class(pdata$YITmedian)


####summary staistics
describe(pdata)
stat.desc(pdata)

###the fixed-effects static logit model by Chamberlain (1980) (model = "basic", default);
###this one had mean
out111 = cquad(YITmedian ~ home,pdata, model = "basic")
summary(out111)

out222 = cquad(YITmedian ~ home + differenceinranking + differenceinwin,pdata, model = "basic")
summary(out222)

out333 = cquad(YITmedian ~ home + differenceinranking + differenceinwin + DifferenceAVGrebounds + differenceFGATT, pdata, model = "basic")
summary(out333)


#### Bartolucci and Nigro (2010). The OG model. 
out4 = cquad(YITmedian ~ home, pdata, model = "extended")
summary(out4)

out44 = cquad(YITmedian ~ home + differenceinranking + differenceinwin, pdata, model = "extended")
summary(out44)

out5 = cquad(YITmedian ~  home + differenceinranking + differenceinwin + DifferenceAVGrebounds + differenceFGATT, pdata, model = "extended")
summary(out5)


#### Bartolucci and Nigro (2012). The approximation of dynamic logit. 
out6 = cquad(YITmedian ~ home,pdata,  model = "pseudo")
summary(out6)

out66 = cquad(YITmedian ~ home + differenceinranking + differenceinwin,pdata, model = "pseudo")
summary(out66)


out7 = cquad(YITmedian ~ home + differenceinranking + differenceinwin + DifferenceAVGrebounds + differenceFGATT,pdata, model = "pseudo")
summary(out7)



