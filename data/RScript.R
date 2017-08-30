library(stringr)
data <- read.delim('Forschungsarbeit Auswehrtungstabelle 1.csv')
data$Förkylning <- factor(data$Förkylning)
data$Halsont <- factor(data$Halsont)
data$Feber <- factor(data$Feber)
data$Ont.i.öron <- factor(data$Ont.i.öron)
data$UVI <- factor(data$UVI)
data$Hosta <- factor(data$Hosta)
data$Kräkningar <- factor(data$Kräkningar)
data$Diarre <- factor(data$Diarre)
data$Magont <- factor(data$Magont)

data$morgontemperatur <-  as.numeric(str_replace( as.character( data$morgontemperatur), ',','.'))

data$Åldern <- factor( as.character(data$Åldern), levels=c('0-3','4-6','7-9','10-13','14-17' ))

save(data,file="data.RData")

plot( as.numeric(as.character(data$Fråga.5)) ~ data$Åldern)
anova(lm( as.numeric(data$Fråga.5) ~ data$Åldern))$"Pr(>F)"[1]
#[1] 1.06104e-05

input_cols <- c('Fråga.5','Fråga.6','Fråga.7','Fråga..8','Fråga..9','Fråga..10.VAS.CRP','Fråga..10.VAS.strep' )

run_annova <- function( colname ) {

	p.vals1 <- unlist(lapply( input_cols , function(cname) {
		anova(lm( as.numeric(as.character(data[,cname])) ~ data[,colname]))$"Pr(>F)"[1]
	} ))
	
	names(p.vals1) <- input_cols
	p.vals1

}

(p.vals1 <- run_annova('Åldern'))

#            Fråga.5             Fråga.6             Fråga.7            Fråga..8 
#       1.061040e-05        1.089857e-08        2.451278e-01        3.397947e-03 
#           Fråga..9   Fråga..10.VAS.CRP Fråga..10.VAS.strep 
#       1.092852e-07        5.611234e-22        3.930611e-07 

# surprisingly good p values here!

## and the extremely goof p values were an error...

# with the corrected script the p values look more reliable:
# the answe to the question: Does the age of the kids affect the answer to the questions:

#            Fråga.5             Fråga.6             Fråga.7            Fråga..8 
#       1.550308e-04        1.022504e-07        2.451278e-01        3.397947e-03 
#           Fråga..9   Fråga..10.VAS.CRP Fråga..10.VAS.strep 
#       1.092852e-07        8.848871e-05        7.102369e-01


#next what would I like to see?

#visual inspection of the plots like
plot( jitter(as.numeric(as.character(data[,input_cols[1]]))) , data$morgontemperatur)
# did show no effect of the temperature on the questions outcome.

temperature_corr <- NULL
for ( i in 1:length(input_cols)){
	t <- cor.test(as.numeric(as.character(data[,input_cols[i]])) , data$morgontemperatur)
	temperature_corr <- rbind(temperature_corr, c(t$estimate, t$p.value))
}
rownames(temperature_corr) <- input_cols 
colnames(temperature_corr) <- c('pearson rho','p value')

temperature_corr
#                      pearson rho   p value
#Fråga.5              0.1449602126 0.3478091
#Fråga.6              0.0471049719 0.7401870
#Fråga.7              0.0616241503 0.6611327
#Fråga..8             0.0313697722 0.8235475
#Fråga..9             0.0494208433 0.7252697
#Fråga..10.VAS.CRP   -0.0001261715 0.9995441
#Fråga..10.VAS.strep -0.0352295203 0.9283052

(p.vals.kön <- run_annova('kön'))
#            Fråga.5             Fråga.6             Fråga.7            Fråga..8 
#          0.9018953           0.9893636           0.3102917           0.4723559 
#           Fråga..9   Fråga..10.VAS.CRP Fråga..10.VAS.strep 
#          0.8700623           0.4540279           0.7181212

(p.vals.Förkylning <- run_annova('Förkylning'))
#            Fråga.5             Fråga.6             Fråga.7            Fråga..8 
#          0.6633117           0.8820971           0.1282242           0.4538891 
#           Fråga..9   Fråga..10.VAS.CRP Fråga..10.VAS.strep 
#          0.7845322           0.4689488           0.9719658 
(p.vals.Ont.i.öron <- run_annova('Ont.i.öron'))
#            Fråga.5             Fråga.6             Fråga.7            Fråga..8 
#         0.65299100          0.27232017          0.32673439          0.09375773 
#           Fråga..9   Fråga..10.VAS.CRP Fråga..10.VAS.strep 
#         0.90514744          0.12489450          0.46642420 
(p.vals.Hosta <- run_annova('Hosta'))
#            Fråga.5             Fråga.6             Fråga.7            Fråga..8 
#         0.32388528          0.34294922          0.43979613          0.03278197 
#           Fråga..9   Fråga..10.VAS.CRP Fråga..10.VAS.strep 
#         0.55829322          0.42841204          0.97432771 

## In other words - only the age of the child affects the pain results

## for all other questions I need more input.


## Now a pie chart for the 'BesöksUrsak'
data$BesöksUrsak <-  rep( '', nrow(data) )
for ( n in c( 6,7,8,10,11,12,13,14) ) {
	data$BesöksUrsak[which(data[,n] == 1)] <- paste( data$BesöksUrsak[which(data[,n] == 1)], colnames(data)[n])
}


## group that to more useful groups:
data$ManCause <- rep( '', nrow(data) )
oli <- NULL
oli <- c( oli , grep ( 'Hosta', data$BesöksUrsak ) )
oli <- c( oli , grep ( 'Halsont', data$BesöksUrsak ) )
oli <- c( oli , grep ( 'Ont.i.öron', data$BesöksUrsak ) )
oli <- c( oli , grep ( 'Förkylning', data$BesöksUrsak ) )
data$ManCause[unique(oli)] <- 'ÖLI'

data$ManCause[match(' Feber Diarre',data$BesöksUrsak)] <- 'GI'
data$ManCause[match(' Kräkningar Diarre',data$BesöksUrsak)] <- 'GI'
data$ManCause[which(is.na(match(data$BesöksUrsak," Diarre"))==F)] <- 'GI'

table( paste ( data$ManCause, data$BesöksUrsak ) )

data$ManCause[grep('UVI', data$BesöksUrsak )] <- 'UVI'

data$ManCause[which(is.na(match(data$BesöksUrsak," Feber"))==F)] <- 'oklart Feber'

data$ManCause[which(is.na(match(data$BesöksUrsak,"" ))==F)] <- 'ej angivna'

data$ManCause[which(is.na(match(data$ManCause,,"" ))==F)] <-  'ej angivna'


## oh great - the parents know how the kids react.
cor.test( as.numeric(as.character(data$Fråga..9)),  as.numeric(as.character(data$Fråga..10.VAS.CRP)) )

#	Pearson's product-moment correlation

#data:  as.numeric(as.character(data$Fråga..9)) and as.numeric(as.character(data$Fråga..10.VAS.CRP))
#t = 11.417, df = 65, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.7174187 0.8836752
#sample estimates:
#      cor 
#0.8168581 


#partent expect and kids feel the pain
cor.test( as.numeric(as.character(data$Fråga.6)),  as.numeric(as.character(data$Fråga..10.VAS.CRP)) )

#	Pearson's product-moment correlation

#data:  as.numeric(as.character(data$Fråga.6)) and as.numeric(as.character(data$Fråga..10.VAS.CRP))
#t = 7.9075, df = 65, p-value = 4.261e-11
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.5530345 0.8050265
#sample estimates:
#     cor 
#0.700223 

## and the parents see what they expected.
cor.test( as.numeric(as.character(data$Fråga.6)),  as.numeric(as.character(data$Fråga..9)) )

#	Pearson's product-moment correlation

#data:  as.numeric(as.character(data$Fråga.6)) and as.numeric(as.character(data$Fråga..9))
#t = 12.313, df = 152, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.6174964 0.7778674
#sample estimates:
#      cor 
#0.7066437 
#
#Warnmeldung:
#In cor.test(as.numeric(as.character(data$Fråga.6)), as.numeric(as.character(data$Fråga..9))) :
#  NAs durch Umwandlung erzeugt

summary(data$Åldern)/155 *100
#     0-3      4-6      7-9    10-13    14-17 
#38.70968 20.64516 13.54839 16.77419 10.32258 
pie(table(data$Åldern))

data$CRP <- rep('NO',nrow(data))
data$CRP[is.na(match(data$Fråga..10.VAS.CRP, 'na'))==T] <- 'Yes'

CRP <- data[which(data$CRP == 'Yes'),]
(table(paste(as.factor(CRP$CRP), CRP$Åldern)) / nrow(CRP) ) *100

#  Yes 0-3 Yes 10-13 Yes 14-17   Yes 4-6   Yes 7-9 
#43.697479 15.126050  7.563025 19.327731 14.285714 

# Show the numbers for Fråga..10.VAS.CRP for the age
res <- table(data$Åldern, data$Fråga..10.VAS.CRP)
res[ which(res == 0)] = '-'
write.table( res, file="Åldern_vs_Fråga10.VAS.CRP.xls", sep="\t" , quote=F)

#4-6 
mean(c(rep(0,3),rep(10,2),rep(2,5),rep(4,4),rep(5,4),rep(6,3),rep(7,1),rep(8,1)))
#[1] 4.304348
sd(c(rep(0,3),rep(10,2),rep(2,5),rep(4,4),rep(5,4),rep(6,3),rep(7,1),rep(8,1)))
#[1] 2.851392
#7-9
mean(c(rep(0,2),rep(10,1),rep(2,6),rep(3,2),rep(4,5),rep(6,1)))
#[1] 3.176471
sd(c(rep(0,2),rep(10,1),rep(2,6),rep(3,2),rep(4,5),rep(6,1)))
#[1] 2.324739
#10-13
mean(c(rep(0,10),rep(2,4),rep(4,3),rep(5,1)))
#[1] 1.388889
sd(c(rep(0,10),rep(2,4),rep(4,3),rep(5,1)))
#[1] 1.786844
#14-17
mean(c(rep(0,6),rep(2,3)))
#[1] 0.6666667
sd(c(rep(0,6),rep(2,3)))
#[1] 1



res2 <- table(data$Åldern, data$Fråga..9)
res2[ which(res2 == 0)] = '-'

#0-3
 mean(c(rep( 0,9),rep(1,5),rep(2,6),rep(3,7),rep(4,2),rep(5,5),rep(6,5),rep(7,8),rep(9,4),rep(10,2)))
#[1] 4.018868
 sd(c(rep( 0,9),rep(1,5),rep(2,6),rep(3,7),rep(4,2),rep(5,5),rep(6,5),rep(7,8),rep(9,4),rep(10,2)))
#[1] 3.06651
#4-6
mean(c(rep(0,7),rep(1,5),rep(2,4),rep(3,2),rep(4,4),rep(5,2),rep(6,4),rep(8,1),rep(10,3)))
#[1] 3.34375
sd(c(rep(0,7),rep(1,5),rep(2,4),rep(3,2),rep(4,4),rep(5,2),rep(6,4),rep(8,1),rep(10,3)))
#[1] 3.127499
#7-9
 mean(c(rep(0,7),rep(1,1),rep(2,3),rep(3,5),rep(4,2),rep(5,1),rep(6,1),rep(10,1)))
#[1] 2.428571
 sd(c(rep(0,7),rep(1,1),rep(2,3),rep(3,5),rep(4,2),rep(5,1),rep(6,1),rep(10,1)))
#[1] 2.521338
#10-13
mean(c(rep(0,9),rep(1,6),rep(2,4),rep(3,4),rep(4,3)))
#[1] 1.461538
 sd(c(rep(0,9),rep(1,6),rep(2,4),rep(3,4),rep(4,3)))
#[1] 1.420726
#14-17
mean(c(rep(0,11),rep(1,4),rep(3,1)))
#[1] 0.4375
sd (c(rep(0,11),rep(1,4),rep(3,1)))
#[1] 0.813941

res3 <- table(data$Åldern, data$Fråga..10.VAS.strep)
res3[ which(res3 == 0)] = '-'

#4-6
mean(c(rep(0,2),rep(10,1),rep(2,2),rep(4,1),rep(5,1),rep(6,2)))
#[1] 3.888889
sd(c(rep(0,2),rep(10,1),rep(2,2),rep(4,1),rep(5,1),rep(6,2)))
#[1] 3.25747
#7-9
mean(c(rep(0,2),rep(4,1),rep(6,1)))
#[1] 2.5
sd(c(rep(0,2),rep(4,1),rep(6,1)))
#[1] 3
#10-13
mean(c(rep(0,1),rep(1,2),rep(2,2),rep(4,3),rep(5,1),rep(6,1)))
#[1] 2.9
sd(c(rep(0,1),rep(1,2),rep(2,2),rep(4,3),rep(5,1),rep(6,1)))
#[1] 1.969207
#14-17
mean(c(rep(0,5),rep(10,1),rep(2,1),rep(6,1)))
#[1] 2.25
sd(c(rep(0,5),rep(10,1),rep(2,1),rep(6,1)))
#[1] 3.770184


data$CRP = is.na(as.numeric(as.character(data[,28])))==F
data$CRP[which(data[,28] =='x')] = TRUE

data$STREP = is.na(as.numeric(as.character(data[,29])))==F
data$STREP[which(data[,29] =='x')] = TRUE


mean_sd_age <- function( column, age, crp=T ) {
	if ( crp ) {
		ids <- which(data[,'Åldern'] == age & data[,'CRP'] )
	}else{
		ids <- which(data[,'Åldern'] == age & data[,'STREP'] )
	}
	#browser()
	m = mean( as.numeric(as.character( data[ids,column] )))
	s = sd ( as.numeric(as.character( data[ids,column] )))
	c (m , s)
}

 mean_sd_age( 'Fråga..9', '0-3', crp=T)
#[1] 4.384615 3.132085
 mean_sd_age( 'Fråga..9', '4-6', crp=T)
#[1] 3.130435 3.034714
 mean_sd_age( 'Fråga..9', '7-9', crp=T)
#[1] 2.470588 2.576763
 mean_sd_age( 'Fråga..9', '10-13', crp=T)
#[1] 1.444444 1.423427
 mean_sd_age( 'Fråga..9', '14-17', crp=T)
#[1] 0.3333333 0.5000000
 mean_sd_age( 'Fråga..9', '0-3', crp=F)
#[1] 6.000000 3.316625
 mean_sd_age( 'Fråga..9', '4-6', crp=F)
#[1] 3.888889 3.480102
 mean_sd_age( 'Fråga..9', '7-9', crp=F)
#[1] 2.250000 2.629956
 mean_sd_age( 'Fråga..9', '10-13', crp=F)
#[1] 1.700000 1.567021
 mean_sd_age( 'Fråga..9', '14-17', crp=F)
#[1] 0.500000 1.069045

plot( jitter(as.numeric(as.character(data$Fråga..9)),0.9), jitter( as.numeric(as.character(data$Fråga..10)),0.9))
cor.test( as.numeric(as.character(data$Fråga.6)),  as.numeric(as.character(data$Fråga..10)) )

#	Pearson's product-moment correlation

#data:  as.numeric(as.character(data$Fråga.6)) and as.numeric(as.character(data$Fråga..10))
#t = 7.5671, df = 93, p-value = 2.692e-11
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.4748496 0.7282514
#sample estimates:
#     cor 
#0.617314 


cor.test( as.numeric(as.character(data$Fråga..9)),  as.numeric(as.character(data$Fråga..10)) )

#	Pearson's product-moment correlation

#data:  as.numeric(as.character(data$Fråga..9)) and as.numeric(as.character(data$Fråga..10))
#t = 11.998, df = 93, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.6855800 0.8477917
#sample estimates:
#      cor 
#0.7794266 
#
#Warnmeldung:
#In cor.test.default(as.numeric(as.character(data$Fråga..9)), as.numeric(as.character(data#$Fråga..10))) :
#  NAs durch Umwandlung erzeugt

plot( as.numeric(as.character(data$Fråga..9[which(data$CRP)])) ~ data$Åldern[which(data$CRP)])
plot( as.numeric(as.character(data$Fråga..9[which(data$STREP)])) ~ data$Åldern[which(data$STREP)])

plot( as.numeric(as.character(data$Fråga..10[which(data$CRP)])) ~ data$Åldern[which(data$CRP)])
plot( as.numeric(as.character(data$Fråga..10[which(data$STREP)])) ~ data$Åldern[which(data$STREP)])


#t.test( as.numeric(as.character(data$kön
id_f =which(data$kön == 'f' & data$Åldern != '10-13')
id_p =which(data$kön == 'p' & data$Åldern != '10-13')

t.test( as.numeric(as.character(data$Fråga..10[id_f])), as.numeric(as.character(data$Fråga..10[id_p])))


#	Welch Two Sample t-test

#data:  as.numeric(as.character(data$Fråga..10[id_f])) and as.numeric(as.character(data$Fråga..10[id_p]))
#t = 0.22983, df = 39.934, p-value = 0.8194
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -1.366913  1.717668
#sample estimates:
#mean of x mean of y 
# 3.365854  3.190476 

#Warnmeldungen:
#1: In t.test(as.numeric(as.character(data$Fråga..10[id_f])), as.numeric(as.character(data$Fråga..10[id_p]))) :
#  NAs durch Umwandlung erzeugt
#2: In t.test.default(as.numeric(as.character(data$Fråga..10[id_f])),  :
#  NAs durch Umwandlung erzeugt

plot( as.numeric(as.character(data$Fråga..9)) ~ data$tidigare_provatgning, 
	ylab="Fråga 9",xlab="tidigare provtagning")

plot( as.numeric(as.character(data$Fråga..10)) ~ data$tidigare_provatgning, 
	ylab="Fråga 10",xlab="tidigare provtagning")




