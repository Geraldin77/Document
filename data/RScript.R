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


mean_sd_age <- function(  column, age, crp=T ) {
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

median_mad_age <- function( column, age, crp=T ) {
	if ( crp ) {
		ids <- which(data[,'Åldern'] == age & data[,'CRP'] )
	}else{
		ids <- which(data[,'Åldern'] == age & data[,'STREP'] )
	}
	#browser()
	m = median( as.numeric(as.character( data[ids,column] )))
	s = mad ( as.numeric(as.character( data[ids,column] )))
	c (m , s)
}

result_table <- function (column,  crp=T) {
	t <- NULL
	for ( n in levels(data[,'Åldern']) ) {
		t = rbind(t, median_mad_age ( column, n, crp ))
	}
	colnames(t) <- c( 'median', 'mad' )
	rownames(t) <- levels(data[,'Åldern'])
	t
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

##2017_08_30

cor.test( as.numeric(as.character(data$Fråga.7)) , as.numeric(as.character(data$Fråga..9)))


#	Pearson's product-moment correlation

#data:  as.numeric(as.character(data$Fråga.7)) and as.numeric(as.character(data$Fråga..9))
#t = -0.53347, df = 153, p-value = 0.5945
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.1993821  0.1153435
#sample estimates:
#        cor 
#-0.04308823 

cor.test( as.numeric(as.character(data$Fråga.7)) , as.numeric(as.character(data$Fråga..10)))

#	Pearson's product-moment correlation

#data:  as.numeric(as.character(data$Fråga.7)) and as.numeric(as.character(data$Fråga..10))
#t = -0.3933, df = 93, p-value = 0.695
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2403184  0.1621254
#sample estimates:
#        cor 
#-0.04074907 


cor.test( as.numeric(as.character(data$Fråga.6)) , as.numeric(as.character(data$Fråga..10)))

#	Pearson's product-moment correlation

#data:  as.numeric(as.character(data$Fråga.6)) and as.numeric(as.character(data$Fråga..10))
#t = 7.5671, df = 93, p-value = 2.692e-11
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.4748496 0.7282514
#sample estimates:
#     cor 
#0.617314 


cor.test( as.numeric(as.character(data$Fråga..9)) , as.numeric(as.character(data$Fråga..10)))

#	Pearson's product-moment correlation

#data:  as.numeric(as.character(data$Fråga..9)) and as.numeric(as.character(data$Fråga..10))
#t = 11.998, df = 93, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.6855800 0.8477917
#sample estimates:
#      cor 
#0.7794266 

plot( jitter(as.numeric(as.character(data$Fråga..9))) , jitter(as.numeric(as.character(data$Fråga..10))))

#And now the pupulation analysis...

summary(data)

#    Enkätnr        Åldern   kön     Antal.barn.i.familjen Mama.papa.närstående
# Min.   :  1.0   0-3  :60   f :87   1 :20                 m :103              
# 1st Qu.: 39.5   4-6  :32   na:13   2 :47                 mp:  4              
# Median : 78.0   7-9  :21   p :55   3 :17                 n :  5              
# Mean   : 78.0   10-13:26           4 : 1                 na:  4              
# 3rd Qu.:116.5   14-17:16           5 : 1                 p : 39              
# Max.   :155.0                      na:69                                     
                                                                              
# Förkylning Halsont Feber  morgontemperatur Ont.i.öron UVI     Hosta 
# 0:90       0:98    0:74   Min.   :37.30    0:140      0:150   0:96  
# 1:65       1:57    1:81   1st Qu.:38.20    1: 15      1:  5   1:59  
#                           Median :39.00                             
#                           Mean   :38.82                             
#                           3rd Qu.:39.50                             
#                           Max.   :40.80                             
#                           NA's   :102                               
# Kräkningar Diarre  Magont  Fråga.2a..J.N Fråga.2b.J.N
# 0:153      0:150   0:133   j:132         j :89       
# 1:  2      1:  5   1: 22   n: 23         n :56       
#                                          na:10       
                                                      
                                                      
                                                      
                                                      
# Fråga.3...1.schlechter..0.unverändert...1.besser. Fråga.4a.N.J Fråga.4a..Antal
# 0 :77                                             j :117       na     :105    
# 1 :24                                             n : 25       3      : 12    
# -1:53                                             na: 13       2      : 10    
# na: 1                                                          1      :  8    
#                                                                5      :  8    
#                                                                10     :  4    
#                                                                (Other):  8    
# Fråga.4bN.J   X4b.Antal      Fråga.5      Fråga.6      Fråga.7      
# j :97       na     :113   0      :31   0      :29   Min.   : 0.000  
# n :30       1      : 17   na     :18   5      :22   1st Qu.: 0.000  
# na:28       2      :  9   1      :16   1      :19   Median : 1.000  
#             3      :  6   5      :16   3      :19   Mean   : 2.206  
#             4      :  3   2      :14   4      :15   3rd Qu.: 3.000  
#             5      :  3   3      :14   2      :13   Max.   :10.000  
#             (Other):  4   (Other):46   (Other):38                   
#    Fråga..8         Fråga..9      Fråga..10.VAS.CRP Fråga..10.VAS.strep
# Min.   : 0.000   Min.   : 0.000   x      :52        na     :113        
# 1st Qu.: 0.000   1st Qu.: 0.000   na     :36        x      : 11        
# Median : 1.000   Median : 2.000   0      :21        0      : 10        
# Mean   : 2.219   Mean   : 3.045   2      :18        2      :  5        
# 3rd Qu.: 4.000   3rd Qu.: 5.000   4      :12        4      :  5        
# Max.   :10.000   Max.   :10.000   5      : 5        6      :  5        
#                                   (Other):11        (Other):  6        
#    CRP            STREP           Fråga..10  tidigare_provatgning
# Mode :logical   Mode :logical   x      :60   no : 22             
# FALSE:36        FALSE:113       0      :30   yes:133             
# TRUE :119       TRUE :42        2      :22                       
# NA's :0         NA's :0         4      :16                       
#                                 6      : 9                       
#                                 5      : 7                       
#                                 (Other):11                       
# tidigare_provatgning_binary
# Mode :logical              
# FALSE:22                   
# TRUE :133                  
# NA's :0       

## drop the double tested childreen:
data2 <- data[-which(data$STREP & data$CRP),]
cor.test( as.numeric(as.character(data2$Fråga..9)) , as.numeric(as.character(data2$Fråga..10)))

#	Pearson's product-moment correlation

#data:  as.numeric(as.character(data2$Fråga..9)) and as.numeric(as.character(data2$Fråga..10))
#t = 12.254, df = 90, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.6991314 0.8567807
#sample estimates:
#     cor 
#0.790722 

#Warnmeldung:
#In cor.test.default(as.numeric(as.character(data2$Fråga..9)), as.numeric(as.character(data2$Fråga..10))) :
#  NAs durch Umwandlung erzeugt

# If anything the data gets better - so we do not really care aboput that.


round(table(data$Åldern) / sum(table(data$Åldern)) * 100 ,1)
#  0-3   4-6   7-9 10-13 14-17 
# 38.7  20.6  13.5  16.8  10.3 

pdf( file="Age_pie.pdf", width=6, height=6)
pie(x=table(data$Åldern), clockwise=T, col=rainbow(5))
dev.off()

table(data$Fråga.2a..J.N) / 155 * 100
#       j        n 
#85.16129 14.83871

apply( data[,c('Kräkningar','Diarre','Magont')],1, function (x) sum(as.numeric(as.character(x))))

data$illnes = ''

for (n in colnames(data)[c(6,7,8,10,11,12,13,14,15)]){
	ids = which(data[,n] == 1)
	data$illnes[ids] = paste( data$illnes[ids], n)
}


## day 2017-08-31
## kids in the family - any influence on pain?
only_1 <- which(as.numeric(as.character(data$Antal.barn.i.familjen)) == 1 )
more_than_1 <- which(as.numeric(as.character(data$Antal.barn.i.familjen)) > 1 )

boxplot( as.numeric(as.character(data$Fråga..9))[only_1], as.numeric(as.character(data$Fråga..9))[more_than_1])

 t.test( as.numeric(as.character(data$Fråga..9))[only_1], as.numeric(as.character(data$Fråga..9))[more_than_1])

#	Welch Two Sample t-test

#data:  as.numeric(as.character(data$Fråga..9))[only_1] and as.numeric(as.character(data$Fråga..9))#[more_than_1]
#t = 1.2734, df = 29.317, p-value = 0.2129
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -0.598901  2.577689
#sample estimates:
#mean of x mean of y 
# 3.550000  2.560606 

wilcox.test( as.numeric(as.character(data$Fråga..9))[only_1], as.numeric(as.character(data$Fråga..9))[more_than_1])

#	Wilcoxon rank sum test with continuity correction

#data:  as.numeric(as.character(data$Fråga..9))[only_1] and as.numeric(as.character(data$Fråga..9))[more_than_1]
#W = 779.5, p-value = 0.2127
#alternative hypothesis: true location shift is not equal to 0

boxplot( as.numeric(as.character(data$Fråga..10))[only_1], as.numeric(as.character(data$Fråga..10))[more_than_1])

t.test( as.numeric(as.character(data$Fråga..10))[only_1], as.numeric(as.character(data$Fråga..10))[more_than_1])

#	Welch Two Sample t-test

#data:  as.numeric(as.character(data$Fråga..10))[only_1] and as.numeric(as.character(data#$Fråga..10))[more_than_1]
#t = 0.2117, df = 11.975, p-value = 0.8359
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -1.897599  2.305932
#sample estimates:
#mean of x mean of y 
# 2.600000  2.395833 

wilcox.test( as.numeric(as.character(data$Fråga..10))[only_1], as.numeric(as.character(data$Fråga..10))[more_than_1])

#	Wilcoxon rank sum test with continuity correction

#data:  as.numeric(as.character(data$Fråga..10))[only_1] and as.numeric(as.character(data$Fråga..10))[more_than_1]
#W = 244.5, p-value = 0.9321
#alternative hypothesis: true location shift is not equal to 0

two_group <- function ( data1, data2 ) {
	d <- vector('numeric', 1000)
        s <- vector('numeric', 1000)
	for ( i in 1:1000 ) {
		t <- wilcox.test( jitter(data1), jitter(data2) )
		d[i] <- t$p.value
		s[i] <- t$statistic
	}
	list( mean.p.value =mean (d), mean.statistics=mean(s))
}

two_group( as.numeric(as.character(data$Fråga..9))[only_1], as.numeric(as.character(data$Fråga..9))[more_than_1])
#$mean.p.value
#[1] 0.2321051

#$mean.statistics
#[1] 779.902


two_group( as.numeric(as.character(data$Fråga..10))[only_1], as.numeric(as.character(data$Fråga..10))[more_than_1] )
#$mean.p.value
#[1] 0.7956872

#$mean.statistics
#[1] 244.338


mean_cor_test <- function (colA, colB,  method="pearson" ) {
	d <- vector('numeric', 1000)
	r <- vector('numeric', 1000)
	for ( i in 1:1000 ) {
		t  <- cor.test( 
			jitter(as.numeric(as.character(data[, colA]))), 
			jitter(as.numeric(as.character(data[, colB]))),
			method=method
		)
		d[i] <- t$p.value
		r[i] <- t$estimate
	}
	list( mean.p.value =mean (d), mean.estimate=mean(r))
}

mean_cor_test( 'Fråga..8', 'Fråga..10')
#$mean.p.value
#[1] 1.435407e-12

#$mean.estimate
#[1] 0.6477754

#Es gab 50 oder mehr Warnungen (Anzeige der ersten 50 mit warnings())
mean_cor_test( 'Fråga..8', 'Fråga..9')
#$mean.p.value
#[1] 1.582381e-12
#
#$mean.estimate
#[1] 0.5292124


 mean_cor_test( 'Fråga..8', 'Fråga..9', 'spearman')
#$mean.p.value
#[1] 1.126521e-08

#$mean.estimate
#[1] 0.4704633

 mean_cor_test( 'Fråga..8', 'Fråga..10', 'spearman')
#$mean.p.value
#[1] 1.464139e-08

#$mean.estimate
#[1] 0.5748656

#Es gab 50 oder mehr Warnungen (Anzeige der ersten 50 mit warnings())

t = read.delim('all_data.csv', sep=';') # manually changed diagnos
DIAGNOS <- as.character(t[,36])
data$Diagnos = factor( str_replace(DIAGNOS, "^ ", "" ), levels=c("GI", "Isolerad feber", "Luftvägssymtom", "Luftvägssymtom+UVI", "UVI" ))


round(table(data$Diagnos) / sum( table(data$Diagnos) ) * 100,1)

#                GI     Isolerad feber     Luftvägssymtom Luftvägssymtom+UVI 
#              10.1               10.1               75.8                1.3 
#               UVI 
#               2.7 



# 2017-09-01

# Könsfördelningen + Diagram i Åldersgruppen


Plot = function (p, file ) {
	pdf(file=paste("../figures/",file,'.pdf',sep=''), width=4, height=4)
	print(p)
	dev.off()
	png(file=paste("../figures/",file,'.png',sep=''), width=600, height=600)
	print(p)
	dev.off()
}


data$data.9 <- as.numeric(as.character(data$Fråga..9))
data$data.10 <- as.numeric(as.character(data$Fråga..10))

data$tidigare_provtagning = FALSE
data$tidigare_provtagning[unique(c(
	which(data$Fråga.4a.N.J=='j'),
	which(data$Fråga.4bN.J=='j')
	))] = TRUE

CRP <- data[which(data$CRP),]
STREP <- data[which(data$STREP),]


library(reshape2)

ag <- table(data$kön , data$Åldern)
ag <- ag[-2,]
ag_melted = melt(ag)
colnames(ag_melted) <- c('kön', 'Ålder','value')

pl = ggplot(data=ag_melted, 
	aes (x=ag_melted$'Ålder', y=ag_melted$'value', fill=ag_melted$'kön')
     ) +
geom_bar(stat="identity", color="black", position=position_dodge())+
theme_minimal()
pl = pl + xlab('Ålder') + ylab('patient [n]') + labs(fill = "Kön")
	
Plot( pl, 'Åldersfördelning_kön')

write.table(ag_melted, file="../tables/Åldersfördelning_kön.xls", sep="\t", quote=F,row.names=F)


## Åldersfördelning_ tidigare provtagning

ta <- table(data$tidigare_provtagning , data$Åldern)

write.table(ta, file="tables/provtagning_age.tab", sep="\t",quote=F)
d <- read.delim("tables/provtagning_age.tab")
colnames(d) <- colnames(ta)
d = rbind(d, d[1,] / ( d[1,] +d[2,]) *100)
d = rbind(d, d[2,] / ( d[1,] +d[2,]) *100)

rownames(d) <- c('No','Yes', 'No [%]', 'Yes [%]' )

d = cbind( measure=rownames(d), d)


ta_melted = melt(d[4:3,])
ta_melted[,1] <- factor( as.character(ta_melted[,1]), levels=c('Yes [%]','No [%]'))

colnames(ta_melted) <- c( 'tidigare_provtagning', 'Ålder', 'value')

pl = ggplot(data=ta_melted, 
	aes (x=Ålder, y=value, fill=tidigare_provtagning)
     ) +
geom_bar(stat="identity", color="black", position=position_dodge())+
#geom_bar(stat="identity", color="black")+
theme_minimal()
pl = pl + xlab('Ålder') + ylab('patient [%]') + labs(fill = "tidigare provtagning")
#pl = pl + scale_fill_manual(values=c('green','lightgray'))

Plot(pl, 'Åldersfördelning_tidigare provtagning')

write.table(ta_melted, file="../tables/Åldersfördelning_tidigare provtagning.xls", sep="\t", quote=F,row.names=F)


#Total %
summary(data$tidigare_provtagning)
#   Mode   FALSE    TRUE    NA's 
#logical      22     133       0 

133/ (133+22) *100
#[1] 85.80645


# Diagramm boxplot Fråga..9 and Fråga..10 STREP, CRP and all



Plot(ggplot(data, aes( Åldern, data.9 )) +geom_boxplot() +ylab('') +
	scale_y_continuous(breaks=c(0,2,4,6,8, 10)),
	'Question_9_all' )


Plot(ggplot(CRP, aes( Åldern, data.9 )) +geom_boxplot() +ylab('')+
	scale_y_continuous(breaks=c(0,2,4,6,8, 10)),
	'Question_9_CRP' )

Plot(ggplot(STREP, aes( Åldern, data.9 )) +geom_boxplot() +ylab('')+
	scale_y_continuous(breaks=c(0,2,4,6,8, 10)),
	'Question_9_STREP' )


## question 10

Plot(ggplot(data, aes( Åldern, data.10 )) +geom_boxplot(na.rm = TRUE) +ylab('')+
	scale_y_continuous(breaks=c(0,2,4,6,8, 10)),
	'Question_10_all' )


Plot(ggplot(CRP, aes( Åldern, data.10 )) +geom_boxplot() +ylab('')+
	scale_y_continuous(breaks=c(0,2,4,6,8, 10)),
	'Question_10_CRP' )

Plot(ggplot(STREP, aes( Åldern, data.10 )) +geom_boxplot() +ylab('')+
	scale_y_continuous(breaks=c(0,2,4,6,8, 10)),
	'Question_10_STREP' )


## question 8

Plot(ggplot(data, aes( Åldern, Fråga..8 )) +geom_boxplot(na.rm = TRUE) +ylab('')+
	scale_y_continuous(breaks=c(0,2,4,6,8, 10)),
	'Question_8_all' )


Plot(ggplot(CRP, aes( Åldern, Fråga..8 )) +geom_boxplot() +ylab('')+
	scale_y_continuous(breaks=c(0,2,4,6,8, 10)),
	'Question_8_CRP' )

Plot(ggplot(STREP, aes( Åldern, Fråga..8 )) +geom_boxplot() +ylab('')+
	scale_y_continuous(breaks=c(0,2,4,6,8, 10)),
	'Question_8_STREP' )


## sum up all statistics that have been run and should/could end up in the final document


mean_cor_test <- function (colA, colB,  method="pearson" ) {
	d <- vector('numeric', 1000)
	r <- vector('numeric', 1000)
	for ( i in 1:1000 ) {
		t  <- cor.test( 
			jitter(as.numeric(as.character(data[, colA]))), 
			jitter(as.numeric(as.character(data[, colB]))),
			method=method
		)
		d[i] <- t$p.value
		r[i] <- t$estimate
	}
	list( test=paste(method, 'n=1000'), comparing = paste( colA, colB, sep=" vs. "), mean.p.value =mean (d), mean.estimate=mean(r))
}

two_group <- function ( data1, data2 ) {
	d <- vector('numeric', 1000)
        s <- vector('numeric', 1000)
	for ( i in 1:1000 ) {
		t <- wilcox.test( jitter(data1), jitter(data2) )
		d[i] <- t$p.value
		s[i] <- t$statistic
	}
	list(test='wilcox.test n=1000', comparing = paste( colA, colB, sep=" vs. "),  mean.p.value =mean (d), mean.statistics=mean(s))
}


stat_results = NULL

mean_cor_test('Fråga.6', 'Fråga..9', 'spearman')
mean_cor_test('Fråga.6', 'Fråga..10.VAS.CRP', 'spearman')
mean_cor_test('Fråga.6', 'Fråga..10.VAS.strep', 'spearman')
mean_cor_test('Fråga.6', 'Fråga..10', 'spearman')

mean_cor_test('Fråga.7', 'Fråga..9', 'spearman')
mean_cor_test('Fråga.7', 'Fråga..10.VAS.CRP', 'spearman')
mean_cor_test('Fråga.7', 'Fråga..10.VAS.strep', 'spearman')
mean_cor_test('Fråga.7', 'Fråga..10', 'spearman')

mean_cor_test('Fråga..8', 'Fråga..9', 'spearman')
mean_cor_test('Fråga..8', 'Fråga..10.VAS.CRP', 'spearman')
mean_cor_test('Fråga..8', 'Fråga..10.VAS.strep', 'spearman')
mean_cor_test('Fråga..8', 'Fråga..10', 'spearman')

mean_cor_test( 'Fråga..9', 'Fråga..10.VAS.CRP', 'spearman')
mean_cor_test( 'Fråga..9', 'Fråga..10.VAS.strep', 'spearman')
mean_cor_test('Fråga..9', 'Fråga..10', 'spearman')












