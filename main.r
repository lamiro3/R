##################데이터 전처리#####################

data2 = read.csv(file = "./data/follow_02_data.csv")
data3 = read.csv(file = "./data/follow_03_data.csv")
data4 = read.csv(file = "./data/follow_04_data.csv")
data5 = read.csv(file = "./data/follow_05_data.csv")

names(data3) = names(data2)
names(data4) = names(data2)
names(data5) = names(data2)

data23 = rbind(data2, data3)
data234 = rbind(data23, data4)
KRHEAL = rbind(data234, data5)

str(KRHEAL)
library(lattice)

KRHEAL = read.csv(file = "./data/KRHEAL.csv")

KRHEAL$T02_WEIGHT = ifelse(KRHEAL$T02_WEIGHT == 99999, NA, KRHEAL$T02_WEIGHT)
KRHEAL$T02_HEIGHT = ifelse(KRHEAL$T02_HEIGHT == 99999, NA, KRHEAL$T02_HEIGHT)
KRHEAL$T02_SBP = ifelse(KRHEAL$T02_SBP == 99999, NA, KRHEAL$T02_SBP)
KRHEAL$T02_DBP = ifelse(KRHEAL$T02_DBP == 99999, NA, KRHEAL$T02_DBP)
KRHEAL$T02_WAIST = ifelse(KRHEAL$T02_WAIST == 99999, NA, KRHEAL$T02_WAIST)
KRHEAL$T02_HIP = ifelse(KRHEAL$T02_HIP == 99999, NA, KRHEAL$T02_HIP)
KRHEAL$T02_PULSE = ifelse(KRHEAL$T02_PULSE == 99999, NA, KRHEAL$T02_PULSE)

KRHEAL[is.na(KRHEAL$T02_WEIGHT)] = mean(KRHEAL$T02_WEIGHT, na.rm = TRUE)
KRHEAL[is.na(KRHEAL$T02_HEIGHT)] = mean(KRHEAL$T02_HEIGHT, na.rm = TRUE)

#####################연속형 데이터 범주화#########################

KRHEAL$T02_AGE_2 = factor(KRHEAL$T02_AGE_2, 
                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                        labels = c("10's", "20's", "30's", "40's", "50's", "60's", "70's", "80's", "90's"))

KRHEAL$T02_DRINK = factor(KRHEAL$T02_DRINK,
                          levels = c(1, 2, 3),
                          labels = c("L", "M", "H"))

KRHEAL$T02_HEIGHT_2 = factor(KRHEAL$T02_HEIGHT_2,
                             levels = c(13:18),
                             labels = c("1.3x(m)", "1.4x(m)", "1.5x(m)", "1.6x(m)", "1.7x(m)", "1.8x(m)"))

KRHEAL$T02_WEIGHT_2 = factor(KRHEAL$T02_WEIGHT_2,
                             levels = c(4:9),
                             labels = c("4x", "5x", "6x", "7x", "8x", "9x"))

####################lattice - color custom######################
xyTheme = standard.theme('pdf')
densTheme = standard.theme('pdf')
dotTheme = standard.theme('pdf')
pulseTheme = standard.theme('pdf')
wahipTheme = standard.theme('pdf')

names(myTheme)
names(densTheme)
names(dotTheme)
names(bwTheme)
names(pulseTheme)
names(wahipTheme)

xyTheme$plot.symbol$col = c("#bc7cff")
xyTheme$plot.line$col = c("#bc7cff")
xyTheme$panel.background$col = "#edeff2"

dotTheme$dot.symbol$col = c("#bc7cff", "#7cff87", "#ff7ccc")

pulseTheme$box.dot$col = "#292e55"
pulseTheme$box.rectangle$col = "#bc7cff"
pulseTheme$box.umbrella$col = "#535daa"

pulseTheme$plot.symbol$col = "#535daa"
pulseTheme$par.title.text$font = "Lucida Console"
pulseTheme$par.title.text$lineheight = 2
pulseTheme$fontsize$text = 10

wahipTheme$box.dot$col = "#555029"
wahipTheme$box.rectangle$col = "#2a552d"
wahipTheme$box.umbrella$col = "#53aa5a"

wahipTheme$plot.symbol$col = "#557757"
wahipTheme$fontsize$text = 10

################음주량 - 체중(연령별)###########################
xyplot(T02_WEIGHT ~ T02_DRINK|T02_AGE_2, 
       data = KRHEAL,
       layout = c(5, 1),
       type = c('p', 'r'),
       main = 'Weight according to the amount of alcohol consumed by age group',
       xlab = 'the amount of alcohol consumed',
       ylab = 'Weight(kg)',
       par.settings = 'xyTheme',
       scales = list(fontfamily = 'serif'),
       strip = strip.custom(bg = '#FFF07C',
                            par.strip.text = list(col = '#353535', cex = 0.7,
                                                  font = 4)))
###############음주량 - BMI##################
densityplot(~T02_BMI, groups = T02_DRINK, data = KRHEAL,
            pch = c(1, 2, 3),
            lty = c(1, 2, 3),
            col = c('red', 'darkgreen', 'blue'),
            key = list(title='alc intake', 
                       text=list(levels(KRHEAL$T02_DRINK)),
                       points=list(pch = c(1, 2, 3), col = c('red', 'darkgreen', 'blue')),
                       lines=list(col=c('red', 'darkgreen', 'blue'), lty = c(1, 2, 3)),
                       cex.title=1, cex=0.9,
                       corner=c(1,1)),
            main = 'BMI according to the alcohol intake',
            xlab = 'BMI(kg/m^2)',
            xlim = c(15, 40),
            )

##################체중 - (수축기/이완기)혈압(음주량별)#####################
dotplot(T02_SBP ~ T02_WEIGHT_2|T02_DRINK,
        data = KRHEAL,
        groups = T02_DRINK,
        main = 'Relationship between systolic blood pressure and body weight according to alcohol intake',
        ylab = 'Systolic blood pressure(mmHg)',
        xlab = 'Weight(kg)',
        type = c('p', 'r'),
        par.settings = 'dotTheme',
        scales = list(fontfamily = 'serif'),
        strip = strip.custom(bg = '#FFF07C',
                             par.strip.text = list(col = '#353535', cex = 0.7,
                                                   font = 4)))

dotplot(T02_DBP ~ T02_WEIGHT_2|T02_DRINK,
        data = KRHEAL,
        groups = T02_DRINK,
        main = 'Relationship between diastolic blood pressure and body weight according to alcohol intake',
        ylab = 'Diastolic blood pressure(mmHg)',
        xlab = 'Weight(kg)',
        type = c('p', 'r'),
        par.settings = 'dotTheme',
        scales = list(fontfamily = 'serif'),
        strip = strip.custom(bg = '#FFF07C',
                             par.strip.text = list(col = '#353535', cex = 0.7,
                                                   font = 4)))

############허리둘레/엉덩이둘레 - 나이(음주량)#############

bwplot(T02_AGE_2~T02_WAIST|T02_DRINK, 
       data = KRHEAL,
       layout = c(1, 3),
       pch = "|",
       xlab = 'Average waist circumference(cm)',
       main = 'Distribution of the average waist circumference according to the alcohol intake',
       notch = TRUE,
       fill = '#53aa5a',
       par.settings = 'wahipTheme',
       scales = list(fontfamily = 'serif'),
       strip = strip.custom(bg = '#FFF07C',
                            par.strip.text = list(col = '#353535', cex = 0.7,
                                                  font = 4)))

bwplot(T02_AGE_2~T02_HIP|T02_DRINK, 
       data = KRHEAL,
       layout = c(1, 3),
       pch = "|",
       xlab = 'Average hip circumference(cm)',
       main = 'Distribution of the average hip circumference according to the alcohol intake',
       notch = TRUE,
       fill = '#53aa5a',
       par.settings = 'wahipTheme',
       scales = list(fontfamily = 'serif'),
       strip = strip.custom(bg = '#FFF07C',
                            par.strip.text = list(col = '#353535', cex = 0.7,
                                                  font = 4)))

################평균심박수 - 음주량(나이)####################
bwplot(T02_PULSE ~ T02_DRINK|T02_AGE_2, 
       data = KRHEAL,
       layout = c(5, 1),
       main = "Average heart rate according to the alcohol intake",
       xlab = 'alcohol intake',
       ylab = 'Average heart rate(Times/min)',
       par.settings = 'pulseTheme',
       scales = list(fontfamily = 'serif'),
       strip = strip.custom(bg = '#FFF07C',
                            par.strip.text = list(col = '#353535', cex = 0.7,
                                                  font = 4)))