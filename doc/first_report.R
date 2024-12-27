library(dplyr)
rm(list = ls())

centre <- read.csv("~/softlinks/mydata/ethirgast/centre_202405131159.csv")
mce <- read.csv("~/softlinks/mydata/ethirgast/mce_202405131200.csv")
ninetydays <- read.csv("~/softlinks/mydata/ethirgast/ninetydays_202405131200.csv")
patient <- read.csv("~/softlinks/mydata/ethirgast/patient_202405131201.csv")
readmission <- read.csv("~/softlinks/mydata/ethirgast/readmission_202405131201.csv")
registration <- read.csv("~/softlinks/mydata/ethirgast/registration_202405131201.csv")

regdata_all <- registration %>%
  filter(CENTREID == 1) %>%
  merge(readmission, by = "MCEID",
        suffixes = c("", "_READMISSION"),
        all.x = TRUE) %>%
  merge(ninetydays, by = "MCEID",
        suffixes = c("", "_READMISSION90"),
        all.x = TRUE) %>%
  merge(mce[,c("MCEID", "PATIENT_ID")], by = "MCEID",
        all.x = TRUE) %>%
  merge(patient[,c("ID", "BIRTH_DATE", "GENDER")],
        by.x = "PATIENT_ID", by.y = "ID",
        all.x = TRUE) %>%
  mutate(PatientAge = lubridate::year(lubridate::as.period(lubridate::interval(
    start = BIRTH_DATE, end = OPERATION_DATE))))

regdata <- regdata_all %>%
  filter(STATUS == 1)

regdata <- regdata %>%
  mutate(
    operation_group = case_when(
      (substr(NCSP, 1, 3) == "JFB" |
         (substr(NCSP, 1, 3) == "JFB" & (substr(NCSP, 4, 5) %in% 20:64))) ~ "Colonic resection",
      substr(NCSP, 1, 3) == "JGB" ~ "Rectal resection",
      substr(NCSP, 1, 3) == "JCC" ~ "Oesofagus resection",
      substr(NCSP, 1, 3) %in% c("JDC", "JDD") ~ "Ventricle resection",
      substr(NCSP, 1, 3) %in% c("JJB") ~ "Liver resection",
      substr(NCSP, 1, 5) %in% c("JLC30","JLC31") ~ "Whipple's procedure",
      substr(NCSP, 1, 5) %in% c('JLC10','JLC11', 'JLC00',
                                'JLC20','JLC40', 'JLC50', 'JLC96') ~ "Distal and other pancreatic"
    ),
    operation_group = ifelse(is.na(operation_group), "Other", operation_group),
    OPERATION_DATE = as.Date(OPERATION_DATE),
    DISCHARGE_DATE = as.Date(DISCHARGE_DATE),
    IN_HOUSE_DEATH_DATE = as.Date(IN_HOUSE_DEATH_DATE)
  )

############ Figure age/gender #################################################
gr <- c(0, seq(20, 80, 10)+1, 120)
regdata$VariabelGr <- cut(regdata$PatientAge, breaks=gr, include.lowest=TRUE, right=FALSE)
grtxt <- c('0-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '80+')
AntHoved <- table(regdata[, c("GENDER", "VariabelGr")])
NHoved <- rowSums(AntHoved)
outfile <- ""
FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett="BlaaOff", pointsizePDF=12)
farger <- FigTypUt$farger
ymax <- max(AntHoved,na.rm=T)*1.25
pos <- barplot(AntHoved, beside=TRUE, las=1, ylab="Number of patients",
               sub="Age group",
               col=farger[c(1,2)], border='white', ylim=c(0, ymax), xaxt='n')
mtext(at=colMeans(pos), grtxt, side=1, las=1, cex=1, adj=0.5, line=0.5)
legend('topright', c(paste0('Male, N=', NHoved[1]), paste0('Female, N=', NHoved[2])), bty='n',
       fill=farger[c(1,2)], border=NA, ncol=1, cex=1)
title(main = "Age at time of operation", line=1, font.main=1, cex.main=1.3)
if ( outfile != '') {dev.off()}



#### Inkluder ACCORDION SCORE fra oppfølgingsskjema
regdata <- regdata %>%
  mutate(ACCORDION_SCORE = as.character(ACCORDION_SCORE),
         ACCORDION_SCORE = ifelse(ACCORDION_SCORE=="Less than 3", 1,
                                  as.numeric(ACCORDION_SCORE)),
         ACCORDION_SCORE_READMISSION = as.character(ACCORDION_SCORE_READMISSION),
         ACCORDION_SCORE_READMISSION = ifelse(ACCORDION_SCORE_READMISSION=="Less than 3",
                                              1, as.numeric(ACCORDION_SCORE_READMISSION)),
         ACCORDION_SCORE_READMISSION = ifelse(STATUS_READMISSION != 1, NA,
                                              ACCORDION_SCORE_READMISSION))
regdata$ACCORDION_SCORE[!is.na(pmax(regdata$ACCORDION_SCORE, regdata$ACCORDION_SCORE_READMISSION))] <-
  pmax(regdata$ACCORDION_SCORE, regdata$ACCORDION_SCORE_READMISSION)[!is.na(pmax(regdata$ACCORDION_SCORE,
                                                                                 regdata$ACCORDION_SCORE_READMISSION))]
regdata$WoundDehiscence <- 0
regdata$WoundDehiscence[which(regdata$RELAPAROTOMY_YES==4 | regdata$RELAPAROTOMY_YES_READMISSION==4)] <- 1
regdata$Anastomosis_leak <- NA
regdata$Anastomosis_leak[regdata$ANASTOMOSIS == 1] <- 0
regdata$Anastomosis_leak[which(regdata$RELAPAROTOMY_YES==1 | regdata$RELAPAROTOMY_YES_READMISSION==1)] <- 1
regdata$Relap <- regdata$RELAPAROTOMY
regdata$Relap[!is.na(regdata$RELAPAROTOMY_READMISSION)] <- pmax(regdata$RELAPAROTOMY, regdata$RELAPAROTOMY_READMISSION)[!is.na(regdata$RELAPAROTOMY_READMISSION)]


# hastegrad : URGENCY elektiv = 1, øhjelp = 2
regdata$Daytime <- NA
regdata$Daytime[as.numeric(regdata$ANESTHESIA_START) %in% 7:15] <- 1
regdata$Daytime[as.numeric(regdata$ANESTHESIA_START) %in% c(1:6, 16:24)] <- 0

WoundDehiscence <- sum(regdata %>% select(WoundDehiscence))
WoundDehiscence_N <- sum(!is.na(regdata %>% select(WoundDehiscence)))
Relap <- sum(regdata %>% select(Relap), na.rm = TRUE)
Relap_N <- sum(!is.na(regdata %>% select(Relap)))
Anastomosis_leak <- sum(regdata %>% select(Anastomosis_leak), na.rm = TRUE)
Anastomosis_leak_N <- sum(!is.na(regdata %>% select(Anastomosis_leak)))






# summerydata <- regdata %>%
#   summarise(
#     WoundDehiscence = sum(WoundDehiscence),
#     N=n()
#
#   )











