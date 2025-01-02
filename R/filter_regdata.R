#' Function that filters the data and returns the filtered dataset along with a
#' text describing the filtering
#'
#' @inheritParams calculate_distribution
#' @param colour_palette Colour palette to be used in figures (Default: BlaaRapp)
#'
#' @return OutData A list containing the filtered dataset and a text describing
#' the filtering. A text string indicating the colour palette is also returned.
#'
#' @export
filter_regdata <- function(RegData,
                           dateFrom='2014-01-01',
                           dateTo="2100-01-01",
                           minage=0,
                           maxage=120,
                           isMan=99,
                           urgency=99,
                           ab_access='',
                           minPRS=0,
                           maxPRS=2.2,
                           ASA='',
                           whoEcog='',
                           modGlasgow = '',
                           malign=99,
                           op_group='',
                           ncsp='',
                           icd='',
                           daytime=99,
                           new_stomi=99,
                           accordion='',
                           colorpalette='BlaaRapp',
                           only_complete=TRUE)
{
  # Define intersect-operator
  "%i%" <- intersect

  Ninn <- dim(RegData)[1]
  indVarIncl <- 1:Ninn
  indAge <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
  indDato <- which(RegData$OperasjonsDato >= datoFra & RegData$OperasjonsDato <= datoTil)
  indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {indKj <- 1:Ninn}
  indOp_gr <- if (op_gruppe[1] != ''){which(RegData$Op_gr %in% as.numeric(op_gruppe))} else {indOp_gr <- 1:Ninn}
  indHast <- if (hastegrad %in% c(1,2)){which(RegData$Hastegrad == hastegrad)} else {indHast <- 1:Ninn}
  indDag <- if (dagtid %in% c(0,1)){which(RegData$Dagtid == dagtid)} else {indDag <- 1:Ninn}
  indBMI <- if (BMI[1] != '') {which(RegData$BMI_kodet %in% as.numeric(BMI))} else {indBMI <- 1:Ninn}
  indTilgang <- if (tilgang[1] != '') {which(RegData$Tilgang %in% as.numeric(tilgang))} else {indTilgang <- 1:Ninn}
  indTilgangUtvidet <- if (tilgang_utvidet[1] != '') {which(RegData$Tilgang_utvidet %in% as.numeric(tilgang_utvidet))} else {indTilgangUtvidet <- 1:Ninn}
  indPRS <- if ((minPRS>0) | (maxPRS<2.2)) {which(RegData$PRSScore >= minPRS & RegData$PRSScore <= maxPRS)} else {indPRS <- 1:Ninn}
  indASA <- if (ASA[1] != '') {which(RegData$ASA %in% as.numeric(ASA))} else {indASA <- 1:Ninn}
  indWHO <- if (whoEcog[1] != '') {which(RegData$WHOECOG %in% as.numeric(whoEcog))} else {indWHO <- 1:Ninn}
  indGlasgow <- if (modGlasgow[1] != '') {which(RegData$ModGlasgowScore %in% as.numeric(modGlasgow))} else {indGlasgow <- 1:Ninn}
  indNCSP <- if (ncsp[1] != '') {which(substr(RegData$Hovedoperasjon, 1, 5) %in% ncsp)} else {indNCSP <- 1:Ninn}
  indForb <- if (forbehandling[1] != '') {which(RegData$Forbehandling %in% as.numeric(forbehandling))} else {indForb <- 1:Ninn}
  indMalign <- if (malign %in% c(0,1)){which(RegData$Malign == malign)} else {indMalign <- 1:Ninn}
  indICD <- if (icd[1] != '') {which(RegData$Hoveddiagnose2 %in% icd)} else {indICD <- 1:Ninn}
  indRobot <- if (robotassiastanse %in% c(0,1)){which(RegData$Robotassistanse == robotassiastanse)} else {indRobot <- 1:Ninn}
  indFerdig <- if (kun_ferdigstilte) {which(RegData$OppfStatus == 1 | is.na(RegData$OppfStatus))} else {indFerdig <- 1:Ninn}
  indStomi <- if (ny_stomi %in% c(0,1)) {which(RegData$NyStomi == ny_stomi)} else {1:Ninn}
  indAccordion <- if (accordion[1] != '') {which(RegData$AccordionGrad %in% as.numeric(accordion))} else {1:Ninn}

  indMed <- indAld %i% indDato %i% indKj %i% indVarMed %i% indOp_gr %i% indElekt %i% indBMI %i%
    indTilgang %i% indPRS %i% indASA %i% indWHO %i% indForb %i% indMalign %i% indNCSP %i% indHast %i%
    indICD %i% indGlasgow %i% indDag %i% indRobot %i% indFerdig %i% indTilgangUtvidet %i%
    indStomi %i% indAccordion
  RegData <- RegData[indMed,]
  if (ncsp[1] != '') {ncsp <- sort(unique(substr(RegData$Hovedoperasjon, 1, 5)))}

  utvalgTxt <-
    c(paste('Operation date: ',
            min(RegData$OperasjonsDato, na.rm=T), ' til ', max(RegData$OperasjonsDato, na.rm=T), sep='' ),
      if ((minald>0) | (maxald<120)) {
        paste('Patients from ', min(RegData$Alder, na.rm=T), ' til ',
              max(RegData$Alder, na.rm=T), ' år', sep='')},
      if (erMann %in% 0:1) {paste('Gender: ', c('Woman', 'Man')[erMann+1], sep='')},
      if (op_gruppe[1] != '') {
        paste0('Operation group(s): ',
               paste(RegData$Operasjonsgrupper[match(op_gruppe, RegData$Op_gr)], collapse = ', '))},
      if (ncsp[1] != '') {
        paste0('NCSP-code(s): ', paste(ncsp[which(!is.na(ncsp[1:9]))], collapse=', '))},
      if (length(ncsp) > 9) {
        paste0('  ', paste(ncsp[which(!is.na(ncsp[10:20]))+9], collapse=', '))},
      if (length(ncsp) > 20) {
        paste0('  ', paste(ncsp[which(!is.na(ncsp[21:31]))+20], collapse=', '))},
      if (length(ncsp) > 31) {
        paste0('  ', paste(ncsp[which(!is.na(ncsp[32:42]))+31], collapse=', '))},
      if (elektiv %in% c(0,1)) {
        paste0('Time of operation: ', c('Within working hours', 'Outside of working hours')[elektiv+1])},
      if (dagtid %in% c(0,1)) {
        paste0('Operert dagtid: ', c('Nei', 'Ja')[dagtid+1])},
      if (hastegrad %in% c(1,2)) {
        paste0('Urgency: ', c('Elective', 'Emergency')[hastegrad])},
      if (BMI[1] != '') {
        paste0('BMI-category: ',
               paste(RegData$BMI_kategori[match(as.numeric(BMI), RegData$BMI_kodet)],
                     collapse=', '))},
      # if (length(valgtShus)>1) {paste0('Valgte RESH: ', paste(as.character(valgtShus), collapse=', '))},
      if (tilgang[1] != '') {
        paste0('Abdominal access: ',
               paste(c('Open', 'Laparoscopic', 'Converted', '',
                       'Endoskopisk')[as.numeric(tilgang)], collapse = ', '))},
      if ((minPRS>0) | (maxPRS<2.2)) {
        paste0('PRS-score from ', sprintf('%.2f', min(RegData$PRSScore, na.rm=T)), ' to ',
               sprintf('%.2f', max(RegData$PRSScore, na.rm=T)))},
      if (ASA[1] != '') {paste0('ASA-score: ', paste(ASA, collapse=','))},
      if (whoEcog[1] != '') {
        paste0('WHO ECOG score: ', paste(whoEcog, collapse=','))},
      if (modGlasgow[1] != '') {
        paste0('Modified Glasgow score: ', paste(modGlasgow, collapse=','))},
      if (malign %in% c(0,1)){
        paste0('Diagnosis: ', c('Benign', 'Malign')[malign+1])},
      if (icd[1] != '') {
        paste0('ICD-10-code(s): ', paste(sub("(\\w+).*", "\\1", icd), collapse=', '))},
      if (robotassiastanse %in% c(0,1)){
        paste0('Minimally invasive: ', c('Conv. laparoscopy', 'Robot assisted')[robotassiastanse+1])},
      if (ny_stomi %in% c(0,1)){paste0('New stomi: ', c('No', 'Yes')[ny_stomi+1])},
      if (accordion[1] != '') {
        paste0('Accordion score: ', paste(c("<3", "", 3:6)[sort(as.numeric(accordion))], collapse=','))},
      if (!kun_ferdigstilte){'Include non-closed registrations: Ja'}
    )


  UtData <- list(RegData=RegData,
                 utvalgTxt=utvalgTxt,
                 fargepalett=fargepalett) #GronnHNpms624,
  return(invisible(UtData))
}
