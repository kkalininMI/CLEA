#' @title BuildENPdatasets function
#' @description This function builds the party nationalization datasets at three different levels
#'              of aggregation:  the national level, the party level, and the constituency level.
#' @usage BuildENPdatasets(dat=NULL, mn.el=FALSE, ineq.ind="Gini",
#'                         filterIndependents=FALSE, filterSingleConstituencies=TRUE)
#' @param dat  dataset "CLEA Lower/Upper Chamber Elections Archive". If omitted, the script downloads data from the CLEA website.
#' @param mn.el adds month to the index variable
#' @param ineq.ind  changes inequality index (by default computes indices based on "Gini"; for more detail about other indices see ineq() package)
#' @param filterIndependents  filters out independent candidates (by default filterIndependents=FALSE, i.e. includes independents)
#' @param filterSingleConstituencies  filters out single-member constituencies (by default filterSingleConstituencies=FALSE,
#'                                    i.e. includes single constituencies)
#' @export
#' @import foreign
#' @import readstata13
#' @import magrittr
#' @import dplyr
#' @import ineq
#' @import XML
#' @importFrom utils unzip download.file
#' @importFrom stats D
#' @return A list with the following parameters:
#' \itemize{
#'   \item national.level.data  -  national nationalization dataset
#'   \item party.level.data -  party nationalization dataset
#'   \item constituency.level.data - constituency nationalization dataset
#' }
#' @examples
#'
#' library(CLEA)
#' library(readstata13)
#'
#' # Open dat file to create ENP datasets
#'  dat<-read.dta13(
#'            unzip(
#'              system.file("clea_lc_20190617.zip", package="CLEA")
#'              ), convert.factors = FALSE)
#'  enp_data<-BuildENPdatasets(dat, filterIndependents=TRUE)
#'
#'
#' # Download file from http://www.electiondataarchive.org/ and create ENP datasets
#'  enp_data<-BuildENPdatasets(dat, filterSingleConstituencies=FALSE)
#'


BuildENPdatasets<-function(dat=NULL, mn.el=FALSE, ineq.ind="Gini", filterIndependents=FALSE,
                           filterSingleConstituencies=TRUE){

  if (is.null(dat)){
    doc.html = htmlTreeParse(
      'http://www.electiondataarchive.org/clea-lower-chamber-elections-archive.php',
      useInternalNodes = TRUE)

    doc.text = xpathSApply(doc.html, "//a/@href")
    download.link <-  doc.text[which(grepl("clea_lc_\\d+_stata.zip",  doc.text))]

    temp <- tempfile()
    print("Loading data from http://www.electiondataarchive.org/...", quote=FALSE)
    download.file(paste("http://www.electiondataarchive.org", download.link, sep=""),temp)
    print("Opening data file...", quote=FALSE)
    dat<-gsub(".zip", "", gsub("/data/releases/", "", download.link))
    data <- read.dta13(unzip(temp, paste(dat,"/", gsub("_stata", ".dta", dat),
                                         sep="")), convert.factors = FALSE)
    unlink(temp)
  } else {
    data <- dat
  }

  if ((mn.el==TRUE & !"mn.el"%in%names(data))|mn.el==FALSE){data$mn.el=data$mn}

  if (ineq.ind!="Gini" & ineq.ind%in%c("Gini", "RS", "Atkinson", "Theil",
                                       "Kolm", "var", "square.var", "entropy")==FALSE){ineq.ind="Gini"}

  data.grid<-data%>%
    select(ctr_n, ctr, yr, mn.el, cst,  cst_n, pty, pty_n)

  data<-data %>%select(ctr_n, ctr, yr, mn.el, cst,  cst_n, pty, pty_n, vv1, pv1, seat)

  data.a<-data.b<-data %>%
    do((function(x) {
      x$vv1<-ifelse(x$vv1<0,NA,x$vv1)
      x$pv1<-ifelse(x$pv1<0,NA,x$pv1)
      na_cand <- ifelse(is.na(x$vv1) | is.na(x$pv1 ),1,0)
      data.frame(x, na_cand)})(.)) %>%
    distinct(.keep_all = TRUE) %>%
    filter(na_cand==0)%>%
    select(-na_cand)

  data.c<-data %>%
    do((function(x) {
      x$vv1<-ifelse(x$vv1<0,NA,x$vv1)
      x$pv1<-ifelse(x$pv1<0,NA,x$pv1)
      x$seat<-ifelse(x$seat<0,NA,x$seat)
      na_cand <- ifelse(is.na(x$vv1) | is.na(x$pv1) | is.na(x$seat),1,0)
      data.frame(x, na_cand)})(.)) %>%
    distinct(.keep_all = TRUE) %>%
    filter(na_cand==0)%>%
    select(-na_cand)


  if(filterSingleConstituencies==TRUE){
    if(filterIndependents==TRUE){
      data.a<-data.a %>%
        group_by(ctr, yr, mn.el)%>%
        arrange(cst)%>%
        do((function(x) {
          sin_const <- ifelse(x$cst[1] == 1 & x$cst[length(x$cst)]==1, 1, 0)
          data.frame(x, sin_const)})(.)) %>%
        filter(sin_const==0,  pty!= 6000) %>%
        select(-sin_const)%>%
        ungroup()

      data.b<-data.b %>%
        filter(pty!= 6000)

      data.c<-data.c %>%
        filter(pty!= 6000)}else{
          data.a<-data.a %>%
            group_by(ctr, yr, mn.el)%>%
            arrange(cst)%>%
            do((function(x) {
              sin_const <- ifelse(x$cst[1] == 1 & x$cst[length(x$cst)]==1, 1, 0)
              data.frame(x, sin_const)})(.)) %>%
            filter(sin_const==0) %>%
            select(-sin_const) %>%
            ungroup()}}

  if(filterSingleConstituencies==FALSE & filterIndependents==TRUE){
    data.a<-data.a %>%
      filter(pty!= 6000)
    data.b<-data.b %>%
      filter(pty!= 6000)
    data.c<-data.c %>%
      filter(pty!= 6000)}

  #Main Script
  print("Computing inequality measures...", quote=FALSE)

  start.timer <- proc.time()
  gini<-data.b%>%
    distinct(ctr, yr, mn.el, cst, pty, .keep_all = TRUE)%>%
    group_by(ctr, yr, mn.el, cst, pty) %>%
    mutate(
      vv1=sum(vv1,na.rm=TRUE),
      pv1=sum(pv1,na.rm=TRUE)) %>%
    group_by(ctr, yr, mn.el) %>%
    mutate(
      nat_vv1 = sum(pv1, na.rm=TRUE)) %>%
    group_by(ctr, yr, mn.el, pty) %>%
    mutate(
      pty_nat_vv1 = sum(pv1, na.rm=TRUE)) %>%
    mutate(
      nat_pvs = pty_nat_vv1/nat_vv1) %>%
    filter(nat_pvs>0.05) %>%
    mutate(
      pvs=pv1/vv1,
      pvs=ifelse(is.infinite(pvs), NA, pvs),
      pvs=ifelse(is.na(pvs), 0, pvs))%>%
    group_by(ctr, yr, mn.el, pty) %>%
    do((function(x) {
      giniI<-ineq(x$pvs, NULL,  type = ineq.ind, na.rm=TRUE)
      data.frame(x, giniI)})(.)) %>%
    mutate(
      giniI=ifelse(giniI<0,0,giniI),
      giniI=ifelse(giniI==0,NA,giniI)) %>%
    select(ctr_n, ctr, yr, mn.el, cst, pty, giniI, pv1, vv1) %>%
    arrange(ctr_n, ctr, yr, mn.el, cst, pty) %>%
    as.data.frame()

  print("Computing Party Level Dataset...", quote=FALSE)

  party.level<-data.b %>%
    distinct(.keep_all = TRUE) %>%
    left_join(gini) %>%
    mutate(
      PNS= 1 - giniI)%>%
    group_by(ctr, yr)%>%
    mutate(
      district_size=length(unique(cst)),
      PNS_s = PNS^(1/(log(district_size))))%>%
    distinct(ctr, yr, mn.el, cst, pty, .keep_all = TRUE)%>%
    group_by(ctr, yr, mn.el)%>%
    mutate(
      nat_vv1=as.double(sum(pv1, na.rm=TRUE)))%>%
    group_by(ctr, yr, mn.el, pty)%>%
    mutate(
      pty_vv1=as.double(sum(pv1, na.rm=TRUE)),
      denominator = nat_vv1 * pty_vv1,
      cst_vv1=vv1,
      vote_share=pv1/cst_vv1)%>%
    arrange(ctr, yr, mn.el, pty, vote_share) %>%
    group_by(ctr, yr, mn.el, pty)%>%
    mutate(
      p_j=cumsum(pv1),
      inside = cst_vv1 * (p_j - (pv1/2)),
      numerator = sum(inside, na.rm=TRUE),
      PNS_w = (2 * numerator)/ denominator)%>%
    group_by(ctr, yr, mn.el, cst)%>%
    mutate(
      cst_vv1_new = sum(pv1, na.rm=TRUE),
      vote_share_new = pv1/cst_vv1_new) %>%
    group_by(ctr, yr, mn.el, pty)%>%
    mutate(
      p_j_new=cumsum(pv1),
      inside_new = cst_vv1_new * (p_j_new - (pv1/2)),
      numerator_new = sum(inside_new, na.rm=TRUE),
      PNS_w_new = (2 * numerator_new)/ denominator,
      alt_vv1 = 0,
      alt_vv1 = ifelse(PNS_w>=1, 1, alt_vv1),
      PNS_w = ifelse(PNS_w>=1, PNS_w_new, PNS_w),
      PNS_w = ifelse(PNS_w==1, NA, PNS_w),
      PNS_w = ifelse(PNS_w>=1, PNS_w_new, PNS_w),
      PNS_s = ifelse(PNS_s==0, NA, PNS_s),
      PNS_w = ifelse(pty == 3999|pty >= 6000|ctr == 292 |
                       ctr ==376|ctr == 674|
                       (ctr == 840 & yr == 1959 & cst ==435), NA, PNS_w),
      alt_vv1 = ifelse(is.na(PNS_w), 0, alt_vv1))%>%
    select(ctr_n, ctr, yr, mn.el, cst, pty_n, pty, cst_vv1, nat_vv1, PNS, PNS_s, PNS_w, alt_vv1)%>%
    mutate(
      top = nat_vv1^2,
      square = cst_vv1^2)%>%
    group_by(ctr, yr, mn.el, cst)%>%
    mutate(
      pid=1:length(nat_vv1),
      helper = rep(NA,length(nat_vv1)),
      helper = ifelse(pid==1, square, helper)) %>% group_by(ctr, yr)%>%
    mutate(
      bottom=sum(helper, na.rm=TRUE),
      power_E = top/bottom,
      PNS_sw = (PNS_w)^(1/(log10(power_E))),
      PNS_sw = ifelse(is.na(PNS_sw), NA, PNS_sw),
      PNS_sw = ifelse(PNS_sw>=1, NA, PNS_sw),
      PNS = ifelse(PNS==1, NA, PNS),
      PNS_s = ifelse(PNS_s==1, NA, PNS_s),
      PNS_s = ifelse(pty >= 3996 & pty < 5000, NA, PNS_s),
      PNS_w=ifelse(pty >= 3996 & pty < 5000, NA, PNS_w),
      PNS_sw=ifelse(pty >= 3996 & pty < 5000, NA, PNS_sw))%>%
    right_join(data.grid)%>%
    select(ctr_n, ctr, yr, mn.el, pty_n, pty, PNS, PNS_s, PNS_w, PNS_sw)%>%
    arrange(ctr_n, ctr, yr, mn.el, pty_n, pty, PNS, PNS_s, PNS_w, PNS_sw)%>%
    distinct(ctr, yr, mn.el, pty, .keep_all = TRUE)
  print("Party Level Dataset has been successfully computed")

  #******************************************
  #*****Party Nationalization Measures*******
  #********National Level Dataset************
  #**************December 2016***************
  #******************************************
  print("Computing National Level Dataset...", quote=FALSE)

  national.level.nat<-data.b %>%
    distinct(ctr, yr, mn.el, cst, pty, .keep_all = TRUE)%>%
    group_by(ctr, yr, mn.el, pty) %>%
    mutate(
      pv1=sum(pv1, na.rm=TRUE)) %>%
    distinct(ctr, yr, mn.el, pty, .keep_all = TRUE)%>%
    group_by(ctr, yr, mn.el) %>%
    mutate(
      nat_vv1 = sum(pv1, na.rm=TRUE),
      party_prop_nat2 = (pv1/nat_vv1)^2,
      denom = sum(party_prop_nat2),
      ENEP_nat = 1/denom) %>%
    select(ctr_n, ctr, yr, mn.el, ENEP_nat)%>%
    mutate(
      ENEP_nat= ifelse(yr < 1834 & ctr == 840, NA, ENEP_nat),
      ENEP_nat= ifelse(yr == 1950 & ctr == 410, NA, ENEP_nat))%>%
    distinct(ctr_n, ctr, yr, mn.el, .keep_all = TRUE)%>%
    arrange(ctr_n, ctr, yr, mn.el)


  national.level.nat2<-data.b %>%
    select(ctr, yr, mn.el, cst, pty, vv1, pv1)%>%
    distinct(.keep_all = TRUE)%>%
    group_by(ctr, yr, mn.el, cst, pty)%>%
    mutate(
      pv1=sum(pv1))%>%
    arrange(ctr, yr, mn.el, cst, pty)%>%
    distinct(.keep_all = TRUE)%>%
    group_by(ctr, yr, mn.el, cst)%>%
    mutate(
      new_vv1=sum(pv1),
      share2 = (pv1/new_vv1)^2,
      denom = sum(share2),
      ENEP_cst = 1/denom,
      indicator = 0,
      indicator= ifelse(new_vv1 != vv1, 1,  indicator))%>%
    distinct(ctr, yr, mn.el, cst, .keep_all = TRUE)%>%
    group_by(ctr, yr, mn.el)%>%
    mutate(
      indicator = ifelse(sum(indicator, na.rm=TRUE)>0,1,indicator),
      nat_vv1 = sum(new_vv1, na.rm=TRUE),
      cst_wght =  new_vv1/nat_vv1,
      weighted =  cst_wght * ENEP_cst,
      ENEP_wght = sum(weighted, na.rm=TRUE),
      ENEP_avg = mean(ENEP_cst, na.rm=TRUE))%>%
    right_join(national.level.nat)%>%
    select(ctr_n, ctr, yr, mn.el, ENEP_nat, ENEP_avg, ENEP_wght, indicator)%>%
    mutate(
      ENEP_avg= ifelse(yr < 1834 & ctr == 840, NA,  ENEP_avg),
      ENEP_avg= ifelse(yr == 1950 & ctr == 410, NA, ENEP_avg),
      ENEP_wght= ifelse(yr < 1834 & ctr == 840, NA, ENEP_wght),
      ENEP_wght= ifelse(yr == 1950 & ctr == 410, NA, ENEP_wght))%>%
    distinct(ctr_n, ctr, yr, mn.el, .keep_all = TRUE)%>%
    mutate(
      Cox =  (ENEP_nat - ENEP_avg)/ ENEP_nat,
      MK_I = (ENEP_nat -  ENEP_avg)/ ENEP_avg,
      MK_I_w = (ENEP_nat - ENEP_wght)/ENEP_wght)

  national.level.mk_n<- data.b %>%
    distinct(ctr, yr, mn.el, pty, cst, .keep_all = TRUE)%>%
    group_by(ctr, yr, mn.el, cst)%>%
    mutate(
      new_vv1=sum(pv1, na.rm=TRUE),
      share2 = (pv1/new_vv1) * (pv1/new_vv1),
      denom = sum(share2, na.rm=TRUE),
      ENEP_cst = 1/denom)%>%
    left_join(national.level.nat2)%>%
    mutate(
      I_i = (( ENEP_nat-ENEP_cst)/ ENEP_cst) * 100)%>%
    group_by(ctr, yr, mn.el)%>%
    mutate(
      alpha = .5,
      beta = .25,
      gamma = .5,
      n_districts = max(cst),
      nat_vote = sum(new_vv1, na.rm=TRUE),
      cst_vote_proportion = new_vv1/nat_vote,
      product =  ENEP_cst *  cst_vote_proportion,
      sum_cst = sum(product, na.rm=TRUE),
      denominator = n_districts * sum_cst,
      W_tilde = ENEP_cst/ denominator,
      I_w = ((ENEP_nat - sum_cst)/sum_cst)*100,
      numerator = (I_i -  I_w)^2 * W_tilde,
      sum_numerator = sum(numerator, na.rm=TRUE),
      coeff_var_I_i = sqrt(sum_numerator)/I_w,
      numerator2 = ((I_i - I_w)^4) * W_tilde,
      sum_numerator2 = sum(numerator2, na.rm=TRUE),
      denominator2 = ((I_i - I_w)^2) * W_tilde,
      sum_denominator2 = sum(denominator2, na.rm=TRUE),
      sq_sum_denominator2 = (sum_denominator2)^2,
      kurtosis_I_i = sum_numerator2/sq_sum_denominator2,
      D =  (coeff_var_I_i)^gamma * (kurtosis_I_i)^(1 - gamma),
      MK_N = ((I_w)^alpha) * (D^(1 - alpha)),
      MK_N_two = ((I_w)^alpha) *  ((coeff_var_I_i)^beta) * ((kurtosis_I_i)^(1 - alpha - beta)))%>%
    arrange(ctr, yr, mn.el, cst, pty)%>%
    group_by(ctr, yr, mn.el)%>%
    mutate(
      nat_vv1 = sum(pv1, na.rm=TRUE))%>%
    group_by(ctr, yr, mn.el, pty)%>%
    mutate(
      pty_vv1 = sum(pv1, na.rm=TRUE))%>%
    select(-c(cst,cst_n, pty_n))%>%
    distinct(ctr, yr, mn.el, pty, .keep_all = TRUE)%>%
    left_join(party.level)%>%
    arrange(ctr, ctr_n, yr, mn.el, pty, pty_n)%>%
    mutate(
      weight = pty_vv1/nat_vv1)%>%
    group_by(ctr, yr, mn.el)%>%
    mutate(
      PSNS = sum(PNS * weight, na.rm=TRUE),
      PSNS_s = sum(PNS_s * weight, na.rm=TRUE),
      PSNS_w = sum(PNS_w * weight, na.rm=TRUE),
      PSNS_sw = sum(PNS_sw *weight, na.rm=TRUE),
      PSNS = ifelse(PSNS == 0|PSNS > 1, NA, PSNS),
      PSNS_s= ifelse(PSNS_s == 0 | PSNS_s > 1, NA,PSNS_s),
      PSNS_w = ifelse(PSNS_w == 0 | PSNS_w > 1, NA, PSNS_w),
      PSNS_sw = ifelse(PSNS_sw == 0 | PSNS_sw > 1, NA, PSNS_sw),
      MK_N_two = as.numeric(sprintf("%.3f", MK_N_two)),
      MK_N = as.numeric(sprintf("%.3f", MK_N)))%>%
    select(ctr,  yr, mn.el, PSNS, PSNS_s, PSNS_w, PSNS_sw, MK_N, MK_N_two)%>%
    summarise_each(funs(mean(., na.rm=TRUE)))%>%
    distinct(ctr, yr, mn.el, .keep_all = TRUE)

  national.level.psns<-data.c %>%
    distinct(ctr, yr, mn.el, pty, cst, .keep_all = TRUE)%>%
    group_by(ctr, yr, mn.el)%>%
    mutate(
      nat_vote = sum(pv1, na.rm=TRUE))%>%
    group_by(ctr, yr, mn.el, cst)%>%
    mutate(
      seat_cst = sum(seat, na.rm=TRUE),
      party_vote_proportion =  pv1/nat_vote)%>%
    arrange(ctr, yr, mn.el, cst, pty)%>%
    group_by(ctr, yr, mn.el)%>%
    mutate(
      seat_total = sum(seat, na.rm=TRUE))%>%
    group_by(ctr, yr, mn.el, pty)%>%
    mutate(
      seat_contest = sum(seat_cst, na.rm=TRUE),
      seat_proportion = seat_contest/seat_total,
      local_E =  party_vote_proportion * seat_proportion)%>%
    group_by(ctr, yr, mn.el)%>%
    mutate(
      local_E = sum(local_E, na.rm=TRUE))%>%
    select(ctr, ctr_n, yr, mn.el, local_E)%>%
    distinct(ctr_n, ctr, yr, mn.el, .keep_all = TRUE)

  national.level<-national.level.nat2 %>%
    left_join(national.level.nat)%>%
    right_join(national.level.mk_n)%>%
    left_join(national.level.psns)%>%
    rename(inflation1=Cox, inflation2=MK_I, inflation3=MK_I_w, inflation4=MK_N, nvvi=indicator,
           ENP_nat=ENEP_nat, ENP_avg=ENEP_avg, ENP_wght=ENEP_wght)%>%
    mutate(
      nvvi = replace(nvvi, is.na(nvvi), 0))%>%
    mutate(
      inflation1 = ifelse(inflation1==0, NA, inflation1),
      inflation2 = ifelse(inflation2==0, NA, inflation2),
      inflation3 = ifelse(inflation3==0, NA, inflation3),
      inflation4 = ifelse(inflation4==0, NA, inflation4),
      local_E = ifelse(local_E == 0, NA, local_E),
      ENP_nat = ifelse(ctr == 144 & yr == 1947 | ctr == 144 & yr == 1952 |
                         ctr == 144 & yr == 1956 | ctr == 144 & yr == 1960 | ctr == 144 & yr == 1965 |
                         ctr == 144 & yr == 1970 | ctr == 144 & yr == 1977, NA, ENP_nat),
      ENP_avg = ifelse(ctr == 144 & yr == 1947 | ctr == 144 & yr == 1952 |
                         ctr == 144 & yr == 1956 | ctr == 144 & yr == 1960 | ctr == 144 & yr == 1965 |
                         ctr == 144 & yr == 1970 | ctr == 144 & yr == 1977, NA, ENP_avg),
      ENP_wght = ifelse(ctr == 144 & yr == 1947 | ctr == 144 & yr == 1952 |
                          ctr == 144 & yr == 1956 | ctr == 144 & yr == 1960 | ctr == 144 & yr == 1965 |
                          ctr == 144 & yr == 1970 | ctr == 144 & yr == 1977, NA, ENP_wght))%>%
    right_join(data.grid)%>%
    distinct(ctr_n, ctr, yr, mn.el, .keep_all = TRUE)%>%
    select(ctr_n, ctr, yr, mn.el, nvvi, ENP_nat, ENP_avg, ENP_wght, inflation1, inflation2, inflation3,
           inflation4, PSNS, PSNS_s, PSNS_w, PSNS_sw, local_E)%>%
    arrange(ctr_n, ctr, yr, mn.el, nvvi, ENP_nat, ENP_avg, ENP_wght, inflation1, inflation2, inflation3,
            inflation4, PSNS, PSNS_s, PSNS_w, PSNS_sw, local_E)
  print("National Level Dataset has been sucessfully computed")


  #******************************************
  #*****Party Nationalization Measures*******
  #*******Constituency Level Dataset*********
  #**************December 2016***************
  #******************************************
  print("Computing Constituency Level Dataset...", quote=FALSE)


  cst.level<-data.b %>%
    distinct(ctr, yr, mn.el, cst, pty, .keep_all = TRUE)%>%
    group_by(ctr, yr, mn.el, cst) %>%
    mutate(
      new_vv1 = sum(pv1,na.rm=TRUE),
      share2 = (pv1/new_vv1)^2,
      denom = sum(share2),
      ENP_cst = 1/denom,
      cvvi = 0,
      cvvi = ifelse(vv1!= new_vv1, 1, cvvi))%>%
    select(ctr, yr, mn.el, cst, cst_n,  ENP_cst, cvvi)%>%
    right_join(data.grid)%>%
    distinct(ctr, yr, mn.el, cst, .keep_all = TRUE)%>%
    left_join(national.level)%>%
    mutate(
      nvvi = ifelse(is.na(nvvi), 0, nvvi),
      cvvi = ifelse(is.na(cvvi), 0, cvvi))%>%
    distinct(ctr, yr, mn.el, cst, .keep_all = TRUE)%>%
    mutate(
      inflation5 = (ENP_nat - ENP_cst)/ENP_cst,
      ENP_cst=ifelse(yr < 1834 & ctr == 840, NA, ENP_cst))%>%
    select(ctr_n, ctr, yr, mn.el, cst_n, cst, nvvi, cvvi, ENP_cst, ENP_nat, ENP_avg,
           ENP_wght, inflation1, inflation2, inflation3, inflation4, inflation5,
           PSNS, PSNS_s, PSNS_w, PSNS_sw, local_E)%>%
    arrange(ctr_n, ctr, yr, mn.el, cst_n, cst, nvvi, cvvi, ENP_cst, ENP_nat, ENP_avg,
            ENP_wght, inflation1, inflation2, inflation3, inflation4, inflation5,
            PSNS, PSNS_s, PSNS_w, PSNS_sw, local_E)%>%
    distinct(ctr, yr, mn.el, cst, .keep_all = TRUE)

  print("Constituency Level Dataset has been successfully computed")
  cat("The entire computation took ", proc.time()[1]-start.timer[1], "secs \n")
  print("Done!", quote=FALSE)

  results=list(national.level.data = national.level,
               party.level.data = party.level,
               cst.level.data = cst.level)
  return(results)}
