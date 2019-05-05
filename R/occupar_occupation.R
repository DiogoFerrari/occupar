
## =====================================================
## Occupation schemes: ISCO, EGP, ESEC, ISEC
## =====================================================
## {{{ Info: general }}}

## Info: 
## - http://www.harryganzeboom.nl/ISCO08/qa-isei-08.htm
## - connelly2016review 
## - Ganzeboom, Harry B.G.; Treiman, Donald J. (2003). "Three Internationally Standardised Measures for Comparative Research on Occupational Status." Pp. 159-193 in Jürgen H.P. Hoffmeyer-Zlotnik & Christof Wolf (Eds.), Advances in Cross-National Comparison. A European Working Book for Demographic and Socio-Economic Variables. New York: Kluwer Academic Press. Pp. 159-193. (6.1Mb)
## - Ganzeboom, Harry B.G.; Treiman, Donald J. (1996). “Internationally Comparable Measures of Occupational Status for the 1988 International Standard Classification of Occupations”. Social Science Research (25), pp. 201-239. (3.9 Mb)
## - Ganzeboom, Harry B.G.; De Graaf, Paul; Treiman, Donald J.; (with De Leeuw, Jan) (1992). "A Standard International Socio-Economic Index of Occupational Status", Social Science Research (21-1), pp. 1-56. (3.5Mb)
## - Ganzeboom, Harry B.G., Luijkx, Ruud; Treiman, Donald J. (1989). "Inter­genera­tional Class Mobility in Comparative Perspective", Research in Social Stratification and Mobility (8), pp. 3-79.
## 
## Schemes are:
## EGP      (Erikson, Goldthorpe and Portocarero, 1979)
## ESeC     (European Socio-Economic Classification as proposed by Rose and Harrison, 2009)
## ISEC-08  (International Socio-Economic Classes 2008),
##          it is a categorization of occupational information coded in the ISCO-08. It gives information on
##          status-in-employment into a 13 category social class scheme than contains well-known social class
##          schemes as EGP and ESEC as special cases. The ISEC categories are:
## SIOPS    (Standard International Occupation Prestige Score)
##          - a.k.a Treiman-scale 
## ISEI-08 (International Socio-Economic Index of occupational status)
##          - continuous hierarchical scale
##          - constructed on a database of 198500 men and women with valid education,
##            occupation and (personal) incomes derived from the combined 2002-2007 issues of the
##            International Social Survey Programme [ISSP].
##          - while the aim of the new ISEI was to produce socio-economic status scores for ISCO-08,
##            it was in fact calculated on occupations classified in ISCO-88 and then converted to
##            ISCO-08 occupations.
##
## Differences:
## - ISEI is empirically constructed, while EGP and ESEC are a-priori constructions
##   that are at best validated on empirical data.
##   It only uses occupational data (i.e. occupation as classified by ISCO), whereas EGP and ESEC
##   heavily use status-in-employment (self-employment and supervision)
##   indicators to construct socio-economic classes.
## - Despite the differences ISEI and EGP (and presumably ESEC) are empirically strongly correlated (0.90),
##   much more strongly than ISEI and SIOPS. If optimally scaled using a single dimension,
##   EGP scores are virtually identical to their ISEI means.
## 


## }}}
## {{{ Info EGP  7   }}}

## EGP      (Erikson, Goldthorpe and Portocarero, 1979)
##          Goldthorpe (1987) created a scheme for British data, which was restricted to the roman numerals and
##          contained SEVEN classes:
## 1 I   Service class I
## 2 II  Service class II
## 3 III Routine non-manual
## 4 IV  Self-employed
## 5 V   Manual supervisors/Lower grade technicians
## 6 VI  Skilled workers
## 7 VII Unskilled workers/Farm labours


## }}}
## {{{ Info EGP 11   }}}

## The scheme was extended into ELEVEN classes (Erikson and Goldthorpe, 1992)
##  1 I     Service class I
##  2 II    Service class II
##  3 III.a Routine non-manual higher grade
##  4 III.b Routine non-manual lower grade
##  5 IV.a  Self-employed with employees
##  6 IV.b  Self-employed with no empoyees
##  7 IV.c  Self-employed Farmers etc
##  8 V     Manual supervisors/Lower grade technicians
##  9 VI    Skilled workers
## 10 VII.a Unskilled workers
## 11 VII.b Farm labours


## }}}
## {{{ Info EGP  5   }}}

## Other versions are FIVE classes scheme 
## 1	I-III      White-collar workers
## 2	IV.a+IV.b  Petty bourgeoisie
## 3	IV.c+VII.b Farm workers
## 4	V+VI       Skilled workers
## 5	VII.a      Non-skilled workers


## }}}
## {{{ Info EGP  3   }}}

## and THREE class scheme
## 1	I-II+IV.a-b Non-manual workers
## 2	IV.c+VII.b  Farm workers
## 3	V+VI+VI.a   Manual workers


## }}}
## {{{ Info ISEC-08  }}}

## I-a     Higher level professionals                     1
## I-b     Higher level managers and entrepreneurs        2
## II-a    Lower level professionals                      3
## II-b    Lower level managers                           4
## III-a   Clerical Routine Non-manual Workers            5
## III-b   Sales and Service Routine Non-manual Workers   6
## IV-a    Small Self-employed with employees             7
## IV-b    Small Self-employed without employers          8
## IV-c    Small Self-employed in agriculture             9
## V       Manual Supervisors                            10
## VI      Skilled Manual Workers                        11
## VII-a   Semi- and Unskilled Manual Workers            12
## VII-b   Agricultural Labourers                        13

## }}}

## ---------------------------------------
## Converting ISCO into Occupation Schemes
## ---------------------------------------
## {{{ docs }}}

#' Recode occupation to EGP class scheme
#'
#' The function creates a factor with the EGP class scheme
#'
#'
#' @param isco88 numeric vector with the isco88 4 digit code.
#' @param n.classes  either 11, 7, 5, or 3, indicating the number of EGP classes to use
#' @inheritParams isco88toESeC
#' @param collapse deprecated
#'
#' @details
#' The number of classes to use in the EGP scheme is based on
#' - Connelly, R., Gayle, V., & Lambert, P. S., (2016) A review of occupation-based social classifications for social survey research, Methodological Innovations, v.9, p.1-14. 
#'
#' @export

## }}}
isco88toEGP <- function(isco88, n.employees=NULL, self.employed=NULL, collapse=FALSE, n.classes=11)
{
    if (n.classes %!in% c(11,7,5,3)) {
        stop("\n\nNumber of classes (n.classes) allowed to compute EGP class scheme are 11, 7, 5, 3.\n\n")
    }

    ## Recode ISCO88 to EGP using Ganzebooms' code
    egp = rep(NA, length(isco88))

    ## Code the simple version, based on 4 digit ISCO88 values
    ## CLASS 1
    egp[isco88 %in% c(1000, 2000, 1100, 2100, 1110, 2110, 1120, 2111, 2112, 
                      2113, 2114, 2120, 2121, 1200, 2122, 1210, 2130, 1220, 2131, 1222, 1223, 
                      2140, 1224, 2141, 1225, 2142, 1226, 2143, 1227, 2144, 1228, 2145, 1229, 
                      2146, 1230, 2147, 1231, 1232, 2149, 1233, 2200, 3143, 1234, 2210, 3144, 
                      1235, 2211, 1236, 2212, 1237, 2213, 1239, 2220, 2221, 1250, 2222, 1251, 
                      2223, 2224, 2229, 2310, 2350, 2351, 2352, 2400, 2411, 2420, 2421, 2422, 
                      2429, 2440, 2441, 2442, 2443, 2445)] <- 1 ## CLASS 1
    ## CLASS 2
    egp[isco88 %in% c(3000, 3100, 3110, 3111, 1130, 3112, 1140, 3113, 1141, 
                      3114, 1142, 3115, 5121, 1143, 3116, 3117, 3118, 3119, 2132, 3120, 2139, 
                      3121, 3122, 3123, 3130, 3131, 3132, 3133, 3139, 5150, 3140, 5151, 2148, 
                      3141, 5152, 3142, 3145, 3150, 3151, 3152, 1240, 3200, 3210, 3211, 1252, 
                      3212, 1300, 3213, 1310, 2230, 3220, 2300, 3221, 1312, 3222, 1313, 2320, 
                      3223, 1314, 2321, 3224, 1315, 2322, 3225, 1316, 2323, 3226, 1317, 2330, 
                      3227, 1318, 2331, 3228, 1319, 2332, 3229, 2340, 3240, 2359, 3241, 3242, 
                      2410, 2412, 2419, 3400, 3410, 3411, 2430, 3412, 2431, 3413, 2432, 3414, 
                      3415, 3416, 3417, 3419, 2444, 3420, 3421, 2446, 3422, 2450, 3423, 2451, 
                      3429, 2452, 2453, 3431, 2454, 3432, 2455, 2460, 3434, 3440, 3441, 3442, 
                      3443, 3444, 3449, 3450, 3451, 3470, 3471, 3472, 3473, 3474, 
                      3475)] <- 2 ## CLASS 2
    
    ## EGP CLASS 3
    egp[isco88 %in% c(4000, 4100, 4122, 9100, 4110, 9110, 4111, 9111, 4112, 9112, 
                      4113, 9113, 4114, 4115, 4120, 4121, 3230, 3231, 3232, 3300, 3310, 3320, 3330, 
                      3340, 3430, 3433, 3439, 3460, 3480)] <- 3 
    
    ## EGP CLASS 4
    egp[isco88 %in% c(5000, 5100, 5110, 5111, 5112, 5113, 5120, 4130, 5131, 
                      4131, 4132, 5133, 4133, 4140, 4141, 4143, 4144, 4190, 4200, 4210, 4211, 
                      4212, 4213, 4214, 4215, 4220, 4221, 5200, 4222, 5210, 4223, 5220, 5230)] <- 4  

    ## CLASSES 5 and 5, see below 

    ## CLASS 7
    egp[isco88 %in% c(3452, 7510)] <- 7 ## CLASS 7
    ## CLASS 8
    egp[isco88 %in% c(7000, 7120, 5122, 7124, 7129, 7130, 7132, 5140, 7133, 5141, 7134, 5143, 7136, 8150, 7137,
                      8151, 7140, 8152, 7141, 8153, 8154, 8155, 5161, 7200, 8159, 
                      5162, 7210, 8160, 7211, 8161, 5164, 7212, 8162, 7213, 8163, 7214, 8170, 7215, 8171, 7216,
                      8172, 7220, 7221, 7222, 7223, 7224, 7230, 7231, 7232, 7233, 7240, 7241, 7242, 
                      7243, 7244, 7245, 7300, 7310, 7311, 7312, 7313, 7323, 7324, 7340, 7341, 7342, 7343, 7344,
                      7345, 7346, 7400, 7410, 7411, 7412, 7413, 7414, 7415, 7416, 7420, 7422, 8311, 
                      7423, 7430, 7433, 7434, 7435, 7436, 8332, 7437, 8333, 7440, 7441, 7442, 7500, 7520)] <- 8 
    ## CLASS 9
    egp[isco88 %in% c(8000, 9000, 7100, 8100, 7110, 8110, 7111, 8111, 7112, 8112, 7113, 8113, 8120, 9120, 7121,
                      8121, 9130, 7122, 8122, 9131, 5123, 7123, 8123, 9132, 5130, 
                      8124, 9133, 8130, 9140, 5132, 8131, 9141, 7131, 8139, 9142, 5139, 8140, 9150, 8141, 9151,
                      8142, 9152, 4142, 5142, 7135, 8143, 9153, 9160, 5149, 9161, 9162, 9200, 7142, 
                      5160, 7143, 5163, 9300, 9310, 5169, 9311, 9312, 9313, 9320, 8200, 9321, 8210, 9322, 8211,
                      9330, 8212, 9331, 8220, 9332, 8221, 9333, 8222, 8223, 8224, 7234, 8229, 8230, 
                      8231, 8232, 8240, 8250, 8251, 8252, 8253, 8260, 8261, 8262, 7320, 8263, 7321, 8264, 7322,
                      8265, 8266, 8269, 7330, 8270, 7331, 8271, 7332, 8272, 8273, 8274, 8275, 8276, 
                      8277, 8278, 8279, 8280, 8281, 8282, 8283, 8284, 8285, 8286, 8290, 8300, 7421, 8310, 8312,
                      7424, 8320, 8321, 7431, 8322, 7432, 8323, 8324, 8330, 8334, 8340, 8400, 
                      7530)] <- 9  

    ## CLASS 10
    egp[isco88 %in% c(6000, 6100, 6110, 6111, 6112, 6113, 6114, 6120, 6121, 6122, 6123, 6124, 6129, 6130, 6134,
                      6140, 6141, 6142, 6150, 6151, 9210, 6152, 9211, 6153, 9212, 
                      6154, 9213, 8331)] <- 10  
    ## CLASS 11
    egp[isco88 %in% c(1221, 6131, 6132, 6133, 6200, 6210, 1311)] <- 11  

    ## CLASSES 5 and 6
    if (!is.null(self.employed) & !is.null(n.employees)) {
        n.employees[is.na(n.employees)]     = 0
        self.employed[is.na(self.employed)] = 0

        ## CLASS 5
        egp[self.employed == 1 & n.employees > 0 ]  = 5
        ## CLASS 6
        egp[self.employed == 1 & n.employees == 0 ] = 6
    }

    egp = egp_get_n_classes(egp, n.classes)
    return(egp)
}
egp_get_n_classes <- function(egp, n.classes)
{
    if (n.classes==11) {
        egp = factor(egp, levels=1:11, labels=c("I     Service class I",
                                                "II    Service class II",
                                                "III.a Routine non-manual, higher grade",
                                                "III.b Routine non-manual, lower grade",
                                                "IV.a  Self-employed with employees",
                                                "IV.b  Self-employed with no empoyees",
                                                "IV.c  Self-employed Farmers etc",
                                                "V     Manual supervisors/Lower grade technicians",
                                                "VI    Skilled workers",
                                                "VII.a Unskilled workers",
                                                "VII.b Farm labours"))
    }   
    if (n.classes==7) {
        egp[egp %in%  c(1)]     = 1
        egp[egp %in%  c(2)]     = 2
        egp[egp %in%  c(3:4)]   = 3
        egp[egp %in%  c(5:7)]   = 4
        egp[egp %in%  c(8)]     = 5
        egp[egp %in%  c(9)]     = 6
        egp[egp %in%  c(10:11)] = 7
        egp = factor(egp, levels=1:7, labels=c("I   Service class I",
                                               "II  Service class II",
                                               "III Routine non-manual",
                                               ## "III.a Routine non-manual, higher grade",
                                               ## "III.b Routine non-manual, lower grade",
                                               "IV  Self-employed",
                                               ## "IV.a  Self-employed with employees",
                                               ## "IV.b  Self-employed with no empoyees",
                                               ## "IV.c  Self-employed Farmers etc",
                                               "V   Manual supervisors/Lower grade technicians",
                                               "VI  Skilled workers",
                                               "VII Unskilled workers/Farm labours"
                                               ## "VII.a Unskilled workers",
                                               ## "VII.b Farm labours"
                                               ))
    }
    if (n.classes==5) {
        egp[egp %in%  c(1:4)]   = 1
        egp[egp %in%  c(5:6)]   = 2
        egp[egp %in%  c(7, 11)] = 3
        egp[egp %in%  c(8:9)]   = 4
        egp[egp %in%  c(10)]    = 5
        egp = factor(egp, levels=1:5, labels=c(
                                          "I-III      White-collar workers",
                                          "IV.a+IV.b  Petty bourgeoisie",
                                          "IV.c+VII.b Farm workers",
                                          "V+VI       Skilled workers",
                                          "VII.a      Non-skilled workers"
                                      ))
    }
    if (n.classes==3) {
        egp[egp %in%  c(1:6)]   = 1
        egp[egp %in%  c(7, 11)] = 2
        egp[egp %in%  c(8:10)]  = 3
        egp = factor(egp, levels=1:3, labels=c(
                                          "I-II+IV.a-b  Non-manual workers",
                                          "IV.c+VII.b  Farm workers",
                                          "V+VI+VI.a Manual workers"
                                      ))
    }
    return(egp)
}

## {{{ docs }}}

#' Recode ISCO-08 into ISEI-08
#'
#' The function recodes ISCO-08 into ISEI-08. 
#'
#' @param isco08 a numeric vector with the ISCO-08 codes
#' @param display.nas  boolean. If true (defaul is \code{FALSE}), displays table with unmatched ISCO-08 codes.
#'
#' @details
#' The ISEI-08 is an update of the ISEI (or ISEI-92). The funtion converts ISCO-08 to that updated version. References:
#'
#' Ganzeboom, H. B., De Graaf, P. M., & Treiman, D. J.,  (1992) A standard international socio-economic index of occupational status, Social science research, 21(1), 1–56. 
#' Ganzeboom, H. B., (2010) A new international socio-economic index (isei) of occupational status for the international standard classification of occupation 2008 (isco-08) constructed with data from the issp 2002-2007, In , Annual Conference of International Social Survey Programme, Lisbon (pp. )
#' 
#' @return
#' It returns a vector with ISEI-08 codes.
#'
#' @export

## }}}
isco08toISEI08 <- function(isco08, display.nas=FALSE)
{
    isei = tibble::tibble(isco08=isco08) 
    isei =  isei %>%
        dplyr::mutate(isei08 = dplyr::case_when(
                                          isco08 ==      0 ~ 51.25,
                                          isco08 ==    100 ~ 60.92,
                                          isco08 ==    110 ~ 60.92,
                                          isco08 ==    200 ~ 51.63,
                                          isco08 ==    210 ~ 51.63,
                                          isco08 ==    300 ~ 29.18,
                                          isco08 ==    310 ~ 29.18,
                                          isco08 ==   1000 ~ 65.12,
                                          isco08 ==   1100 ~ 71.72,
                                          isco08 ==   1110 ~ 74.50,
                                          isco08 ==   1111 ~ 68.77,
                                          isco08 ==   1112 ~ 78.76,
                                          isco08 ==   1113 ~ 64.98,
                                          isco08 ==   1114 ~ 71.29,
                                          isco08 ==   1120 ~ 70.34,
                                          isco08 ==   1200 ~ 72.94,
                                          isco08 ==   1210 ~ 72.24,
                                          isco08 ==   1211 ~ 73.38,
                                          isco08 ==   1212 ~ 74.79,
                                          isco08 ==   1213 ~ 70.57,
                                          isco08 ==   1219 ~ 68.54,
                                          isco08 ==   1220 ~ 73.71,
                                          isco08 ==   1221 ~ 71.39,
                                          isco08 ==   1222 ~ 75.25,
                                          isco08 ==   1223 ~ 81.92,
                                          isco08 ==   1300 ~ 65.25,
                                          isco08 ==   1310 ~ 49.48,
                                          isco08 ==   1311 ~ 49.48,
                                          isco08 ==   1312 ~ 49.48,
                                          isco08 ==   1320 ~ 61.57,
                                          isco08 ==   1321 ~ 65.42,
                                          isco08 ==   1322 ~ 61.57,
                                          isco08 ==   1323 ~ 59.89,
                                          isco08 ==   1324 ~ 58.07,
                                          isco08 ==   1330 ~ 78.86,
                                          isco08 ==   1340 ~ 65.01,
                                          isco08 ==   1341 ~ 65.01,
                                          isco08 ==   1342 ~ 65.01,
                                          isco08 ==   1343 ~ 65.01,
                                          isco08 ==   1344 ~ 65.01,
                                          isco08 ==   1345 ~ 65.01,
                                          isco08 ==   1346 ~ 65.01,
                                          isco08 ==   1349 ~ 65.01,
                                          isco08 ==   1400 ~ 51.01,
                                          isco08 ==   1410 ~ 43.85,
                                          isco08 ==   1411 ~ 43.85,
                                          isco08 ==   1412 ~ 43.85,
                                          isco08 ==   1420 ~ 51.56,
                                          isco08 ==   1430 ~ 51.01,
                                          isco08 ==   1431 ~ 51.01,
                                          isco08 ==   1439 ~ 51.01,
                                          isco08 ==   2000 ~ 76.24,
                                          isco08 ==   2100 ~ 79.49,
                                          isco08 ==   2110 ~ 84.16,
                                          isco08 ==   2111 ~ 84.61,
                                          isco08 ==   2112 ~ 84.61,
                                          isco08 ==   2113 ~ 83.50,
                                          isco08 ==   2114 ~ 86.81,
                                          isco08 ==   2120 ~ 81.78,
                                          isco08 ==   2130 ~ 80.46,
                                          isco08 ==   2131 ~ 80.46,
                                          isco08 ==   2132 ~ 78.17,
                                          isco08 ==   2133 ~ 80.46,
                                          isco08 ==   2140 ~ 79.05,
                                          isco08 ==   2141 ~ 79.05,
                                          isco08 ==   2142 ~ 81.40,
                                          isco08 ==   2143 ~ 79.05,
                                          isco08 ==   2144 ~ 77.10,
                                          isco08 ==   2145 ~ 82.31,
                                          isco08 ==   2146 ~ 79.31,
                                          isco08 ==   2149 ~ 78.69,
                                          isco08 ==   2150 ~ 80.75,
                                          isco08 ==   2151 ~ 80.78,
                                          isco08 ==   2152 ~ 80.75,
                                          isco08 ==   2153 ~ 80.75,
                                          isco08 ==   2160 ~ 79.74,
                                          isco08 ==   2161 ~ 79.74,
                                          isco08 ==   2162 ~ 79.74,
                                          isco08 ==   2163 ~ 79.74,
                                          isco08 ==   2164 ~ 79.74,
                                          isco08 ==   2165 ~ 72.96,
                                          isco08 ==   2166 ~ 79.74,
                                          isco08 ==   2200 ~ 76.98,
                                          isco08 ==   2210 ~ 88.70,
                                          isco08 ==   2211 ~ 88.70,
                                          isco08 ==   2212 ~ 81.92,
                                          isco08 ==   2220 ~ 68.70,
                                          isco08 ==   2221 ~ 68.70,
                                          isco08 ==   2222 ~ 68.70,
                                          isco08 ==   2230 ~ 76.98,
                                          isco08 ==   2240 ~ 76.98,
                                          isco08 ==   2250 ~ 84.14,
                                          isco08 ==   2260 ~ 75.43,
                                          isco08 ==   2261 ~ 88.31,
                                          isco08 ==   2262 ~ 81.13,
                                          isco08 ==   2263 ~ 75.43,
                                          isco08 ==   2264 ~ 67.94,
                                          isco08 ==   2265 ~ 65.23,
                                          isco08 ==   2266 ~ 75.43,
                                          isco08 ==   2267 ~ 75.43,
                                          isco08 ==   2269 ~ 75.43,
                                          isco08 ==   2300 ~ 75.54,
                                          isco08 ==   2310 ~ 85.41,
                                          isco08 ==   2320 ~ 72.30,
                                          isco08 ==   2330 ~ 82.41,
                                          isco08 ==   2340 ~ 71.45,
                                          isco08 ==   2341 ~ 76.49,
                                          isco08 ==   2342 ~ 58.77,
                                          isco08 ==   2350 ~ 68.88,
                                          isco08 ==   2351 ~ 77.88,
                                          isco08 ==   2352 ~ 70.89,
                                          isco08 ==   2353 ~ 68.88,
                                          isco08 ==   2354 ~ 68.88,
                                          isco08 ==   2355 ~ 68.88,
                                          isco08 ==   2356 ~ 68.88,
                                          isco08 ==   2359 ~ 66.42,
                                          isco08 ==   2400 ~ 73.91,
                                          isco08 ==   2410 ~ 75.50,
                                          isco08 ==   2411 ~ 76.65,
                                          isco08 ==   2412 ~ 75.50,
                                          isco08 ==   2413 ~ 75.50,
                                          isco08 ==   2420 ~ 70.09,
                                          isco08 ==   2421 ~ 70.09,
                                          isco08 ==   2422 ~ 72.94,
                                          isco08 ==   2423 ~ 68.55,
                                          isco08 ==   2424 ~ 70.09,
                                          isco08 ==   2430 ~ 73.91,
                                          isco08 ==   2431 ~ 73.91,
                                          isco08 ==   2432 ~ 73.91,
                                          isco08 ==   2433 ~ 73.91,
                                          isco08 ==   2434 ~ 73.91,
                                          isco08 ==   2500 ~ 75.13,
                                          isco08 ==   2510 ~ 74.66,
                                          isco08 ==   2511 ~ 74.66,
                                          isco08 ==   2512 ~ 74.66,
                                          isco08 ==   2513 ~ 74.66,
                                          isco08 ==   2514 ~ 74.66,
                                          isco08 ==   2519 ~ 74.70,
                                          isco08 ==   2520 ~ 75.13,
                                          isco08 ==   2521 ~ 75.13,
                                          isco08 ==   2522 ~ 75.13,
                                          isco08 ==   2523 ~ 75.13,
                                          isco08 ==   2529 ~ 75.13,
                                          isco08 ==   2600 ~ 75.67,
                                          isco08 ==   2610 ~ 85.13,
                                          isco08 ==   2611 ~ 86.72,
                                          isco08 ==   2612 ~ 88.96,
                                          isco08 ==   2619 ~ 81.05,
                                          isco08 ==   2620 ~ 71.55,
                                          isco08 ==   2621 ~ 77.19,
                                          isco08 ==   2622 ~ 70.40,
                                          isco08 ==   2630 ~ 77.24,
                                          isco08 ==   2631 ~ 80.92,
                                          isco08 ==   2632 ~ 83.09,
                                          isco08 ==   2633 ~ 83.81,
                                          isco08 ==   2634 ~ 85.85,
                                          isco08 ==   2635 ~ 70.50,
                                          isco08 ==   2636 ~ 71.55,
                                          isco08 ==   2640 ~ 72.83,
                                          isco08 ==   2641 ~ 72.83,
                                          isco08 ==   2642 ~ 72.83,
                                          isco08 ==   2643 ~ 80.92,
                                          isco08 ==   2650 ~ 63.31,
                                          isco08 ==   2651 ~ 61.82,
                                          isco08 ==   2652 ~ 64.44,
                                          isco08 ==   2653 ~ 61.82,
                                          isco08 ==   2654 ~ 63.31,
                                          isco08 ==   2655 ~ 70.10,
                                          isco08 ==   2656 ~ 54.00,
                                          isco08 ==   2659 ~ 37.59,
                                          isco08 ==   3000 ~ 56.03,
                                          isco08 ==   3100 ~ 52.40,
                                          isco08 ==   3110 ~ 53.60,
                                          isco08 ==   3111 ~ 55.03,
                                          isco08 ==   3112 ~ 59.35,
                                          isco08 ==   3113 ~ 51.92,
                                          isco08 ==   3114 ~ 56.38,
                                          isco08 ==   3115 ~ 53.77,
                                          isco08 ==   3116 ~ 59.45,
                                          isco08 ==   3117 ~ 62.79,
                                          isco08 ==   3118 ~ 50.73,
                                          isco08 ==   3119 ~ 51.35,
                                          isco08 ==   3120 ~ 38.18,
                                          isco08 ==   3121 ~ 37.83,
                                          isco08 ==   3122 ~ 40.54,
                                          isco08 ==   3123 ~ 37.83,
                                          isco08 ==   3130 ~ 33.66,
                                          isco08 ==   3131 ~ 46.73,
                                          isco08 ==   3132 ~ 37.22,
                                          isco08 ==   3133 ~ 33.66,
                                          isco08 ==   3134 ~ 33.66,
                                          isco08 ==   3135 ~ 33.66,
                                          isco08 ==   3139 ~ 31.46,
                                          isco08 ==   3140 ~ 54.86,
                                          isco08 ==   3141 ~ 54.86,
                                          isco08 ==   3142 ~ 58.05,
                                          isco08 ==   3143 ~ 54.86,
                                          isco08 ==   3150 ~ 63.29,
                                          isco08 ==   3151 ~ 56.41,
                                          isco08 ==   3152 ~ 52.70,
                                          isco08 ==   3153 ~ 73.71,
                                          isco08 ==   3154 ~ 69.24,
                                          isco08 ==   3155 ~ 67.04,
                                          isco08 ==   3200 ~ 55.40,
                                          isco08 ==   3210 ~ 54.92,
                                          isco08 ==   3211 ~ 57.04,
                                          isco08 ==   3212 ~ 57.37,
                                          isco08 ==   3213 ~ 48.66,
                                          isco08 ==   3214 ~ 54.92,
                                          isco08 ==   3220 ~ 56.98,
                                          isco08 ==   3221 ~ 56.00,
                                          isco08 ==   3222 ~ 51.93,
                                          isco08 ==   3230 ~ 51.57,
                                          isco08 ==   3240 ~ 24.79,
                                          isco08 ==   3250 ~ 53.15,
                                          isco08 ==   3251 ~ 47.83,
                                          isco08 ==   3252 ~ 53.15,
                                          isco08 ==   3253 ~ 53.15,
                                          isco08 ==   3254 ~ 59.85,
                                          isco08 ==   3255 ~ 53.15,
                                          isco08 ==   3256 ~ 44.92,
                                          isco08 ==   3257 ~ 57.25,
                                          isco08 ==   3258 ~ 53.15,
                                          isco08 ==   3259 ~ 61.91,
                                          isco08 ==   3300 ~ 57.64,
                                          isco08 ==   3310 ~ 57.13,
                                          isco08 ==   3311 ~ 72.27,
                                          isco08 ==   3312 ~ 59.76,
                                          isco08 ==   3313 ~ 55.25,
                                          isco08 ==   3314 ~ 69.76,
                                          isco08 ==   3315 ~ 57.68,
                                          isco08 ==   3320 ~ 57.97,
                                          isco08 ==   3321 ~ 60.29,
                                          isco08 ==   3322 ~ 57.03,
                                          isco08 ==   3323 ~ 56.35,
                                          isco08 ==   3324 ~ 61.18,
                                          isco08 ==   3330 ~ 56.64,
                                          isco08 ==   3331 ~ 54.62,
                                          isco08 ==   3332 ~ 56.64,
                                          isco08 ==   3333 ~ 60.44,
                                          isco08 ==   3334 ~ 62.39,
                                          isco08 ==   3339 ~ 59.89,
                                          isco08 ==   3340 ~ 57.99,
                                          isco08 ==   3341 ~ 62.13,
                                          isco08 ==   3342 ~ 57.99,
                                          isco08 ==   3343 ~ 54.55,
                                          isco08 ==   3344 ~ 57.99,
                                          isco08 ==   3350 ~ 61.60,
                                          isco08 ==   3351 ~ 65.64,
                                          isco08 ==   3352 ~ 67.11,
                                          isco08 ==   3353 ~ 54.27,
                                          isco08 ==   3354 ~ 59.18,
                                          isco08 ==   3355 ~ 63.03,
                                          isco08 ==   3359 ~ 64.40,
                                          isco08 ==   3400 ~ 52.57,
                                          isco08 ==   3410 ~ 54.35,
                                          isco08 ==   3411 ~ 57.00,
                                          isco08 ==   3412 ~ 52.72,
                                          isco08 ==   3413 ~ 54.54,
                                          isco08 ==   3420 ~ 50.90,
                                          isco08 ==   3421 ~ 50.90,
                                          isco08 ==   3422 ~ 50.90,
                                          isco08 ==   3423 ~ 50.90,
                                          isco08 ==   3430 ~ 50.15,
                                          isco08 ==   3431 ~ 50.15,
                                          isco08 ==   3432 ~ 57.64,
                                          isco08 ==   3433 ~ 50.15,
                                          isco08 ==   3434 ~ 50.15,
                                          isco08 ==   3435 ~ 50.15,
                                          isco08 ==   3500 ~ 60.93,
                                          isco08 ==   3510 ~ 62.45,
                                          isco08 ==   3511 ~ 61.07,
                                          isco08 ==   3512 ~ 62.45,
                                          isco08 ==   3513 ~ 62.45,
                                          isco08 ==   3514 ~ 62.45,
                                          isco08 ==   3520 ~ 56.50,
                                          isco08 ==   3521 ~ 56.57,
                                          isco08 ==   3522 ~ 56.50,
                                          isco08 ==   4000 ~ 43.51,
                                          isco08 ==   4100 ~ 43.33,
                                          isco08 ==   4110 ~ 43.33,
                                          isco08 ==   4120 ~ 44.94,
                                          isco08 ==   4130 ~ 44.87,
                                          isco08 ==   4131 ~ 48.27,
                                          isco08 ==   4132 ~ 40.56,
                                          isco08 ==   4200 ~ 41.22,
                                          isco08 ==   4210 ~ 43.06,
                                          isco08 ==   4211 ~ 48.10,
                                          isco08 ==   4212 ~ 48.82,
                                          isco08 ==   4213 ~ 50.05,
                                          isco08 ==   4214 ~ 50.05,
                                          isco08 ==   4220 ~ 39.02,
                                          isco08 ==   4221 ~ 49.30,
                                          isco08 ==   4222 ~ 39.02,
                                          isco08 ==   4223 ~ 38.58,
                                          isco08 ==   4224 ~ 39.02,
                                          isco08 ==   4225 ~ 39.02,
                                          isco08 ==   4226 ~ 39.02,
                                          isco08 ==   4227 ~ 39.02,
                                          isco08 ==   4229 ~ 39.02,
                                          isco08 ==   4300 ~ 44.08,
                                          isco08 ==   4310 ~ 50.57,
                                          isco08 ==   4311 ~ 50.37,
                                          isco08 ==   4312 ~ 57.38,
                                          isco08 ==   4313 ~ 50.57,
                                          isco08 ==   4320 ~ 36.10,
                                          isco08 ==   4321 ~ 32.50,
                                          isco08 ==   4322 ~ 41.63,
                                          isco08 ==   4323 ~ 41.27,
                                          isco08 ==   4400 ~ 42.30,
                                          isco08 ==   4410 ~ 42.30,
                                          isco08 ==   4411 ~ 42.30,
                                          isco08 ==   4412 ~ 27.52,
                                          isco08 ==   4413 ~ 51.77,
                                          isco08 ==   4414 ~ 54.67,
                                          isco08 ==   4415 ~ 42.30,
                                          isco08 ==   4416 ~ 42.30,
                                          isco08 ==   4419 ~ 44.72,
                                          isco08 ==   5000 ~ 29.32,
                                          isco08 ==   5100 ~ 27.57,
                                          isco08 ==   5110 ~ 45.46,
                                          isco08 ==   5111 ~ 46.76,
                                          isco08 ==   5112 ~ 38.44,
                                          isco08 ==   5113 ~ 47.42,
                                          isco08 ==   5120 ~ 24.53,
                                          isco08 ==   5130 ~ 25.04,
                                          isco08 ==   5131 ~ 25.04,
                                          isco08 ==   5132 ~ 25.04,
                                          isco08 ==   5140 ~ 31.08,
                                          isco08 ==   5141 ~ 31.08,
                                          isco08 ==   5142 ~ 31.08,
                                          isco08 ==   5150 ~ 25.46,
                                          isco08 ==   5151 ~ 25.20,
                                          isco08 ==   5152 ~ 32.20,
                                          isco08 ==   5153 ~ 21.82,
                                          isco08 ==   5160 ~ 30.59,
                                          isco08 ==   5161 ~ 32.75,
                                          isco08 ==   5162 ~ 24.07,
                                          isco08 ==   5163 ~ 34.25,
                                          isco08 ==   5164 ~ 30.59,
                                          isco08 ==   5165 ~ 30.59,
                                          isco08 ==   5169 ~ 32.75,
                                          isco08 ==   5200 ~ 29.73,
                                          isco08 ==   5210 ~ 26.64,
                                          isco08 ==   5211 ~ 28.84,
                                          isco08 ==   5212 ~ 23.53,
                                          isco08 ==   5220 ~ 29.47,
                                          isco08 ==   5221 ~ 35.34,
                                          isco08 ==   5222 ~ 44.14,
                                          isco08 ==   5223 ~ 28.48,
                                          isco08 ==   5230 ~ 30.90,
                                          isco08 ==   5240 ~ 39.04,
                                          isco08 ==   5241 ~ 39.73,
                                          isco08 ==   5242 ~ 39.04,
                                          isco08 ==   5243 ~ 39.04,
                                          isco08 ==   5244 ~ 38.88,
                                          isco08 ==   5245 ~ 39.04,
                                          isco08 ==   5246 ~ 39.04,
                                          isco08 ==   5249 ~ 39.04,
                                          isco08 ==   5300 ~ 25.09,
                                          isco08 ==   5310 ~ 24.98,
                                          isco08 ==   5311 ~ 24.98,
                                          isco08 ==   5312 ~ 24.98,
                                          isco08 ==   5320 ~ 26.64,
                                          isco08 ==   5321 ~ 26.64,
                                          isco08 ==   5322 ~ 21.64,
                                          isco08 ==   5329 ~ 26.64,
                                          isco08 ==   5400 ~ 36.86,
                                          isco08 ==   5410 ~ 36.86,
                                          isco08 ==   5411 ~ 46.38,
                                          isco08 ==   5412 ~ 51.50,
                                          isco08 ==   5413 ~ 48.13,
                                          isco08 ==   5414 ~ 23.80,
                                          isco08 ==   5419 ~ 33.83,
                                          isco08 ==   6000 ~ 19.20,
                                          isco08 ==   6100 ~ 19.41,
                                          isco08 ==   6110 ~ 16.34,
                                          isco08 ==   6111 ~ 11.56,
                                          isco08 ==   6112 ~ 18.95,
                                          isco08 ==   6113 ~ 20.91,
                                          isco08 ==   6114 ~ 12.87,
                                          isco08 ==   6120 ~ 22.21,
                                          isco08 ==   6121 ~ 21.13,
                                          isco08 ==   6122 ~ 19.83,
                                          isco08 ==   6123 ~ 28.04,
                                          isco08 ==   6129 ~ 28.04,
                                          isco08 ==   6130 ~ 17.79,
                                          isco08 ==   6200 ~ 18.29,
                                          isco08 ==   6210 ~ 19.78,
                                          isco08 ==   6220 ~ 16.33,
                                          isco08 ==   6221 ~ 17.00,
                                          isco08 ==   6222 ~ 13.35,
                                          isco08 ==   6223 ~ 20.69,
                                          isco08 ==   6224 ~ 11.01,
                                          isco08 ==   6300 ~ 11.01,
                                          isco08 ==   6310 ~ 11.01,
                                          isco08 ==   6320 ~ 11.01,
                                          isco08 ==   6330 ~ 11.01,
                                          isco08 ==   6340 ~ 11.01,
                                          isco08 ==   7000 ~ 28.53,
                                          isco08 ==   7100 ~ 25.39,
                                          isco08 ==   7110 ~ 25.94,
                                          isco08 ==   7111 ~ 33.76,
                                          isco08 ==   7112 ~ 22.57,
                                          isco08 ==   7113 ~ 23.96,
                                          isco08 ==   7114 ~ 21.96,
                                          isco08 ==   7115 ~ 26.62,
                                          isco08 ==   7119 ~ 26.92,
                                          isco08 ==   7120 ~ 25.26,
                                          isco08 ==   7121 ~ 22.16,
                                          isco08 ==   7122 ~ 22.75,
                                          isco08 ==   7123 ~ 18.02,
                                          isco08 ==   7124 ~ 27.81,
                                          isco08 ==   7125 ~ 24.09,
                                          isco08 ==   7126 ~ 29.16,
                                          isco08 ==   7127 ~ 25.26,
                                          isco08 ==   7130 ~ 23.63,
                                          isco08 ==   7131 ~ 22.77,
                                          isco08 ==   7132 ~ 24.49,
                                          isco08 ==   7133 ~ 30.47,
                                          isco08 ==   7200 ~ 29.81,
                                          isco08 ==   7210 ~ 27.61,
                                          isco08 ==   7211 ~ 28.03,
                                          isco08 ==   7212 ~ 28.52,
                                          isco08 ==   7213 ~ 25.51,
                                          isco08 ==   7214 ~ 26.60,
                                          isco08 ==   7215 ~ 28.60,
                                          isco08 ==   7220 ~ 29.84,
                                          isco08 ==   7221 ~ 25.63,
                                          isco08 ==   7222 ~ 33.16,
                                          isco08 ==   7223 ~ 28.70,
                                          isco08 ==   7224 ~ 33.90,
                                          isco08 ==   7230 ~ 31.15,
                                          isco08 ==   7231 ~ 30.78,
                                          isco08 ==   7232 ~ 47.74,
                                          isco08 ==   7233 ~ 31.72,
                                          isco08 ==   7234 ~ 31.15,
                                          isco08 ==   7300 ~ 31.00,
                                          isco08 ==   7310 ~ 30.35,
                                          isco08 ==   7311 ~ 35.70,
                                          isco08 ==   7312 ~ 35.66,
                                          isco08 ==   7313 ~ 28.12,
                                          isco08 ==   7314 ~ 24.43,
                                          isco08 ==   7315 ~ 25.78,
                                          isco08 ==   7316 ~ 30.14,
                                          isco08 ==   7317 ~ 28.95,
                                          isco08 ==   7318 ~ 28.97,
                                          isco08 ==   7319 ~ 30.35,
                                          isco08 ==   7320 ~ 31.50,
                                          isco08 ==   7321 ~ 35.33,
                                          isco08 ==   7322 ~ 30.49,
                                          isco08 ==   7323 ~ 28.24,
                                          isco08 ==   7400 ~ 37.34,
                                          isco08 ==   7410 ~ 36.97,
                                          isco08 ==   7411 ~ 36.35,
                                          isco08 ==   7412 ~ 36.92,
                                          isco08 ==   7413 ~ 39.45,
                                          isco08 ==   7420 ~ 41.68,
                                          isco08 ==   7421 ~ 43.76,
                                          isco08 ==   7422 ~ 36.92,
                                          isco08 ==   7500 ~ 23.97,
                                          isco08 ==   7510 ~ 23.46,
                                          isco08 ==   7511 ~ 20.95,
                                          isco08 ==   7512 ~ 23.57,
                                          isco08 ==   7513 ~ 27.30,
                                          isco08 ==   7514 ~ 22.79,
                                          isco08 ==   7515 ~ 34.12,
                                          isco08 ==   7516 ~ 26.96,
                                          isco08 ==   7520 ~ 23.65,
                                          isco08 ==   7521 ~ 21.81,
                                          isco08 ==   7522 ~ 25.23,
                                          isco08 ==   7523 ~ 20.78,
                                          isco08 ==   7530 ~ 22.03,
                                          isco08 ==   7531 ~ 23.47,
                                          isco08 ==   7532 ~ 24.88,
                                          isco08 ==   7533 ~ 21.24,
                                          isco08 ==   7534 ~ 22.25,
                                          isco08 ==   7535 ~ 28.08,
                                          isco08 ==   7536 ~ 18.07,
                                          isco08 ==   7540 ~ 43.19,
                                          isco08 ==   7541 ~ 27.30,
                                          isco08 ==   7542 ~ 27.30,
                                          isco08 ==   7543 ~ 43.19,
                                          isco08 ==   7544 ~ 55.96,
                                          isco08 ==   7549 ~ 43.19,
                                          isco08 ==   8000 ~ 25.45,
                                          isco08 ==   8100 ~ 23.41,
                                          isco08 ==   8110 ~ 31.44,
                                          isco08 ==   8111 ~ 35.07,
                                          isco08 ==   8112 ~ 25.11,
                                          isco08 ==   8113 ~ 35.82,
                                          isco08 ==   8114 ~ 26.13,
                                          isco08 ==   8120 ~ 25.91,
                                          isco08 ==   8121 ~ 30.32,
                                          isco08 ==   8122 ~ 22.36,
                                          isco08 ==   8130 ~ 29.30,
                                          isco08 ==   8131 ~ 29.14,
                                          isco08 ==   8132 ~ 31.34,
                                          isco08 ==   8140 ~ 25.49,
                                          isco08 ==   8141 ~ 23.09,
                                          isco08 ==   8142 ~ 24.85,
                                          isco08 ==   8143 ~ 32.67,
                                          isco08 ==   8150 ~ 16.80,
                                          isco08 ==   8151 ~ 17.85,
                                          isco08 ==   8152 ~ 18.03,
                                          isco08 ==   8153 ~ 13.24,
                                          isco08 ==   8154 ~ 16.08,
                                          isco08 ==   8155 ~ 20.35,
                                          isco08 ==   8156 ~ 20.35,
                                          isco08 ==   8157 ~ 16.80,
                                          isco08 ==   8159 ~ 21.20,
                                          isco08 ==   8160 ~ 18.13,
                                          isco08 ==   8170 ~ 22.40,
                                          isco08 ==   8171 ~ 27.25,
                                          isco08 ==   8172 ~ 19.08,
                                          isco08 ==   8180 ~ 24.15,
                                          isco08 ==   8181 ~ 21.13,
                                          isco08 ==   8182 ~ 23.19,
                                          isco08 ==   8183 ~ 24.15,
                                          isco08 ==   8189 ~ 24.16,
                                          isco08 ==   8200 ~ 24.93,
                                          isco08 ==   8210 ~ 24.93,
                                          isco08 ==   8211 ~ 27.91,
                                          isco08 ==   8212 ~ 23.88,
                                          isco08 ==   8219 ~ 24.68,
                                          isco08 ==   8300 ~ 26.80,
                                          isco08 ==   8310 ~ 38.80,
                                          isco08 ==   8311 ~ 45.76,
                                          isco08 ==   8312 ~ 29.80,
                                          isco08 ==   8320 ~ 30.11,
                                          isco08 ==   8321 ~ 28.48,
                                          isco08 ==   8322 ~ 30.34,
                                          isco08 ==   8330 ~ 25.71,
                                          isco08 ==   8331 ~ 26.85,
                                          isco08 ==   8332 ~ 25.95,
                                          isco08 ==   8340 ~ 21.08,
                                          isco08 ==   8341 ~ 13.34,
                                          isco08 ==   8342 ~ 24.45,
                                          isco08 ==   8343 ~ 24.80,
                                          isco08 ==   8344 ~ 18.08,
                                          isco08 ==   8350 ~ 37.92,
                                          isco08 ==   9000 ~ 16.50,
                                          isco08 ==   9100 ~ 14.64,
                                          isco08 ==   9110 ~ 14.64,
                                          isco08 ==   9111 ~ 16.38,
                                          isco08 ==   9112 ~ 14.21,
                                          isco08 ==   9120 ~ 14.57,
                                          isco08 ==   9121 ~ 14.82,
                                          isco08 ==   9122 ~ 14.57,
                                          isco08 ==   9123 ~ 14.57,
                                          isco08 ==   9129 ~ 14.57,
                                          isco08 ==   9200 ~ 11.87,
                                          isco08 ==   9210 ~ 11.74,
                                          isco08 ==   9211 ~ 11.74,
                                          isco08 ==   9212 ~ 11.74,
                                          isco08 ==   9213 ~ 11.74,
                                          isco08 ==   9214 ~ 11.74,
                                          isco08 ==   9215 ~ 12.01,
                                          isco08 ==   9216 ~ 12.34,
                                          isco08 ==   9300 ~ 17.53,
                                          isco08 ==   9310 ~ 16.39,
                                          isco08 ==   9311 ~ 15.35,
                                          isco08 ==   9312 ~ 17.56,
                                          isco08 ==   9313 ~ 15.35,
                                          isco08 ==   9320 ~ 17.55,
                                          isco08 ==   9321 ~ 17.55,
                                          isco08 ==   9329 ~ 16.36,
                                          isco08 ==   9330 ~ 19.66,
                                          isco08 ==   9331 ~ 20.27,
                                          isco08 ==   9332 ~ 16.89,
                                          isco08 ==   9333 ~ 17.69,
                                          isco08 ==   9334 ~ 19.66,
                                          isco08 ==   9400 ~ 16.50,
                                          isco08 ==   9410 ~ 16.50,
                                          isco08 ==   9411 ~ 16.50,
                                          isco08 ==   9412 ~ 16.50,
                                          isco08 ==   9500 ~ 23.43,
                                          isco08 ==   9510 ~ 13.72,
                                          isco08 ==   9520 ~ 25.20,
                                          isco08 ==   9600 ~ 24.07,
                                          isco08 ==   9610 ~ 14.39,
                                          isco08 ==   9611 ~ 14.39,
                                          isco08 ==   9612 ~ 14.39,
                                          isco08 ==   9613 ~ 13.87,
                                          isco08 ==   9620 ~ 27.91,
                                          isco08 ==   9621 ~ 25.06,
                                          isco08 ==   9622 ~ 27.91,
                                          isco08 ==   9623 ~ 30.99,
                                          isco08 ==   9624 ~ 27.91,
                                          isco08 ==   9629 ~ 27.91,
                                          )
                      )
    if (display.nas) display.nas(isei, conv.from="isco08", conv.to='isei08')
    return(isei$isei08)
}
## {{{ docs }}}

#' Recode ISCO-08 into ISEI (a.k.a. ISEI-92)
#'
#' The function recodes ISCO-08 into ISEI-92. 
#'
#' @param isco88 a numeric vector with the ISCO-08 codes
#' @param display.nas  boolean. If true (defaul is \code{FALSE}), displays table with unmatched ISCO-08 codes.
#'
#' @details
#'
#' Ganzeboom, H. B., De Graaf, P. M., & Treiman, D. J.,  (1992) A standard international socio-economic index of occupational status, Social science research, 21(1), 1–56. 
#' 
#' @return
#' It returns a vector with ISEI-92 codes.
#'
#' @export
## }}}
isco08toISEI92 <- function(isco08, display.nas=FALSE)
{
    msg <- paste0('\n','The function "isco08toISEI92()" first converts ISCO-08 to ISCO-88, and then computes the ISEI-92 using the latter.',  '\n'); cat(msg)
    isei = tibble::tibble(isco08=isco08) 
    isei = isei %>% dplyr::mutate(isco88 = isco08to88(isco08, display.nas),
                                  isei92 = isco88toISEI92(isco88, display.nas)) 
    ## message already displayed in the covertion above
    ## if (display.nas) display.nas(isei, conv.from="isco88", conv.to='isei92') 
    return(isei$isei92)
}


## {{{ docs }}}

#' Recode ISCO-88 into ISEI (a.k.a. ISEI-92)
#'
#' The function recodes ISCO-88 into ISEI-92. 
#'
#' @param isco88 a numeric vector with the ISCO-88 codes
#' @param display.nas  boolean. If true (defaul is \code{FALSE}), displays table with unmatched ISCO-88 codes.
#'
#' @details
#'
#' Ganzeboom, H. B., De Graaf, P. M., & Treiman, D. J.,  (1992) A standard international socio-economic index of occupational status, Social science research, 21(1), 1–56. 
#' 
#' @return
#' It returns a vector with ISEI-92 codes.
#'
#' @export
## }}}
isco88toISEI92 <- function(isco88, display.nas=FALSE)
{
    isei = tibble::tibble(isco88=isco88) 
    isei =  isei %>%
        dplyr::mutate(isei92 = dplyr::case_when(
                                          isco88 %in% 1    ~ 56,
                                          isco88 %in% 10   ~ 56,
                                          isco88 %in% 100  ~ 56,
                                          isco88 %in% 1000 ~ 56,
                                          isco88 %in% 11   ~ 70,
                                          isco88 %in% 110  ~ 70,
                                          isco88 %in% 1100 ~ 70,
                                          isco88 %in% 111  ~ 77,
                                          isco88 %in% 1110 ~ 77,
                                          isco88 %in% 112  ~ 77,
                                          isco88 %in% 1120 ~ 77,
                                          isco88 %in% 113  ~ 66,
                                          isco88 %in% 1130 ~ 66,
                                          isco88 %in% 114  ~ 58,
                                          isco88 %in% 1140 ~ 58,
                                          isco88 %in% 1141 ~ 58,
                                          isco88 %in% 1142 ~ 58,
                                          isco88 %in% 1143 ~ 58,
                                          isco88 %in% 12   ~ 68,
                                          isco88 %in% 120  ~ 68,
                                          isco88 %in% 1200 ~ 68,
                                          isco88 %in% 121  ~ 70,
                                          isco88 %in% 1210 ~ 70,
                                          isco88 %in% 1212 ~ 70,
                                          isco88 %in% 1213 ~ 70,
                                          isco88 %in% 1214 ~ 70,
                                          isco88 %in% 1215 ~ 70,
                                          isco88 %in% 1216 ~ 70,
                                          isco88 %in% 1217 ~ 70,
                                          isco88 %in% 1218 ~ 70,
                                          isco88 %in% 1219 ~ 70,
                                          isco88 %in% 1210 ~ 70,
                                          isco88 %in% 122  ~ 67,
                                          isco88 %in% 1220 ~ 67,
                                          isco88 %in% 1221 ~ 67,
                                          isco88 %in% 1222 ~ 67,
                                          isco88 %in% 1223 ~ 67,
                                          isco88 %in% 1224 ~ 59,
                                          isco88 %in% 1225 ~ 59,
                                          isco88 %in% 1226 ~ 59,
                                          isco88 %in% 1227 ~ 87,
                                          isco88 %in% 1228 ~ 59,
                                          isco88 %in% 1229 ~ 67,
                                          isco88 %in% 123  ~ 61,
                                          isco88 %in% 1230 ~ 61,
                                          isco88 %in% 1231 ~ 69,
                                          isco88 %in% 1232 ~ 69,
                                          isco88 %in% 1233 ~ 56,
                                          isco88 %in% 1234 ~ 69,
                                          isco88 %in% 1235 ~ 69,
                                          isco88 %in% 1236 ~ 69,
                                          isco88 %in% 1237 ~ 69,
                                          isco88 %in% 1239 ~ 69,
                                          isco88 %in% 124  ~ 58,
                                          isco88 %in% 1240 ~ 58,
                                          isco88 %in% 125  ~ 64,
                                          isco88 %in% 1250 ~ 64,
                                          isco88 %in% 1251 ~ 70,
                                          isco88 %in% 1252 ~ 60,
                                          isco88 %in% 13   ~ 51,
                                          isco88 %in% 130  ~ 51,
                                          isco88 %in% 1300 ~ 51,
                                          isco88 %in% 131  ~ 51,
                                          isco88 %in% 1310 ~ 51,
                                          isco88 %in% 1311 ~ 43,
                                          isco88 %in% 1312 ~ 56,
                                          isco88 %in% 1313 ~ 51,
                                          isco88 %in% 1314 ~ 49,
                                          isco88 %in% 1315 ~ 44,
                                          isco88 %in% 1316 ~ 51,
                                          isco88 %in% 1317 ~ 51,
                                          isco88 %in% 1318 ~ 51,
                                          isco88 %in% 1319 ~ 51,
                                          isco88 %in% 2    ~ 70,
                                          isco88 %in% 20   ~ 70,
                                          isco88 %in% 200  ~ 70,
                                          isco88 %in% 2000 ~ 70,
                                          isco88 %in% 21   ~ 69,
                                          isco88 %in% 210  ~ 69,
                                          isco88 %in% 2100 ~ 69,
                                          isco88 %in% 211  ~ 74,
                                          isco88 %in% 2110 ~ 74,
                                          isco88 %in% 2111 ~ 74,
                                          isco88 %in% 2112 ~ 74,
                                          isco88 %in% 2113 ~ 74,
                                          isco88 %in% 2114 ~ 74,
                                          isco88 %in% 212  ~ 71,
                                          isco88 %in% 2120 ~ 71,
                                          isco88 %in% 2121 ~ 71,
                                          isco88 %in% 2122 ~ 71,
                                          isco88 %in% 213  ~ 71,
                                          isco88 %in% 2130 ~ 71,
                                          isco88 %in% 2131 ~ 71,
                                          isco88 %in% 2132 ~ 71,
                                          isco88 %in% 2139 ~ 71,
                                          isco88 %in% 214  ~ 73,
                                          isco88 %in% 2140 ~ 73,
                                          isco88 %in% 2141 ~ 69,
                                          isco88 %in% 2142 ~ 69,
                                          isco88 %in% 2143 ~ 68,
                                          isco88 %in% 2144 ~ 68,
                                          isco88 %in% 2145 ~ 67,
                                          isco88 %in% 2146 ~ 71,
                                          isco88 %in% 2147 ~ 67,
                                          isco88 %in% 2148 ~ 56,
                                          isco88 %in% 2149 ~ 69,
                                          isco88 %in% 22   ~ 80,
                                          isco88 %in% 220  ~ 80,
                                          isco88 %in% 2200 ~ 80,
                                          isco88 %in% 221  ~ 78,
                                          isco88 %in% 2210 ~ 78,
                                          isco88 %in% 2211 ~ 77,
                                          isco88 %in% 2212 ~ 77,
                                          isco88 %in% 2213 ~ 79,
                                          isco88 %in% 222  ~ 85,
                                          isco88 %in% 2220 ~ 85,
                                          isco88 %in% 2221 ~ 88,
                                          isco88 %in% 2222 ~ 85,
                                          isco88 %in% 2223 ~ 83,
                                          isco88 %in% 2224 ~ 74,
                                          isco88 %in% 2229 ~ 85,
                                          isco88 %in% 223  ~ 43,
                                          isco88 %in% 2230 ~ 43,
                                          isco88 %in% 23   ~ 69,
                                          isco88 %in% 230  ~ 69,
                                          isco88 %in% 2300 ~ 69,
                                          isco88 %in% 231  ~ 77,
                                          isco88 %in% 2310 ~ 77,
                                          isco88 %in% 232  ~ 69,
                                          isco88 %in% 2320 ~ 69,
                                          isco88 %in% 2321 ~ 70,
                                          isco88 %in% 2322 ~ 66,
                                          isco88 %in% 233  ~ 66,
                                          isco88 %in% 2330 ~ 66,
                                          isco88 %in% 2331 ~ 66,
                                          isco88 %in% 2332 ~ 43,
                                          isco88 %in% 234  ~ 66,
                                          isco88 %in% 2340 ~ 66,
                                          isco88 %in% 235  ~ 66,
                                          isco88 %in% 2350 ~ 66,
                                          isco88 %in% 2351 ~ 70,
                                          isco88 %in% 2352 ~ 70,
                                          isco88 %in% 2359 ~ 65,
                                          isco88 %in% 24   ~ 68,
                                          isco88 %in% 240  ~ 68,
                                          isco88 %in% 2400 ~ 68,
                                          isco88 %in% 241  ~ 69,
                                          isco88 %in% 2410 ~ 69,
                                          isco88 %in% 2411 ~ 69,
                                          isco88 %in% 2412 ~ 69,
                                          isco88 %in% 2419 ~ 69,
                                          isco88 %in% 242  ~ 85,
                                          isco88 %in% 2420 ~ 85,
                                          isco88 %in% 2421 ~ 85,
                                          isco88 %in% 2422 ~ 90,
                                          isco88 %in% 2429 ~ 82,
                                          isco88 %in% 243  ~ 65,
                                          isco88 %in% 2430 ~ 65,
                                          isco88 %in% 2431 ~ 65,
                                          isco88 %in% 2432 ~ 65,
                                          isco88 %in% 244  ~ 65,
                                          isco88 %in% 2440 ~ 65,
                                          isco88 %in% 2441 ~ 78,
                                          isco88 %in% 2442 ~ 71,
                                          isco88 %in% 2443 ~ 71,
                                          isco88 %in% 2444 ~ 65,
                                          isco88 %in% 2445 ~ 71,
                                          isco88 %in% 2446 ~ 51,
                                          isco88 %in% 245  ~ 61,
                                          isco88 %in% 2450 ~ 61,
                                          isco88 %in% 2451 ~ 65,
                                          isco88 %in% 2452 ~ 54,
                                          isco88 %in% 2453 ~ 64,
                                          isco88 %in% 2454 ~ 64,
                                          isco88 %in% 2455 ~ 64,
                                          isco88 %in% 246  ~ 53,
                                          isco88 %in% 2460 ~ 53,
                                          isco88 %in% 247  ~ NA_real_,
                                          isco88 %in% 2470 ~ NA_real_,
                                          isco88 %in% 3    ~ 54,
                                          isco88 %in% 30   ~ 54,
                                          isco88 %in% 300  ~ 54,
                                          isco88 %in% 3000 ~ 54,
                                          isco88 %in% 31   ~ 50,
                                          isco88 %in% 310  ~ 50,
                                          isco88 %in% 3100 ~ 50,
                                          isco88 %in% 311  ~ 49,
                                          isco88 %in% 3110 ~ 49,
                                          isco88 %in% 3111 ~ 45,
                                          isco88 %in% 3112 ~ 45,
                                          isco88 %in% 3113 ~ 46,
                                          isco88 %in% 3114 ~ 46,
                                          isco88 %in% 3115 ~ 54,
                                          isco88 %in% 3116 ~ 54,
                                          isco88 %in% 3117 ~ 54,
                                          isco88 %in% 3118 ~ 51,
                                          isco88 %in% 3119 ~ 53,
                                          isco88 %in% 312  ~ 52,
                                          isco88 %in% 3120 ~ 52,
                                          isco88 %in% 3121 ~ 52,
                                          isco88 %in% 3122 ~ 52,
                                          isco88 %in% 3123 ~ 52,
                                          isco88 %in% 313  ~ 52,
                                          isco88 %in% 3130 ~ 52,
                                          isco88 %in% 3131 ~ 48,
                                          isco88 %in% 3132 ~ 57,
                                          isco88 %in% 3133 ~ 57,
                                          isco88 %in% 3139 ~ 52,
                                          isco88 %in% 314  ~ 57,
                                          isco88 %in% 3140 ~ 57,
                                          isco88 %in% 3141 ~ 52,
                                          isco88 %in% 3142 ~ 52,
                                          isco88 %in% 3143 ~ 69,
                                          isco88 %in% 3144 ~ 69,
                                          isco88 %in% 3145 ~ 50,
                                          isco88 %in% 315  ~ 50,
                                          isco88 %in% 3150 ~ 50,
                                          isco88 %in% 3151 ~ 50,
                                          isco88 %in% 3152 ~ 50,
                                          isco88 %in% 32   ~ 48,
                                          isco88 %in% 320  ~ 48,
                                          isco88 %in% 3200 ~ 48,
                                          isco88 %in% 321  ~ 50,
                                          isco88 %in% 3210 ~ 50,
                                          isco88 %in% 3211 ~ 50,
                                          isco88 %in% 3212 ~ 50,
                                          isco88 %in% 3213 ~ 50,
                                          isco88 %in% 322  ~ 55,
                                          isco88 %in% 3220 ~ 55,
                                          isco88 %in% 3221 ~ 51,
                                          isco88 %in% 3222 ~ 51,
                                          isco88 %in% 3223 ~ 51,
                                          isco88 %in% 3224 ~ 60,
                                          isco88 %in% 3225 ~ 51,
                                          isco88 %in% 3226 ~ 60,
                                          isco88 %in% 3227 ~ 51,
                                          isco88 %in% 3228 ~ 51,
                                          isco88 %in% 3229 ~ 51,
                                          isco88 %in% 323  ~ 38,
                                          isco88 %in% 3230 ~ 38,
                                          isco88 %in% 3231 ~ 38,
                                          isco88 %in% 3232 ~ 38,
                                          isco88 %in% 3240 ~ 49,
                                          isco88 %in% 3241 ~ 51,
                                          isco88 %in% 3242 ~ 38,
                                          isco88 %in% 33   ~ 38,
                                          isco88 %in% 330  ~ 38,
                                          isco88 %in% 3300 ~ 38,
                                          isco88 %in% 331  ~ 38,
                                          isco88 %in% 3310 ~ 38,
                                          isco88 %in% 332  ~ 38,
                                          isco88 %in% 3320 ~ 38,
                                          isco88 %in% 333  ~ 38,
                                          isco88 %in% 3330 ~ 38,
                                          isco88 %in% 334  ~ 38,
                                          isco88 %in% 3340 ~ 38,
                                          isco88 %in% 34   ~ 55,
                                          isco88 %in% 340  ~ 55,
                                          isco88 %in% 3400 ~ 55,
                                          isco88 %in% 341  ~ 55,
                                          isco88 %in% 3410 ~ 55,
                                          isco88 %in% 3411 ~ 61,
                                          isco88 %in% 3412 ~ 54,
                                          isco88 %in% 3413 ~ 59,
                                          isco88 %in% 3414 ~ 56,
                                          isco88 %in% 3415 ~ 56,
                                          isco88 %in% 3416 ~ 50,
                                          isco88 %in% 3417 ~ 56,
                                          isco88 %in% 3419 ~ 55,
                                          isco88 %in% 342  ~ 55,
                                          isco88 %in% 3420 ~ 55,
                                          isco88 %in% 3421 ~ 55,
                                          isco88 %in% 3422 ~ 55,
                                          isco88 %in% 3423 ~ 55,
                                          isco88 %in% 3429 ~ 55,
                                          isco88 %in% 343  ~ 54,
                                          isco88 %in% 3430 ~ 54,
                                          isco88 %in% 3431 ~ 54,
                                          isco88 %in% 3432 ~ 59,
                                          isco88 %in% 3433 ~ 51,
                                          isco88 %in% 3434 ~ 61,
                                          isco88 %in% 3439 ~ 54,
                                          isco88 %in% 344  ~ 56,
                                          isco88 %in% 3440 ~ 56,
                                          isco88 %in% 3441 ~ 56,
                                          isco88 %in% 3442 ~ 57,
                                          isco88 %in% 3443 ~ 56,
                                          isco88 %in% 3444 ~ 46,
                                          isco88 %in% 3449 ~ 56,
                                          isco88 %in% 345  ~ 56,
                                          isco88 %in% 3450 ~ 56,
                                          isco88 %in% 3451 ~ 55,
                                          isco88 %in% 3452 ~ 56,
                                          isco88 %in% 346  ~ 43,
                                          isco88 %in% 3460 ~ 43,
                                          isco88 %in% 347  ~ 52,
                                          isco88 %in% 3470 ~ 52,
                                          isco88 %in% 3471 ~ 53,
                                          isco88 %in% 3472 ~ 64,
                                          isco88 %in% 3473 ~ 50,
                                          isco88 %in% 3474 ~ 50,
                                          isco88 %in% 3475 ~ 54,
                                          isco88 %in% 348  ~ 38,
                                          isco88 %in% 3480 ~ 38,
                                          isco88 %in% 4    ~ 45,
                                          isco88 %in% 40   ~ 45,
                                          isco88 %in% 400  ~ 45,
                                          isco88 %in% 4000 ~ 45,
                                          isco88 %in% 41   ~ 45,
                                          isco88 %in% 410  ~ 45,
                                          isco88 %in% 4100 ~ 45,
                                          isco88 %in% 411  ~ 51,
                                          isco88 %in% 4110 ~ 51,
                                          isco88 %in% 4111 ~ 51,
                                          isco88 %in% 4112 ~ 50,
                                          isco88 %in% 4113 ~ 50,
                                          isco88 %in% 4114 ~ 51,
                                          isco88 %in% 4115 ~ 53,
                                          isco88 %in% 412  ~ 51,
                                          isco88 %in% 4120 ~ 51,
                                          isco88 %in% 4121 ~ 51,
                                          isco88 %in% 4122 ~ 51,
                                          isco88 %in% 413  ~ 36,
                                          isco88 %in% 4130 ~ 36,
                                          isco88 %in% 4131 ~ 32,
                                          isco88 %in% 4132 ~ 43,
                                          isco88 %in% 4133 ~ 45,
                                          isco88 %in% 414  ~ 39,
                                          isco88 %in% 4140 ~ 39,
                                          isco88 %in% 4141 ~ 39,
                                          isco88 %in% 4142 ~ 39,
                                          isco88 %in% 4143 ~ 39,
                                          isco88 %in% 4144 ~ 39,
                                          isco88 %in% 419  ~ 39,
                                          isco88 %in% 4190 ~ 39,
                                          isco88 %in% 42   ~ 49,
                                          isco88 %in% 420  ~ 49,
                                          isco88 %in% 4200 ~ 49,
                                          isco88 %in% 421  ~ 48,
                                          isco88 %in% 4210 ~ 48,
                                          isco88 %in% 4211 ~ 53,
                                          isco88 %in% 4212 ~ 46,
                                          isco88 %in% 4213 ~ 40,
                                          isco88 %in% 4214 ~ 40,
                                          isco88 %in% 4215 ~ 40,
                                          isco88 %in% 422  ~ 52,
                                          isco88 %in% 4220 ~ 52,
                                          isco88 %in% 4221 ~ 52,
                                          isco88 %in% 4222 ~ 52,
                                          isco88 %in% 4223 ~ 52,
                                          isco88 %in% 5    ~ 40,
                                          isco88 %in% 50   ~ 40,
                                          isco88 %in% 500  ~ 40,
                                          isco88 %in% 5000 ~ 40,
                                          isco88 %in% 51   ~ 38,
                                          isco88 %in% 510  ~ 38,
                                          isco88 %in% 5100 ~ 38,
                                          isco88 %in% 511  ~ 34,
                                          isco88 %in% 5110 ~ 34,
                                          isco88 %in% 5111 ~ 34,
                                          isco88 %in% 5112 ~ 34,
                                          isco88 %in% 5113 ~ 34,
                                          isco88 %in% 512  ~ 32,
                                          isco88 %in% 5120 ~ 32,
                                          isco88 %in% 5121 ~ 30,
                                          isco88 %in% 5122 ~ 30,
                                          isco88 %in% 5123 ~ 34,
                                          isco88 %in% 513  ~ 25,
                                          isco88 %in% 5130 ~ 25,
                                          isco88 %in% 5131 ~ 25,
                                          isco88 %in% 5132 ~ 25,
                                          isco88 %in% 5133 ~ 25,
                                          isco88 %in% 5139 ~ 25,
                                          isco88 %in% 514  ~ 30,
                                          isco88 %in% 5140 ~ 30,
                                          isco88 %in% 5141 ~ 29,
                                          isco88 %in% 5142 ~ 19,
                                          isco88 %in% 5143 ~ 54,
                                          isco88 %in% 5149 ~ 19,
                                          isco88 %in% 515  ~ 43,
                                          isco88 %in% 5150 ~ 43,
                                          isco88 %in% 5151 ~ 43,
                                          isco88 %in% 5152 ~ 43,
                                          isco88 %in% 516  ~ 47,
                                          isco88 %in% 5160 ~ 47,
                                          isco88 %in% 5161 ~ 42,
                                          isco88 %in% 5162 ~ 50,
                                          isco88 %in% 5163 ~ 40,
                                          isco88 %in% 5164 ~ 40,
                                          isco88 %in% 5169 ~ 40,
                                          isco88 %in% 52   ~ 43,
                                          isco88 %in% 520  ~ 43,
                                          isco88 %in% 5200 ~ 43,
                                          isco88 %in% 521  ~ 43,
                                          isco88 %in% 5210 ~ 43,
                                          isco88 %in% 522  ~ 43,
                                          isco88 %in% 5220 ~ 43,
                                          isco88 %in% 523  ~ 37,
                                          isco88 %in% 5230 ~ 37,
                                          isco88 %in% 6    ~ 23,
                                          isco88 %in% 60   ~ 23,
                                          isco88 %in% 600  ~ 23,
                                          isco88 %in% 6000 ~ 23,
                                          isco88 %in% 61   ~ 23,
                                          isco88 %in% 610  ~ 23,
                                          isco88 %in% 6100 ~ 23,
                                          isco88 %in% 611  ~ 23,
                                          isco88 %in% 6110 ~ 23,
                                          isco88 %in% 6111 ~ 23,
                                          isco88 %in% 6112 ~ 23,
                                          isco88 %in% 6113 ~ 23,
                                          isco88 %in% 6114 ~ 23,
                                          isco88 %in% 612  ~ 23,
                                          isco88 %in% 6120 ~ 23,
                                          isco88 %in% 6121 ~ 23,
                                          isco88 %in% 6122 ~ 23,
                                          isco88 %in% 6123 ~ 23,
                                          isco88 %in% 6124 ~ 23,
                                          isco88 %in% 6129 ~ 23,
                                          isco88 %in% 613  ~ 23,
                                          isco88 %in% 6130 ~ 23,
                                          isco88 %in% 6131 ~ 23,
                                          isco88 %in% 6132 ~ 27,
                                          isco88 %in% 6133 ~ 28,
                                          isco88 %in% 6134 ~ 28,
                                          isco88 %in% 614  ~ 22,
                                          isco88 %in% 6140 ~ 22,
                                          isco88 %in% 6141 ~ 22,
                                          isco88 %in% 6142 ~ 22,
                                          isco88 %in% 615  ~ 28,
                                          isco88 %in% 6150 ~ 28,
                                          isco88 %in% 6151 ~ 28,
                                          isco88 %in% 6152 ~ 28,
                                          isco88 %in% 6153 ~ 28,
                                          isco88 %in% 6154 ~ 28,
                                          isco88 %in% 62   ~ 16,
                                          isco88 %in% 620  ~ 16,
                                          isco88 %in% 6200 ~ 16,
                                          isco88 %in% 621  ~ 16,
                                          isco88 %in% 6210 ~ 16,
                                          isco88 %in% 7    ~ 34,
                                          isco88 %in% 70   ~ 34,
                                          isco88 %in% 700  ~ 34,
                                          isco88 %in% 7000 ~ 34,
                                          isco88 %in% 71   ~ 31,
                                          isco88 %in% 710  ~ 31,
                                          isco88 %in% 7100 ~ 31,
                                          isco88 %in% 711  ~ 30,
                                          isco88 %in% 7110 ~ 30,
                                          isco88 %in% 7111 ~ 30,
                                          isco88 %in% 7112 ~ 30,
                                          isco88 %in% 7113 ~ 27,
                                          isco88 %in% 712  ~ 30,
                                          isco88 %in% 7120 ~ 30,
                                          isco88 %in% 7121 ~ 29,
                                          isco88 %in% 7122 ~ 29,
                                          isco88 %in% 7123 ~ 26,
                                          isco88 %in% 7124 ~ 29,
                                          isco88 %in% 7129 ~ 30,
                                          isco88 %in% 713  ~ 34,
                                          isco88 %in% 7130 ~ 34,
                                          isco88 %in% 7131 ~ 19,
                                          isco88 %in% 7132 ~ 30,
                                          isco88 %in% 7133 ~ 31,
                                          isco88 %in% 7134 ~ 34,
                                          isco88 %in% 7135 ~ 26,
                                          isco88 %in% 7136 ~ 33,
                                          isco88 %in% 7137 ~ 37,
                                          isco88 %in% 7139 ~ NA_real_,
                                          isco88 %in% 714  ~ 29,
                                          isco88 %in% 7140 ~ 29,
                                          isco88 %in% 7141 ~ 29,
                                          isco88 %in% 7142 ~ 32,
                                          isco88 %in% 7143 ~ 29,
                                          isco88 %in% 72   ~ 34,
                                          isco88 %in% 720  ~ 34,
                                          isco88 %in% 7200 ~ 34,
                                          isco88 %in% 721  ~ 31,
                                          isco88 %in% 7210 ~ 31,
                                          isco88 %in% 7211 ~ 29,
                                          isco88 %in% 7212 ~ 30,
                                          isco88 %in% 7213 ~ 33,
                                          isco88 %in% 7214 ~ 30,
                                          isco88 %in% 7215 ~ 30,
                                          isco88 %in% 7216 ~ 30,
                                          isco88 %in% 722  ~ 35,
                                          isco88 %in% 7220 ~ 35,
                                          isco88 %in% 7221 ~ 33,
                                          isco88 %in% 7222 ~ 40,
                                          isco88 %in% 7223 ~ 34,
                                          isco88 %in% 7224 ~ 24,
                                          isco88 %in% 723  ~ 34,
                                          isco88 %in% 7230 ~ 34,
                                          isco88 %in% 7231 ~ 34,
                                          isco88 %in% 7232 ~ 42,
                                          isco88 %in% 7233 ~ 33,
                                          isco88 %in% 7234 ~ 23,
                                          isco88 %in% 724  ~ 40,
                                          isco88 %in% 7240 ~ 40,
                                          isco88 %in% 7241 ~ 40,
                                          isco88 %in% 7242 ~ 39,
                                          isco88 %in% 7243 ~ 41,
                                          isco88 %in% 7244 ~ 40,
                                          isco88 %in% 7245 ~ 38,
                                          isco88 %in% 73   ~ 34,
                                          isco88 %in% 730  ~ 34,
                                          isco88 %in% 7300 ~ 34,
                                          isco88 %in% 731  ~ 38,
                                          isco88 %in% 7310 ~ 38,
                                          isco88 %in% 7311 ~ 38,
                                          isco88 %in% 7312 ~ 38,
                                          isco88 %in% 7313 ~ 38,
                                          isco88 %in% 732  ~ 28,
                                          isco88 %in% 7320 ~ 28,
                                          isco88 %in% 7321 ~ 27,
                                          isco88 %in% 7322 ~ 29,
                                          isco88 %in% 7323 ~ 29,
                                          isco88 %in% 7324 ~ 29,
                                          isco88 %in% 733  ~ 29,
                                          isco88 %in% 7330 ~ 29,
                                          isco88 %in% 7331 ~ 29,
                                          isco88 %in% 7332 ~ 29,
                                          isco88 %in% 734  ~ 40,
                                          isco88 %in% 7340 ~ 40,
                                          isco88 %in% 7341 ~ 40,
                                          isco88 %in% 7342 ~ 40,
                                          isco88 %in% 7343 ~ 42,
                                          isco88 %in% 7344 ~ 40,
                                          isco88 %in% 7345 ~ 37,
                                          isco88 %in% 7346 ~ 38,
                                          isco88 %in% 74   ~ 33,
                                          isco88 %in% 740  ~ 33,
                                          isco88 %in% 7400 ~ 33,
                                          isco88 %in% 741  ~ 30,
                                          isco88 %in% 7410 ~ 30,
                                          isco88 %in% 7411 ~ 30,
                                          isco88 %in% 7412 ~ 31,
                                          isco88 %in% 7413 ~ 30,
                                          isco88 %in% 7414 ~ 30,
                                          isco88 %in% 7415 ~ 30,
                                          isco88 %in% 7416 ~ 30,
                                          isco88 %in% 742  ~ 33,
                                          isco88 %in% 7420 ~ 33,
                                          isco88 %in% 7421 ~ 33,
                                          isco88 %in% 7422 ~ 33,
                                          isco88 %in% 7423 ~ 33,
                                          isco88 %in% 7424 ~ 33,
                                          isco88 %in% 743  ~ 36,
                                          isco88 %in% 7430 ~ 36,
                                          isco88 %in% 7431 ~ 29,
                                          isco88 %in% 7432 ~ 29,
                                          isco88 %in% 7433 ~ 45,
                                          isco88 %in% 7434 ~ 36,
                                          isco88 %in% 7435 ~ 36,
                                          isco88 %in% 7436 ~ 33,
                                          isco88 %in% 7437 ~ 28,
                                          isco88 %in% 744  ~ 31,
                                          isco88 %in% 7440 ~ 31,
                                          isco88 %in% 7441 ~ 31,
                                          isco88 %in% 7442 ~ 31,
                                          isco88 %in% 75   ~ 42,
                                          isco88 %in% 750  ~ 42,
                                          isco88 %in% 7500 ~ 42,
                                          isco88 %in% 751  ~ 42,
                                          isco88 %in% 7510 ~ 42,
                                          isco88 %in% 752  ~ 39,
                                          isco88 %in% 7520 ~ 39,
                                          isco88 %in% 753  ~ 26,
                                          isco88 %in% 7530 ~ 26,
                                          isco88 %in% 8    ~ 31,
                                          isco88 %in% 80   ~ 31,
                                          isco88 %in% 800  ~ 31,
                                          isco88 %in% 8000 ~ 31,
                                          isco88 %in% 81   ~ 30,
                                          isco88 %in% 810  ~ 30,
                                          isco88 %in% 8100 ~ 30,
                                          isco88 %in% 811  ~ 35,
                                          isco88 %in% 8110 ~ 35,
                                          isco88 %in% 8111 ~ 35,
                                          isco88 %in% 8112 ~ 35,
                                          isco88 %in% 8113 ~ 35,
                                          isco88 %in% 812  ~ 30,
                                          isco88 %in% 8120 ~ 30,
                                          isco88 %in% 8121 ~ 31,
                                          isco88 %in% 8122 ~ 30,
                                          isco88 %in% 8123 ~ 28,
                                          isco88 %in% 8124 ~ 30,
                                          isco88 %in% 813  ~ 22,
                                          isco88 %in% 8130 ~ 22,
                                          isco88 %in% 8131 ~ 22,
                                          isco88 %in% 8139 ~ 22,
                                          isco88 %in% 814  ~ 27,
                                          isco88 %in% 8140 ~ 27,
                                          isco88 %in% 8141 ~ 27,
                                          isco88 %in% 8142 ~ 27,
                                          isco88 %in% 8143 ~ 27,
                                          isco88 %in% 815  ~ 35,
                                          isco88 %in% 8150 ~ 35,
                                          isco88 %in% 8151 ~ 35,
                                          isco88 %in% 8152 ~ 35,
                                          isco88 %in% 8153 ~ 35,
                                          isco88 %in% 8154 ~ 35,
                                          isco88 %in% 8155 ~ 35,
                                          isco88 %in% 8159 ~ 35,
                                          isco88 %in% 816  ~ 32,
                                          isco88 %in% 8160 ~ 32,
                                          isco88 %in% 8161 ~ 33,
                                          isco88 %in% 8162 ~ 27,
                                          isco88 %in% 8163 ~ 33,
                                          isco88 %in% 817  ~ 26,
                                          isco88 %in% 8170 ~ 26,
                                          isco88 %in% 8171 ~ 26,
                                          isco88 %in% 8172 ~ 26,
                                          isco88 %in% 82   ~ 32,
                                          isco88 %in% 820  ~ 32,
                                          isco88 %in% 8200 ~ 32,
                                          isco88 %in% 821  ~ 36,
                                          isco88 %in% 8210 ~ 36,
                                          isco88 %in% 8211 ~ 36,
                                          isco88 %in% 8212 ~ 30,
                                          isco88 %in% 822  ~ 30,
                                          isco88 %in% 8220 ~ 30,
                                          isco88 %in% 8221 ~ 30,
                                          isco88 %in% 8222 ~ 30,
                                          isco88 %in% 8223 ~ 30,
                                          isco88 %in% 8224 ~ 30,
                                          isco88 %in% 8229 ~ 30,
                                          isco88 %in% 823  ~ 30,
                                          isco88 %in% 8230 ~ 30,
                                          isco88 %in% 8231 ~ 30,
                                          isco88 %in% 8232 ~ 30,
                                          isco88 %in% 824  ~ 29,
                                          isco88 %in% 8240 ~ 29,
                                          isco88 %in% 825  ~ 38,
                                          isco88 %in% 8250 ~ 38,
                                          isco88 %in% 8251 ~ 38,
                                          isco88 %in% 8252 ~ 38,
                                          isco88 %in% 8253 ~ 38,
                                          isco88 %in% 826  ~ 30,
                                          isco88 %in% 8260 ~ 30,
                                          isco88 %in% 8261 ~ 29,
                                          isco88 %in% 8262 ~ 29,
                                          isco88 %in% 8263 ~ 32,
                                          isco88 %in% 8264 ~ 24,
                                          isco88 %in% 8265 ~ 32,
                                          isco88 %in% 8266 ~ 32,
                                          isco88 %in% 8269 ~ 32,
                                          isco88 %in% 827  ~ 29,
                                          isco88 %in% 8270 ~ 29,
                                          isco88 %in% 8271 ~ 29,
                                          isco88 %in% 8272 ~ 29,
                                          isco88 %in% 8273 ~ 29,
                                          isco88 %in% 8274 ~ 29,
                                          isco88 %in% 8275 ~ 29,
                                          isco88 %in% 8276 ~ 29,
                                          isco88 %in% 8277 ~ 29,
                                          isco88 %in% 8278 ~ 29,
                                          isco88 %in% 8279 ~ 29,
                                          isco88 %in% 828  ~ 31,
                                          isco88 %in% 8280 ~ 31,
                                          isco88 %in% 8281 ~ 30,
                                          isco88 %in% 8282 ~ 34,
                                          isco88 %in% 8283 ~ 34,
                                          isco88 %in% 8284 ~ 30,
                                          isco88 %in% 8285 ~ 30,
                                          isco88 %in% 8286 ~ 30,
                                          isco88 %in% 8287 ~ NA_real_,
                                          isco88 %in% 829  ~ 26,
                                          isco88 %in% 8290 ~ 26,
                                          isco88 %in% 83   ~ 32,
                                          isco88 %in% 830  ~ 32,
                                          isco88 %in% 8300 ~ 32,
                                          isco88 %in% 831  ~ 36,
                                          isco88 %in% 8310 ~ 36,
                                          isco88 %in% 8311 ~ 41,
                                          isco88 %in% 8312 ~ 32,
                                          isco88 %in% 832  ~ 34,
                                          isco88 %in% 8320 ~ 34,
                                          isco88 %in% 8321 ~ 30,
                                          isco88 %in% 8322 ~ 30,
                                          isco88 %in% 8323 ~ 30,
                                          isco88 %in% 8324 ~ 34,
                                          isco88 %in% 833  ~ 26,
                                          isco88 %in% 8330 ~ 26,
                                          isco88 %in% 8331 ~ 26,
                                          isco88 %in% 8332 ~ 26,
                                          isco88 %in% 8333 ~ 28,
                                          isco88 %in% 8334 ~ 28,
                                          isco88 %in% 834  ~ 32,
                                          isco88 %in% 8340 ~ 32,
                                          isco88 %in% 840  ~ 24,
                                          isco88 %in% 8400 ~ 24,
                                          isco88 %in% 9    ~ 20,
                                          isco88 %in% 90   ~ 20,
                                          isco88 %in% 900  ~ 20,
                                          isco88 %in% 9000 ~ 20,
                                          isco88 %in% 91   ~ 25,
                                          isco88 %in% 910  ~ 25,
                                          isco88 %in% 9100 ~ 25,
                                          isco88 %in% 911  ~ 29,
                                          isco88 %in% 9110 ~ 29,
                                          isco88 %in% 9111 ~ 29,
                                          isco88 %in% 9112 ~ 28,
                                          isco88 %in% 9113 ~ 29,
                                          isco88 %in% 912  ~ 28,
                                          isco88 %in% 9120 ~ 28,
                                          isco88 %in% 913  ~ 16,
                                          isco88 %in% 9130 ~ 16,
                                          isco88 %in% 9131 ~ 16,
                                          isco88 %in% 9132 ~ 16,
                                          isco88 %in% 9133 ~ 16,
                                          isco88 %in% 914  ~ 23,
                                          isco88 %in% 9140 ~ 23,
                                          isco88 %in% 9141 ~ 23,
                                          isco88 %in% 9142 ~ 23,
                                          isco88 %in% 915  ~ 27,
                                          isco88 %in% 9150 ~ 27,
                                          isco88 %in% 9151 ~ 25,
                                          isco88 %in% 9152 ~ 27,
                                          isco88 %in% 9153 ~ 27,
                                          isco88 %in% 916  ~ 23,
                                          isco88 %in% 9160 ~ 23,
                                          isco88 %in% 9161 ~ 23,
                                          isco88 %in% 9162 ~ 23,
                                          isco88 %in% 92   ~ 16,
                                          isco88 %in% 920  ~ 16,
                                          isco88 %in% 9200 ~ 16,
                                          isco88 %in% 921  ~ 16,
                                          isco88 %in% 9210 ~ 16,
                                          isco88 %in% 9211 ~ 16,
                                          isco88 %in% 9212 ~ 16,
                                          isco88 %in% 9213 ~ 16,
                                          isco88 %in% 93   ~ 23,
                                          isco88 %in% 930  ~ 23,
                                          isco88 %in% 9300 ~ 23,
                                          isco88 %in% 931  ~ 21,
                                          isco88 %in% 9310 ~ 21,
                                          isco88 %in% 9311 ~ 21,
                                          isco88 %in% 9312 ~ 21,
                                          isco88 %in% 9313 ~ 21,
                                          isco88 %in% 932  ~ 20,
                                          isco88 %in% 9320 ~ 20,
                                          isco88 %in% 9321 ~ 20,
                                          isco88 %in% 9322 ~ 24,
                                          isco88 %in% 933  ~ 29,
                                          isco88 %in% 9330 ~ 29,
                                          isco88 %in% 9331 ~ 22,
                                          isco88 %in% 9332 ~ 22,
                                          isco88 %in% 9333 ~ 30,
                                      )
                      )
    if (display.nas) display.nas(isei, conv.from="isco88", conv.to='isei92')
    return(isei$isei92)
}

isco88toISEI08 <- function(isco88, display.nas=FALSE)
{
    msg <- paste0('\n','The function "isco88toISEI08()" first converts ISCO-88 to ISCO-08, and then computes the ISEI-08 using the latter.',  '\n'); cat(msg)

    isei = tibble::tibble(isco08=isco88to08(isco88, TRUE) )
    isei =  isei %>% dplyr::mutate(isei08 = isco08toISEI08(isco08, TRUE))
    ## already displayed in the functions above
    ## if (display.nas) display.nas(isei, conv.from="isco88", conv.to='isei92')
    return(isei$isei08)
}


## {{{ docs }}}
#' Compute ESeC 
#'
#' This function returns the ESeC (European Socio-economic classification) scheme
#'
#'
#' @param isco88 a vector with the ISCO-88 codes
#' @param employed a vector containing the value 1 whenever the person is employed
#' @param unemployed a vector containing the value 1 whenever the person is unemployed
#' @param self.employed a vector containing the value 1 whenever the person is self-employed
#' @param supervisor a vector containing the value 1 whenever the person is a supervisor
#' @param n.employees a vector containing the number of employees.
#' @param n.classes either one of the following integers: 10 (default), 7 ,6, or 4 indicating the number of categories to be used inthe ESEeC scheme. See details.
#' @param keep.unemployed boolean (default \code{TRUE}) indicating to keep (if \code{TRUE}) or not (if \code{FALSE}) the class of unemployed in the reduced ESeC scheme (with number of classes equal ot 7, 6, or 4). See details.
#' @param isco88.armed.forces.code numeric vector with the code for armed forces. Default is \code{c(0, 10, 100, 110)}
#'
#' @return It returns a list with two elements, the ESec code and its label
#' @details Harrison & Rose, 2006, "(ESeC) User Guide" explains the conceptual derivation of the class scheme. In the page 9-10, they suggest a possible reductions in the number of classes from 10 to 6, 5, or 3. The reduced ESeC class scheme in that case excludes the unemployed, but they state that it can be added if desired. This script by default keep the unemployed class, producing reduced schemes with 7, 6, or 4 categories. To exclude the unemployed, set \code{keep.unemployed=TRUE}
#'
#' @export
## }}}
isco88toESeC <- function(isco88, employed=NULL, unemployed=NULL, self.employed=NULL, supervisor=NULL, n.employees=NULL, n.classes=10, keep.unemployed=TRUE, isco88.armed.forces.code=c(0, 10, 100, 110))
{
    ## check https://www.iser.essex.ac.uk/archives/esec/user-guide
    ## check number of categories allowed
    ## ----------------------------------
    if (n.classes %!in% c(10,7,6,4)) {
        stop("\n\nNumber of classes allowed to compute ESeC class scheme are 10, 7, 6, or 4.\n\n")
    }
    ## recode armed forces
    ## -------------------
    if (!is.null(isco88.armed.forces.code)) {
        isco88[isco88 %in% isco88.armed.forces.code] = 10
    }else{
        on.exit(cat("\n\nNo code provided for armed forces to compute ESeC in the parameter isco88.armed.forces.code"))
    }
    ## error control
    ## -------------
    if (any(isco88[(!is.na(isco88)) & isco88 != 10] %>% nchar < 4)) {
        stop("\n\nPlease, provide isco88 with 4 digits for all cases to compute ESeC . Check isco88 for armed foreces and provide it in the parameter isco88.armed.forces.code! \n\n")
    }

    if (is.null(employed) | is.null(unemployed) | is.null(self.employed) | is.null(supervisor) | is.null(n.employees)) {
        stop("\n\nThe parameters employed, unemployed, self.employed, supervisor, and n.employees must be non-NULL to compute ESeC .\n\n")
    }

    ## reduces isco88 to 3 digits
    ## --------------------------
    isco88 = isco88 %>% stringr::str_replace(string=., pattern=".$", replacement="")

    ## set indicator variables
    ## -----------------------
    self.employed[self.employed != 1 | is.na(self.employed)] = 0
    unemployed   [unemployed    != 1 | is.na(unemployed)   ] = 0
    employed     [employed      != 1 | is.na(employed)     ] = 0
    supervisor   [supervisor    != 1 | is.na(supervisor)   ] = 0
    
    ## Compute employment status categories
    ## ------------------------------------
    status = rep(NA, length(isco88))
    status[self.employed == 1 & n.employees > 9]                       <- 1  #### 1: self-employed 10+ employees
    status[self.employed == 1 & n.employees %in% 1:9]                  <- 2  #### 2: small employers <10
    status[self.employed == 1 & n.employees == 0]                      <- 3  #### 3: self-employed, no employees 
    status[self.employed != 1 & employed    == 1 & supervisor == 1]    <- 4  #### 4: supervisors
    status[self.employed != 1 & employed    == 1 & supervisor != 1]    <- 5  #### 5: employee
    status[unemployed    == 1]                                         <- 6  #### 5: unemployed

    ## Compute ESeC based on status categories
    ## ---------------------------------------
    ## ESeC status categories:
    ## 1  Large employers, higher grade professional, administrative and managerial occupations                                        /Higher salariat
    ## 2  Lower grade professionals, administrative and managerial occupations and higher grade technician and supervisory occupations /Lower salariat
    ## 3  Intermediate occupations                                                                                                     /Higher grade white collar workers
    ## 4  Small employers and self-employed occupations(non-agriculture)                                                               /Petit bourgeoisie or independents
    ## 5  Small employers and self-employed occupations (agriculture)                                                                  /Petit bourgeoisie or independents (agriculture)
    ## 6  Lower supervisors and technicians                                                                                            /Higher grade blue collar workers
    ## 7  Lower services, sales and clerical occupations                                                                               /Lower grade white collar workers
    ## 8  Lower technical occupations                                                                                                  /Skilled workers
    ## 9  Routine occupations                                                                                                          /semi- and non-skilled workers
    ## 10 Never worked and long-term unemployed                                                                                        /Unemployed

    esec = rep(NA, length(isco88))
    ## ------------------------------------------
    ## 1) Self-employed 10+ employees. Defaults to 1 
    ## ------------------------------------------
    esec[status == 1] <- 1
    esec[status == 1 & isco88 %in% c(344, 345)] <- 2
    esec[status == 1 & isco88 %in% c(011, 516)] <- 3
    esec[status == 1 & isco88 == 621] <- 5
    ## ------------------------------------------
    ## 2) Small employers <10. Defaults to 4
    ## ------------------------------------------
    esec[status == 2] <- 4
    esec[status == 2 & isco88 %in% c(010, 110, 111, 114, 200, 210, 211, 212, 213, 214, 220, 221, 222, 231, 235, 240, 241, 242)] <- 1
    esec[status == 2 & isco88 %in% c(223, 230, 232, 233, 234, 243, 244, 245, 246, 247, 310, 311, 312, 314, 320, 321, 322, 323, 334, 342, 344, 345, 348)] <- 2
    esec[status == 2 & isco88 %in% c(011, 516)] <- 3
    esec[status == 2 & isco88 %in% c(600, 610, 611, 612, 613, 614, 615, 621, 920, 921)] <- 5
    ## ------------------------------------------
    ## 3) Self-employed with no employees. Defaults to 4
    ## ------------------------------------------
    esec[status == 3] <- 4
    esec[status == 3 & isco88 %in% c(010, 110, 111, 114, 200, 210, 211, 212, 213, 214, 220, 221, 222, 231, 235, 240, 241, 242)] <- 1
    esec[status == 3 & isco88 %in% c(223, 230, 232, 233, 234, 243, 244, 245, 246, 247, 310, 311, 312, 314, 320, 321, 322, 323, 334, 342, 344, 345, 348)] <- 2
    esec[status == 3 & isco88 %in% c(011,516)] <- 3
    esec[status == 3 & isco88 %in% c(600, 610, 611, 612, 613, 614, 615, 621, 920, 921)] <- 5
    ## ------------------------------------------
    ## 4) Supervisors. Defaults to 6
    ## ------------------------------------------
    esec[status == 4] <- 6
    esec[status == 4 & isco88 %in% c(010, 100, 110, 111, 114, 120, 121, 123, 200, 210, 211, 212, 213, 214, 220, 221, 222, 231, 235, 240, 241, 242)] <- 1
    esec[status == 4 & isco88 %in% c(011, 122, 130,131, 223, 230, 232, 233, 234, 243, 244, 245, 246, 247, 300, 310, 311, 312, 313, 314, 320, 321, 322, 323, 330, 331, 332, 333, 334, 340,341, 342, 343, 
                                                    344, 345, 346, 347, 348, 400, 410, 411, 412, 419, 420, 521)] <- 2
    esec[status == 4 & isco88 == 621] <- 5
    ## ------------------------------------------
    ## 5) Employees
    ## ------------------------------------------
    esec[status == 5 & isco88 %in% c(010, 100,110,111,114,120, 121, 123, 200, 210, 211, 212, 213, 214, 220, 221, 222, 231, 235, 240, 241, 242)] <- 1
    esec[status == 5 & isco88 %in% c(122,130,131, 223, 230, 232, 233, 234, 243, 244, 245, 246, 247, 310, 311, 312, 314, 320, 321, 322, 323, 334, 342, 344, 345, 348, 521)] <- 2
    esec[status == 5 & isco88 %in% c(011, 300, 330, 331, 332, 333, 340, 341, 343, 346, 347, 400, 410, 411, 412, 419, 420)] <- 3
    esec[status == 5 & isco88 == 621] <- 5
    esec[status == 5 & isco88 %in% c(313, 315, 730,731)] <- 6
    esec[status == 5 & isco88 %in% c(413, 421, 422, 500, 510, 511, 513, 514, 516, 520, 522, 911)] <- 7
    esec[status == 5 & isco88 %in% c(600, 610, 611, 612, 613, 614, 615, 700, 710, 711, 712, 713, 714, 720, 721, 722, 723, 724, 732, 733, 734, 740, 741, 742, 743, 744, 825, 831, 834)] <- 8
    esec[status == 5 & isco88 %in% c(414, 512, 800, 810, 811,  812, 813, 814, 815, 816, 817, 820, 821, 822, 823, 824, 826, 827, 828, 829, 830, 832, 833,900, 910, 912, 913, 914, 915, 916, 920, 921, 930, 931, 932, 933)] <- 9
    ## ------------------------------------------
    ## 6) Unemployed
    ## ------------------------------------------
    esec[status == 6] <- 10

    ## Harrison & Rose, 2006, "(ESeC) User Guide", pg. 9-10
    ## ----------------------------------------------------
    ## The 10 class model may be collapsed to 6, 5 or 3 classes. I keep one more for unemployed
    ##
    ## - 6-class: 
    ##  - classes 1 and 2 = class 1 "the salariat"
    ##  - classes 3 and 6 = "intermediate employee" class 2
    ##  - classes 4 and 5 =  class 3 "small employers and self-employed"
    ##  - class 7 becomes class 4;
    ##  - class 8 becomes class 5;
    ##  - class 9 becomes class 6.
    ##
    ## - 5-class model:
    ##  - classes 5 and 6 in the sidata class model: 
    ##      "lower technical and routine occupations".
    ##
    ## - the three class model:
    ##  - classes 1 and 2=salariat;
    ##  - 3, 4, 5 and 6=intermediate;
    ##  - 7, 8 and 9=working class.
    ##
    ## - Class 10 may be added as an additional in any of the models, 
    ##      if desired.;
    ## Collapse 7 class
    if (n.classes == 7) {
        esec[esec %in% 1:2] <- 1
        esec[esec %in% c(3,6)] <- 2
        esec[esec %in% c(4,5)] <- 3
        esec[esec == 7] <- 4
        esec[esec == 8] <- 5
        esec[esec == 9] <- 6
        esec[esec == 10] <- 7
        if (!keep.unemployed) {
            esec[esec == 7] <- NA
        }
    }  
    ## Collapse 6 class
    if (n.classes == 6) {
        esec[esec %in% 1:2] <- 1
        esec[esec %in% c(3,6)] <- 2
        esec[esec %in% c(4,5)] <- 3
        esec[esec == 7] <- 4
        esec[esec %in% 8:9] <- 5
        esec[esec %in% 10] <- 6
        if (!keep.unemployed) {
            esec[esec == 6] <- NA
        }
    }  
    ## Collapse 4 class
    if (n.classes == 4) {
        esec[esec %in% 1:2] <- 1
        esec[esec %in% 3:6] <- 2
        esec[esec %in% 7:9] <- 3
        esec[esec == 10] <- 4
        if (!keep.unemployed) {
            esec[esec == 4] <- NA
        }
    }

    esec = set.esec.labels(esec, n.classes=n.classes)
    return(esec)
}
set.esec.labels <- function(esec, n.classes=NULL) 
{
    if (n.classes == 10) {
        labels = factor(esec, levels=1:10, labels = c(
                                               'Higher salariat',                   # 1 
                                               'Lower salariat',                    # 2 
                                               'Higher grade white collar workers', # 3
                                               'Petit bourgeoisie',                 # 4
                                               'Petit bourgeoisie (agriculture)',   # 5
                                               'Higher grade blue collar workers',  # 6 
                                               'Lower grade white collar workers',  # 7
                                               'Skilled workers',                   # 8
                                               'Semi- and non-skilled workers',     # 9
                                               'Unemployed'                         # 10
                                           )) %>% droplevels()
    }
    if (n.classes == 7) {
        labels = factor(esec, levels=1:7, labels = c(
                                              'Salariat',                         # 1
                                              'Intermediate employee',            # 2 
                                              'Small/self employers',             # 3 
                                              'Lower grade white collar workers', # 4
                                              'Skilled workers',                  # 5
                                              'Semi- and non-skilled workers'  ,  # 6
                                               'Unemployed'                       # 10
                                         )) %>% droplevels()
    }
    if (n.classes == 6) {
        labels = factor(esec, levels=1:6, labels = c(
                                              'Salariat',                           # 1
                                              'Intermediate employee',              # 2 
                                              'Small/self employers',               # 3 
                                              'Lower grade white collar workers',   # 4
                                              'Skilled and semi- and non-skilled workers',# 5
                                               'Unemployed'                         # 6
                                         )) %>% droplevels()
    }
    if (n.classes == 4) {
        labels = factor(esec, levels=1:4, labels = c('Salariat', 'Intermediate employee/small employers', 'Working class', 'Unemployed')) %>% droplevels()
    }
    return(labels)

}
