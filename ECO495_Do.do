****** Introduction *********************************************
****************************************************************
import excel "C:\Users\User\Documents\College\ISU\ECO492_Capstone\DebtGrowthRegions.xlsx", sheet("WEO_Data (1)") firstrow case(lower) clear
destring gdpgrowthrate- externaldebtofgdp, replace force
label variable countrygroupname "Region"
rename countrygroupname region
egen id=group(region)

xtline externaldebtofgdp, i( region) t( year) scale(.75) byopts(iscale(*.75))
xtline gdpgrowthrate , i( region) t( year) scale(.75) byopts(iscale(*.75))

xtline externaldebtofgdp, overlay i( region) t( year) scale(.75) 
xtline gdpgrowthrate , overlay i( region) t( year) scale(.75) 

tsset id year
twoway(tsline externaldebtofgdp if region=="Latin America and the Caribbean")
twoway(tsline gdpgrowthrate if region=="Latin America and the Caribbean")

******** Rest of Paper *****************************************
****************************************************************
import excel "C:\Users\User\Documents\College\ISU\ECO492_Capstone\Capstone_Data_Rauzi.xlsx", sheet("Capstone_Data_Rauzi") firstrow case(lower) clear
destring year-debtgdp, replace force
gen lrgdppc=log(rgdppc)
gen ldebtgdp=log(debtgdp)
gen lcap=log(grosscapitalformationofgd)
gen limport=log(importsofgoodsandserv)
gen lrev=log(revenueexcludinggrantsof)

gen caribbean=0
replace caribbean=1 if country=="Antigua and Barbuda" |country=="British Virgin Islands" |country=="Barbados" | country=="Cuba" |country=="Dominica"| country=="Dominican Republic" | country=="Grenada"    
replace caribbean=1 if country=="Haiti" |country=="Jamaica" |country=="Puerto Rico" | country=="Trinidad and Tobago" |country=="Turks and Caicos Islands" | country=="St. Lucia" | country=="St. Vincent and the Grenadines"

gen dropvalue=0
replace dropvalue=1 if country=="British Virgin Islands" | country=="Cuba" | country=="Suriname" | country=="Turks and Caicos Islands"  |country=="" |country=="Puerto Rico"

gen restrict=0
replace restrict=1 if country=="Trinidad and Tobago" |country=="Grenada" |country=="Dominica" |country=="Antigua and Barbuda" |country=="St. Vincent and the Grenadines"

drop if year> 2015
drop if year<1982

label variable rgdppc "Real GDP Per Capita"
label variable importsofgoodsandservices "Imports (% of GDP)"
label variable debtgdp "Debt (% of GDP)"

save main, replace

************** summary statitics ***************************************
sort country
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count mean sd) by(country)

***** Caribbean Island Country summary statistics

xtline rgdppc if caribbean==1 & dropvalue==0, overlay i(country) t(year) scale(.75)
xtline rgdppc if caribbean==1 & dropvalue==1, overlay i(country) t(year) scale(.75)

xtline debtgdp if caribbean==1 & dropvalue==0, overlay i(country) t(year) scale(.75)
xtline debtgdp if caribbean==1 & dropvalue==1, overlay i(country) t(year) scale(.75)

xtline grosscapitalformationofgd if caribbean==1 & dropvalue==0, overlay i(country) t(year) scale(.75)
xtline grosscapitalformationofgd if caribbean==1 & dropvalue==1, overlay i(country) t(year) scale(.75)

xtline importsofgoodsandserv if caribbean==1 & dropvalue==0, overlay i(country) t(year) scale(.75)
xtline importsofgoodsandserv if caribbean==1 & dropvalue==1, overlay i(country) t(year) scale(.75)

sort dropvalue 
tabstat rgdppc debtgdp grosscapitalformationofgd importsofgoodsandserv if caribbean==1, s(count, mean, median, sd) by(dropvalue) 

*Two sample t-test for equality of means
ttest rgdppc if caribbean==1, by(dropvalue) unequal
ttest debtgdp if caribbean==1, by(dropvalue) unequal
ttest grosscapitalformationofgd if caribbean==1, by(dropvalue) unequal
ttest importsofgoodsandserv if caribbean==1, by(dropvalue) unequal

*ranksum test for equality of medians
ranksum rgdppc if caribbean==1, by(dropvalue)
ranksum grosscapitalformationofgd if caribbean==1, by(dropvalue)
ranksum importsofgoodsandserv if caribbean==1, by(dropvalue)

***** Non-Caribbean Island Country summary statistics
sort(country,year)

xtline rgdppc if caribbean==0 & dropvalue==0, overlay i(country) t(year) scale(.75)
xtline rgdppc if caribbean==0 & dropvalue==1, overlay i(country) t(year) scale(.75)

xtline debtgdp if caribbean==0 & dropvalue==0, overlay i(country) t(year) scale(.75)
xtline debtgdp if caribbean==0 & dropvalue==1, overlay i(country) t(year) scale(.75)

xtline grosscapitalformationofgd if caribbean==0 & dropvalue==0, overlay i(country) t(year) scale(.75)
xtline grosscapitalformationofgd if caribbean==0 & dropvalue==1, overlay i(country) t(year) scale(.75)

xtline importsofgoodsandserv if caribbean==0 & dropvalue==0, overlay i(country) t(year) scale(.75)
xtline importsofgoodsandserv if caribbean==0 & dropvalue==1, overlay i(country) t(year) scale(.75)

sort dropvalue caribbean
tabstat rgdppc debtgdp grosscapitalformationofgd importsofgoodsandserv if caribbean==0, s(count, mean, median, sd) by(dropvalue) 

*Two sample t-test for equality of means
ttest rgdppc if caribbean==0, by(dropvalue) unequal
ttest debtgdp if caribbean==0, by(dropvalue) unequal
ttest grosscapitalformationofgd if caribbean==0, by(dropvalue) unequal
ttest importsofgoodsandserv if caribbean==0, by(dropvalue) unequal

*ranksum test for equality of medians
ranksum rgdppc if caribbean==0, by(dropvalue)
ranksum debtgdp if caribbean==0, by(dropvalue)
ranksum grosscapitalformationofgd if caribbean==0, by(dropvalue)
ranksum importsofgoodsandserv if caribbean==0, by(dropvalue)


********************* Antigua and Barbuda *******************************************
************************************************************************************
************************************************************************************
use main.dta, clear
keep if country=="Antigua and Barbuda"
drop if ldebtgdp==.
save antigua, replace
tsset year, yearly

*********** summary statistics *****************************
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

*********** log gdp unit root tests
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(1)

varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** log debt gdp unit root test
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(0)

varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)


*********** log imports unit root tests
varsoc limport
dfuller limport, noconstant lags(0)

*first difference
varsoc D.limport
dfuller D.limport, noconstant lags(1)

************* Lag selection criteria
varsoc lrgdppc ldebtgdp limport

*** testing for autocorrelation
var lrgdppc ldebtgdp limport, lags(1/2)
varlmar

******************* Todo Yammotto method ********************
var lrgdppc ldebtgdp limport, lags(1/3)
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp)
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc)

***************** testing for cointegration rank ************************
vecrank lrgdppc ldebtgdp limport, lags(2)


****************** Argentina *********************************************************
*****************************************************************************
**********************************************************************
use main.dta, clear
keep if id==2
save argen, replace
tsset year, yearly

********* summary statistics *********************
forvalues t=1982/1989{
	replace lrev=2.3406069*(1-((3.097796/2.3406069)^(1/26)-1))^(1990-`t') if year==`t'
	}

drop if ldebtgdp==.
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)

** first difference
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(0)
** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log gross capital formation
** levels
varsoc lcap
dfuller lcap, noconstant lags(1)

**first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log imports
** levels
varsoc limport
dfuller limport, noconstant lags(0)

**first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)

**first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)


******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport, lags(1/3)
varlmar

*********** estimating VAR
var lrgdppc ldebtgdp lcap limport lrev, lags(1/4) 

******** Testing for Granger Causality
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp )
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc )

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
irf create argen, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(argen)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(argen) 

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(argen)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(argen) 

************** test for cointegration
drop if ldebtgdp==.
vecrank lrgdppc ldebtgdp lcap limport, lags(4)

**********************************************************************************
***************************Barbados**********************************************
*********************************************************************************
use main.dta, clear
keep if country=="Barbados"
save barbados, replace
tsset year, yearly

forvalues t=1982/2002{
	replace lrev=3.3237178*(1-((3.2829683/3.3237178)^(1/13)-1))^(2003-`t') if year==`t'
	}

********* summary statistics ************************
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)

** first difference
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(2)
** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log gross capital formation
** levels
var lcap, lags(1/8)
varsoc
dfuller lcap, noconstant lags(0)

**first difference
gen dlcap=D.lcap
var dlcap, lags(1/8)
varsoc dlcap
dfuller dlcap, noconstant lags(0)

********** Testing order of integration for log imports
** levels
var limport, lags(1/8)
varsoc
dfuller limport, noconstant lags(2)

**first difference
gen dlimport=D.limport
var dlimport, lags(1/8)
varsoc dlimport
dfuller dlimport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(3)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
varlmar

*********** estimating VAR
var lrgdppc ldebtgdp lcap limport lrev, lags(1/4) 

******** Testing for Granger Causality
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp  )
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc)

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3) 
irf create barbados, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(barbados)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(barbados)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(barbados)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(barbados)

********** Cointegration test
vecrank lrgdppc ldebtgdp lcap limport, lags(5)

*********** estimating VECM
vec lrgdppc ldebtgdp lcap limport, lags(5) rank(3)
vecnorm
veclmar

vec ldebtgdp lrgdppc lcap limport, lags(5) rank(3)

**********************************************************************************
***************************Belize**********************************************
*********************************************************************************
use main.dta, clear
keep if country=="Belize"
save belize, replace
tsset year, yearly

******** constant growth rate extrapolation

replace limport=3.9371829*(1+((3.9687672/3.9371829)^(1/2)-1))^(1) if year==1991

********* summary statistics ********************************
forvalues t=1982/1989{
	replace lrev=3.236692*(1-((3.3407977/3.236692)^(1/24)-1))^(1990-`t') if year==`t'
	}

forvalues t=1970/1979{
	replace lcap=3.1490865*(1-((3.0200918/3.1490865)^(1/26)-1))^(1980-`t') if year==`t'
	}

forvalues t=1970/1979{
	replace limport=4.0498967*(1-((4.1886916/4.0498967)^(1/26)-1))^(1980-`t') if year==`t'
	}

forvalues t=1970/1981{
	replace lrev=3.2052789*(1-((3.0643811/3.2052789)^(1/25)-1))^(1982-`t') if year==`t'
	}

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
var lrgdppc , lags(1/8)
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)

** first difference
gen dlrgdppc=D.lrgdppc
var dlrgdppc, lags(1/8)
varsoc dlrgdppc
dfuller dlrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
var ldebtgdp, lags(1/8)
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(2)
** first difference
gen dldebtgdp=D.ldebtgdp
var dldebtgdp, lags(1/8)
varsoc dldebtgdp
dfuller dldebtgdp, noconstant lags(0)

********** Testing order of integration for log gross capital formation
** levels
var lcap, lags(1/8)
varsoc
dfuller lcap, noconstant lags(0)

**first difference
gen dlcap=D.lcap
var dlcap, lags(1/8)
varsoc dlcap
dfuller dlcap, noconstant lags(0)

********** Testing order of integration for log imports
** levels
var limport, lags(1/8)
varsoc
dfuller limport, noconstant lags(0)

**first difference
gen dlimport=D.limport
var dlimport, lags(1/8)
varsoc dlimport
dfuller dlimport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)

**first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)


******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
varlmar

*********** estimating VAR
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3) 

******** Testing for Granger Causality
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp )
* growth causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc )

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2) 
irf create belize, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(belize)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(belize)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(belize)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(belize)


************************** Bolivia ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if id==5
save bolivia, replace
tsset year, yearly

forvalues t=1982/1984{
	replace lrev=2.28503752*(1-((3.1469762/2.2850375)^(1/23)-1))^(1985-`t') if year==`t'
	}

forvalues t=2008/2015{
	replace lrev=3.1469762*(1+((3.1469762/2.2850375)^(1/23)-1))^(`t'-2007) if year==`t'
	}

forvalues t=1970/1981{
	replace lrev=2.1903172*(1+((2.93363/2.1903172)^(1/22)-1))^(1982-`t') if year==`t'
	}

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
var lrgdppc, lags(1/8)
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(3)
** first difference 
gen dlrgdppc=D.lrgdppc
var dlrgdppc, lags(1/8)
varsoc dlrgdppc
dfuller dlrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
var ldebtgdp, lags(1/8)
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(0)

********** Testing order of integration for log capital
** levels
var lcap, lags(1/8)
varsoc lcap
dfuller lcap, noconstant lags(0)
** first difference 
gen dlcap=D.lcap
var dlcap, lags(1/8)
varsoc dlcap
dfuller dlcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
var limport, lags(1/8)
varsoc limport
dfuller limport, noconstant lags(0)
** first difference 
gen dlimport=D.limport
var dlimport, lags(1/8)
varsoc dlimport
dfuller dlimport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)
** first difference 
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3) 
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp )
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc )

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport, lags(1/5) 
irf create bolivia, set(myirf) order(lrgdppc ldebtgdp lcap limport) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(bolivia) 
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(bolivia)

************************** Brazil ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Brazil"
save brazil, replace
tsset year, yearly

forvalues t=1995/1996{
	replace lrev=3.0683451*(1-((3.0683451/3.2713675)^(1/3)-1))^(1997-`t') if year==`t'
	}
forvalues t=1970/1979{
	replace lrev=3.1184657*(1-((3.0300069/3.1184657)^(1/24)-1))^(1980-`t') if year==`t'
	}


tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)
********** Testing order of integration for log rgdppc
** levels
var lrgdppc, lags(1/8)
varsoc
dfuller lrgdppc, noconstant lags(0)

** first difference
var D.lrgdppc, lags(1/8)
varsoc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
var ldebtgdp, lags(1/8)
varsoc
dfuller ldebtgdp, noconstant lags(3)
** first difference
var D.ldebtgdp, lags(1/8)
varsoc
dfuller D.ldebtgdp, noconstant lags(7)

********** Testing order of integration for log capital
** levels
var lcap, lags(1/8)
varsoc lcap
dfuller lcap, noconstant lags(1)
** first difference 
var D.lcap, lags(1/8)
varsoc 
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
var limport, lags(1/8)
varsoc 
dfuller limport, noconstant lags(0)
** first difference 
var D.limport, lags(1/8)
varsoc 
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev 
dfuller lrev, noconstant lags(1)
** first difference 
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/4) 
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc)

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3) 
irf create brazil, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(brazil)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(brazil)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(brazil)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(brazil)


************************** Chile ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Chile"
save chile, replace
tsset year, yearly

**************** summary statistics *************************
drop if ldebtgdp==.
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
var lrgdppc, lags(1/8)
varsoc
dfuller lrgdppc, noconstant lags(1)

** first difference
var D.lrgdppc, lags(1/8)
varsoc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
var ldebtgdp, lags(1/8)
varsoc
dfuller ldebtgdp, noconstant lags(7)
** first difference
var D.ldebtgdp, lags(1/8)
varsoc
dfuller D.ldebtgdp, noconstant lags(7)

********** Testing order of integration for log capital
** levels
var lcap, lags(1/8)
varsoc lcap
dfuller lcap, noconstant lags(0)
** first difference 
var D.lcap, lags(1/8)
varsoc 
dfuller D.lcap, noconstant lags(1)

********** Testing order of integration for log import
** levels
var limport, lags(1/8)
varsoc limport
dfuller limport, noconstant lags(2)
** first difference 
var D.limport, lags(1/8)
varsoc 
dfuller D.limport, noconstant lags(1)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)
** first difference 
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp )
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc )

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
irf create chile, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(chile)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(chile)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(chile)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(chile)

************************** Colombia ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Colombia"
save chile, replace
tsset year, yearly

************* summary statistics ********************
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

forvalues t=1982/1997{
	replace lrev=2.9006376*(1-((2.7404754/2.9006376)^(1/3)-1))^(1998-`t') if year==`t'
	}

forvalues t=2001/2002{
	replace lrev=3.12842581*(1-((3.1284258/2.7404754)^(1/3)-1))^(2003-`t') if year==`t'
	}

forvalues t=2004/2007{
	replace lrev=3.088522*(1-((3.088522/3.1284258)^(1/8)-1))^(2008-`t') if year==`t'
	}

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

drop if ldebtgdp==.
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
var lrgdppc, lags(1/8)
varsoc
dfuller lrgdppc, noconstant lags(0)

** first difference
var D.lrgdppc, lags(1/8)
varsoc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
var ldebtgdp, lags(1/8)
varsoc
dfuller ldebtgdp, noconstant lags(1)
** first difference
var D.ldebtgdp, lags(1/8)
varsoc
dfuller D.ldebtgdp, noconstant lags(7)

********** Testing order of integration for log capital
** levels
var lcap, lags(1/8)
varsoc lcap
dfuller lcap, noconstant lags(0)
** first difference 
var D.lcap, lags(1/8)
varsoc 
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
var limport, lags(1/8)
varsoc limport
dfuller limport, noconstant lags(0)
** first difference 
var D.limport, lags(1/8)
varsoc 
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log import
** levels
varsoc lrev
dfuller lrev, noconstant lags(3)
** first difference 
varsoc D.lrev
dfuller D.lrev, noconstant lags(3)
** second difference 
varsoc D2.lrev
dfuller D2.lrev, noconstant lags(2)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev 
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/4)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp )
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc )

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport, lags(1/3)
irf create colombia, set(myirf) order(lrgdppc ldebtgdp lcap limport) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(colombia)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(colombia)

************************** Costa Rica ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if id==10
save costaRica, replace
tsset year, yearly

*********** summary statistics ************************************
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)
drop if ldebtgdp==.
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
var lrgdppc, lags(1/8)
varsoc
dfuller lrgdppc, noconstant lags(0)
** first difference 
var D.lrgdppc, lags(1/8)
varsoc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
var ldebtgdp, lags(1/8)
varsoc
dfuller ldebtgdp, noconstant lags(7)
** first difference
var D.ldebtgdp, lags(1/8)
varsoc
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log capital
** levels
var lcap, lags(1/8)
varsoc lcap
dfuller lcap, noconstant lags(0)
** first difference 
var D.lcap, lags(1/8)
varsoc 
dfuller D.lcap, noconstant lags(1)

********** Testing order of integration for log import
** levels
var limport, lags(1/8)
varsoc limport
dfuller limport, noconstant lags(0)
** first difference 
var D.limport, lags(1/8)
varsoc
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)
** first difference 
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3) 
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/4) 
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp )
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc )

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
irf create costaRica, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(costaRica)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(costaRica)

************************** Dominica ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Dominica"
save dominica, replace
tsset year, yearly

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
var lrgdppc, lags(1/8)
varsoc
dfuller lrgdppc, noconstant lags(0)
** first difference 
var D.lrgdppc, lags(1/8)
varsoc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
var ldebtgdp, lags(1/8)
varsoc
dfuller ldebtgdp, noconstant lags(1)
** first difference
var D.ldebtgdp, lags(1/8)
varsoc
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log import
** levels
var limport, lags(1/8)
varsoc limport
dfuller limport, noconstant lags(3)
** first difference 
var D.limport, lags(1/8)
varsoc
dfuller D.limport, noconstant lags(6)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp limport 
*********** Testing for seriel correlation
var lrgdppc ldebtgdp limport, lags(1/2) 
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp limport, lags(1/3) 
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc)

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp limport, lags(6) 
irf create dominica, set(myirf) order(lrgdppc ldebtgdp limport) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(dominica)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(dominica)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(dominica)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(dominica)

************************** Dominican Republic ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Dominican Republic"
save dominicanRepublic, replace
tsset year, yearly

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
var lrgdppc, lags(1/8)
varsoc
dfuller lrgdppc, noconstant lags(0)
** first difference 
var D.lrgdppc, lags(1/8)
varsoc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
var ldebtgdp, lags(1/8)
varsoc
dfuller ldebtgdp, noconstant lags(0)

** first difference
var D.ldebtgdp, lags(1/8)
varsoc
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log capital
** levels
var lcap, lags(1/8)
varsoc
dfuller lcap, noconstant lags(0)

** first difference
var D.lcap, lags(1/8)
varsoc
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
var limport, lags(1/8)
varsoc
dfuller limport, noconstant lags(0)

** first difference
var D.limport, lags(1/8)
varsoc
dfuller D.limport, noconstant lags(3)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)

** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp )
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc)

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3) 
irf create dominicanR, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(dominicanR)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(dominicanR)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(dominicanR)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(dominicanR)

************************** Ecuador ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Ecuador"
save ecuador, replace
tsset year, yearly

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
var lrgdppc, lags(1/8)
varsoc
dfuller lrgdppc, noconstant lags(2)

* first difference
var D.lrgdppc, lags(1/8)
varsoc
dfuller D.lrgdppc, noconstant lags(1)

********** Testing order of integration for log debt gdp
** levels
var ldebtgdp, lags(1/8)
varsoc
dfuller ldebtgdp, noconstant lags(1)

** first difference
var D.ldebtgdp, lags(1/8)
varsoc
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log cap
** levels
var lcap, lags(1/8)
varsoc
dfuller lcap, noconstant lags(0)

** first difference
var D.lcap, lags(1/8)
varsoc
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
var limport, lags(1/8)
varsoc
dfuller limport, noconstant lags(0)

** first difference
var D.limport, lags(1/8)
varsoc
dfuller D.limport, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport, lags(1/3)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport, lags(1/4)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc)

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport, lags(1/3)
irf create Ecuador, set(myirf) order(lrgdppc ldebtgdp lcap limport) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(Ecuador)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(Ecuador)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(Ecuador)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(Ecuador)

************************** El Salvador ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="El Salvador"
save salvador, replace
tsset year, yearly

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

forvalues t=1982/1997{
	replace lrev=2.6679642*(1-((3.0356863/2.6679642)^(1/18)-1))^(1998-`t') if year==`t'
	}
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
var lrgdppc, lags(1/8)
varsoc
dfuller lrgdppc, noconstant lags(6)

* first difference
var D.lrgdppc, lags(1/8)
varsoc
dfuller D.lrgdppc, noconstant lags(5)

* second difference
var D2.lrgdppc, lags(1/8)
varsoc
dfuller D2.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
var ldebtgdp, lags(1/8)
varsoc
dfuller ldebtgdp, noconstant lags(7)

** first difference
var D.ldebtgdp, lags(1/8)
varsoc
dfuller D.ldebtgdp, noconstant lags(2)

********** Testing order of integration for log cap
** levels
var lcap, lags(1/8)
varsoc
dfuller lcap, noconstant lags(0)

** first difference
var D.lcap, lags(1/8)
varsoc
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
var limport, lags(1/8)
varsoc
dfuller limport, noconstant lags(2)

** first difference
var D.limport, lags(1/8)
varsoc
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(2)

** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(1)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/4)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp )
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc )

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
irf create ElSalvador, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(ElSalvador)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(ElSalvador)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(ElSalvador)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(ElSalvador)

************************** Grenada ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Grenada"
save grenada, replace
tsset year, yearly

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
var lrgdppc, lags(1/8)
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)

* first difference
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(3)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(2)

** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller limport, noconstant lags(1)

** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp limport
*********** Testing for seriel correlation
var lrgdppc ldebtgdp limport, lags(1)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp limport, lags(1/2)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc)

************************** Guatemala ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Guatemala"
save guatemala, replace
tsset year, yearly

forvalues t=1982/1989{
	replace lrev=2.0642562*(1-((2.3800955/2.0642562)^(1/26)-1))^(1990-`t') if year==`t'
	}

********* summary statistics **********************************
drop if ldebtgdp==.
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(3)
** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(2)

********** Testing order of integration for log capital
** levels
varsoc lcap
dfuller lcap, noconstant lags(0)
** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller limport, noconstant lags(0)
** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(2)
** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(1)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev

*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/4)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc)

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
irf create Guatemala, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(Guatemala)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(Guatemala)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(Guatemala)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(Guatemala)


************************** Guyana ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Guyana"
save guyana, replace
tsset year, yearly

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(0)

********** Testing order of integration for log capital
** levels
varsoc lcap
dfuller lcap, noconstant lags(0)
** first difference 
varsoc D.lcap
dfuller D.lcap, noconstant lags(3)

********** Testing order of integration for log imports
** levels
varsoc limport
dfuller limport, noconstant lags(0)
** first difference 
varsoc D.limport
dfuller D.limport, noconstant lags(0)


******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport, lags(1)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport, lags(1/2)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc)

****************** Impulse Response Function
var lrgdppc ldebtgdp lcap limport, lags(1)
irf create guyana, set(myirf) order(lrgdppc ldebtgdp lcap limport) replace

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(guyana)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(guyana)

************************** Haiti ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Haiti"
save haiti, replace
tsset year, yearly

******* constant growth rate extrapolation
forvalues t=1982/1987{
	replace lcap=2.8713419*(1-((3.4781392/2.8713419)^(1/27)-1))^(1988-`t') if year==`t'
	replace limport=3.4805083*(1-((3.9228506/3.4805083)^(1/27)-1))^(1988-`t') if year==`t'
	}

************ summary statistics ******************************
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(1)

*first difference
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(1)

** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(1)

************* Testing order of integration for log capital
** levels
varsoc lcap
dfuller lcap, noconstant lags(2)

** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(1)

************* Testing order of integration for log imports
** levels
varsoc limport
dfuller limport, noconstant lags(1)

** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport, lags(1/4)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport, lags(1/5)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp L4.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc L4.lrgdppc)

************ impulse response function
var lrgdppc ldebtgdp lcap limport, lags(1/4)
irf create haiti, set(myirf) order(lrgdppc ldebtgdp lcap limport) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(haiti)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(haiti)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(haiti)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(haiti)


************************** Honduras ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Honduras"
save honduras, replace
tsset year, yearly

forvalues t=1982/2002{
	replace lrev=2.9935045*(1-((3.1511602/2.9935045)^(1/13)-1))^(2003-`t') if year==`t'
	}

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(3)

*first difference
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(1)

** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log capital
** levels
varsoc lcap
dfuller lcap, noconstant lags(2)

** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(1)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller limport, noconstant lags(0)

** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)

** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc)

************************** Jamaica ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Jamaica"
save jamaica, replace
tsset year, yearly

forvalues t=1982/1987{
	replace lrev=3.3747678*(1-((3.3130119/3.3747678)^(1/28)-1))^(1988-`t') if year==`t'
	}

**************** summary statistics *******************************
drop if ldebtgdp==.
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(3)

** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log capital
** levels
varsoc lcap
dfuller lcap, noconstant lags(0)

** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller import, noconstant lags(0)

** first difference
varsoc D.import
dfuller D.import, noconstant lags(0)

********** Testing order of integration for log rev
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)
** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp )
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc )

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
irf create Jamaica, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(Jamaica)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(Jamaica)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(Jamaica)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(Jamaica)

************************** Mexico ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Mexico"
save mexico, replace
tsset year, yearly

forvalues t=2001/2007{
	replace lrev=3.0275371*(1-((3.0275371/2.49405)^(1/27)-1))^(2008-`t') if year==`t'
	}

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)

** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(0)

** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log cap
** levels
varsoc lcap
dfuller lcap, noconstant lags(0)

** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller limport, noconstant lags(0)

** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)

** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/4)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc)

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
irf create Mexico, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(Mexico)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(Mexico)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(Mexico)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(Mexico)

************************** Nicaragua ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Nicaragua"
save nicaragua, replace
tsset year, yearly

forvalues t=1982/1989{
	replace lrev=3.4005649*(1-((2.8170879/3.4005649)^(1/26)-1))^(1990-`t') if year==`t'
	}

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(1)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(1)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(1)
** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log capital
** levels
varsoc lcap
dfuller lcap, noconstant lags(2)
** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(1)

********** Testing order of integration for log import
varsoc limport
dfuller limport, noconstant lags(0)
** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log revenue
varsoc lrev
dfuller lrev, noconstant lags(0)
** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp )
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc )

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1)
irf create Nicaragua, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(Nicaragua)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(Nicaragua)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(Nicaragua)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(Nicaragua)

************************** Panama ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Panama"
save panama, replace
tsset year, yearly

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(2)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(1)

** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log capital
** levels
varsoc lcap
dfuller lcap, noconstant lags(1)

** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(1)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller limport, noconstant lags(0)

** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport, lags(1)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport, lags(1/2)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc)

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport, lags(1)
irf create Panama, set(myirf) order(lrgdppc ldebtgdp lcap limport) replace

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(Panama)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(Panama)

************************** Paraguay ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Paraguay"
save paraguay, replace
tsset year, yearly

forvalues t=1982/2004{
	replace lrev=2.6743779*(1-((2.8724096/2.6743779)^(1/11)-1))^(2005-`t') if year==`t'
	}

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(3)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(3)

** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log capital
** levels
varsoc lcap
dfuller lcap, noconstant lags(0)

** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller limport, noconstant lags(0)

** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log rev
** levels
varsoc lrev
dfuller lrev, noconstant lags(3)

** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(3)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/4)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc)

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
irf create Paraguay, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(Paraguay)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(Paraguay)

************************** Peru ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Peru"
save peru, replace
tsset year, yearly

************** summary statistics ************************
drop if ldebtgdp==.

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)


********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(0)
** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(3)

********** Testing order of integration for log cap
** levels
varsoc lcap
dfuller lcap, noconstant lags(0)
** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller limport, noconstant lags(1)
** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)
** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(2)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/4)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp )
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc )

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
irf create Peru, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(Peru)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(Peru)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(Peru)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(Peru)

************************** Puerto Rico ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Puerto Rico"
drop if ldebtgdp==.
save peru, replace
tsset year, yearly

tabstat lrgdppc ldebtgdp lcap limport, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(2)

** second difference 
varsoc D2.lrgdppc
dfuller D2.lrgdppc, noconstant lags(1)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(2)
** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(2)

********** Testing order of integration for log cap
** levels
varsoc lcap
dfuller lcap, noconstant lags(0)
** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller limport, noconstant lags(0)
** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(2)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport, lags(1/2)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport, lags(1/3)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc)

************************** St. Lucia ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="St. Lucia"
save stlucia, replace
tsset year, yearly

forvalues t=1982/1999{
	replace lrev=3.0697148*(1-((3.0668437/3.06971489)^(1/16)-1))^(2000-`t') if year==`t'
	}

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(0)
** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log cap
** levels
varsoc lcap
dfuller lcap, noconstant lags(1)
** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller limport, noconstant lags(0)
** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)
** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(2)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/4)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc)

*************** impulse response function *****************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
irf create St_Lucia, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(St_Lucia)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(St_Lucia)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(St_Lucia)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(St_Lucia)

************************** St. Vincent and the Grenadines ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="St. Vincent and the Grenadines"
save st.vincent, replace
tsset year, yearly

******* constant growth rate extrapolation
forvalues t=1982/2012{
	replace lcap=3.3115971*(1-((3.2495925/3.311597)^(1/2)-1))^(2013-`t') if year==`t'
	}

forvalues t=1982/1989{
	replace lrev=3.0497396*(1-((3.2362087/3.0497396)^(1/26)-1))^(1990-`t') if year==`t'
	}

************* summary statistics ****************************
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(0)
** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(1)

********** Testing order of integration for log cap
** levels
varsoc lcap
dfuller lcap, noconstant lags(2)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller limport, noconstant lags(1)
** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(1)
** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc)

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/2)
irf create StVincent, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(StVincent)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(StVincent)

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(StVincent)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(StVincent)

************************** Trinidad and Tobago ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Trinidad and Tobago"
save trinidad, replace
tsset year, yearly

forvalues t=1982/2000{
	replace lrev=3.3009448*(1-((3.6482584/3.3009448)^(1/15)-1))^(2001-`t') if year==`t'
	}

*********** summary statistics *********************
drop if ldebtgdp==.
tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(3)
** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)
** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lrev 
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lrev, lags(1/4)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lrev, lags(1/5)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp L4.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc L4.lrgdppc)

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lrev, lags(1/4)
irf create Trinidad, set(myirf) order(lrgdppc ldebtgdp lrev) replace

irf graph oirf, impulse(ldebtgdp) response(lrgdppc) irf(Trinidad)
irf table oirf, impulse(ldebtgdp) response(lrgdppc) irf(Trinidad)

************************** Uruguay ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Uruguay"
save uruguay, replace
tsset year, yearly

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd)

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(1)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(2)
** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log cap
** levels
varsoc lcap
dfuller lcap, noconstant lags(0)
** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(1)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller limport, noconstant lags(0)
** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

********** Testing order of integration for log revenue
** levels
varsoc lrev
dfuller lrev, noconstant lags(0)
** first difference
varsoc D.lrev
dfuller D.lrev, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport lrev
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport lrev, lags(1/4)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp L2.ldebtgdp L3.ldebtgdp )
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc L2.lrgdppc L3.lrgdppc )

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport lrev, lags(1/3)
irf create Uruguay, set(myirf) order(lrgdppc ldebtgdp lcap limport lrev) replace

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(Uruguay)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(Uruguay)

************************** Venezuela ***********************************************
***********************************************************************************
***********************************************************************************
use main.dta, clear
keep if country=="Venezuela, RB"
drop if year<1998 |year==2015
save venezuela, replace
tsset year, yearly

tabstat lrgdppc ldebtgdp lcap limport lrev, s(count, mean, sd) 

********** Testing order of integration for log rgdppc
** levels
varsoc lrgdppc
dfuller lrgdppc, noconstant lags(0)
** first difference 
varsoc D.lrgdppc
dfuller D.lrgdppc, noconstant lags(0)

********** Testing order of integration for log debt gdp
** levels
varsoc ldebtgdp
dfuller ldebtgdp, noconstant lags(2)
** first difference
varsoc D.ldebtgdp
dfuller D.ldebtgdp, noconstant lags(0)

********** Testing order of integration for log cap
** levels
varsoc lcap
dfuller lcap, noconstant lags(3)
** first difference
varsoc D.lcap
dfuller D.lcap, noconstant lags(0)

********** Testing order of integration for log import
** levels
varsoc limport
dfuller limport, noconstant lags(0)
** first difference
varsoc D.limport
dfuller D.limport, noconstant lags(0)

******** ********estimating VAR with Toda and Yammotto method**********************
*********** choosing lag length
varsoc lrgdppc ldebtgdp lcap limport
*********** Testing for seriel correlation
var lrgdppc ldebtgdp lcap limport, lags(1)
varlmar

*********** Testing Granger Causality
var lrgdppc ldebtgdp lcap limport, lags(1/2)
*debt causes gdp
test ([lrgdppc]: L.ldebtgdp)
*gdp causes debt
test ([ldebtgdp]: L.lrgdppc)

******************** Impulse Response Function *************************************
var lrgdppc ldebtgdp lcap limport, lags(1)
irf create Venezuela, set(myirf) order(lrgdppc ldebtgdp lcap limport) replace

irf graph oirf, impulse(lrgdppc) response(ldebtgdp) irf(Venezuela)
irf table oirf, impulse(lrgdppc) response(ldebtgdp) irf(Venezuela)

********* Examing unstable relationships *********************
gen relationship="stable" if country=="Brazil" |country=="St. Lucia"
replace relationship="unstable" if country=="Guatemala" |country=="Haiti" |country=="Mexico" |country=="Nicaragua"

gen bidirectional=1 if country=="Guatemala" |country=="Haiti" |country=="Mexico" |country=="Nicaragua" |country=="Brazil" |country=="St. Lucia"

graph box rgdppc if bidirectional==1 & relationship=="unstable"& debtgdp<100, over(relationship) over(country) scale(.75)
graph box rgdppc if bidirectional==1 & relationship=="stable", over(relationship) over(country) yscale(log) scale(.75)

graph box debtgdp if bidirectional==1 & relationship=="unstable", over(relationship) over(country) scale(.75)
graph box debtgdp if bidirectional==1 & relationship=="stable", over(relationship) over(country) yscale(log) scale(.75)

graph box revenueexcludinggrantsof if bidirectional==1 & relationship=="unstable"& debtgdp<100, over(relationship) over(country) scale(.75)
graph box revenueexcludinggrantsof if bidirectional==1 & relationship=="stable", over(relationship) over(country) yscale(log) scale(.75)

graph box grosscapitalformationofgd if bidirectional==1 & relationship=="unstable"& debtgdp<100, over(relationship) over(country) scale(.75)
graph box grosscapitalformationofgd if bidirectional==1 & relationship=="stable", over(relationship) over(country) yscale(log) scale(.75)

tabstat debtgdp if relationship=="unstable", s(mean, p25, median, p75, iqr, min, max) by(country)
tabstat debtgdp if relationship=="stable", s(mean, p25, median, p75, iqr, min, max) by(country)


