/*import data*/
ods rtf file="project.rtf";

%let path = /home/yiziyingchen0/Longitudinal/;
proc import datafile="&path.master.csv" dbms=csv out=suicide replace;
informat HDI 6.;
run;



data all_suicide;
infile "&path.master.csv"
delimiter=',' missover
firstobs=2 DSD lrecl = 32767;
informat country $20.;
   informat year best12.;
   informat sex $6.;
   informat age $11. ;
   informat suicide_no best12. ;
   informat population best12.;
   informat suicide_rate best12.;
   informat contry_year $7.;
   informat HDI best12.;
   informat gdp $15.;
   informat gdp_capita best12.;
   informat generation $15.;
   format country $20.;
   format year best32.;
   format sex $6.;
   format age $11. ;
   format suicide_no best32. ;
   format population best32.;
   format suicide_rate best32.;
   format contry_year $7.;
   format HDI best32.;
   format gdp $15.;
   format gdp_capita best32.;
   format generation $15.;
input country $ 
year 
sex $ 
age $ 
suicide_no 
population 
suicide_rate 
contry_year $
HDI 
gdp
gdp_capita
generation $
;
run;

/*male = 0, female =1*/
data suicide;
set all_suicide;
if generation = 'G.I. Generation' then gen = 0;
else if generation = 'Silent' then gen = 1;
else if generation = 'Boomers' then gen = 2;
else if generation = 'Generation X' then gen = 3;
else if generation = 'Millenials' then gen = 4;
else if generation = 'Generation Z' then gen = 5;
if sex = 'male' then sex_cat = 0;
else sex_cat = 1;
if age = '5-14 years' then do age_cat = 0; new_age = 9.5;  end;
if age = '15-24 years' then do age_cat = 1; new_age = 19.5; end;
if age = '25-34 years' then do age_cat = 2; new_age = 29.5; end;
if age = '35-54 years' then do age_cat = 3; new_age = 44.5; end;
if age = '55-74 years' then do age_cat = 4; new_age = 64.5; end;
if age = '75+ years' then do age_cat = 5; new_age = 75.5; end;
ID = _n_;
age2 = new_age*new_age;
pop2 = population*population;
rate = suicide_rate/100;
if rate >0 then rate_cat = 1; else rate_cat = 0;
if country in ('United Kingdom','United States', 'Mauritius', 'Mexico', 'Japan', 'Australia') then output; 
run;

Libname out '/home/yiziyingchen0/Longitudinal/';
data out.suicide_data;
    Set suicide;
 Run;



*check US suicide data structure;
proc univariate data = suicide; RUN;
proc means data = suicide; var rate; run;

/******************************************************
************EXPLORATORY ANALYSIS;
******************************************************/
title "exploring data features";
/*DESCRIPTIVE STATISTICS: FREQUENCY TABLES*/
proc freq data=all_suicide;
tables country/out=country_count;
run;
proc sort data=country_count;
by descending count ; quit;
run;
proc print data = country_count;
run;

proc freq data=suicide;
tables rate/out=rate_range;
run;

proc freq data=suicide;
tables sex*year/norow nopercent CHISQ;
tables generation*year/norow nopercent CHISQ;
tables age*year/norow nopercent CHISQ;
tables age*sex/norow nopercent CHISQ;
tables age*generation/norow nopercent CHISQ;
run;

*average trend plot;

**(1)AGE to suicide rate by sex;
goptions reset=all;
proc gplot data=suicide;
plot rate*new_age = SEX;
symbol V=NONE I=SM50S COLOR=BLUE WIDTH=3; 
SYMBOL2 V=NONE I=SM50S COLOR=BLACK WIDTH=3;
TITLE 'GROUP AVERAGE SUICIDE TREND LINES of M AND F'; RUN; QUIT;
**(2)YEAR to suicide rate by SEX;
proc gplot data=suicide;
plot rate*YEAR = SEX;
symbol V=NONE I=SM50S COLOR=BLUE WIDTH=3; 
SYMBOL2 V=NONE I=SM50S COLOR=BLACK WIDTH=3;
TITLE 'GROUP AVERAGE SUICIDE TREND LINES of M AND F'; RUN; QUIT;

**(3)AGE to suicide rate by GENERATION;
proc gplot data=suicide;
plot rate*new_age = GEN;
symbol V=NONE I=SM50S COLOR=BLUE WIDTH=2 line=1; 
SYMBOL2 V=NONE I=SM50S COLOR=BLACK WIDTH=2 line=2;
SYMBOL3 V=NONE I=SM50S COLOR=RED WIDTH=2 line=3;
SYMBOL4 V=NONE I=SM50S COLOR=GREEN WIDTH=2 line=4;
SYMBOL5 V=NONE I=SM50S COLOR=PURPLE WIDTH=2 line=5;
SYMBOL6 V=NONE I=SM50S COLOR=YELLOW WIDTH=2 line=6;
TITLE 'GROUP AVERAGE SUICIDE TREND LINES of GENERATIONS'; RUN; QUIT;

**(4)YEAR to suicide rate by GENERATION;
proc gplot data=suicide;
plot rate*YEAR = GEN;
symbol V=NONE I=SM50S COLOR=BLUE WIDTH=2 line=1; 
SYMBOL2 V=NONE I=SM50S COLOR=BLACK WIDTH=2 line=2;
SYMBOL3 V=NONE I=SM50S COLOR=RED WIDTH=2 line=3;
SYMBOL4 V=NONE I=SM50S COLOR=GREEN WIDTH=2 line=4;
SYMBOL5 V=NONE I=SM50S COLOR=PURPLE WIDTH=2 line=5;
SYMBOL6 V=NONE I=SM50S COLOR=YELLOW WIDTH=2 line=6;
TITLE 'GROUP AVERAGE SUICIDE TREND LINES of GENERATION'; RUN; QUIT;

/******************************************************
************REPEATED MEASURE;
******************************************************/

*CONTRASTS;
proc reg data=suicide;
model rate = sex_cat gen population new_age year gdp_capita/ selection=stepwise;
run;

proc glm data=suicide;
class sex gen;
model rate = sex gen new_age year gdp_capita/solution;
random sex gen;
contrast 'Linear Trend contrast' GEN -5 -3 -1 1 3 5; 
CONTRAST 'Quadratic Trend contrast' GEN 1 -1 0 0 -1 1 ; 
CONTRAST '1st vs 2nd GENERATION point contrast' GEN 1 -1 0 0 0 0; 
CONTRAST '1st vs 3rd GENERATION point contrast' GEN 1 0 -1 0 0 0; 
CONTRAST '1st vs 4th GENERATION point contrast' GEN 1 0 0 -1 0 0; 
CONTRAST '1st vs 5th GENERATION point contrast' GEN 1 0 0 0 -1 0; 
CONTRAST '1st vs 6th GENERATION point contrast' GEN 1 0 0 0 0 -1; 
CONTRAST '2nd vs 3rd GENERATION point contrast' GEN 0 -1 1 0 0 0; 
CONTRAST '2nd vs 4th GENERATION point contrast' GEN 0 -1 0 1 0 0; 
CONTRAST '2nd vs 5th GENERATION point contrast' GEN 0 -1 0 0 1 0;
CONTRAST '2nd vs 6th GENERATION point contrast' GEN 0 1 0 0 0 -1;  
CONTRAST '3rd vs 4th GENERATION point contrast' GEN 0 0 -1 1 0; 
CONTRAST '3rd vs 5th GENERATION point contrast' GEN 0 0 1 0 -1;
CONTRAST '3rd vs 6th GENERATION point contrast' GEN 0 0 1 0 0 -1; 
CONTRAST '4th vs 5th GENERATION point contrast' GEN 0 0 0 1 -1 0;
CONTRAST '4th vs 6th GENERATION point contrast' GEN 0 0 0 1 0 -1;
CONTRAST '5th vs 6th GENERATION point contrast' GEN 0 0 0 0 1 -1; 
CONTRAST '3rd vs {1st, 2nd} GENERATION point contrast' GEN -0.5 -0.5 1 0 0 0;
CONTRAST '4th vs {1st, 2nd, 3rd} GENERATION point contrast' GEN -0.5 -0.5 -0.5 1.5 0 0;
CONTRAST '5th vs {1st, 2nd, 3rd, 4th} GENERATION point contrast' GEN -0.5 -0.5 -0.5 -0.5 2 0;
CONTRAST '6th vs {1st, 2nd, 3rd, 4th, 5th} GENERATION point contrast' GEN -0.5 -0.5 -0.5 -0.5 -0.5 2.5;
CONTRAST '{4th, 5th, 6th} vs {1st, 2nd, 3rd} GENERATION point contrast' GEN -1 -1 -1 1 1 1;
CONTRAST '1st vs {2nd, 3rd} GENERATION point contrast' GEN -1 0.5 0.5 0 0;
CONTRAST '2nd vs {1st, 3rd, 4th} GENERATION point contrast' GEN 0 3 -1 -1 -1;
CONTRAST '3rd vs {1st, 2nd, 4th} GENERATION point contrast' GEN 0 0 -1 0.5 0.5;
CONTRAST '4th vs {2nd, 3rd} GENERATION point contrast' GEN 0 0 -1 0.5 0.5;
run;quit;

/******************************************************
************FIXED and RANDOM EFFECTS in LME (*RANDON INTERCEPT only);
******************************************************/
data suicide; set suicide;
if country = 'United States' then c = 0; 
if country = 'United Kingdom' then c = 1; 
if country = 'Mauritius' then c = 2; 
if country = 'Mexico' then c = 3; 
if country = 'Japan' then c = 4; 
if country = 'Australia' then c = 5; 

if c = 0 then c1 = 1; else c1 = 0;
if c = 1 then c2 = 1; else c2 = 0;
if c = 2 then c3 = 1; else c3 = 0;
if c = 3 then c4 = 1; else c4 = 0;
if c = 4 then c5 = 1; else c5 = 0;
if c = 5 then c6 = 1; else c6 = 0;
run;


**SIMPLE Ri correlation structure;
proc mixed data=suicide;
class YEAR GEN C SEX;
model rate = c1 c2 c3 c4 c5 c6 POPULATION gdp_capita GEN SEX
C1*new_age C2*new_age C3*new_age C4*new_age C5*new_age C6*new_age 
C1*age2 C2*age2 C3*age2 C4*age2 C5*age2 C6*age2
/noint solution outp = simple_out;
repeated /type = simple subject = YEAR r rcorr;
random int/ type=UN subject= YEAR g gcorr;
RUN; *AIC =14658.0, df=2;

**EXP Ri correlation structure--nonpositive definite estimated Ri matrix;
proc mixed data=suicide;
class YEAR GEN C SEX;
model rate = c1 c2 c3 c4 c5 c6 POPULATION gdp_capita GEN SEX
C1*new_age C2*new_age C3*new_age C4*new_age C5*new_age C6*new_age 
C1*age2 C2*age2 C3*age2 C4*age2 C5*age2 C6*age2
/noint solution outp=exp_out;
repeated / type = sp(exp)(new_age) subject = YEAR r rcorr;
random int/ type=UN subject= YEAR g gcorr;
RUN;

**CS Ri correlation structure;
proc mixed data=suicide;
class YEAR GEN C SEX;
model rate = c1 c2 c3 c4 c5 c6 POPULATION gdp_capita GEN SEX
C1*new_age C2*new_age C3*new_age C4*new_age C5*new_age C6*new_age 
C1*age2 C2*age2 C3*age2 C4*age2 C5*age2 C6*age2
/noint solution outp=cs_out;
repeated / type = CS  subject = YEAR r rcorr;
random int/ type=UN subject= YEAR g gcorr;
RUN; *AIC =14654.8, df=3;

**AR(1) Ri correlation structure--inefficient, huge size of Ri covariance matrix;
proc mixed data=suicide;
class YEAR GEN C SEX;
model rate = c1 c2 c3 c4 c5 c6 POPULATION gdp_capita GEN SEX
C1*new_age C2*new_age C3*new_age C4*new_age C5*new_age C6*new_age 
C1*age2 C2*age2 C3*age2 C4*age2 C5*age2 C6*age2
/NOINT solution outp=AR_out;
repeated / type = AR(1)  subject = YEAR r rcorr;
random int/ type=UN subject= YEAR g gcorr;
RUN; *AIC = 14265.3, df = 3;

**TOEPLITZ Ri correlation structure--Unable to make Hessian positive definite;
proc mixed data=suicide;
class YEAR GEN C SEX;
model rate = c1 c2 c3 c4 c5 c6 POPULATION gdp_capita GEN SEX
C1*age_cat C2*age_cat C3*age_cat C4*age_cat C5*age_cat C6*age_cat 
C1*age2 C2*age2 C3*age2 C4*age2 C5*age2 C6*age2
/noint solution outp = TOEP_out;
repeated /type = TOEP subject = YEAR r rcorr;
random int/ type=UN subject= YEAR g gcorr;
RUN;

/*hypothesis testing using LR test: G² = -2(likelihoodreducted - log-likelihoodfull)
to test forwhich working structure provides best fit to the data.
Here, non-nested models (CS and SIMPLE), use min(AIC) model;
Note: Everything is nested within the unstructured model and compound symmetry and AR-1 are nested within Toeplitz.
*/ 


/***DIAGNOSTICS: ASSESSING NORMALITY OF THE RANDOM EFFECTS***/
proc mixed data=suicide METHOD=MIVQUE0;
class YEAR GEN C SEX;
model rate = c1 c2 c3 c4 c5 c6 POPULATION gdp_capita GEN SEX
C1*new_age C2*new_age C3*new_age C4*new_age C5*new_age C6*new_age 
C1*age2 C2*age2 C3*age2 C4*age2 C5*age2 C6*age2
/noint solution;
repeated / type = AR(1)  subject = YEAR r rcorr;
random int / type=UN subject= YEAR g gcorr S;
ODS OUTPUT SOLUTIONR=RANDOMEFFECTS;
RUN;


PROC SORT DATA=RANDOMEFFECTS; BY EFFECT;
RUN;

PROC UNIVARIATE DATA=RANDOMEFFECTS;
VAR ESTIMATE; BY EFFECT;
HISTOGRAM / NORMAL;
TITLE 'DISTRIBUTION OF RANDOM EFFECTS'; RUN;



/***DIAGNOSTICS: CHECKING FOR ANY SYSTEMATIC DEPARTURES w. RAW RESIDUALS***/
**type of scatterplot;
proc mixed data=suicide METHOD=MIVQUE0;
class YEAR GEN C SEX;
model rate = c1 c2 c3 c4 c5 c6 POPULATION GEN SEX gdp_capita 
C1*new_age C2*new_age C3*new_age C4*new_age C5*new_age C6*new_age 
C1*age2 C2*age2 C3*age2 C4*age2 C5*age2 C6*age2
/noint solution OUTPM=PREDM OUTP=PREDP;
repeated / type = AR(1)  subject = YEAR r rcorr;
random int new_age/ type=UN subject= YEAR g gcorr S;
RUN;
**(1) residuals against the predicted mean. (2) residuals against selected covariates;
GOPTIONS RESET=ALL; PROC GPLOT DATA=PREDM;
PLOT RESID*(PRED rate new_age YEAR SEX GEN gdp_capita POPULATION)/ VREF=0; SYMBOL V=STAR C=BLUE;
RUN; QUIT;

**population level data available;
GOPTIONS RESET=ALL; PROC GPLOT DATA=PREDP;
PLOT RESID*(PRED rate new_age YEAR SEX GEN gdp_capita POPULATION)/ VREF=0; SYMBOL V=STAR C=BLUE;
run;quit;


/***DIAGNOSTICS: CHECKING FOR ANY SYSTEMATIC DEPARTURES w. SCALED RESIDUALS***/
proc mixed data=suicide METHOD=MIVQUE0;
class YEAR GEN C SEX;
model rate = c1 c2 c3 c4 c5 c6 POPULATION gdp_capita SEX GEN
C1*new_age C2*new_age C3*new_age C4*new_age C5*new_age C6*new_age 
C1*age2 C2*age2 C3*age2 C4*age2 C5*age2 C6*age2
/noint solution OUTPM=PREDM OUTP=PREDP VCIRY;
repeated / type = AR(1)  subject = YEAR r rcorr;
random int new_age/ type=UN subject= YEAR g gcorr S;
RUN;

GOPTIONS RESET=ALL; PROC GPLOT DATA=PREDM;
PLOT SCALEDRESID*(PRED rate new_age YEAR gdp_capita SEX GEN POPULATION)/VREF=0; SYMBOL V=STAR C=BLUE;
TITLE 'Residual Diagnostics';
RUN; QUIT;


/***DIAGNOSTICS: ASSESSING NORMALITY OF TRANSFORMED RESIDUALS***/
PROC CAPABILITY DATA=PREDM;
QQPLOT SCALEDRESID;
TITLE 'Normal QQ Plot';
RUN;


/***LME SAMPLE SEMIVARIOGRAM PLOT RESIDUALS***/
**(1)FOR POPULATION-AVERAGE;
%INCLUDE "&path.semivariogramRandom.sas";
%semivarr(suicide, YEAR, new_age, rate, YEAR GEN C SEX, 
c1 c2 c3 c4 c5 c6 POPULATION gdp_capita SEX GEN
C1*new_age C2*new_age C3*new_age C4*new_age C5*new_age C6*new_age
C1*age2 C2*age2 C3*age2 C4*age2 C5*age2 C6*age2, CS, new_age age2, OUTPM);
**(2) FOR SUBJECT-SPECIFIC RESIDUALS;
%semivarr(suicide, YEAR, new_age, rate, YEAR GEN C SEX, 
c1 c2 c3 c4 c5 c6 POPULATION gdp_capita SEX GEN
C1*new_age C2*new_age C3*new_age C4*new_age C5*new_age C6*new_age
C1*age2 C2*age2 C3*age2 C4*age2 C5*age2 C6*age2, CS, new_age age2, OUTP);

/** test the difference in suicide rate trajectory between countries**/
proc mixed data=suicide METHOD=MIVQUE0;
class YEAR GEN C SEX;
model rate = c1 c2 c3 c4 c5 c6 POPULATION gdp_capita SEX GEN
C1*new_age C2*new_age C3*new_age C4*new_age C5*new_age C6*new_age 
C1*age2 C2*age2 C3*age2 C4*age2 C5*age2 C6*age2
/noint solution OUTPM=PREDM VCIRY;
contrast '6 DF Test of Whether HOUSEDF Trajectory Differs Between
different countries' new_age*C1 1, new_age*C2 1, new_age*C2 1, new_age*C3 1, new_age*C4 1, new_age*C5 1, new_age*C6 1/ chisq;
contrast '1 DF Test of Whether HOUSEDF Trajectory Differs Between countries' new_age*C1 1 new_age*C2 1 new_age*C3 1
new_age*C4 1 new_age*C5 1 new_age*C6 1/ chisq;

contrast '2 DF Test of Whether HOUSEDF Trajectory Differs Between
US(1) & UK(2)' new_age*C1 1 new_age*C2 -1, age2*C1 1 age2*C2 -1/ chisq;
contrast '1 DF Test of Whether HOUSEDF Trajectory Differs Between
US(1) & UK(2)' new_age*C1 1 new_age*C2 1 age2*C1 1 age2*C2 1/ chisq;

contrast '2 DF Test of Whether HOUSEDF Trajectory Differs Between
US(1) & Mauritius(3)' new_age*C1 1 new_age*C3 -1, age2*C1 1 age2*C3 -1/ chisq;
contrast '1 DF Test of Whether HOUSEDF Trajectory Differs Between
US(1) & Mauritius(3)' new_age*C1 1 new_age*C3 1 age2*C1 1 age2*C3 1/ chisq;

contrast '2 DF Test of Whether HOUSEDF Trajectory Differs Between
US(1) & Mexico(4)' new_age*C1 1 new_age*C4 -1, age2*C1 1 age2*C4 -1/ chisq;
contrast '1 DF Test of Whether HOUSEDF Trajectory Differs Between
US(1) & Mexico(4)' new_age*C1 1 new_age*C4 1 age2*C1 1 age2*C4 1/ chisq;

contrast '2 DF Test of Whether HOUSEDF Trajectory Differs Between
US(1) & Japan(5)' new_age*C1 1 new_age*C5 -1, age2*C1 1 age2*C5 -1/ chisq;
contrast '1 DF Test of Whether HOUSEDF Trajectory Differs Between
US(1) & Japan(5)' new_age*C1 1 new_age*C5 1 age2*C1 1 age2*C5 1/ chisq;

contrast '2 DF Test of Whether HOUSEDF Trajectory Differs Between
US(1) & Australia(6)' new_age*C1 1 new_age*C5 -1, age2*C1 1 age2*C5 -1/ chisq;
contrast '1 DF Test of Whether HOUSEDF Trajectory Differs Between
US(1) & Australia(6)' new_age*C1 1 new_age*C5 1 age2*C1 1 age2*C5 1/ chisq;

repeated / type = CS  subject = YEAR r rcorr;
random int new_age/ type=UN subject= YEAR g gcorr S;
RUN;

**logistic model selection;
proc logistic data=suicide covout;
class gen sex c age_cat year;
model rate_cat(event='1')=new_age gen age_cat c population year gdp_capita gdp_capita*year gdp_capita*c new_age*gen population*c
/ selection=stepwise slentry=0.1 slstay=0.1 details lackfit;
output out=pred p=phat lower=lcl upper=ucl
predprob=(individual crossvalidate);
run;

**Exporing ODDS, LOG ODDS**;
PROC SORT DATA=suicide; 
BY gen year; 
RUN;
PROC MEANS DATA=suicide;
VAR rate_cat;
BY gen year;
OUTPUT OUT=binary_means; RUN;

PROC PRINT DATA=binary_means; run;

DATA binary_ODDS; SET binary_means; 
WHERE _STAT_ = 'MEAN';
obs_prop=rate_cat;
ODDS= rate_cat/(1-rate_cat); 
LOGIT = LOG(ODDS);
RUN;

/*******************************************
***PLOT Observed LOG-ODDS versus WEEK FOR TWO GROUPS;
*******************************************/
GOPTIONS RESET=ALL;
PROC GPLOT DATA=binary_ODDS;
PLOT logit*YEAR=GEN;
SYMBOL1 V=CIRCLE I=JOIN COLOR=BLACK LINE=2;
SYMBOL2 V=DIAMOND I=JOIN COLOR=RED LINE=1;
SYMBOL3 V=CIRCLE I=JOIN COLOR=BLUE LINE=2;
SYMBOL4 V=DIAMOND I=JOIN COLOR=GRENN LINE=1;
SYMBOL5 V=CIRCLE I=JOIN COLOR=YELLOW LINE=2;
SYMBOL6 V=DIAMOND I=JOIN COLOR=PURPLE LINE=1;
TITLE 'OBSERVED GROUP Proportions OVER TIME';
RUN; 
QUIT;


ods rtf close;

/************************************/
/* LOGIT RANDOM-INTERCEPT MODEL FOR ORDINAL RESPONSES */
/************************************/
/*********** GLIMMIX ****************/
/************************************/

**Pseudo-likelihood update fails in outer iteration 5.;
PROC GLIMMIX DATA=suicide; 
CLASS gen year; 
MODEL rate_cat = year gen gdp_capita new_age population/S DIST=BINOMIAL LINK=PROBIT; 
RANDOM INTERCEPT / SUB=year TYPE=AR(1) S;
 output out=BINOM pearson=pearson_BINOM pred(BLUP ILINK)=PREDICTED_binom; RUN;

PROC MEANS DATA =binom n mean var;
 var pearson_binom; run;

 	/*output a dataset for plotting results*/
 data binom_plot;
 set binom;
 keep id year gen gdp_capita new_age population predicted_binom;
 label predicted_binom="Expected_Poisson";
 run;
 
/****************************************/
/****************************************/
/*>*>*>*>*>*RUNNING GEE PROCEDURES*<*<*<*<*<*/
/****************************************/

%INCLUDE "&path.QIC.sas";
*FULL CLUSTER;
%QIC(DATA=suicide, POPTIONS=DESC, CLASS= C gen YEAR SEX , RESPONSE=rate_cat, MODEL= SEX new_age SEX*new_age,
DIST= BIN, MOPTIONS= LINK=LOGIT, SUBJECT=C, LOGOR=FULLCLUST, WITHIN= GEN,P=PRED, QICOPTIONS=NOPRINT, APPENDTO=QICSUMMARY);
*TOEPLITZ with Type=Toep;
%QIC(DATA=suicide, POPTIONS=DESC, CLASS= C gen YEAR SEX AGE_CAT, RESPONSE=rate_cat, MODEL= SEX new_age SEX*new_age,
DIST= BIN, MOPTIONS= LINK=LOGIT, SUBJECT=C, type=MDEP(2), WITHIN=  GEN,P=PRED, QICOPTIONS=NOPRINT, APPENDTO=QICSUMMARY);
*EXCHANGE;
%QIC(DATA=suicide, POPTIONS=DESC, CLASS= C YEAR SEX AGE_CAT, RESPONSE=rate_cat, MODEL= SEX new_age SEX*new_age,
DIST= BIN, MOPTIONS= LINK=LOGIT, SUBJECT=C, LOGOR=EXCH, WITHIN=  GEN,P=PRED, QICOPTIONS=NOPRINT, APPENDTO=QICSUMMARY);
*AR(1);
%QIC(DATA=suicide, POPTIONS=DESC, CLASS= C gen YEAR SEX AGE_CAT, RESPONSE=rate_cat, MODEL= SEX new_age SEX*new_age,
DIST= BIN, MOPTIONS= LINK=LOGIT, SUBJECT=C, TYPE=AR(1), WITHIN=  GEN,P=PRED, QICOPTIONS=NOPRINT, APPENDTO=QICSUMMARY);
*INDEPENDENT;
%QIC(DATA=suicide, POPTIONS=DESC, CLASS= C gen YEAR SEX AGE_CAT, RESPONSE=rate_cat, MODEL= SEX new_age SEX*new_age,
DIST= BIN, MOPTIONS= LINK=LOGIT, SUBJECT=C, TYPE=IND, WITHIN=  GEN,P=PRED, QICOPTIONS=NOPRINT, APPENDTO=QICSUMMARY);
*UNSTRUCTURED;
%QIC(DATA=suicide, POPTIONS=DESC, CLASS= C gen YEAR SEX AGE_CAT, RESPONSE=rate_cat, MODEL= SEX new_age SEX*new_age,
DIST= BIN, MOPTIONS= LINK=LOGIT, SUBJECT=C, TYPE=UNSTR, WITHIN=  GEN,P=PRED, QICOPTIONS=NOPRINT, APPENDTO=QICSUMMARY);

proc sort data=QICsummary;
by QIC;
run;
proc print data=QICsummary;
run;

proc sql; delete from QICSUMMARY;quit;




*cross-sectional;
proc genmod data = SUICIDE;
  class id;
  model y4 = trt age trt*age /dist=zip;
  zeromodel trt/link = logit ;
run;