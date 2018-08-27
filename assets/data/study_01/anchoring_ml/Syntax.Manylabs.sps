get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\ManylabsData.sav'.
DATASET NAME Manylabs. 
dataset activate Manylabs.

***syntax for many labs analyses**
*drop test sessions**

select if nativelang2 NE 'test'. 
select if imagineddescribe NE 'test'.
EXECUTE.

sort cases by referrer.
autorecode referrer /into sample /print.

 * compute filter_oxy=0.
 * if sample=17 filter_oxy=1.
 * filter by filter_oxy.
 * exe.

***single study hypothesis tests**

*Sunk cost

*create unique dependent and independent variables, compute t-test, compute d*

COMPUTE sunkgroup=0.
if sunkcostb GE 1 sunkgroup=1.
EXECUTE.

compute sunkDV=sum(sunkcosta, sunkcostb).
EXECUTE.

value labels sunkgroup 0 'paid'  1 'free' . 

T-TEST GROUPS=sunkgroup(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=sunkDV
  /CRITERIA=CI(.99).


****ALL FOLLOWING TS ARE PAID-FREE*********

      split file layered by sample.
      T-TEST GROUPS=sunkgroup(0 1)
        /MISSING=ANALYSIS
        /VARIABLES=sunkDV
        /CRITERIA=CI(.95).

SPLIT FILE off.
use all.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\sunkd.sav'
  /BREAK=referrer sunkgroup
  /sunkcost_mean=MEAN(sunkDV) 
  /sunkcost_sd=SD(sunkDV)
  /sunk_N_Subj=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\sunkd.sav'.
dataset name  Sunkd.
dataset activate Sunkd.


CASESTOVARS 
  /ID=referrer 
  /INDEX=sunkgroup 
  /GROUPBY=VARIABLE.

compute Sunkd = (sunkcost_mean..00 - sunkcost_mean.1.00)/(sqrt(mean(sunkcost_sd..00*sunkcost_sd..00, sunkcost_sd.1.00*sunkcost_sd.1.00))).
exe.

      VARIABLE LABELS sunkd 'Sunk Cost - Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=sunkd.

      NPTESTS 
        /ONESAMPLE TEST (sunkd) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\sunkd.sav'.

DATASET CLOSE Sunkd. 
dataset activate Manylabs. 

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\sunkGrandd.sav'
  /BREAK=sunkgroup
  /session_status=first(session_status)
  /sunkcost_mean=MEAN(sunkdv) 
  /sunk_sd=SD(sunkDV)
  /sunk_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\sunkGrandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=sunkgroup 
  /GROUPBY=VARIABLE.

compute sunkGrandd=(sunkcost_mean..00-sunkcost_mean.1.00)/(sqrt(mean(sunk_sd..00*sunk_sd..00, sunk_sd.1.00*sunk_sd.1.00))).
compute sunkLB=sunkGrandd-((2.54*sunk_sd.1.00)/(sqrt(sunk_N.1.00))).
compute sunkUB=sunkGrandd+((2.54*sunk_sd..00)/(sqrt(sunk_N..00))).
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\sunkGrandd.sav'.

dataset activate Manylabs. 

**Gain vs Loss framing**

COMPUTE gainlossgroup = 0.
IF     (INDEX(UPCASE(task_id.3),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.4),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.5),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.6),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.7),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.8),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.9),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.10),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.11),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.12),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.13),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.14),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.15),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.16),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.17),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.18),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.19),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.20),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.21),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.22),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.23),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
IF     (INDEX(UPCASE(task_id.24),'DISEASEFRAMINGA') EQ 1) gainlossgroup = 1.
EXECUTE.

VALUE LABELS gainlossgroup 0 'People will die' 1 'People will be saved'.


compute gainlossDV=sum(diseaseframinga, diseaseframingb).
EXECUTE.

value labels gainlossDV 1 'chose program with exact outcome' 2 'chose program with probability outcome' .

CROSSTABS 
  /TABLES=gainlossgroup BY gainlossDV 
  /FORMAT=AVALUE TABLES 
  /STATISTICS=CHISQ CORR 
  /CELLS=COUNT ROW COLUMN TOTAL BPROP 
   /COUNT ROUND CELL.

split file layered by sample. 
CROSSTABS 
  /TABLES=gainlossgroup BY gainlossDV 
  /FORMAT=AVALUE TABLES 
  /STATISTICS=CHISQ CORR 
  /CELLS=COUNT BPROP 
  /COUNT ROUND CELL.

split file off. 

compute glbinary=gainlossdv-1. 
EXECUTE.

compute glbina=diseaseframinga-1. 
compute glbinb=diseaseframingb-1.
EXECUTE.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\gainlossd.sav'
  /BREAK=referrer gainlossgroup
  /gainloss_mean=MEAN(glbinary) 
  /gainloss_sd=SD(glbinary)
  /gainloss_N_Subj=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\gainlossd.sav'.
dataset name  GainLoss.
dataset activate GainLoss.

CASESTOVARS 
  /ID=referrer 
  /INDEX=gainlossgroup 
  /GROUPBY=VARIABLE.

compute gainlossd = (gainloss_mean..00 - gainloss_mean.1.00)/(sqrt(mean(gainloss_sd..00*gainloss_sd..00, gainloss_sd.1.00*gainloss_sd.1.00))).
exe.

 VARIABLE LABELS gainlossd 'Gain vs Loss framing  - Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=gainlossd.

      NPTESTS 
        /ONESAMPLE TEST (gainlossd) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\gainlossd.sav'.

DATASET CLOSE GainLoss. 
dataset activate Manylabs.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\gainlossGrandd.sav'
  /BREAK=gainlossgroup
  /session_status=first(session_status)
  /gainloss_mean=MEAN(gainlossdv) 
  /gainloss_sd=SD(gainlossDV)
  /gainloss_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\gainlossGrandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=gainlossgroup 
  /GROUPBY=VARIABLE.

compute gainlossGrandd=(gainloss_mean..00-gainloss_mean.1.00)/(sqrt(mean(gainloss_sd..00*gainloss_sd..00, gainloss_sd.1.00*gainloss_sd.1.00))).
compute gainlossLB=gainlossGrandd-((2.54*gainloss_sd.1.00)/(sqrt(gainloss_N.1.00))).
compute gainlossUB=gainlossGrandd+((2.54*gainloss_sd..00)/(sqrt(gainloss_N..00))).
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\gainlossGrandd.sav'.

dataset activate Manylabs. 

***ANCHORING***
**drop text within numeric value**


COMPUTE anchoring1a=REPLACE(anchoring1a, " million", "000000").
COMPUTE anchoring1b=REPLACE(anchoring1b, " million", "000000").
COMPUTE anchoring2a=REPLACE(anchoring2a, " million", "000000").
COMPUTE anchoring2b=REPLACE(anchoring2b, " million", "000000").
COMPUTE anchoring3a=REPLACE(anchoring3a, " million", "000000").
COMPUTE anchoring3b=REPLACE(anchoring3b, " million", "000000").
COMPUTE anchoring4a=REPLACE(anchoring4a, " million", "000000").
COMPUTE anchoring4b=REPLACE(anchoring4b, " million", "000000").

COMPUTE anchoring1a=REPLACE(anchoring1a,  ",", "").
COMPUTE anchoring1b=REPLACE(anchoring1b,  ",", "").
COMPUTE anchoring2a=REPLACE(anchoring2a,  ",", "").
COMPUTE anchoring2b=REPLACE(anchoring2b,  ",", "").
COMPUTE anchoring3a=REPLACE(anchoring3a,  ",", "").
COMPUTE anchoring3b=REPLACE(anchoring3b,  ",", "").
COMPUTE anchoring4a=REPLACE(anchoring4a,  ",", "").
COMPUTE anchoring4b=REPLACE(anchoring4b,  ",", "").

COMPUTE anchoring1a=REPLACE(anchoring1a,  " ", "").
COMPUTE anchoring1b=REPLACE(anchoring1b,  " ", "").
COMPUTE anchoring2a=REPLACE(anchoring2a,  " ", "").
COMPUTE anchoring2b=REPLACE(anchoring2b,  " ", "").
COMPUTE anchoring3a=REPLACE(anchoring3a,  " ", "").
COMPUTE anchoring3b=REPLACE(anchoring3b,  " ", "").
COMPUTE anchoring4a=REPLACE(anchoring4a,  " ", "").
COMPUTE anchoring4b=REPLACE(anchoring4b,  " ", "").

COMPUTE anchoring1akm=REPLACE(anchoring1akm, " ", "").
COMPUTE anchoring1bkm=REPLACE(anchoring1bkm,  " ", "").
COMPUTE anchoring3ameter=REPLACE(anchoring3ameter,  " ", "").
COMPUTE anchoring3bmeter=REPLACE(anchoring3bmeter,  " ", "").

EXECUTE.

****examine oxy data***

 * freq user_agent.

 * crosstabs user_agent by pi_referrer.

 * sort cases by sample session_id computer_id. 
 * autorecode user_agent /into computer_id. 

 * sort cases by computer_id  session_id .
 * EXECUTE.

 * select if computer_id=4 OR computer_id=7 OR computer_id=10. 
 * exe.

 * split file layered by computer_id.
 * split file off. 

 * freq computer_id.

**convert string to numeric variables**


ALTER TYPE 
   anchoring1a to anchoring4b (F8.0).
EXECUTE.

des anchoring1a to anchoring4b .

ALTER TYPE 
   anchoring1akm  anchoring1bkm anchoring3ameter anchoring3bmeter (F8.0).
EXECUTE.

*convert km and meters into miles and feet** 

if anchoring1akm GE 1 anchoring1a=anchoring1akm*.62137.
if anchoring1bkm GE 1 anchoring1b=anchoring1bkm*.62137.
if anchoring3ameter GE 1 anchoring3a=anchoring3ameter*3.2808.
if anchoring3bmeter GE 1 anchoring3b=anchoring3bmeter*3.2808.
EXECUTE.

*recode out-of-range values to missing*


recode anchoring1a anchoring1b (lowest thru 1500=SYSMIS). 
recode anchoring1a anchoring1b (6000 thru highest=SYSMIS).  
recode anchoring2a anchoring2b (lowest thru 200000=SYSMIS). 
recode anchoring2a anchoring2b (5000000 thru highest=SYSMIS). 
recode anchoring3a anchoring3b (lowest thru 2000=SYSMIS) .
recode anchoring3a anchoring3b (45500 thru highest=SYSMIS).
recode anchoring4a anchoring4b (lowest thru 100=SYSMIS) .
recode anchoring4a anchoring4b (50000 thru highest=SYSMIS).
EXECUTE.

**compute group variables and dvs**

compute anch1group=0.
compute anch2group=0.
compute anch3group=0.
compute anch4group=0.

if anchoring1b GE 1 anch1group=1. 
if anchoring2b GE 1 anch2group=1. 
if anchoring3b GE 1 anch3group=1. 
if anchoring4b GE 1 anch4group=1. 
EXECUTE.

value labels anch1group anch2group anch3group anch4group 0 'lowanchor'  1 'highanchor' .

compute anchoring1=sum(anchoring1a, anchoring1b).
compute anchoring2=sum(anchoring2a, anchoring2b).
compute anchoring3=sum(anchoring3a, anchoring3b).
compute anchoring4=sum(anchoring4a, anchoring4b).
EXECUTE.

des anchoring1.
des anchoring2.
des anchoring3.
des anchoring4.


**Original analyses***

 * T-TEST GROUPS=anch1group(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Anchoring1 
  /CRITERIA=CI(.99).

 * T-TEST GROUPS=anch2group(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Anchoring2 
  /CRITERIA=CI(.99).

 * T-TEST GROUPS=anch3group(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Anchoring3 
  /CRITERIA=CI(.99).

 * T-TEST GROUPS=anch4group(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Anchoring4 
  /CRITERIA=CI(.99).

graph /HISTOGRAM anchoring1.
graph /HISTOGRAM anchoring2.
graph /HISTOGRAM anchoring3.
graph /HISTOGRAM anchoring4.


rank anchoring1 anchoring2 anchoring3 anchoring4 . 
Compute Ranch1 = Ranchori.
compute Ranch2=RAN001.
compute Ranch3=RAN002.
compute Ranch4=RAN003.
EXECUTE.

variable labels Ranch1 'Rank-transformed Anchoring 1 dv'. 
variable labels Ranch2 'Rank-transformed Anchoring 2 dv'. 
variable labels Ranch3 'Rank-transformed Anchoring 3 dv'. 
variable labels Ranch4 'Rank-transformed Anchoring 4 dv'. 

graph /HISTOGRAM Ranch1.
graph /HISTOGRAM Ranch2.
graph /HISTOGRAM Ranch3.
graph /HISTOGRAM Ranch4.


**ALL FOLLOWING ts ARE HIGH ANCHOR - LOW ANCHOR***

T-TEST GROUPS=anch1group(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Ranch1 
  /CRITERIA=CI(.99).

T-TEST GROUPS=anch2group(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Ranch2 
  /CRITERIA=CI(.99).

T-TEST GROUPS=anch3group(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Ranch3 
  /CRITERIA=CI(.99).

T-TEST GROUPS=anch4group(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Ranch4 
  /CRITERIA=CI(.99).

split file layered by sample. 
T-TEST GROUPS=anch1group(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Ranch1 
  /CRITERIA=CI(.99).

T-TEST GROUPS=anch2group(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Ranch2 
  /CRITERIA=CI(.99).

T-TEST GROUPS=anch3group(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Ranch3 
  /CRITERIA=CI(.99).

T-TEST GROUPS=anch4group(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Ranch4 
  /CRITERIA=CI(.99).
split file off.


**compute ds**
*anchoring1*

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring1d.sav' OVERWRITE=yes
  /BREAK=referrer anch1group
  /anchoring1_mean=MEAN(Ranch1) 
  /anchoring1_sd=SD(Ranch1)
  /N_Subj_Anch1=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring1d.sav'.
dataset name  Anchoring1.
dataset activate Anchoring1.

CASESTOVARS 
  /ID=referrer 
  /INDEX=anch1group 
  /GROUPBY=VARIABLE.

compute Anchoring1d = (anchoring1_mean.1.00 - anchoring1_mean..00)/(sqrt(mean(anchoring1_sd..00*anchoring1_sd..00, anchoring1_sd.1.00*anchoring1_sd.1.00))).
exe.

      VARIABLE LABELS Anchoring1d 'Distance to NYC more than 1500 vs less than 6000- Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=Anchoring1d.

      NPTESTS 
        /ONESAMPLE TEST (Anchoring1d) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.


save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring1d.sav'.

DATASET CLOSE Anchoring1. 
dataset activate Manylabs.


AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring1Grandd.sav' OVERWRITE=yes
  /BREAK=anch1group
  /session_status=first(session_status)
  /anchoring1_mean=MEAN(Ranch1) 
  /anchoring1_sd=SD(Ranch1)
  /anchoring1_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring1Grandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=anch1group 
  /GROUPBY=VARIABLE.

compute anchoring1Grandd=(anchoring1_mean.1.00-anchoring1_mean..00)/(sqrt(mean(anchoring1_sd..00*anchoring1_sd..00, anchoring1_sd.1.00*anchoring1_sd.1.00))).
exe.
compute anchoring1LB=anchoring1Grandd-(2.58*anchoring1_sd.1.00/(sqrt(anchoring1_N.1.00))).
compute anchoring1UB=anchoring1Grandd+(2.58*anchoring1_sd..00/(sqrt(anchoring1_N..00))).
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring1Grandd.sav'.

dataset activate Manylabs. 

*anchoring2*

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring2d.sav' OVERWRITE=yes
 /BREAK=referrer anch2group
  /anchoring2_mean=MEAN(Ranch2) 
  /anchoring2_sd=SD(Ranch2)
  /N_Subj_Anch2=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring2d.sav'.
dataset name  Anchoring2.
dataset activate Anchoring2.

CASESTOVARS 
  /ID=referrer 
  /INDEX=anch2group 
  /GROUPBY=VARIABLE.

compute Anchoring2d = (anchoring2_mean.1.00 - anchoring2_mean..00)/(sqrt(mean(anchoring2_sd..00*anchoring2_sd..00, anchoring2_sd.1.00*anchoring2_sd.1.00))).
exe.


      VARIABLE LABELS Anchoring2d 'Population of Chicago more than 200k vs less than 6 millions- Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=Anchoring2d.

      NPTESTS 
        /ONESAMPLE TEST (Anchoring2d) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring2d.sav'.

DATASET CLOSE Anchoring2. 
dataset activate Manylabs. 

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring2Grandd.sav' OVERWRITE=yes
  /BREAK=anch2group
  /session_status=first(session_status)
  /anchoring2_mean=MEAN(Ranch2) 
  /anchoring2_sd=SD(Ranch2)
  /anchoring2_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring2Grandd.sav'. 

CASESTOVARS 
  /ID=session_status 
  /INDEX=anch2group 
  /GROUPBY=VARIABLE.

compute anchoring2Grandd=(anchoring2_mean.1.00-anchoring2_mean..00)/(sqrt(mean(anchoring2_sd..00*anchoring2_sd..00, anchoring2_sd.1.00*anchoring2_sd.1.00))).
exe.

compute anchoring2LB=anchoring2Grandd-(2.58*anchoring2_sd.1.00/(sqrt(anchoring2_N.1.00))).
compute anchoring2UB=anchoring2Grandd+(2.58*anchoring2_sd..00/(sqrt(anchoring2_N..00))).
EXECUTE.


save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring2Grandd.sav'. 

dataset activate Manylabs. 

**anchoring3**


AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring3d.sav' OVERWRITE=yes
 /BREAK=referrer anch3group
  /anchoring3_mean=MEAN(Ranch3) 
  /anchoring3_sd=SD(Ranch3)
  /N_Subj_Anch3=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring3d.sav'.
dataset name  Anchoring3.
dataset activate Anchoring3.


CASESTOVARS 
  /ID=referrer 
  /INDEX=anch3group 
  /GROUPBY=VARIABLE.

compute Anchoring3d = (anchoring3_mean.1.00 - anchoring3_mean..00)/(sqrt(mean(anchoring3_sd..00*anchoring3_sd..00, anchoring3_sd.1.00*anchoring3_sd.1.00))).
exe.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring3d.sav'.

      VARIABLE LABELS Anchoring3d 'Mt Everest more than 200 feet vs less than 45500- Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=Anchoring3d.

      NPTESTS 
        /ONESAMPLE TEST (Anchoring3d) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.


DATASET CLOSE Anchoring3. 
DATASET ACTIVATE Manylabs. 

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring3Grandd.sav' OVERWRITE=yes
  /BREAK=anch3group
  /session_status=first(session_status)
  /anchoring3_mean=MEAN(Ranch3) 
  /anchoring3_sd=SD(Ranch3)
  /anchoring3_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring3Grandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=anch3group 
  /GROUPBY=VARIABLE.

compute anchoring3Grandd=(anchoring3_mean.1.00-anchoring3_mean..00)/(sqrt(mean(anchoring3_sd..00*anchoring3_sd..00, anchoring3_sd.1.00*anchoring3_sd.1.00))).
exe.

compute anchoring3LB=anchoring3Grandd-(2.58*anchoring3_sd.1.00/(sqrt(anchoring3_N.1.00))).
compute anchoring3UB=anchoring3Grandd+(2.58*anchoring3_sd..00/(sqrt(anchoring3_N..00))).
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring3Grandd.sav'.

dataset activate Manylabs. 

**anchoring4**

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring4d.sav' OVERWRITE=yes
 /BREAK=referrer anch4group
  /anchoring4_mean=MEAN(Ranch4) 
  /anchoring4_sd=SD(Ranch4)
  /N_Subj_Anch4=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring4d.sav'.
dataset name  Anchoring4.
dataset activate Anchoring4.


CASESTOVARS 
  /ID=referrer 
  /INDEX=anch4group 
  /GROUPBY=VARIABLE.

compute Anchoring4d = (anchoring4_mean.1.00 - anchoring4_mean..00)/(sqrt(mean(anchoring4_sd..00*anchoring4_sd..00, anchoring4_sd.1.00*anchoring4_sd.1.00))).
exe.

      VARIABLE LABELS Anchoring4d 'Babies born more than 100 vs less than 50000- Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=Anchoring4d.

      NPTESTS 
        /ONESAMPLE TEST (Anchoring4d) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring4d.sav'.


DATASET CLOSE Anchoring4. 
DATASET ACTIVATE Manylabs.


AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring4Grandd.sav' OVERWRITE=yes
  /BREAK=anch4group
  /session_status=first(session_status)
  /anchoring4_mean=MEAN(Ranch4) 
  /anchoring4_sd=SD(Ranch4)
  /anchoring4_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring4Grandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=anch4group 
  /GROUPBY=VARIABLE.

compute anchoring4Grandd=(anchoring4_mean.1.00-anchoring4_mean..00)/(sqrt(mean(anchoring4_sd..00*anchoring4_sd..00, anchoring4_sd.1.00*anchoring4_sd.1.00))).
exe.
compute anchoring4LB=anchoring4Grandd-(2.58*anchoring4_sd.1.00/(sqrt(anchoring4_N.1.00))).
compute anchoring4UB=anchoring4Grandd+(2.58*anchoring4_sd..00/(sqrt(anchoring4_N..00))).
EXECUTE.


save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring4Grandd.sav'.

dataset activate Manylabs. 

***Retrospective gamblers' fallacy***

ALTER TYPE 
   gamblerfallacya to gamblerfallacyb (F8.0).
EXECUTE.
COMPUTE gambfalgroup=0.
if gamblerfallacya GE 1 gambfalgroup=1.
EXECUTE.

 
 GRAPH /HISTOGRAM gamblerfallacya. 
 GRAPH /HISTOGRAM gamblerfallacyb. 

***normalize gambfaldv***


compute gamblerfallacya =sqrt(gamblerfallacya).
compute gamblerfallacyb=sqrt(gamblerfallacyb).

compute gambfalDV=sum(gamblerfallacya, gamblerfallacyb).
EXECUTE.

des gambfalDV. 

**read SD to set threshold for outliers**

AGGREGATE 
  /OUTFILE=* MODE=ADDVARIABLES OVERWRITE=yes
  /BREAK= 
  /gamblerfallacya_sd=SD(gamblerfallacya). 
AGGREGATE 
  /OUTFILE=* MODE=ADDVARIABLES OVERWRITE=yes
  /BREAK= 
  /gamblerfallacyb_sd=SD(gamblerfallacyb). 

recode gamblerfallacya gamblerfallacyb (25 thru highest=sysmis).
EXECUTE.

****gambler's fallacy dv has been sqrt-transformed and outliers > 3SD have been set to system missing values***

  GRAPH /HISTOGRAM gambfalDV . 
  GRAPH /HISTOGRAM gamblerfallacyb. 

compute gambfalDV=sum(gamblerfallacya, gamblerfallacyb).
EXECUTE.

variable labels gambfalDV 'sqrt-transformed dep var for gambler fallacy study (responses higher than 3 SDs dropped)'.

value labels gambfalgroup 0 'two6'  1 'three6' . 

AGGREGATE 
  /OUTFILE=* MODE=ADDVARIABLES 
  /BREAK= 
  /gambfalDV_sd=SD(gambfalDV). 

**ALL FOLLOWING ts ARE THREE 6 - TWO 6**

T-TEST GROUPS=gambfalgroup(1 0)
  /MISSING=ANALYSIS
  /VARIABLES=gambfalDV
  /CRITERIA=CI(.99).

split file layered by sample.
T-TEST GROUPS=gambfalgroup(1 0)
  /MISSING=ANALYSIS
  /VARIABLES=gambfalDV
  /CRITERIA=CI(.95).

split file off.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\gambfald.sav'
  /BREAK=referrer  gambfalgroup
  /gambfal_mean=MEAN(gambfalDV) 
  /gambfal_sd=SD(gambfalDV)
  /gambfal_N_Subj=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\gambfald.sav'.
dataset name  Gambfald.
dataset activate Gambfald.

CASESTOVARS 
  /ID=referrer 
  /INDEX=gambfalgroup 
  /GROUPBY=VARIABLE.

compute gambfald = (gambfal_mean.1.00 - gambfal_mean..00)/(sqrt(mean(gambfal_sd..00*gambfal_sd..00, gambfal_sd.1.00*gambfal_sd.1.00))).
EXECUTE.

      VARIABLE LABELS gambfald '# throws estimation: two 6s vs 3 6s- Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=gambfald.

      NPTESTS 
        /ONESAMPLE TEST (gambfald) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\gambfald.sav'.

DATASET CLOSE Gambfald. 

DATASET ACTIVATE Manylabs.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\gambfalGrandd.sav'
  /BREAK=gambfalgroup
  /session_status=first(session_status)
  /gambfal_mean=MEAN(gambfalDV) 
  /gambfal_sd=SD(gambfalDV)
  /gambfal_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\gambfalGrandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=gambfalgroup 
  /GROUPBY=VARIABLE.

compute gambfalGrandd=(gambfal_mean.1.00-gambfal_mean..00)/(sqrt(mean(gambfal_sd..00*gambfal_sd..00, gambfal_sd.1.00*gambfal_sd.1.00))).
exe.
compute gambfalLB=gambfalGrandd-(2.58*gambfal_sd.1.00/(sqrt(gambfal_N.1.00))).
compute gambfalUB=gambfalGrandd+(2.58*gambfal_sd..00/(sqrt(gambfal_N..00))).
EXECUTE.


save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\gambfalGrandd.sav'.

dataset activate Manylabs. 

****	Low-vs.-high category scales***

ALTER TYPE 
   scalesa to scalesb (F8.0).
EXECUTE.

FREQUENCIES scalesa scalesb.

COMPUTE scalesgroup=1.
if scalesa GE 1 scalesgroup=0.
EXECUTE.

recode scalesa (1=0) (2=0) (3=0) (4=0) (5=0) (6=1) into scalesreca.  
recode scalesb (1=0) (2=1) (3=1) (4=1) (5=1) (6=1) into scalesrecb.  
EXECUTE.

compute scales=sum(scalesreca, scalesrecb).
EXECUTE.

variable labels scales 'dependent variable for Low vs High category scale study'. 
value labels scalesgroup 0 'low category scale'  1 'high category scale' . 
VALUE LABELS scales 0 'less than 2 1/2 hrs' 1 'more than 2 1/2 hrs'.

CROSSTABS 
  /TABLES=scalesgroup BY scales 
  /FORMAT=AVALUE TABLES 
  /STATISTICS=CHISQ CORR 
  /CELLS=COUNT ROW COLUMN TOTAL BPROP 
  /COUNT ROUND CELL.

split file layered by sample. 
CROSSTABS 
  /TABLES=scalesgroup BY scales 
  /FORMAT=AVALUE TABLES 
  /STATISTICS=CHISQ CORR 
  /CELLS=COUNT ROW COLUMN TOTAL BPROP 
  /COUNT ROUND CELL.

split file off.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\scalesd.sav'
  /BREAK=referrer scalesgroup
  /scales_mean=MEAN(scales) 
  /scales_sd=SD(scales)
  /scales_N_Subj=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\scalesd.sav'.
dataset name scalesd.
dataset activate scalesd.

CASESTOVARS 
  /ID=referrer 
  /INDEX=scalesgroup 
  /GROUPBY=VARIABLE.

compute scalesd=(scales_mean.1.00-scales_mean..00)/(sqrt(mean(scales_sd..00*scales_sd..00, scales_sd.1.00*scales_sd.1.00))).
exe.

      VARIABLE LABELS scalesd '# of ss. watching more than 2 1/2 hours: low vs high category scale - Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=scalesd.

      NPTESTS 
        /ONESAMPLE TEST (scalesd) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\scalesd.sav'.

DATASET CLOSE scalesd. 

DATASET ACTIVATE Manylabs.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\scalesGrandd.sav'
  /BREAK=scalesgroup
  /session_status=first(session_status)
  /scales_mean=MEAN(scales) 
  /scales_sd=SD(scales)
  /scales_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\scalesGrandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=scalesgroup 
  /GROUPBY=VARIABLE.

compute scalesGrandd=(scales_mean.1.00-scales_mean..00)/(sqrt(mean(scales_sd..00*scales_sd..00, scales_sd.1.00*scales_sd.1.00))).
exe.
compute scalesLB=scalesGrandd-(2.58*scales_sd.1.00/(sqrt(scales_N.1.00))).
compute scalesUB=scalesGrandd+(2.58*scales_sd..00/(sqrt(scales_N..00))).
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\scalesGrandd.sav'.

dataset activate Manylabs. 

***norm of reciprocity***

*(US-first is "a").

ALTER TYPE 
   reciprocityothera to reciprocityusb (F1.0).
EXECUTE.

COMPUTE reciprocitygroup=0.
if reciprocityusa GE 1 reciprocitygroup=1.
EXECUTE.

compute reciprocityother=sum(reciprocityothera, reciprocityotherb)-1.
compute reciprocityus=sum(reciprocityusa, reciprocityusb)-1.
EXECUTE.

value labels reciprocitygroup 0 'Asked second'  1 'Asked first' . 
VALUE LABELS reciprocityus 0 'yes' 1 'no'.

recode reciprocityus (0=1) (1=0).
exe.

VALUE LABELS reciprocityus 0 'no' 1 'yes'.
variable labels reciprocityus 'Do you think the United STates should let reporters in?'.


CROSSTABS 
  /TABLES=reciprocitygroup BY reciprocityus 
  /FORMAT=AVALUE TABLES 
  /STATISTICS=CHISQ CORR 
  /CELLS=COUNT ROW COLUMN TOTAL BPROP 
  /COUNT ROUND CELL.

split file layered by sample.
CROSSTABS 
  /TABLES=reciprocitygroup BY reciprocityus 
  /FORMAT=AVALUE TABLES 
  /STATISTICS=CHISQ CORR 
  /CELLS=COUNT BPROP 
  /COUNT ROUND CELL.
split file off.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\reciprocityd.sav'
  /BREAK=referrer reciprocitygroup
  /reciprocity_mean=MEAN(reciprocityus) 
  /reciprocity_sd=SD(reciprocityus)
  /reciprocity_N_Subj=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\reciprocityd.sav'.
dataset name  reciprocityd.
dataset activate reciprocityd.

CASESTOVARS 
  /ID=referrer 
  /INDEX=reciprocitygroup 
  /GROUPBY=VARIABLE.

compute reciprocityd=(reciprocity_mean..00-reciprocity_mean.1.00)/(sqrt(mean(reciprocity_sd..00*reciprocity_sd..00, reciprocity_sd.1.00*reciprocity_sd.1.00))).
exe.

      VARIABLE LABELS reciprocityd 'Do you think the United States should let reporters from North Korea come in here? Before vs after the opposite question - Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=reciprocityd.

      NPTESTS 
        /ONESAMPLE TEST (reciprocityd) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\reciprocityd.sav'.

DATASET CLOSE reciprocityd. 

DATASET ACTIVATE Manylabs.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\reciprocityGrandd.sav'
  /BREAK=reciprocitygroup
  /session_status=first(session_status)
  /reciprocity_mean=MEAN(reciprocityus) 
  /reciprocity_sd=SD(reciprocityus)
  /reciprocity_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\reciprocityGrandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=reciprocitygroup 
  /GROUPBY=VARIABLE.

compute reciprocityGrandd=(reciprocity_mean..00-reciprocity_mean.1.00)/(sqrt(mean(reciprocity_sd..00*reciprocity_sd..00, reciprocity_sd.1.00*reciprocity_sd.1.00))).
exe.
compute reciprocityLB=reciprocityGrandd-(2.58*reciprocity_sd..00/(sqrt(reciprocity_N..00))).
compute reciprocityUB=reciprocityGrandd+(2.58*reciprocity_sd.1.00/(sqrt(reciprocity_N.1.00))).
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\reciprocityGrandd.sav'.

DATASET ACTIVATE Manylabs.

***allowed/forbidden***

**forbidden is group 'a'**


ALTER TYPE 
   allowedforbiddena to allowedforbiddenb (F1.0).
EXECUTE.

freq allowedforbiddena  allowedforbiddenb.

COMPUTE allowedforbiddenGroup=0.
if allowedforbiddena GE 1 allowedforbiddenGroup=1.
EXECUTE.

compute allowedforbidden=sum(allowedforbiddena, allowedforbiddenb)-1.
EXECUTE.

variable labels allowedforbidden 'Allowed/Forbidden study dep var'. 

value labels allowedforbiddenGroup 1 'forbidden'  0 'allowed' . 
value labels allowedforbidden 0 'YES'  1 'NO' . 

recode allowedforbidden (0=1) (1=0).
value labels allowedforbidden 0 'NO'  1 'YES' . 
exe.

CROSSTABS 
  /TABLES=allowedforbiddenGroup BY  allowedforbidden
  /FORMAT=AVALUE TABLES 
  /STATISTICS=CHISQ CORR 
  /CELLS=COUNT PROP ROW COLUMN TOTAL 
  /COUNT ROUND CELL.

split file layered by sample. 
CROSSTABS 
  /TABLES=allowedforbidden BY allowedforbiddenGroup 
  /FORMAT=AVALUE TABLES 
  /STATISTICS=CHISQ CORR 
  /CELLS=COUNT PROP ROW COLUMN TOTAL 
  /COUNT ROUND CELL.
split file off. 

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\allowedforbiddend.sav'
  /BREAK=referrer allowedforbiddengroup
  /allowedforbidden_mean=MEAN(allowedforbidden) 
  /allowedforbidden_sd=SD(allowedforbidden)
  /allowedforbidden_N_Subj=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\allowedforbiddend.sav'.
dataset name  allowedforbiddend.
dataset activate allowedforbiddend.

CASESTOVARS 
  /ID=referrer 
  /INDEX=allowedforbiddengroup 
  /GROUPBY=VARIABLE.

compute allowedforbiddend = (allowedforbidden_mean..00 - allowedforbidden_mean.1.00)/(sqrt(mean(allowedforbidden_sd..00*allowedforbidden_sd..00, allowedforbidden_sd.1.00*allowedforbidden_sd.1.00))).
exe.

      VARIABLE LABELS allowedforbiddend 'Allow speeches against democracy - Forbid vs Allow wording of the question- Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=allowedforbiddend.

      NPTESTS 
        /ONESAMPLE TEST (allowedforbiddend) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\allowedforbiddend.sav'.

DATASET CLOSE allowedforbiddend. 

DATASET ACTIVATE Manylabs.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\allowedforbiddenGrandd.sav'
  /BREAK=allowedforbiddengroup
  /session_status=first(session_status)
  /allowedforbidden_mean=MEAN(allowedforbidden) 
  /allowedforbidden_sd=SD(allowedforbidden)
  /allowedforbidden_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\allowedforbiddenGrandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=allowedforbiddengroup 
  /GROUPBY=VARIABLE.

 * compute pooledSD=(sqrt(mean(allowedforbidden_sd..00*allowedforbidden_sd..00, allowedforbidden_sd.1.00*allowedforbidden_sd.1.00))).
 * compute SE=pooledSD/sqrt(allowedforbidden_N..00+allowedforbidden_N.1.00).
 * EXECUTE.

compute allowedforbiddenGrandd=(allowedforbidden_mean..00-allowedforbidden_mean.1.00)/(sqrt(mean(allowedforbidden_sd..00*allowedforbidden_sd..00, allowedforbidden_sd.1.00*allowedforbidden_sd.1.00))).
exe.
compute allowedforbiddenLB=allowedforbiddenGrandd-(2.58*allowedforbidden_sd.1.00/(sqrt(allowedforbidden_N.1.00))).
compute allowedforbiddenUB=allowedforbiddenGrandd+(2.58*allowedforbidden_sd..00/(sqrt(allowedforbidden_N..00))).
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\allowedforbiddenGrandd.sav'.

DATASET ACTIVATE Manylabs.

***quote attribution***


split file off.
use all.


ALTER TYPE 
   quotea to quoteb (F1.0).
EXECUTE.

recode quotea (1=9) (9=1) (8=2) (2=8) (7=3) (3=7) (6=4) (4=6) (5=5) INTO quotearec.
recode quoteb (1=9) (9=1) (8=2) (2=8) (7=3) (3=7) (6=4) (4=6) (5=5) INTO quotebrec.

COMPUTE quoteGroup=0.
if quotea GE 1 quoteGroup=1.
EXECUTE.

compute quote=sum(quotearec, quotebrec).
EXECUTE.

value labels quoteGroup 0 'disliked source'  1 'liked source' . 

variable labels quote 'Agreement with the quote'. 
FREQUENCIES quote. 

**ALL THE FOLLOWING ts ARE LIKED SOURCE - DISLIKED SOURCE***

T-TEST GROUPS=quoteGroup(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=quote 
  /CRITERIA=CI(.99).

split file layered by sample.
T-TEST GROUPS=quoteGroup(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=quote 
  /CRITERIA=CI(.95).
split file off.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\quoted.sav' 
  /BREAK=referrer quotegroup
  /quote_mean=MEAN(quote) 
  /quote_sd=SD(quote)
  /quote_N_Subj=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\quoted.sav'.
dataset name  quoted.
dataset activate quoted.

CASESTOVARS 
  /ID=referrer 
  /INDEX=quotegroup 
  /GROUPBY=VARIABLE.

compute quoted=(quote_mean.1.00-quote_mean..00)/(sqrt(mean(quote_sd..00*quote_sd..00, quote_sd.1.00*quote_sd.1.00))).
exe.

 VARIABLE LABELS quoted 'Agreement with the sentence "I have sworn to only live free, even if I find bitter the taste of death." - Liked vs disliked source - Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=quoted.

      NPTESTS 
        /ONESAMPLE TEST (quoted) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\quoted.sav'. 

DATASET CLOSE quoted. 

DATASET ACTIVATE Manylabs.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\quoteGrandd.sav'
  /BREAK=quotegroup
  /session_status=first(session_status)
  /quote_mean=MEAN(quote) 
  /quote_sd=SD(quote)
  /quote_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\quoteGrandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=quotegroup 
  /GROUPBY=VARIABLE.

compute quoteGrandd=(quote_mean.1.00-quote_mean..00)/(sqrt(mean(quote_sd..00*quote_sd..00, quote_sd.1.00*quote_sd.1.00))).
exe.
compute quoteLB=quoteGrandd-(2.58*quote_sd.1.00/(sqrt(quote_N.1.00))).
compute quoteUB=quoteGrandd+(2.58*quote_sd..00/(sqrt(quote_N..00))).
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\quoteGrandd.sav'.

dataset activate Manylabs.

***flag priming***

split file off.
use all.

ALTER TYPE 
   flagdv1 to flagtimeestimate4 (F1.0).
EXECUTE.

ALTER TYPE 
   noflagtimeestimate1 to noflagtimeestimate4 (F1.0).
EXECUTE.


RELIABILITY 
  /VARIABLES= flagdv1 flagdv2 flagdv3 flagdv4 flagdv5 flagdv6 flagdv7 flagdv8  
  /SCALE('ALL VARIABLES') ALL 
  /MODEL=ALPHA 
  /STATISTICS=DESCRIPTIVE SCALE 
  /SUMMARY=TOTAL.

compute totalflagestimations=(flagtimeestimate1 + flagtimeestimate2 + flagtimeestimate3 + flagtimeestimate4). 
compute totalnoflagtimeestimations=sum(noflagtimeestimate1, noflagtimeestimate2,  noflagtimeestimate3, noflagtimeestimate4). 
EXECUTE.

compute flagfilter=0.
EXECUTE.
if totalflagestimations GE 1 flagfilter=1.
if totalnoflagtimeestimations GE 1 flagfilter=1.
if us_or_international =1 flagfilter=0. 
filter by flagfilter.

compute flagdv=mean(flagdv1 to flagdv8). 
EXECUTE.

variable labels flagdv 'Dependent variable for Flag Priming study (mean of 8 items)'.
 
compute flagGroup=2. 
execute.
if totalflagestimations GE 1 flagGroup=1. 
if totalnoflagtimeestimations GE 1 flagGroup=0.
EXECUTE.

MISSING VALUES flagGroup (2).

VALUE LABELS flagGroup 0 'no prime' 1 'flag prime'.

variable labels flagfilter 'filter by this variable to exclude ss from flag priming analyses (criteria: missed 1 time estimation response; subject is intl)'.
VALUE LABELS flagfilter 0 'exclude' 1 'include'.


**ALL THE FOLLOWING ts ARE PRIME - CONTROL

T-TEST GROUPS=flagGroup(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=flagdv 
  /CRITERIA=CI(.99).

split file layered by sample.
T-TEST GROUPS=flagGroup(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=flagdv 
  /CRITERIA=CI(.95).
split file off. 
use all.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\flagprimed.sav' 
  /BREAK=referrer flagGroup
  /flag_mean=MEAN(flagdv) 
  /flag_sd=SD(flagdv)
  /flag_N_Subj=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\flagprimed.sav'.
dataset name  flagd.
dataset activate flagd.

SORT CASES BY referrer flagGroup.
CASESTOVARS
  /ID=referrer
  /INDEX=flagGroup
  /GROUPBY=VARIABLE.

compute d_den=mean(flag_sd..00, flag_sd.1.00).
EXECUTE.
compute d_num = (flag_mean.1.00 - flag_mean..00). 
compute flagd= d_num/d_den.
EXECUTE.

      VARIABLE LABELS flagd 'Right-wing attitude - Flag prime vs control - Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=flagd.

      NPTESTS 
        /ONESAMPLE TEST (flagd) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\flagprimed.sav'. 

DATASET CLOSE flagd. 

DATASET ACTIVATE Manylabs.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\flagGrandd.sav'
  /BREAK=flaggroup
  /session_status=first(session_status)
  /flag_mean=MEAN(flagDV) 
  /flag_sd=SD(flagDV)
  /flag_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\flagGrandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=flaggroup 
  /GROUPBY=VARIABLE.

compute flagGrandd=(flag_mean.1.00-flag_mean..00)/(sqrt(mean(flag_sd..00*flag_sd..00, flag_sd.1.00*flag_sd.1.00))).
exe.
compute flagLB=flagGrandd-(2.58*flag_sd.1.00/(sqrt(flag_N.1.00))).
compute flagUB=flagGrandd+(2.58*flag_sd..00/(sqrt(flag_N..00))).
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\flagGrandd.sav'.

dataset activate Manylabs.

filter off.

**currency priming**

split file off.
filter off.
use all.

ALTER TYPE 
   moneyagea moneyageb moneygendera moneygenderb (F1.0).
EXECUTE.

ALTER TYPE 
   sysjust1 to sysjust8 (F1.0).
EXECUTE.

RELIABILITY 
  /VARIABLES= sysjust1 to sysjust8
  /SCALE('ALL VARIABLES') ALL 
  /MODEL=ALPHA 
  /STATISTICS=DESCRIPTIVE SCALE 
  /SUMMARY=TOTAL.

*compute DV dropping ss with less than 6 out of 8 valid responses**

compute Sysjust= mean.6(sysjust1, sysjust2, sysjust3, sysjust4, sysjust5, sysjust6, sysjust7, sysjust8). 
exe.

variable labels Sysjust 'Dependent variable for currency priming study (dropped ss with less than 6 valid responses)'. 

des sysjust1 to sysjust8 .

freq sysjust.


alter type task_id.3 to task_id.24 (A15). 

COMPUTE MoneyGroup = 0.
IF     (INDEX(UPCASE(task_id.3),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.4),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.5),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.6),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.7),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.8),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.9),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.10),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.11),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.12),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.13),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.14),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.15),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.16),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.17),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.18),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.19),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.20),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.21),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.22),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.23),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
IF     (INDEX(UPCASE(task_id.24),'MONEYPRIMEA') EQ 1) MoneyGroup = 1.
EXECUTE.

variable lables MoneyGroup 'experimental conditions for currency priming'.
VALUE LABELS MoneyGroup 0 'Control group' 1 'Money priming group'.

compute moneyfilter=0.
if moneypriorder EQ 1 moneyfilter=1. 
filter by moneyfilter.

variable labels moneyfilter 'filter by this var to include in the currency priming analysis only ss who took this study first'.

**ALL THE FOLLOWING ts ARE PRIMING - CONTROL**

T-TEST GROUPS=MoneyGroup(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Sysjust 
  /CRITERIA=CI(.99).

filter off.

split file layered by sample. 
T-TEST GROUPS=MoneyGroup(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Sysjust 
  /CRITERIA=CI(.95).
split file off.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\moneyprimed.sav' 
  /BREAK=referrer MoneyGroup
  /money_mean=MEAN(Sysjust) 
  /money_sd=SD(Sysjust)
  /money_N_Subj=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\moneyprimed.sav'.
dataset name  moneyd.
dataset activate moneyd.

SORT CASES BY referrer moneyGroup.
CASESTOVARS
  /ID=referrer
  /INDEX=moneyGroup
  /GROUPBY=VARIABLE.

compute d_den=mean(money_sd..00, money_sd.1.00).
EXECUTE.
compute d_num = (money_mean.1.00 - money_mean..00). 
compute moneyd= d_num/d_den.
EXECUTE.

      VARIABLE LABELS moneyd 'System jusitfication - Money prime vs control - Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=moneyd.

      NPTESTS 
        /ONESAMPLE TEST (moneyd) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\moneyprimed.sav'. 

DATASET CLOSE moneyd. 

DATASET ACTIVATE Manylabs.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\moneyGrandd.sav'
  /BREAK=moneygroup
  /session_status=first(session_status)
  /money_mean=MEAN(Sysjust) 
  /money_sd=SD(Sysjust)
  /money_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\moneyGrandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=moneygroup 
  /GROUPBY=VARIABLE.

compute moneyGrandd=(money_mean.1.00-money_mean..00)/(sqrt(mean(money_sd..00*money_sd..00, money_sd.1.00*money_sd.1.00))).
exe.
compute moneyLB=moneyGrandd-(2.58*money_sd.1.00/(sqrt(money_N.1.00))).
compute moneyUB=moneyGrandd+(2.58*money_sd..00/(sqrt(money_N..00))).
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\moneyGrandd.sav'.

DATASET ACTIVATE Manylabs.

**imagined contact**

split file off.
use all.

ALTER TYPE 
   imaginedexplicit1 to imaginedexplicit4 (F1.0).
EXECUTE.

alter type task_id.3 to task_id.24 (A15). 

COMPUTE ContactGroup = 0.
IF     (INDEX(UPCASE(task_id.3),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.4),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.5),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.6),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.7),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.8),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.9),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.10),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.11),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.12),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.13),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.14),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.15),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.16),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.17),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.18),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.19),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.20),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.21),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.22),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.23),'IMAGINEDA') EQ 1) ContactGroup = 1.
IF     (INDEX(UPCASE(task_id.24),'IMAGINEDA') EQ 1) ContactGroup = 1.
EXECUTE.

VALUE LABELS ContactGroup 0 'Control group' 1 'Contact group'.

RELIABILITY 
  /VARIABLES= imaginedexplicit1 to imaginedexplicit4
  /SCALE('ALL VARIABLES') ALL 
  /MODEL=ALPHA 
  /STATISTICS=DESCRIPTIVE SCALE 
  /SUMMARY=TOTAL.

compute Imagineddv= mean(imaginedexplicit1 to imaginedexplicit4). 
exe.

variable labels Imagineddv 'intentions to interact with muslims -dv  for imagined contact'. 

**ALL THE FOLLOWING ts ARE CONTACT - CONTROL**

T-TEST GROUPS=ContactGroup(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Imagineddv 
  /CRITERIA=CI(.99).

split file layered by sample.
T-TEST GROUPS=ContactGroup(1 0) 
  /MISSING=ANALYSIS 
  /VARIABLES=Imagineddv 
  /CRITERIA=CI(.95).
split file off.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\contactd.sav' 
  /BREAK=referrer ContactGroup
  /contact_mean=MEAN(Imagineddv) 
  /contact_sd=SD(Imagineddv)
  /contact_N_Subj=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\contactd.sav'.
dataset name  Contactd.
dataset activate Contactd.

SORT CASES BY referrer ContactGroup.
CASESTOVARS
  /ID=referrer
  /INDEX=ContactGroup
  /GROUPBY=VARIABLE.

compute d_den=mean(contact_sd..00, contact_sd.1.00).
EXECUTE.
compute d_num = (contact_mean.1.00 - contact_mean..00). 
compute contactd= d_num/d_den.
EXECUTE.

      VARIABLE LABELS contactd 'Intentions of contact with Islam - Imagined contact vs control - Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=contactd.

      NPTESTS 
        /ONESAMPLE TEST (contactd) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\contactd.sav'. 

DATASET CLOSE contactd. 

DATASET ACTIVATE Manylabs.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\contactGrandd.sav'
  /BREAK=contactgroup
  /session_status=first(session_status)
  /contact_mean=MEAN(ImaginedDV) 
  /contact_sd=SD(ImaginedDV)
  /contact_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\contactGrandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=contactgroup 
  /GROUPBY=VARIABLE.

compute contactGrandd=(contact_mean.1.00-contact_mean..00)/(sqrt(mean(contact_sd..00*contact_sd..00, contact_sd.1.00*contact_sd.1.00))).
exe.
compute contactLB=contactGrandd-(2.58*contact_sd.1.00/(sqrt(contact_N.1.00))).
compute contactUB=contactGrandd+(2.58*contact_sd..00/(sqrt(contact_N..00))).
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\contactGrandd.sav'.

dataset activate Manylabs.


*** IAT***

split file off. 
use all.

alter type iatexplicitart1 to iatexplicitmath6 (F1.0).

autorecode sex /into partgender /print.

variable labels partgender 'participant gender'. 
value labels partgender 1 'no response' 2 'female' 3 'male'. 

compute IATfilter = 1.
if iat_exclude =1 IATfilter=0.
EXECUTE.
filter by IATfilter.
EXECUTE.

variable labels IATfilter 'Filter by this variable to exclude ss form the analysis of the IAT study (see manuscript for exclusion criteria)'.

des meanerror d_art. 
freq partgender iat_exclude.


**ALL THE FOLLOWING ts ARE MALES - FEMALES**

T-TEST GROUPS=partgender(2 3) 
  /MISSING=ANALYSIS 
  /VARIABLES=d_art 
  /CRITERIA=CI(.99).

split file layered by sample. 
T-TEST GROUPS=partgender(2 3) 
  /MISSING=ANALYSIS 
  /VARIABLES=d_art 
  /CRITERIA=CI(.95).
split file off.


AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\iatd.sav' 
  /BREAK=referrer partgender
  /D_mean=MEAN(d_art) 
  /D_sd=SD(d_art)
  /D_N_Subj=N.


get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\iatd.sav'.
dataset name  IATD.
dataset activate IATD.

select if partgender GE 2.
exe.

SORT CASES BY referrer partgender.
CASESTOVARS
  /ID=referrer
  /INDEX=partgender
  /GROUPBY=VARIABLE.

compute d_den=mean(D_sd.2, D_sd.3).
EXECUTE.
compute d_num = (D_mean.2 - D_mean.3). 
compute IATd= d_num/d_den.
EXECUTE.

     VARIABLE LABELS IATd 'Math-Arts IAT effect (D) Women vs Men- Standardized mean difference'. 
      GRAPH 
        /HISTOGRAM=IATd.

      NPTESTS 
        /ONESAMPLE TEST (IATd) KOLMOGOROV_SMIRNOV(UNIFORM=SAMPLE ) 
        /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
        /CRITERIA ALPHA=0.05 CILEVEL=95.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\iatd.sav'. 

DATASET CLOSE iatd. 

DATASET ACTIVATE Manylabs.

AGGREGATE
  /OUTFILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\iatGrandd.sav'
  /BREAK=partgender
  /session_status=first(session_status)
  /iat_mean=MEAN(d_art) 
  /iat_sd=SD(d_art)
  /iat_N=N.

get file='C:\Users\Vianello\Documents\Dropbox\Manylabs\iatGrandd.sav'.

CASESTOVARS 
  /ID=session_status 
  /INDEX=partgender 
  /GROUPBY=VARIABLE.

compute iatGrandd=(iat_mean.2-iat_mean.3)/(sqrt(mean(iat_sd.2*iat_sd.2, iat_sd.3*iat_sd.3))).
exe.
compute iatLB=iatGrandd-(2.58*iat_sd.2/(sqrt(iat_N.2))).
compute iatUB=iatGrandd+(2.58*iat_sd.3/(sqrt(iat_N.3))).
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\iatGrandd.sav'.

dataset activate Manylabs.

RELIABILITY 
  /VARIABLES= iatexplicitart1 to iatexplicitart6
  /SCALE('ALL VARIABLES') ALL 
  /MODEL=ALPHA 
  /STATISTICS=DESCRIPTIVE SCALE 
  /SUMMARY=TOTAL.

RELIABILITY 
  /VARIABLES= iatexplicitmath1 to iatexplicitmath6
  /SCALE('ALL VARIABLES') ALL 
  /MODEL=ALPHA 
  /STATISTICS=DESCRIPTIVE SCALE 
  /SUMMARY=TOTAL.

recode iatexplicitart1 to iatexplicitmath6 (-3=7) (-2=6) (-1=5) (0=4) (1=3) (2=2) (1=1) (else=copy). 
EXECUTE.

compute IATexpart=mean(iatexplicitart1 to iatexplicitart6). 
compute IATexpmath=mean(iatexplicitmath1 to iatexplicitmath6).
exe.

recode iatexplicitmath1 to iatexplicitmath6 (1=7) (7=1) (6=2) (2=6) (3=5) (5=3) (4=4).
EXECUTE.

RELIABILITY 
  /VARIABLES= iatexplicitart1 to iatexplicitmath6
  /SCALE('ALL VARIABLES') ALL 
  /MODEL=ALPHA 
  /STATISTICS=DESCRIPTIVE SCALE 
  /SUMMARY=TOTAL.

compute IATexp.overall=mean(iatexplicitart1 to iatexplicitmath6).
execute.

des IATexpart IATexpmath IATexp.overall.

count totexpmissed= iatexplicitart1 to iatexplicitmath6 (SYSMIS).
EXECUTE.

FREQUENCIES totexpmissed. 

compute IATEXPfilter=1.
if totexpmissed GT 0 IATEXPfilter=0. 
if iat_exclude =1 IATEXPfilter=0.
EXECUTE.
filter by IATEXPfilter. 

variable labels IATEXPfilter 'filter by this var to exclude participants who did not respond to a particular item for both math and arts explicit items'. 

correlations d_art IATexpart IATexpmath IATexp.overall.  

split file layered by sample. 
correlations d_art IATexp.overall.  
split file off. 
use all. 


GRAPH /HISTOGRAM gambfaldv. 
GRAPH /HISTOGRAM sunkDV. 
GRAPH /HISTOGRAM Anchoring1. 
GRAPH /HISTOGRAM Anchoring2. 
GRAPH /HISTOGRAM Anchoring3. 
GRAPH /HISTOGRAM Anchoring4. 
GRAPH /HISTOGRAM allowedforbidden. 
GRAPH /HISTOGRAM Sysjust. 
GRAPH /HISTOGRAM Imagineddv. 
GRAPH /HISTOGRAM d_art. 
GRAPH /HISTOGRAM reciprocityus. 
GRAPH /HISTOGRAM quote. 
GRAPH /HISTOGRAM scales. 
GRAPH /HISTOGRAM flagdv. 
GRAPH /HISTOGRAM gainlossDV. 

filter off.

***match files of effect sizes**

GET 
  FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\allowedforbiddend.sav'. 
DATASET NAME effectsizes.
DATASET CLOSE Manylabs.
DATASET ACTIVATE effectsizes.

MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring1d.sav' 
   /BY referrer . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring2d.sav' 
   /BY referrer . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring3d.sav' 
   /BY referrer . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring4d.sav' 
   /BY referrer . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\contactd.sav' 
   /BY referrer . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\flagprimed.sav' 
   /BY referrer. 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\gainlossd.sav' 
   /BY referrer . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\gambfald.sav' 
   /BY referrer . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\iatd.sav' 
   /BY referrer. 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\moneyprimed.sav' 
   /BY referrer . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\quoted.sav' 
   /BY referrer . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\reciprocityd.sav' 
   /BY referrer . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\scalesd.sav' 
   /BY referrer . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\sunkd.sav' 
   /BY referrer . 
EXECUTE.


**add IE corrs**

compute IATr=0.
compute IATr_n=0.
if referrer='abington' IATr=.207.
if referrer='abington' IATr_n=78.
if referrer='brasilia' IATr=.422.
if referrer='brasilia' IATr_n=114.
if referrer='charles' IATr=.294.
if referrer='charles' IATr_n=81.
if referrer='conncoll' IATr=.52.
if referrer='conncoll' IATr_n=91.
if referrer='csun' IATr=.207.
if referrer='csun' IATr_n=87.
if referrer='help' IATr=.326.
if referrer='help' IATr_n=94.
if referrer='ithaca' IATr=.392.
if referrer='ithaca' IATr_n=84.
if referrer='jmu' IATr=.496.
if referrer='jmu' IATr_n=161.
if referrer='ku' IATr=.276.
if referrer='ku' IATr_n=104.
if referrer='laurier' IATr=.392.
if referrer='laurier' IATr_n=102.
if referrer='lse' IATr=.37.
if referrer='lse' IATr_n=266.
if referrer='luc' IATr=.448.
if referrer='luc' IATr_n=132.
if referrer='mcdaniel' IATr=.401.
if referrer='mcdaniel' IATr_n=90.
if referrer='msvu' IATr=.416.
if referrer='msvu' IATr_n=62.
if referrer='mturk' IATr=.330.
if referrer='mturk' IATr_n=896.
if referrer='osu' IATr=.256.
if referrer='osu' IATr_n=89.
if referrer='oxy' IATr=.396.
if referrer='oxy' IATr_n=114.
if referrer='pi' IATr=.346.
if referrer='pi' IATr_n=1228.
if referrer='psu' IATr=.41.
if referrer='psu' IATr_n=88.
if referrer='qccuny' IATr=.392.
if referrer='qccuny' IATr_n=95.
if referrer='qccuny2' IATr=-.448.
if referrer='qccuny2' IATr_n=12.
if referrer='sdsu' IATr=.347.
if referrer='sdsu' IATr_n=152.
if referrer='swps' IATr=.305.
if referrer='swps' IATr_n=73.
if referrer='swpson' IATr=.416.
if referrer='swpson' IATr_n=162.
if referrer='tamu' IATr=.312.
if referrer='tamu' IATr_n=181.
if referrer='tamuc' IATr=.315.
if referrer='tamuc' IATr_n=82.
if referrer='tamuon' IATr=.444.
if referrer='tamuon' IATr_n=203.
if referrer='tilburg' IATr=.348.
if referrer='tilburg' IATr_n=78.
if referrer='ufl' IATr=.598.
if referrer='ufl' IATr_n=124.
if referrer='unipd' IATr=.516.
if referrer='unipd' IATr_n=132.
if referrer='uva' IATr=.542.
if referrer='uva' IATr_n=78.
if referrer='vcu' IATr=.424.
if referrer='vcu' IATr_n=99.
if referrer='wisc' IATr=.505.
if referrer='wisc' IATr_n=89.
if referrer='wku' IATr=.381.
if referrer='wku' IATr_n=92.
if referrer='wl' IATr=.306.
if referrer='wl' IATr_n=89.
if referrer='wpi' IATr=.363.
if referrer='wpi' IATr_n=82.
EXECUTE.

compute IATr_d=2*IATr/sqrt(1-IATr*IATr). 
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\Results\effectsizes.all.sav' .



*******match files of overall effect sizes*******



***match files of effect sizes**

GET 
  FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\allowedforbiddenGrandd.sav'. 
DATASET NAME overalleffectsizes.
DATASET ACTIVATE overalleffectsizes.

MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring1Grandd.sav' 
   /BY session_status . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring2Grandd.sav' 
   /BY session_status .  
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring3Grandd.sav' 
   /BY session_status .  
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\anchoring4Grandd.sav' 
   /BY session_status   . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\contactGrandd.sav' 
   /BY session_status. 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\flagGrandd.sav' 
   /BY session_status  . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\gainlossGrandd.sav' 
   /BY session_status . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\gambfalGrandd.sav' 
   /BY session_status   . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\iatGrandd.sav' 
   /BY session_status   . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\moneyGrandd.sav' 
   /BY session_status  . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\quoteGrandd.sav' 
   /BY session_status   . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\reciprocityGrandd.sav' 
   /BY session_status  . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\scalesGrandd.sav' 
   /BY session_status  . 
EXECUTE.
MATCH FILES /FILE=* 
  /FILE='C:\Users\Vianello\Documents\Dropbox\Manylabs\sunkGrandd.sav' 
   /BY session_status  . 
EXECUTE.

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\effectsizes.overall.all.sav' .



VARSTOCASES
  /ID=id
  /MAKE Mean1 FROM allowedforbidden_mean..00 anchoring1_mean..00 anchoring2_mean..00 
    anchoring3_mean..00 anchoring4_mean..00 contact_mean..00 flag_mean..00 gainloss_mean..00 
    gambfal_mean..00 iat_mean.2 money_mean..00 quote_mean..00 reciprocity_mean..00 scales_mean..00 sunkcost_mean..00
  /MAKE Mean2 FROM allowedforbidden_mean.1.00 anchoring1_mean.1.00 anchoring2_mean.1.00 
    anchoring3_mean.1.00 anchoring4_mean.1.00 contact_mean.1.00 flag_mean.1.00 gainloss_mean.1.00 
    gambfal_mean.1.00 iat_mean.3 money_mean.1.00 quote_mean.1.00 reciprocity_mean.1.00 scales_mean.1.00 sunkcost_mean.1.00
  /MAKE SD1 FROM allowedforbidden_sd..00 anchoring1_sd..00 anchoring2_sd..00 anchoring3_sd..00 
    anchoring4_sd..00 contact_sd..00 flag_sd..00 gainloss_sd..00 gambfal_sd..00 iat_sd.2 money_sd..00 
    quote_sd..00 reciprocity_sd..00 scales_sd..00 sunk_sd..00
  /MAKE SD2 FROM allowedforbidden_sd.1.00 anchoring1_sd.1.00 anchoring2_sd.1.00 anchoring3_sd.1.00 
    anchoring4_sd.1.00 contact_sd.1.00 flag_sd.1.00 gainloss_sd.1.00 gambfal_sd.1.00 iat_sd.3 
    money_sd.1.00 quote_sd.1.00 reciprocity_sd.1.00 scales_sd.1.00 sunk_sd.1.00
  /MAKE N1 FROM allowedforbidden_N..00 anchoring1_N..00 anchoring2_N..00 anchoring3_N..00 
    anchoring4_N..00 contact_N..00 flag_N..00 gainloss_N..00 gambfal_N..00 iat_N.2 money_N..00 
    quote_N..00 reciprocity_N..00 scales_N..00 sunk_N..00
  /MAKE N2 FROM allowedforbidden_N.1.00 anchoring1_N.1.00 anchoring2_N.1.00 anchoring3_N.1.00 
    anchoring4_N.1.00 contact_N.1.00 flag_N.1.00 gainloss_N.1.00 gambfal_N.1.00 iat_N.3 money_N.1.00 
    quote_N.1.00 reciprocity_N.1.00 scales_N.1.00 sunk_N.1.00
  /MAKE Overalld FROM allowedforbiddenGrandd anchoring1Grandd anchoring2Grandd anchoring3Grandd 
    anchoring4Grandd contactGrandd flagGrandd gainlossGrandd gambfalGrandd iatGrandd moneyGrandd 
    quoteGrandd reciprocityGrandd scalesGrandd sunkGrandd
  /MAKE LB FROM allowedforbiddenLB anchoring1LB anchoring2LB anchoring3LB anchoring4LB contactLB 
    flagLB gainlossLB gambfalLB iatLB moneyLB quoteLB reciprocityLB scalesLB sunkLB
  /MAKE UB FROM allowedforbiddenUB anchoring1UB anchoring2UB anchoring3UB anchoring4UB contactUB 
    flagUB gainlossUB gambfalUB iatUB moneyUB quoteUB reciprocityUB scalesUB sunkUB
  /INDEX=Indice1(Mean1) 
  /KEEP=session_status 
  /NULL=KEEP
  /COUNT=count "N_cases".

save outfile='C:\Users\Vianello\Documents\Dropbox\Manylabs\Results\effectsizes.overall.all.sav' /drop id session_status count.



DATASET ACTIVATE effectsizes.
DATASET DECLARE means.SDs.unweighted.
AGGREGATE
  /OUTFILE='means.SDs.unweighted'
  /BREAK=
  /allowedforbiddend_mean=MEAN(allowedforbiddend) 
  /Anchoring1d_mean=MEAN(Anchoring1d) 
  /Anchoring2d_mean=MEAN(Anchoring2d) 
  /Anchoring3d_mean=MEAN(Anchoring3d) 
  /Anchoring4d_mean=MEAN(Anchoring4d) 
  /contactd_mean=MEAN(contactd) 
  /flagd_mean=MEAN(flagd) 
  /gainlossd_mean=MEAN(gainlossd) 
  /gambfald_mean=MEAN(gambfald) 
  /IATd_mean=MEAN(IATd) 
  /moneyd_mean=MEAN(moneyd) 
  /quoted_mean=MEAN(quoted) 
  /reciprocityd_mean=MEAN(reciprocityd) 
  /scalesd_mean=MEAN(scalesd) 
  /Sunkd_mean=MEAN(Sunkd) 
  /IATr_d_mean=MEAN(IATr_d)
  /allowedforbiddend_SD=SD(allowedforbiddend) 
  /Anchoring1d_SD=SD(Anchoring1d) 
  /Anchoring2d_SD=SD(Anchoring2d) 
  /Anchoring3d_SD=SD(Anchoring3d) 
  /Anchoring4d_SD=SD(Anchoring4d) 
  /contactd_SD=SD(contactd) 
  /flagd_SD=SD(flagd) 
  /gainlossd_SD=SD(gainlossd) 
  /gambfald_SD=SD(gambfald) 
  /IATd_SD=SD(IATd) 
  /moneyd_SD=SD(moneyd) 
  /quoted_SD=SD(quoted) 
  /reciprocityd_SD=SD(reciprocityd) 
  /scalesd_SD=SD(scalesd) 
  /Sunkd_SD=SD(Sunkd) 
  /IATr_d_SD=SD(IATr_d).

dataset activate means.SDs.unweighted.

VARSTOCASES
  /ID=id
  /MAKE Mean FROM allowedforbiddend_mean Anchoring1d_mean Anchoring2d_mean Anchoring3d_mean 
    Anchoring4d_mean contactd_mean flagd_mean gainlossd_mean gambfald_mean IATd_mean moneyd_mean 
    quoted_mean reciprocityd_mean scalesd_mean Sunkd_mean IATr_d_mean
  /MAKE SD FROM allowedforbiddend_SD Anchoring1d_SD Anchoring2d_SD Anchoring3d_SD Anchoring4d_SD 
    contactd_SD flagd_SD gainlossd_SD gambfald_SD IATd_SD moneyd_SD quoted_SD reciprocityd_SD 
    scalesd_SD Sunkd_SD IATr_d_SD
  /INDEX=Indice1(Mean) 
  /KEEP=
  /NULL=KEEP.

compute LB.mean= Mean-2.58*SD/sqrt(36).
compute UB.mean= Mean+2.58*SD/sqrt(36).
EXECUTE.

save outfile ='C:\Users\Vianello\Documents\Dropbox\Manylabs\Results\sample.means.SDs.CI.sav'.


******investigate qccuny cases***

 * USE ALL.
 * COMPUTE filter_$=(referrer='qccuny2').
 * VARIABLE LABELS filter_$ 'qccuny2 (FILTER)'.
 * VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
 * FORMATS filter_$ (f1.0).
 * FILTER BY filter_$.
 * EXECUTE.

 * FREQUENCIES compensation recruitment separatedornot  ethnicity order meanlatency meanerror block2_meanerror
block3_meanerror block5_meanerror block6_meanerror lat11 lat12 lat21 lat22 sd1 sd2 d_art1 d_art2 d_art
/STATISTICS mean STDDEV median. 


