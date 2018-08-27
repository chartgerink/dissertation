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


autorecode sex /into partgender /print.

variable labels partgender 'participant gender'. 
value labels partgender 1 'no response' 2 'female' 3 'male'. 