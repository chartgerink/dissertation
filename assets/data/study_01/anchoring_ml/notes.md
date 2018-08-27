# Notes within the project of detecting (potential) data fabrication by summary statistics
---
#  May 19, 2015
1. Many Labs 1 (ML1) osf page: https://osf.io/wx7ck/
2. Datasets.zip downloaded from https://osf.io/pqf9r/

# May 20, 2015

1. Syntax.Manylabs.sps downloaded from https://osf.io/er9xg/ on May 20, 2015
2. Ran lines 230-262; 287-289 of `manylabs1_anchoring/Syntax.Manylabs.sps` on to clean anchoring data in `manylabs1_anchoring/Datasets/Full_Dataset_De-Identified.sav`
3. Did not run 293-303 because miles and km are substantially different. Due to multiplication, variances of distributions are exponentially different (i.e., if Y = 2*X, var(Y) = 2^2*var(X)). Since the study will ask participants the questions in miles, only those are looked at in the ML data.
4. Ran lines 305-342
5. Saved datafile to `manylabs1_anchoring/chjh ml1_anchoring cleaned.sav`, made it read only, and git commit.
6. Ran lines 1639-1642 to code `partgender`. Forgot this before. New commit.
7. Ran `chjh ml1_project script.R` to create the data file for this project at the respondent level (still needs to be made into the summary statistics level)
8. git commit
9. Had to remove the working directory file because I am stupid: working directory has to be set before this would work anyway
10. moved the script for saving the responses to the data folder, to have data and the scripts that give the data in the same place, with the same filename.
11. Was finally able to create a dataset of summary statistics after reshuffling the data a bit and using `ddply()`

# May 21, 2015

1. Started writing the testing functions to detect (potential) data fabrication. These include 4 methods: (i) Simonsohn method, (ii) equal-n, (iii) reversed Fisher method for gender effects, and (iv) combination test of methods i-iii with Fisher method.
2. Noted error in `chjh_ml1_summarystats.csv`: forgot to differentiate per study. Now corrected in code and datafile (checked and there are 16 means, 16 sds, and 16 sample sizes per lab = 48 summary stats per lab).
3. Done programming first version of Simonsohn method. Tested based on the example in the Simonsohn (2013) paper and got similar results (p = .00015 in the paper, I got .0001; .00016; .0002. Fluctuation is just due to Monte Carlo simulation process)

# February 19, 2016
1. Redoing the syntax running because the km to miles should be included. It does not alter variance structure or relative differences so we are OK there.
2. Open `Full_Dataset_De-Identified.sav` and `Syntax.Manylabs.sps`
3. Run lines 230-342 to clean response data
4. Run lines 1639-1642 to create gender variable (1 = no response, 2 = female, 3 = male)
5. Save the resulting file to `chjh ml1_anchoring cleaned.sav` and the syntax used to `chjh syntax.sps`.