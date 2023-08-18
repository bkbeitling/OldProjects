*Survivor data: Predicting whether a contestant is invited to compete on a subsequent season;
*Variables of interest: Age, Placement, IndImmunities, TC_Pct, Idol_Score, Average_Confessionals;

*Import data;
proc import out= survivor 
            datafile= "C:\Users\b864b226\Desktop\Stat 835\SurvivorAnalysis\Survivor Dataset.csv" 
            dbms=CSV REPLACE;
     getnames=yes;
     datarow=2; 
run;

*Removing the recent seasons (where returnees = 0);
data survivornorecent;
	set survivor;
	if Season > 34 then delete;
run;

*Removing all except recent seasons (where returnees = 0) for prediction purposes;
data survivorrecent;
	set survivor;
	if Season < 35 then delete;
run;

*Univariate logistic regression of significant predictors above and predictors;
proc logistic data = survivornorecent desc;
	class InvitedBack;
	model InvitedBack = Age;
run;
quit;

proc logistic data = survivornorecent desc;
	class InvitedBack;
	model InvitedBack = Placement;
run;
quit;

proc logistic data = survivornorecent desc;
	class InvitedBack;
	model InvitedBack = IndImmunities;
run;
quit;

proc logistic data = survivornorecent desc;
	class InvitedBack;
	model InvitedBack = TC_Pct;
run;
quit;

proc logistic data = survivornorecent desc;
	class InvitedBack;
	model InvitedBack = Idol_Score;
run;
quit;

proc logistic data = survivornorecent desc;
	class InvitedBack;
	model InvitedBack = Average_Confessionals;
run;
quit;

*Graphing relationship between outcome variable and each predictor;
proc reg data=survivornorecent plots=predictions(X=Placement);
  model InvitedBack = Placement;
quit;

proc reg data=survivornorecent plots=predictions(X=IndImmunities);
  model InvitedBack = IndImmunities;
quit;

proc reg data=survivornorecent plots=predictions(X=TC_Pct);
  model InvitedBack = TC_Pct;
quit;

proc reg data=survivornorecent plots=predictions(X=Idol_Score);
  model InvitedBack = Idol_Score;
quit;

proc reg data=survivornorecent plots=predictions(X=Average_Confessionals);
  model InvitedBack = Average_Confessionals;
quit;

*Scatter plot matrix to check for multicollinearity;
proc sgscatter data = survivornorecent;
    matrix InvitedBack Placement IndImmunities TC_Pct Idol_Score Average_Confessionals;
run;

*Correlation matrix to check for multicollinearity;
proc corr data = survivornorecent;
    var InvitedBack Placement IndImmunities TC_Pct Idol_Score Average_Confessionals;
run;

*Fitting multiple logistic regression model with all variables with univariate significance;
proc logistic data = survivornorecent desc;
  model InvitedBack = Placement IndImmunities TC_Pct Idol_Score Average_Confessionals;
run;
quit;

*Removing Idol_Score due to probable multicollinearity;
proc logistic data = survivornorecent desc;
  model InvitedBack = Placement IndImmunities TC_Pct Average_Confessionals;
  output out=survivorpred p=phat lower=lcl upper=ucl
             predprobs=individual;
run;
quit;

*Adding predicted probability to return to the prediction dataset for recent seasons;
data survivorpred;
	set survivorrecent;
	predictval = 1/(1+exp(-1*(-2.9978 -.0606*Placement + .2075*IndImmunities + 0.0112*TC_Pct + 0.4352*Average_Confessionals)));
run;

*Filtering to a list of likely returnees per predicted probability;
data invitedback (keep = Name Placement IndImmunities TC_Pct Average_Confessionals predictval);
	set survivorpred;
	if predictval < 0.5 then delete;
run;

*Print likely returnees;
proc print data = invitedback;
run;




