This project constructs the zero-coupon term structure of Indian sovereign debt as of August 2, 2026 and compares two standard curve-fitting techniques: the natural cubic spline and the Nelson-Siegel parametric model. 
The starting dataset, pulled from Bloomberg, contained 1,000 raw price quotes spanning India Government Bonds, Principal Strips and Coupon Strips. 
After cleaning, filtering for near-par instruments to limit coupon and tax-timing distortion and bootstrapping semi-annual spot rates (supplemented by principal-strip zero rates in the two maturity bands where G-Sec liquidity is thin), a hybrid spot rate curve spanning 0.5 to 29 years was assembled.
The cubic spline, being an interpolant rather than a regression, reproduces every input point exactly (SSE = RMSE = MAE = 0). Nelson-Siegel, constrained to three shape parameters and a decay parameter, fits the same data with an RMSE of about 13.7 basis points. 
The two curves track each other closely through the belly of the curve (roughly 5 to 20 years) and diverge mainly at the long end, where the spline follows a sharp dip-and-jump in the bootstrapped rates that the smoother Nelson-Siegel form cannot reproduce. 
The choice between the two is therefore less about which model “wins” statistically and more about what the curve will be used for — exact repricing of the underlying instruments versus a smooth, economically interpretable summary of the curve.

Files:-
1) The "FIS project analysis.R" contains the R script.
2) "Indian sovereign.xlsx" is the raw data from Bloomberg while "Indian sovereign.csv" is it's csv version.
3) "Report.pdf" is the full report.
4) "PPT.pdf" is the presentation slides.
