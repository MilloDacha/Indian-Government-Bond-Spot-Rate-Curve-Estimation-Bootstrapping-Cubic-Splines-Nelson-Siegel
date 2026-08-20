#install.packages("ggplot2")  #remove the first hash-tag if the package is not installed already
library(ggplot2)

#loading up the dataset and preliminary data cleaning
d=read.csv(file.choose(),header = TRUE)   #choose "Indian sovereign.csv" file on the pop-up prompt
str(d)  #overall structure of the dataset
dim(d)  #total dimensions of the data matrix
d$Ask.Price=as.numeric(d$Ask.Price)   #converting the price from character to numeric data type
colSums(is.na(d))   #44 missing values under price variable


#Data cleaning

##removing missing values
d=na.omit(d)    #removed the missing values
colSums(is.na(d))   #44 missing values present under price
dim(d)  #total datapoints got reduced to 956 from earlier 1000
##adding time to maturity (the dataset was collected on 2nd August, 2026)
valuation_date = as.Date("2026-08-02")    #reference date
d$Maturity = as.Date(d$Maturity, format = "%d-%m-%Y")   #changing the data type of 'Maturity' variable from character to date
d$TTM = as.numeric(d$Maturity - valuation_date) / 365.25    #adding the required time to maturity variable
summary(d$TTM)    #summary of the time to maturity in the dataset
##removing option bonds
table(d$Maturity.Type)  #1 Call/Sink option bond present
d=d[-which(d$Maturity.Type=="CALL/SINK"),]
table(d$Maturity.Type)  #No more option bonds present
##checking for bonds outside of the region India
table(d$Country.Region)   #No outside bonds present
##checking for non-INR currency bonds
table(d$Currency)   #All remaining bonds are INR based only
##checking for "India Special Government Bond"
table(d$Name)   #none present
##removing "Reserve Bank of India Sovereign Gold Bond"
table(d$Name)   #6 nos. present
d=d[-which(d$Name=="Reserve Bank of India Sovereign Gold Bond"),]
table(d$Name)   #removed the concerned bond type
##creating separate India Government Bond (not including the coupon strips bond) dataset
d.gsec=d[which(d$Name=="India Government Bond"),]
table(d.gsec$Name)  #only normal gsec present
dim(d.gsec)   #105 datapoints; 12 variables
##creating separate principal strips govt. bonds dataset
d.pStrip=d[which(d$Name=="India Government Bond Principal Strips"),]
table(d.pStrip$Name)  #only principal strips bonds present
dim(d.pStrip)   #17 datapoints; 12 variables
##creating separate coupon strips govt. bonds dataset
d.cStrips=d[which(d$Name=="India Government Bond Coupon Strips"),]
table(d.cStrips$Name)  #only coupon strip bonds present
dim(d.cStrips)   #827 datapoints; 12 variables
##plotting the gsec data points available across different maturity periods
ordered.gsec=d.gsec[order(d.gsec$TTM),]   #arranging as per time to maturity
plot(
  ordered.gsec$TTM,
  ordered.gsec$Ask.Price,
  main = "Indian Government Securities: Price vs Time to Maturity",
  xlab = "Time to Maturity (Years)",
  ylab = "Ask Price",
  pch = 19
)
lower_par = 90
upper_par = 110
abline(h = lower_par, lty = 2)  #showing upper near-par boundary
abline(h = upper_par, lty = 2)  #showing lower near-par boundary
abline(h = 100, lty = 1)  #showing par value
d.gsec.nearpar = d.gsec[d.gsec$Ask.Price>=lower_par & d.gsec$Ask.Price<=upper_par,] #filtered near-par gsec bonds
summary(d.gsec.nearpar$Ask.Price)
plot(
  sort(d.gsec.nearpar$TTM),
  rep(1, nrow(d.gsec.nearpar)),
  main = "Maturity Coverage of Near-Par G-Secs",
  xlab = "Time to Maturity (Years)",
  ylab = "",
  yaxt = "n",
  pch = 19
)
hist(d.gsec.nearpar$Ask.Price,main = "Frequency distribution of near-par bonds",xlab = "Ask Price") 
100*nrow(d.gsec.nearpar)/nrow(d.gsec)   #around 94.29% of datapoints retained, i.e. 99 out of 105


#Generating datapoints for regression methods

##For normal coupon paying gsec bonds
FV = 100
valuation_date = as.Date("2026-08-02")
###Semi-annual target maturities
target_maturities = seq(
  from = 0.5,
  to = floor(max(d.gsec.nearpar$TTM) * 2) / 2,
  by = 0.5
)
maturity_tolerance = 0.10   #Maximum allowed deviation from target maturity
###Creating buckets of the target maturities and grouping the bonds with maturities close to them accordingly
eligible_list = list()    #Create an empty list to store eligible bonds
####Creates a list of eligible bonds, i.e. bonds falling under the target maturity+-tolerance level
for (T in target_maturities) {
  eligible = d.gsec.nearpar[abs(d.gsec.nearpar$TTM - T) <= maturity_tolerance,]
  if (nrow(eligible) > 0) {
    eligible$Target_Maturity = T
    eligible$Maturity_Difference = abs(eligible$TTM - T)
    eligible_list[[as.character(T)]] = eligible
  }
}
d.eligible = do.call(rbind, eligible_list)  #converting the list to data frame of eligible bonds
rownames(d.eligible) = NULL
d.eligible[,c("Coupon","Maturity","TTM","Ask.Price","Target_Maturity","Maturity_Difference")] #listing out all eligible bonds
table(d.eligible$Target_Maturity)   #nos. of eligible bonds present under each target maturity value/bucket
####Choosing the bonds which are closest to par value for each target maturity bucket
selected_bonds = do.call(
  rbind,
  lapply(
    split(d.eligible, d.eligible$Target_Maturity),
    function(x) {
      x[which.min(abs(x$Ask.Price - FV)), ]
    }
  )
)
rownames(selected_bonds) = NULL
####Lists out all the selected near-par bonds
selected_bonds[,c("Coupon","TTM","Target_Maturity","Ask.Price")]  #there are certain target maturity buckets which don't have any bonds under them
plot(
  selected_bonds$Target_Maturity,
  selected_bonds$TTM,
  pch = 19,
  xlab = "Target Maturity (Years)",
  ylab = "Actual TTM (Years)",
  main = "Target vs Actual Maturity of Selected G-Secs"
)
abline(0, 1, lty = 2)

##For principal strips bonds
###Spot rates directly from the principal strips 
d.pStrip$Spot_Rate = (FV/d.pStrip$Ask.Price)^(1/d.pStrip$TTM) - 1
d.pStrip$Spot_Rate_Pct = 100*d.pStrip$Spot_Rate
d.pStrip[order(d.pStrip$TTM),c("Maturity","TTM","Ask.Price","Spot_Rate_Pct")] #all the principal strip bonds and their spot rates

##Spot rate generation using hybrid bootstrap with interpolation and principal strips bonds' spot rates
###Finalize limits and sort
max_maturity = 29.0
selected_bonds = selected_bonds[selected_bonds$Target_Maturity <= max_maturity, ]
selected_bonds = selected_bonds[order(selected_bonds$Target_Maturity), ]
###Define the "Huge Gaps" to be filled by Principal Strips, i.e. gaps between 8.0 and 11.0; 15.0 and 27.5
strips_gaps = c(seq(8.5, 10.5, by=0.5), seq(15.5, 27.0, by=0.5))
###Interpolate these specific gaps directly from the Principal Strips curve
p_strip_interp = approx(x = d.pStrip$TTM, y = d.pStrip$Spot_Rate, xout = strips_gaps)
###Initialize the curve dataframe with the Principal Strips gaps pre-filled
known_curve = data.frame(
  Target_Maturity = p_strip_interp$x,
  Spot_Rate = p_strip_interp$y,
  Tag = "Principal Strips Extracted"
)
###Iterate through selected G-Sec bonds to bootstrap
last_gsec_mat = 0
last_gsec_rate = 0 
for (i in 1:nrow(selected_bonds)) {
  current_bond = selected_bonds[i, ]
  T_current = current_bond$Target_Maturity
  coupon_rate = current_bond$Coupon / 100
  ask_price = current_bond$Ask.Price
  # Cash flow timeline for this bond (multiples of 0.5)
  cf_times = seq(0.5, T_current, by=0.5)
  # Identify which cash flows need G-Sec interpolation
  # These are times NOT yet in known_curve, and NOT the current maturity itself
  existing_times = known_curve$Target_Maturity
  missing_times = cf_times[!(cf_times %in% existing_times) & cf_times != T_current]
  # Objective function to solve for S_current (Spot Rate at T_current)
  pv_diff <- function(S_current) {
    PV_total = 0
    for (t in cf_times) {
      if (t %in% existing_times) {
        # Rate is already known (either from previous G-Sec or Strips pre-fill)
        S_t = known_curve$Spot_Rate[known_curve$Target_Maturity == t]
      } else if (t == T_current) {
        # The rate we are currently solving for
        S_t = S_current
      } else {
        # Linear interpolation for small missing gaps (e.g., 3.0 and 3.5)
        fraction = (t - last_gsec_mat) / (T_current - last_gsec_mat)
        S_t = last_gsec_rate + fraction * (S_current - last_gsec_rate)
      }
      # Cash flow amount: Semi-annual coupon, plus principal at maturity
      CF = (coupon_rate * 100) / 2
      if (t == T_current) {
        CF = CF + 100
      }
      # Discount to Present Value (Annual compounding matching Strips logic)
      DF = 1 / ((1 + S_t)^t)
      PV_total = PV_total + (CF * DF)
    }
    return(PV_total - ask_price)
  }
  # Use uniroot to find the exact spot rate that prices the bond correctly
  # (Searches between 0.1% and 20% yields)
  root_res = uniroot(pv_diff, lower = 0.001, upper = 0.20)
  S_current = root_res$root
  # Add the bootstrapped rate to known_curve
  known_curve = rbind(known_curve, data.frame(
    Target_Maturity = T_current,
    Spot_Rate = S_current,
    Tag = "Bootstrapped G-Sec"
  ))
  # Calculate and formally add the inferred G-Sec interpolated rates
  for (t in missing_times) {
    fraction = (t - last_gsec_mat) / (T_current - last_gsec_mat)
    S_t = last_gsec_rate + fraction * (S_current - last_gsec_rate)
    
    known_curve = rbind(known_curve, data.frame(
      Target_Maturity = t,
      Spot_Rate = S_t,
      Tag = "Interpolated G-Sec"
    ))
  }
  # Update last known G-Sec reference for the next loop iteration
  last_gsec_mat = T_current
  last_gsec_rate = S_current
}
###Clean, sort, and calculate percentage
known_curve = known_curve[order(known_curve$Target_Maturity), ]
rownames(known_curve) = NULL
known_curve$Spot_Rate_Pct = known_curve$Spot_Rate * 100
###View the final assembled dataset
known_curve  
###Create a color-coded plot of the final Hybrid Spot Rate Curve
ggplot(known_curve, aes(x = Target_Maturity, y = Spot_Rate_Pct, color = Tag, shape = Tag)) +
  geom_point(size = 3.5, alpha = 0.9) +
  geom_line(color = "gray60", linetype = "dashed", size = 0.7, aes(group = 1)) +
  scale_color_manual(values = c(
    "Bootstrapped G-Sec" = "#1f77b4",          # Solid Blue
    "Interpolated G-Sec" = "#ff7f0e",          # Orange
    "Principal Strips Extracted" = "#2ca02c"   # Green
  )) +
  scale_shape_manual(values = c(
    "Bootstrapped G-Sec" = 16,                 # Circle
    "Interpolated G-Sec" = 17,                 # Triangle
    "Principal Strips Extracted" = 15          # Square
  )) +
  labs(
    title = "Hybrid Bootstrapped Spot Rate Curve",
    subtitle = "G-Sec Bootstrap combined with Linear Interpolation & Principal Strips Fills",
    x = "Target Maturity (Years)",
    y = "Spot Rate (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )
###Adding raw spot rates from principal strips bonds in the graph
####Filter Principal Strips to match the max maturity range (<= 29 years)
pstrip_plot_data <- d.pStrip[d.pStrip$TTM <= max_maturity, ]
####Build the combined visualization
ggplot() +
  # Dashed underlying trendline for the assembled known_curve
  geom_line(
    data = known_curve, 
    aes(x = Target_Maturity, y = Spot_Rate_Pct), 
    color = "gray60", 
    linetype = "dashed", 
    size = 0.7
  ) +
  # Known curve points categorized by origin tag
  geom_point(
    data = known_curve, 
    aes(x = Target_Maturity, y = Spot_Rate_Pct, color = Tag, shape = Tag), 
    size = 3, 
    alpha = 0.85
  ) +
  # Overlay raw Principal Strips as RED CROSSES (shape = 4 is 'X', shape = 3 is '+')
  geom_point(
    data = pstrip_plot_data, 
    aes(x = TTM, y = Spot_Rate_Pct), 
    color = "red", 
    shape = 4,      # Use shape = 3 for '+' crosses instead of 'X'
    size = 4, 
    stroke = 1.5    # Controls line thickness of the cross
  ) +
  # Color and Shape mappings for the hybrid curve tags
  scale_color_manual(values = c(
    "Bootstrapped G-Sec"          = "#1f77b4",  # Blue
    "Interpolated G-Sec"          = "#ff7f0e",  # Orange
    "Principal Strips Extracted"  = "#2ca02c"   # Green
  )) +
  scale_shape_manual(values = c(
    "Bootstrapped G-Sec"          = 16,  # Filled Circle
    "Interpolated G-Sec"          = 17,  # Filled Triangle
    "Principal Strips Extracted"  = 15   # Filled Square
  )) +
  labs(
    title = "Hybrid Spot Rate Curve vs. Raw Principal Strips",
    subtitle = "Red 'X' crosses indicate raw Principal Strips spot rates across maturities",
    x = "Time to Maturity / Target Maturity (Years)",
    y = "Spot Rate (%)",
    color = "Curve Component",
    shape = "Curve Component"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 13)
  )


#Modelling

fit_data = known_curve[order(known_curve$Target_Maturity),]   #Arrange observations by maturity
fit_data[,c("Target_Maturity", "Spot_Rate", "Spot_Rate_Pct", "Tag")]    #Check the data

##Cubic spline fitting
###Creating the curve function, it takes in the maturity and outputs spot rate
cubic_spline = splinefun(
  x = fit_data$Target_Maturity,
  y = fit_data$Spot_Rate,
  method = "natural"
)
###creating dense maturity grid for smooth curve
maturity_grid = seq(min(fit_data$Target_Maturity),max(fit_data$Target_Maturity),by = 0.01)
###generating the spline curve datapoints from the spline curve function for graph
spline_curve = data.frame(
  Maturity = maturity_grid,
  Spot_Rate = cubic_spline(maturity_grid)
)
spline_curve$Spot_Rate_Pct=100*spline_curve$Spot_Rate
plot(
  spline_curve$Maturity,
  spline_curve$Spot_Rate_Pct,
  type = "l",
  lwd = 2,
  xlab = "Time to Maturity (Years)",
  ylab = "Spot Rate (%)",
  main = "Cubic Spline Fitted Spot Rate Curve",
  col="blue"
)
points(fit_data$Target_Maturity,fit_data$Spot_Rate_Pct,pch = 1)
legend(
  "bottomright",                                # Places legend in bottom right (change to "topright" if overlapping data)
  legend = c("Cubic Spline Fitted Line", "Observed/Bootstrapped"), # Labels for the legend
  col = c("blue", "black"),                     # Blue for the spline line, black for points
  lty = c(1, NA),                               # Solid line for Spline, no line for Observed
  pch = c(NA, 1),                               # No point for Spline, open circle for Observed
  lwd = c(2, NA),                               # Match line width (lwd = 2) for Spline
  bty = "o"                                     # Adds a box around the legend
)

##Nelson-Siegel method
###creating function to give loading values for a given tau and maturity values
NS_loadings = function(m, tau) {
  # m/tau term from the Nelson-Siegel equation
  x = m / tau
  # First loading
  L1 = (1 - exp(-x)) / x
  # Second loading
  L2 = L1 - exp(-x)
  return(
    data.frame(
      L1 = L1,
      L2 = L2
    )
  )
}
###checking if the function works and can be used with linear regression modelling to get betas
tau = 2   #random tau value
loadings = NS_loadings(fit_data$Target_Maturity,tau)  #getting loading values
NS_model = lm(fit_data$Spot_Rate~loadings$L1+loadings$L2) #linear regression model to get beta values
summary(NS_model)   #works fine
###grid search over different tau values
tau_grid = seq(from = 0.1,to = 10,by = 0.1) #defining the grid search space for tau
NS_results = data.frame() #initialising the output result's dataframe
for (tau in tau_grid) {
  # Calculate Nelson-Siegel loadings
  loadings = NS_loadings(fit_data$Target_Maturity,tau)
  # Estimate beta coefficients
  model = lm(fit_data$Spot_Rate~loadings$L1+loadings$L2)
  # Store results
  NS_results = rbind(
    NS_results,
    data.frame(
      Tau = tau,
      Beta0 = coef(model)[1],
      Beta1 = coef(model)[2],
      Beta2 = coef(model)[3],
      SSR = sum(residuals(model)^2),
      R_Squared = summary(model)$r.squared
    )
  )
}
str(NS_results)   #dataframe containing results from corresponding 100 tau values
NS_results[which.min(NS_results$SSR),]  #the best tau value
###optimal Nelson-Siegel parameters
best_NS = NS_results[which.min(NS_results$SSR),]
best_tau = best_NS$Tau
best_beta0 = best_NS$Beta0
best_beta1 = best_NS$Beta1
best_beta2 = best_NS$Beta2
best_tau  #optimal tau value is 10
best_beta0  #corresponding optimal beta0 value is 0.09419659
best_beta1  #corresponding optimal beta1 value is -0.03580717
best_beta2  #corresponding optimal beta2 value is -0.006493508
###plotting SSR vs tau to visualise best tau
plot(
  NS_results$Tau,
  NS_results$SSR,
  type = "l",
  lwd = 2,
  xlab = expression(tau),
  ylab = "Sum of Squared Residuals",
  main = "Nelson-Siegel: Selection of Tau"
)
points(best_tau,best_NS$SSR,pch = 19)
###plotting the fitted line
x_grid=maturity_grid / best_tau
L1_grid=(1-exp(-x_grid))/x_grid
L2_grid=L1_grid-exp(-x_grid)
NS_curve = data.frame(
  Maturity = maturity_grid,
  Spot_Rate = best_beta0 + best_beta1*L1_grid + best_beta2*L2_grid
)
NS_curve$Spot_Rate_Pct=100*NS_curve$Spot_Rate
plot(
  NS_curve$Maturity,
  NS_curve$Spot_Rate_Pct,
  type = "l",
  lwd = 2,
  xlab = "Time to Maturity (Years)",
  ylab = "Spot Rate (%)",
  main = "Nelson-Siegel Fitted Spot Rate Curve",
  col = "red"
)
points(fit_data$Target_Maturity,fit_data$Spot_Rate_Pct,pch = 1)
legend(
  "bottomright",                         # Places legend in bottom right (adjust to "topright" if needed)
  legend = c("Fitted Line", "Observed"), # Labels for the legend
  col = c("red", "black"),             # Line and point colours
  lty = c(1, NA),                        # Solid line for Fitted, no line for Observed
  pch = c(NA, 1),                        # No point for Fitted, open circle for Observed
  lwd = c(2, NA),                        # Match line width for Fitted
  bty = "o"                              # Adds a box around the legend ("n" removes it)
)


##Comparison between the two methods

###Plotting both the curves along with the actual/bootstrapped/interpolated/extracted spot rates
plot(
  fit_data$Target_Maturity,
  100 * fit_data$Spot_Rate,
  pch = 1,
  xlab = "Time to Maturity (Years)",
  ylab = "Spot Rate (%)",
  main = "Spot Rate Curve: Cubic Spline vs Nelson-Siegel",
  xlim = range(fit_data$Target_Maturity),
  ylim = range(
    c(
      100 * fit_data$Spot_Rate,
      spline_curve$Spot_Rate_Pct,
      NS_curve$Spot_Rate_Pct
    )
  )
)
####Cubic spline fitted curve
lines(
  spline_curve$Maturity,
  spline_curve$Spot_Rate_Pct,
  lwd = 2,
  col = "blue"
)
####Nelson-Siegel fitted curve
lines(
  NS_curve$Maturity,
  NS_curve$Spot_Rate_Pct,
  lwd = 2,
  lty = 2,
  col = "red"
)
####Legend
legend(
  "bottomright",
  legend = c(
    "Observed / Bootstrap Spot Rates",
    "Cubic Spline",
    "Nelson-Siegel"
  ),
  pch = c(1, NA, NA),        # open circle for observed points
  lty = c(NA, 1, 2),         # no line, solid, dashed
  lwd = c(NA, 2, 2),
  col = c("black", "blue", "red"),
  pt.cex = 0.8,
  cex = 0.7,
  bty = "n"                  # removes legend box
)

###Comparing the fitness of both curves
####CUBIC SPLINE PERFORMANCE
spline_pred=cubic_spline(fit_data$Target_Maturity)
spline_residuals=fit_data$Spot_Rate-spline_pred
spline_SSE=sum(spline_residuals^2)
spline_RMSE=sqrt(mean(spline_residuals^2))
spline_MAE=mean(abs(spline_residuals))
spline_SSE  #0 SSE (Squared Sum of Errors)
spline_RMSE   #0 RMSE (Root Mean Squared Errors)
spline_MAE  #0 MAE (Mean Absolute Error)
####NELSON-SIEGEL PERFORMANCE
NS_pred =
  best_beta0 +
  best_beta1 *
  (
    (1 - exp(-fit_data$Target_Maturity / best_tau)) /
      (fit_data$Target_Maturity / best_tau)
  ) +
  best_beta2 *
  (
    (
      (1 - exp(-fit_data$Target_Maturity / best_tau)) /
        (fit_data$Target_Maturity / best_tau)
    ) -
      exp(-fit_data$Target_Maturity / best_tau)
  )
NS_residuals=fit_data$Spot_Rate-NS_pred
NS_SSE=sum(NS_residuals^2)
NS_RMSE=sqrt(mean(NS_residuals^2))
NS_MAE =mean(abs(NS_residuals))
NS_SSE  #SSE = 0.0001089353
NS_RMSE   #RMSE = 0.001370473
NS_MAE  #MAE = 0.0009702676
####Combined performance comparison
model_comparison = data.frame(
  Model = c("Cubic Spline","Nelson-Siegel"),
  SSE = c(spline_SSE,NS_SSE),
  RMSE = c(spline_RMSE,NS_RMSE),
  MAE = c(spline_MAE,NS_MAE)
)
model_comparison  #tabular format showcasing of performances





