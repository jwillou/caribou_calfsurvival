library(survival)
library(survminer)
library(geosphere)
library(scales)
library(vioplot)
library(ggplot2)
library(see)
library(tidyverse)
library(reshape2)
library(ggdist)
library(dplyr)
library(gghalves)
library(broom)

setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - archive/Caribou/MCH Calf Survival/")
#setwd("/Users/jrw0107/Google Drive/My Drive/Willoughby lab/projects - archive/Caribou/calfsurvival_figures/")
#setwd("/Users/rsattler/OneDrive - Western EcoSystems Technology Inc/Documents/RStudio/Mulchatna/calf")
par(mfrow = c(1, 1))

#read in and manipulate data
data = read.table("MCH Calf Fate and Survival.csv", header=TRUE, sep=",")
data = data[!is.na(data$Weight) & !is.na(data$BY) & (data$SexCode == 0 | data$SexCode == 1), ]
data = data[data$Weight > 0 & data$BY >= 2000, ]
data$Year = data$BY
data$Sex = ifelse(data$SexCode == 0, "Female", "Male")
data$Sex = as.factor(data$Sex)

# Prepare survival variables 
data$Status = as.numeric(data$FateCode)
data$Status[data$Status == 2] = NA  # remove unknown fate
data = data[!is.na(data$Status), ]
data$DaysAlive = as.numeric(as.character(data$MinDaysAlive.FromBirth))
data$DaysAlive[data$DaysAlive>140] = 140
data = data[!is.na(data$DaysAlive), ] #Need to limit this to 6 months. currently calling the whole dataset.
data$Status[data$DaysAlive>=140] = 0
data$YearFactor = as.factor(data$Year)

# extract birth year
data$DOB = as.Date(data$DOB, format = "%m/%d/%y")
data$YearBorn = format(data$DOB, "%Y")
data$YearBorn = as.numeric(data$YearBorn)

#survival summary
data.summary = read.table("survival_0day6mo.csv", header=T, sep=",") #updated with Renae's new input 4/21/2025

#capture data - currently 2021 only!
cdata  = data[data$Year==2021,]
cdata0 = cdata[cdata$EstAgeAtCap==0,]
cdata1 = cdata[cdata$EstAgeAtCap==1,]
cdata2 = cdata[cdata$EstAgeAtCap==2,]
cdata3 = cdata[cdata$EstAgeAtCap==3,]
fdata  = read.table("focusfollow_cleaned_edited.csv", header=TRUE, sep=",")

#location data reformatted
locdata = read.csv("Cap calf Surival loc Master_2025_editformat2.csv", header = TRUE, stringsAsFactors = FALSE)

###Question 1: (calf weights): Did calf weights increase over the study?####
# Fit linear model combined
data$CGcode = as.factor(data$CGcode)
allmodel = lm(Weight ~ Year + CGcode + Sex , data = data)
summary(allmodel)

#Fit linear model per CG
westcg <-
  data |>
  filter(CGcode == 0)
westmodel = lm(Weight ~ Year, data = westcg)
summary(westmodel)

eastcg <-
  data |>
  filter(CGcode == 1)
eastmodel = lm(Weight ~ Year, data = eastcg)
summary(eastmodel)

# Prepare data
eastcg = data %>% filter(CGcode == 1) %>% mutate(Group = "East")
westcg = data %>% filter(CGcode == 0) %>% mutate(Group = "West")
combined = bind_rows(eastcg, westcg)
combined$BY = as.factor(combined$BY)

# Plot both groups with regression lines
ggplot(combined, aes(x = Year, y = Weight, color = Group)) +
  geom_jitter(alpha = 0.6, width = 0.25, height = 0, size = 1.5) +  # Jittered points
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +               # Regression lines
  scale_color_manual(values = c("West" = "firebrick3", "East" = "dodgerblue3")) +
  labs(
    x = "Year",
    y = "Weight",
    title = "Linear Model Fit: Weight ~ Year (East and West Groups)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
  )

# Prepare data
mdata = data %>% filter(Sex =="Male")
fdata = data %>% filter(Sex =="Female")
combined = bind_rows(mdata, fdata)
combined$BY = as.factor(combined$BY)

# Plot both groups with regression lines
ggplot(combined, aes(x = Year, y = Weight, color = Sex)) +
  geom_jitter(alpha = 0.6, width = 0.25, height = 0, size = 1.5) +  # Jittered points
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +               # Regression lines
  scale_color_manual(values = c("Male" = "navy", "Female" = "orchid")) +
  labs(
    x = "Year",
    y = "Weight",
    title = "Linear Model Fit: Weight ~ Year (Male and Female Groups)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
  )

# Kaplan-Meier by CGcode - keep or not?
surv_obj <- Surv(data$DaysAlive, data$Status)
fit_km <- survfit(surv_obj ~ CGcode, data = data)
ggsurvplot(
  fit_km,
  data = data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Days Alive",
  ylab = "Survival Probability",
  legend.title = "Calving Group",
  palette = c("firebrick3", "dodgerblue3")
)


###Question 2: (Calf survival summer): Did calf survival increase over the study period in east vs. west?####
colors3 = c("firebrick3", "dodgerblue3")
groups  = c("West", "East") 

#make the plot box
plot(-100,-100, xlim=c(2011,2021), ylim=c(0,1), xlab="Year", ylab="Survival")
for(g in 1:length(groups)){
  temp = data.summary[data.summary$Population==as.character(groups[g]),]
  polygon(x=c(temp$Year, rev(temp$Year)), y=c(temp$L95, rev(temp$U95)), col = alpha(colors3[g], 0.3), border=F)
  points(x=temp$Year, y=temp$Survival, pch=20, col=colors3[g])
  lines(x=temp$Year, y=temp$Survival, lty=1, lwd=2, col=colors3[g])
}

#is east vs. west calf survival statistically different? 
survall = lm(DaysAlive ~ Year + CGcode, data = data)
summary(survall)  

westcg <-
  data |>
  filter(CGcode == 0)
westsurvmodel = lm(DaysAlive ~ Year, data = westcg)
summary(westsurvmodel)

eastcg <-
  data |>
  filter(CGcode == 1)
eastsurvmodel = lm(DaysAlive ~ Year, data = eastcg)
summary(eastsurvmodel)

# Kaplan-Meier surv object
SurvObj = with(data, Surv(time = DaysAlive, event = Status))
fit = survfit(SurvObj ~ CGcode, data = data)
colorsFM = c("firebrick3", "dodgerblue3")
ggsurvplot(fit, data = data, conf.int = TRUE, ggtheme = theme_minimal(), xlab = "Days from Birth", ylab = "Survival Probability", legend.title = "CGcode", palette = c(colorsFM[1], colorsFM[2]))
mvsf.surv = survdiff(SurvObj ~ CGcode, data = data)
print(mvsf.surv)

###Question 3: what were the strongest predictors of calf survival to 6 months?####
#find best calf survival model with AIC
response = "Surv(DaysAlive, Status)"
vars     =  c("Weight", "Sex", "YearFactor", "CGcode")
model_results = data.frame(Model = character(), AIC = numeric(), stringsAsFactors = FALSE)

# Loop over all model combinations
for (k in 1:length(vars)) {
  combos = combn(vars, k, simplify = FALSE)
  for (combo in combos) {
    formula_str = paste(response, "~", paste(combo, collapse = " + "))
    model = coxph(as.formula(formula_str), data = data)
    model_results = rbind(model_results, data.frame(Model = formula_str,  AIC = AIC(model), stringsAsFactors = FALSE))
  }
}

# Add the full model, sort, show me the money
model_results = model_results[order(model_results$AIC), ]
model_results$DeltaAIC = round(model_results$AIC - min(model_results$AIC), 3)
aic_weights = exp(-0.5 * model_results$DeltaAIC)
model_results$AICweight = round(aic_weights / sum(aic_weights), 3)
print(model_results)
write.table(model_results, "model_results.csv", row.names=F, col.names=T, sep=",")

#plot
surv_model = coxph(Surv(DaysAlive, Status) ~ Weight + Sex + YearFactor + CGcode, data = data)
summary(surv_model)
cox_model = coxph(Surv(DaysAlive, Status) ~ Sex + Weight + YearBorn + CGcode, data = data)
summary(cox_model)
cox.zph(cox_model)

tidy(surv_model, exponentiate = TRUE, conf.int = TRUE)
tidy(surv_model, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  labs(
    x = "Covariate",
    y = "Hazard Ratio (95% CI)",
    title = "Effect of Covariates on Calf Survival"
  ) +
  theme_minimal()

# plot with the best colors
colors9 = c("darkorchid4", "navy", "dodgerblue3", "forestgreen", "chartreuse2", "goldenrod1", "orange", "darkorange3", "firebrick4")
data$YearBorn <- as.factor(data$YearBorn) # Make sure YearBorn is a factor to preserve order in the legend
#this is by sex
p <- ggsurvplot(
  survfit(Surv(DaysAlive, Status) ~ YearBorn + Sex, data = data),
  data = data,
  conf.int = FALSE,
  palette = colors9,
  xlab = "Days from Birth",
  ylab = "Survival Probability",
  legend.title = "Year",
  facet.by = "Sex",
  ggtheme = theme_bw() +  # theme_bw has panel.border
    theme(
      panel.grid = element_blank(),           # no gridlines
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # full black box
      axis.line = element_blank(),             # prevent axis lines duplicating border
      panel.spacing = unit(1, "lines"),         # more space between panels
      strip.background = element_blank(),      # clean facet label background
      strip.text = element_text(size = 12)      # control facet label size
    )
)
print(p)

#this is by location
p <- ggsurvplot(
  survfit(Surv(DaysAlive, Status) ~ YearBorn + CGcode, data = data),
  data = data,
  conf.int = FALSE,
  palette = colors9,
  xlab = "Days from Birth",
  ylab = "Survival Probability",
  legend.title = "Year",
  facet.by = "CGcode",
  ggtheme = theme_bw() +  # theme_bw has panel.border
    theme(
      panel.grid = element_blank(),           # no gridlines
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # full black box
      axis.line = element_blank(),             # prevent axis lines duplicating border
      panel.spacing = unit(1, "lines"),         # more space between panels
      strip.background = element_blank(),      # clean facet label background
      strip.text = element_text(size = 12)      # control facet label size
    )
)
print(p)

#Question 4: Did survival differ between Male and Female calves?####
# Kaplan-Meier surv object
SurvObj = with(data, Surv(time = DaysAlive, event = Status))
fit = survfit(SurvObj ~ Sex, data = data)
colorsFM = c("violetred", "darkturquoise")
ggsurvplot(fit, data = data, conf.int = TRUE, xlab = "Days from Birth", ylab = "Survival Probability", legend.title = "Sex", palette = c(colorsFM[1], colorsFM[2]),
           ggtheme = theme_bw() +  # theme_bw has panel.border
             theme(
               panel.grid = element_blank(),           # no gridlines
               panel.border = element_rect(color = "black", fill = NA, size = 1),  # full black box
               axis.line = element_blank(),             # prevent axis lines duplicating border
               panel.spacing = unit(1, "lines"),         # more space between panels
               strip.background = element_blank(),      # clean facet label background
               strip.text = element_text(size = 12)      # control facet label size
             )
)
           
mvsf.surv = survdiff(SurvObj ~ SexCode, data = data)
print(mvsf.surv)

###Question 5 (Calf survival winter): What are calf survival rates over winter?
#DID NOT RUN IN R#
# we can change to R but I wasn't sure which datasheet

###Question 6 (effects of capture): does the distance moved 24, 48, 72 hours post capture differ from movement of non-captured calves?####
cols4 = c("black", "firebrick4", "darkorange3", "goldenrod1", "chartreuse3")
intervals = list(c(0, 1), c(1, 2), c(2, 3), c(3, 4), c(4, 5), c(5, 6), c(6, 7))
results = NULL

# Loop over each interval
for (interval in intervals) {
  start_day = interval[1]
  end_day = interval[2]
  
  # Column names
  startStatusCol = paste0("Day", start_day, "Status")
  startLatCol    = paste0("Day", start_day, "Lat")
  startLongCol   = paste0("Day", start_day, "Long")
  endStatusCol   = paste0("Day", end_day, "Status")
  endLatCol      = paste0("Day", end_day, "Lat")
  endLongCol     = paste0("Day", end_day, "Long")
  
  # Get coordinates
  startLat  = as.numeric(locdata[[startLatCol]])
  startLong = as.numeric(locdata[[startLongCol]])
  endLat    = as.numeric(locdata[[endLatCol]])
  endLong   = as.numeric(locdata[[endLongCol]])
  
  # Only use rows where both statuses are 1 (alive) AND coords are complete
  alive     = locdata[[startStatusCol]] == 1 & locdata[[endStatusCol]] == 1
  has_coords = !is.na(startLat) & !is.na(startLong) & !is.na(endLat) & !is.na(endLong)
  valid = alive & has_coords
  
  # Calculate Haversine distance in meters
  distance = mapply(function(lat1, lon1, lat2, lon2) {
    distHaversine(c(lon1, lat1), c(lon2, lat2))
  }, startLat[valid], startLong[valid], endLat[valid], endLong[valid])
  
  # Store results for this interval
  res_interval = data.frame(
    ID = locdata$ID[valid],
    EstAgeatCapture = locdata$EstAgeatCapture[valid],
    MinDaysAlive = locdata$MinDaysAlive[valid],
    Interval = paste0("Day", start_day, "-Day", end_day),
    StartStatus = locdata[[startStatusCol]][valid],
    EndStatus = locdata[[endStatusCol]][valid],
    StartLat = startLat[valid],
    StartLong = startLong[valid],
    EndLat = endLat[valid],
    EndLong = endLong[valid],
    Distance = distance,
    stringsAsFactors = FALSE
  )
  results = rbind(results, res_interval)
}

# Post-processing
results = results[!is.na(results$Distance),]
results$MinDaysAlive[results$MinDaysAlive > 10] = 10
results$captured = ifelse(results$EstAgeatCapture == -1, 0, 1)
results$Distance = results$Distance / 1000  # Convert to km

# Quick summaries
table(results$EstAgeatCapture, results$Interval)
table(results$captured, results$Interval)

# Plotting setup
intervals_unique = sort(unique(results$Interval))
cols = cols4

# comparing movement distances by capture age and days old
results$DaySinceBirth = as.numeric(sub("Day(\\d+)-.*", "\\1", results$Interval))
results$CaptureLabel <- ifelse(results$EstAgeatCapture == -1, "Not Captured", paste0("Captured Day ", results$EstAgeatCapture))
groups = unique(results$CaptureLabel)
days   = sort(unique(results$DaySinceBirth))
group_colors = setNames(cols4[1:length(groups)], groups)

# Initialize lists to hold data
mean_matrix = matrix(NA, nrow = length(days), ncol = length(groups), dimnames = list(paste0("Day", days), groups))
se_matrix   = mean_matrix

# Fill in mean and SE
for (i in seq_along(groups)) {
  g <- groups[i]
  for (j in seq_along(days)) {
    d <- days[j]
    sub <- results[results$DaySinceBirth == d & results$CaptureLabel == g, ]
    if (nrow(sub) > 0) {
      mean_matrix[j, i] <- mean(sub$Distance, na.rm = TRUE)
      se_matrix[j, i] <- sd(sub$Distance, na.rm = TRUE) / sqrt(nrow(sub))
    }
  }
}
perc_diff = data.frame(age = row.names(mean_matrix))
perc_diff$Captured0 = (mean_matrix[,1] - mean_matrix[,2])/mean_matrix[,2] *100
perc_diff$Captured1 = (mean_matrix[,3] - mean_matrix[,2])/mean_matrix[,2] *100
perc_diff$Captured2 = (mean_matrix[,4] - mean_matrix[,2])/mean_matrix[,2] *100
perc_diff$Captured3 = (mean_matrix[,5] - mean_matrix[,2])/mean_matrix[,2] *100


# Force capture group order
group_order  = c("Not Captured", paste0("Captured Day ", sort(unique(results$EstAgeatCapture[results$EstAgeatCapture != -1]))))
groups       = intersect(group_order, unique(results$CaptureLabel))
group_colors = setNames( c("black", "firebrick4", "darkorange3", "goldenrod1", "chartreuse3"), groups)

# plot movement and survival plots on the same scale
par(mfrow = c(2, 1),           # 2 rows, 1 column
    mar = c(1.5, 4, 2, 1),     # margins: bottom, left, top, right (reduced bottom/top)
    oma = c(3, 0, 1, 0))       # outer margin for shared x-axis label

plot(NULL, xlim = range(days), ylim = c(0, max(mean_matrix + se_matrix, na.rm = TRUE)), xlab = "Day Since Birth", ylab = "Mean Distance Moved (km)", main = NULL)
for (i in seq_along(groups)) {
  group = groups[i]
  means = mean_matrix[, group]
  ses   = se_matrix[, group]
  valid_idx = which(!is.na(means) & !is.na(ses) & is.finite(means + ses))
  
  if (length(valid_idx) >= 2) {
    x_vals = days[valid_idx]
    y_vals = means[valid_idx]
    y_upper = y_vals + ses[valid_idx]
    y_lower = y_vals - ses[valid_idx]

    polygon(c(x_vals, rev(x_vals)),c(y_lower, rev(y_upper)),col = adjustcolor(group_colors[group], alpha.f = 0.2),border = NA)
    lines(x_vals, y_vals, col = group_colors[group], lwd = 2)
    points(x_vals, y_vals, pch = 16, col = group_colors[group])
  }
}

#focus only on 2021 for survival
cols4 = c("black", "firebrick4", "darkorange3", "goldenrod1", "chartreuse3")
cdata  = read.table("MCH Calf Fate and Survival.csv", header=TRUE, sep=",")
cdata  = cdata[cdata$BY==2021,]
cdata0 = cdata[cdata$EstAgeAtCap==0,]
cdata1 = cdata[cdata$EstAgeAtCap==1,]
cdata2 = cdata[cdata$EstAgeAtCap==2,]
cdata3 = cdata[cdata$EstAgeAtCap==3,]
fdata  = read.table("focusfollow_cleaned_edited.csv", header=TRUE, sep=",")

#combine to single dataset
data2021 = data.frame(group=c(rep(-1, nrow(fdata)), rep(0, nrow(cdata0)), rep(1, nrow(cdata1)), rep(2, nrow(cdata2)), rep(3, nrow(cdata3))),
                  daysalive = c(fdata$DaysAlive, cdata0$MinDaysAlive.FromBirth, cdata1$MinDaysAlive.FromBirth, cdata2$MinDaysAlive.FromBirth, cdata3$MinDaysAlive.FromBirth),
                  status = c(fdata$Status, cdata0$FateCode, cdata1$FateCode, cdata2$FateCode, cdata3$FateCode))
data2021$status[data2021$status==2] = NA
data2021 = data2021[!is.na(data2021$status),]
data2021$daysalive = as.numeric(as.character(data2021$daysalive))
data2021$daysalive[data2021$daysalive>10] = 10

# Create a "start" time variable f
data2021$start = ifelse(data2021$group == -1, 0,
                    ifelse(data2021$group == 0, 0,
                           ifelse(data2021$group == 1, 1,
                                  ifelse(data2021$group == 2, 2,
                                         ifelse(data2021$group == 3, 3, NA)))))

# Helper function to plot a survival curve with confidence intervals
plot_group_curve <- function(fit, capture_day, col) {
  # Extract times, survival estimates, and confidence limits
  times <- fit$time
  survs <- fit$surv
  lower <- fit$lower
  upper <- fit$upper
  
  # Subset only times >= capture_day
  keep <- times >= capture_day
  times <- times[keep]
  survs <- survs[keep]
  lower <- lower[keep]
  upper <- upper[keep]
  
  # If the first time is after capture_day, prepend a point at capture_day
  if (length(times) == 0 || times[1] > capture_day) {
    if (length(survs) > 0) {
      times <- c(capture_day, times)
      survs <- c(survs[1], survs)
      lower <- c(lower[1], lower)
      upper <- c(upper[1], upper)
    } else {
      times <- capture_day
      survs <- 1
      lower <- 1
      upper <- 1
    }
  }
  
  # Plot the main survival curve as a step function
  lines(times, survs, type = "s", col = col, lwd = 2)
  
  # Create a grid from capture_day to the maximum time to construct the polygon
  grid <- seq(capture_day, max(times), length.out = 200)
  upper_step <- stepfun(times, c(upper[1], upper))
  lower_step <- stepfun(times, c(lower[1], lower))
  
  # Create polygon coordinates that follow the step functions
  poly_x <- c(grid, rev(grid))
  poly_y <- c(upper_step(grid), rev(lower_step(grid)))
  polygon(poly_x, poly_y, col = adjustcolor(col, alpha.f = 0.2), border = NA)
}

# Kaplan-Meier surv object
# Fit separate survfit objects for each group using left truncation
bump = 1e-5
fitNeg = survfit(Surv(time = start, time2 = daysalive + bump, event = status) ~ 1, data = data2021[data2021$group == -1,])
fit0   = survfit(Surv(time = start, time2 = daysalive + bump, event = status) ~ 1, data = data2021[data2021$group == 0,])
fit1   = survfit(Surv(time = start, time2 = daysalive + bump, event = status) ~ 1, data = data2021[data2021$group == 1,])
fit2   = survfit(Surv(time = start, time2 = daysalive + bump, event = status) ~ 1, data = data2021[data2021$group == 2,])

#summaries
summary_neg = summary(fitNeg)
summary_0   = summary(fit0)
summary_1   = summary(fit1)
summary_2   = summary(fit2)

# Convert summaries to data frames
df_neg = data.frame(time = summary_neg$time,surv = summary_neg$surv, lower = summary_neg$lower,upper = summary_neg$upper,n.risk = summary_neg$n.risk,n.event = summary_neg$n.event,n.censor = summary_neg$n.censor)
df_0 = data.frame(time = summary_0$time,surv = summary_0$surv,lower = summary_0$lower,upper = summary_0$upper, n.risk = summary_0$n.risk,n.event = summary_0$n.event,n.censor = summary_0$n.censor)
df_1 = data.frame(time = summary_1$time,surv = summary_1$surv,lower = summary_1$lower,upper = summary_1$upper,n.risk = summary_1$n.risk,n.event = summary_1$n.event, n.censor = summary_1$n.censor)
df_2 = data.frame(time = summary_2$time,surv = summary_2$surv,lower = summary_2$lower,upper = summary_2$upper,n.risk = summary_2$n.risk,n.event = summary_2$n.event,n.censor = summary_2$n.censor)

# pull from survival above, for just 2021 above
df_neg$Group = "Not Captured"
df_0$Group   = "Captured Day 0"
df_1$Group   = "Captured Day 1"
df_2$Group   = "Captured Day 2"
surv_df_all = rbind(df_neg, df_0, df_1, df_2)#, df_3)
surv_groups = c("Not Captured", "Captured Day 0", "Captured Day 1", "Captured Day 2")#, "Captured Day 3")
surv_colors = c("Not Captured"     = "black",
                 "Captured Day 0"   = "firebrick4",
                 "Captured Day 1"   = "darkorange3",
                 "Captured Day 2"   = "goldenrod1")
names(surv_colors) = surv_groups

# add survival to 6 days
plot(NULL, xlim = c(0, 6), ylim = c(0.5, 1), xlab = "", ylab = "Survival Probability")
for (g in surv_groups) {
  df <- surv_df_all[surv_df_all$Group == g, ]
  lines(df$time, df$surv, type = "s", lwd = 2, col = surv_colors[g])
  points(df$time, df$surv, pch = 16, col = surv_colors[g])
}
mtext("Day Since Birth", side = 1, outer = TRUE, line = 1, cex = 1)


