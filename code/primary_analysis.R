# Title: Superiority of PID Primary Analysis File -----

# Notes: ----
    #* Description: Script for the primary analyses to examine the predictive superiority of partisan identification on vote choice
    #* Updated: 2022 - 04 - 27
    #* Updated by: dcr 

# Setup: ----
    #* Modularly load functions from packages
#install.packages('box')#package used for modularly loading functions from packages
box::use(
    tibble = tibble[as_tibble, tibble],
    ggplot2 = ggplot2[ggplot, geom_line, aes, labs, theme_bw, geom_point, geom_hline, ggsave],
    dplyr = dplyr[filter],
    urca = urca[ur.df, ur.pp, ur.ers, ur.kpss, summary],
    vrtest = vrtest[Auto.VR],
    forecast = forecast[arfima,tslm,forecast],
    modelsummary = modelsummary[modelsummary],
    dynamac = dynamac[dynardl, dynardl.auto.correlated, pssbounds],
    patchwork = patchwork[...],
    fracdiff = fracdiff[diffseries],
    xtable = xtable[xtable]
)
    #* Source script that cleans all of the datasets and will load the series
source('code/combined_data.R')
ts = as_tibble(combined) #rename the resulting dataframe and call it ts

# Hunting NA's ----
voteDemNaN = subset(ts, ts$voteDem == 'NaN') # missing 1948 and 1954
pidNaN = subset(ts, ts$pid == 'NaN') #missing 1948
ideoNaN = subset(ts, ts$ideo == 'NaN') #missing prior to 1972
femaleNaN = subset(ts, ts$female == 'NaN')#no missing
whiteNaN = subset(ts, ts$white == 'NaN')#missing in 1954
blackNaN = subset(ts, ts$black == 'NaN')#missing in 1954
hispanicNaN = subset(ts, ts$hispanic == 'NaN')#missing in 1954
approvalNaN = subset(ts, ts$approval == 'NaN')
educationNaN = subset(ts, ts$education == 'NaN')
    #* Note: Most NA's come before 1972... filter those out ----
ts = ts |>
    filter(year >= 1972)
# Plot the series ---
seriesPlot = ggplot(data = ts) +
    geom_line(aes(x = year, y = voteDem), linetype = 1) +
    geom_point(aes(x = year, y = voteDem), shape = 1) +
    geom_line(aes(x = year, y = pid), linetype = 2) +
    geom_point(aes(x = year, y = pid), shape = 1) +
    geom_line(aes(x = year, y = ideo), linetype = 3) +
    geom_point(aes(x = year, y = ideo), shape = 1) + 
    geom_line(aes(x = year, y = female), linetype = 1) +
    geom_point(aes(x = year, y = female), shape = 2) +
    geom_line(aes(x = year, y = white), linetype = 2) +
    geom_point(aes(x = year, y = white), shape = 2) +
    geom_line(aes(x = year, y = black), linetype = 3) +
    geom_point(aes(x = year, y = black), shape = 2) +
    geom_line(aes(x = year, y = hispanic), linetype = 1) +
    geom_point(aes(x = year, y = hispanic), shape = 3) +
    geom_line(aes(x = year, y = approval), linetype = 2) +
    geom_point(aes(x = year, y = approval), shape = 3) +
    geom_line(aes(x = year, y = education), linetype = 3) +
    geom_point(aes(x = year, y = education), shape = 3) +
    theme_bw() +
    labs(y = '', x = 'Year', caption = 'Data Source: American National Election Studies and Cooperative Congressional Election Studies')

# Unit root testing ----

    #* Vote Choice ----
summary(ur.df(ts$voteDem, type = 'none', lags = 10, selectlags = 'BIC'))#I(1)
kpssVoteDem = ur.kpss(ts$voteDem, type = 'mu')#Just below maybe I(1) or I(0)...
kpssVoteDemDiff = ur.kpss(diff(ts$voteDem), type = 'mu')#I(0)
voteDemVRPre = Auto.VR(ts$voteDem) # Reject Null: Presence of Positive serial correlation
dVoteDem = arfima(ts$voteDem) # What we estimate d as
diffVoteDem = diffseries(ts$voteDem, dVoteDem$d) # fractionally difference the series
voteDemVRPost = Auto.VR(diffVoteDem) # do the variance ratio test on the differenced series. Now stationary

    #* PID ----
summary(ur.df(ts$pid, type = 'none', lags = 10, selectlags = 'BIC'))#I(1)
kpssPID = ur.kpss(ts$pid, type = 'mu')#I(1)
kpssPIDDiff = ur.kpss(diff(ts$pid), type = 'mu')#I(0)
pidVRPre = Auto.VR(ts$pid) # Reject Null: Presence of Positive serial correlation
dPID = arfima(ts$pid) # What we estimate d as
diffPID = diffseries(ts$pid, dPID$d) # fractionally difference the series
pidVRPost = Auto.VR(diffPID) # do the variance ratio test on the differnced series. Now stationary

    #* Ideology ----
summary(ur.df(ts$ideo, type = 'none', lags = 10, selectlags = 'BIC'))#I(1)
kpssIdeo = ur.kpss(ts$ideo, type = 'mu')#I(1)
kpssIdeoDiff = ur.kpss(diff(ts$ideo), type = 'mu')#I(0)
ideoVRPre = Auto.VR(ts$ideo) # Reject Null: Presence of Positive serial correlation
dIdeo = arfima(ts$ideo) # What we estimate d as
diffIdeo = diffseries(ts$ideo, dIdeo$d) # fractionally difference the series
ideoVRPost = Auto.VR(diffIdeo) # do the variance ratio test on the differenced series. now stationary

    #* Female ----
summary(ur.df(ts$female, type = 'none', lags = 10, selectlags = 'BIC'))#I(1)
kpssFemale = ur.kpss(ts$female, type = 'mu')#I(1)
kpssFemaleDiff = ur.kpss(diff(ts$female), type = 'mu')#I(0)
femaleVRPre = Auto.VR(ts$female) # Reject Null: Presence of Positive serial correlation
dFemale = arfima(ts$female) # what we estimate d as
diffFemale = diffseries(ts$female, dFemale$d) # fractionally difference the series
femaleVRPost = Auto.VR(diffFemale) # do the variance ratio test on the differenced series. Now stationary

    #* White ----
summary(ur.df(ts$white, type = 'none', lags = 10, selectlags = 'BIC'))#I(1)
kpssWhite = ur.kpss(ts$white, type = 'mu')#I(1)
kpssWhiteDiff = ur.kpss(diff(ts$white), type = 'mu')#I(0)
whiteVRPre = Auto.VR(ts$white) # Reject Null: Presence of Positive serial correlation
dWhite = arfima(ts$white) # what we estimate d as
diffWhite = diffseries(ts$white, dWhite$d) # fractionally difference the series
whiteVRPost = Auto.VR(diffWhite) # do the variance ratio test on the differenced series. Now stationary

    #* Black ----
summary(ur.df(ts$black, type = 'none', lags = 10, selectlags = 'BIC'))#I(1)
kpssBlack = ur.kpss(ts$black, type = 'mu')#I(0)
blackVRPre = Auto.VR(ts$black) # Reject Null: Presence of Positive serial correlation
dBlack = arfima(ts$black) # what we estimate d as
diffBlack = diffseries(ts$black, dBlack$d) # fractionally difference the series
blackVRPost = Auto.VR(diffBlack) # do the variance ratio test on the differenced series. Now stationary

    #* Presidential Approval ----
summary(ur.df(ts$approval, type = 'none', lags = 10, selectlags = 'BIC'))#I(1)
kpssApproval = ur.kpss(ts$approval, type = 'mu')#Maybe I(1)??
kpssApprovalDiff = ur.kpss(diff(ts$approval), type = 'mu')#I(0)
approvalVRPre = Auto.VR(ts$approval)# Reject Null: Presence of Positive serial correlation
dApproval = arfima(ts$approval) # what we estimate d as
diffApproval = diffseries(ts$approval, dApproval$d) # fractionally difference the series
approvalVRPost = Auto.VR(diffApproval) # do the variance ratio test on the differenced sereies. Now stationary

    #* Education ----
summary(ur.df(ts$education, type = 'none', lags = 10, selectlags = 'BIC'))#I(1)
kpssEducation = ur.kpss(ts$education, type = 'mu')#I(1)
kpssEducationDiff = ur.kpss(diff(ts$education), type = 'mu')#I(0)
educationVRPre = Auto.VR(ts$education) # Reject Null: Presence of Positive serial correlation
dEducation = arfima(ts$education) # what we estimate d as
diffEducation = diffseries(ts$education, dEducation$d) # fractionally difference the series
educationVRPost = Auto.VR(diffEducation) # do the variance ratio test on the differnced series. Now stationary

    #* Add fractionally differenced variables to dataset ----
ts = ts |>
    cbind(diffVoteDem, diffPID, diffIdeo, diffFemale, diffWhite, diffBlack, diffApproval, diffEducation)

    #* Make table of results for unit root testing ----
unitrootDF = tibble(
    Variable = c('voteDem', 'pid', 'ideo', 'female', 'white', 'black', 'approval', 'education'),
    `T Statistic` = c(0, 0, 0, 0, 0, 0, 0, 0)
) # create an empty tribble object
unitrootDFPRE = unitrootDF |>
    mutate(`T Statistic` = case_when(Variable == 'voteDem' ~ pidVRPre$stat, 
    Variable == 'pid' ~ pidVRPre$stat,
    Variable == 'ideo' ~ ideoVRPre$stat,
    Variable == 'female' ~ femaleVRPre$stat,
    Variable == 'white' ~ whiteVRPre$stat,
    Variable == 'black' ~ blackVRPre$stat,
    Variable == 'approval' ~ approvalVRPre$stat,
    Variable == 'education' ~ educationVRPre$stat)) # put the pre-differencing difference variance ratio statistics in tribble object

preTable = xtable(unitrootDFPRE) # make a table using this tribble object
print(preTable, type = 'latex', file = 'tables_figures/vr_test_pre.tex') # save this table as a latex document

unitrootDFPost = unitrootDF |>
    mutate(`T Statistic` = case_when(Variable == 'voteDem' ~ pidVRPost$stat,
    Variable == 'pid' ~ pidVRPost$stat,
    Variable == 'ideo' ~ ideoVRPost$stat,
    Variable == 'female' ~ femaleVRPost$stat,
    Variable == 'white' ~ whiteVRPost$stat,
    Variable == 'black' ~ blackVRPost$stat,
    Variable == 'approval' ~ approvalVRPost$stat,
    Variable == 'education' ~ educationVRPost$stat)) # put the post-differincing variance ratio statistics in tribble object

postTable = xtable(unitrootDFPost) # make a table using this tribble object
print(postTable, type = 'latex', file = 'tables_figures/vr_test_post.tex') # save this table as a latex document

# Model
    #* ARDL(1,1) = ECM
        #** All Variables
ardl = tslm(diffVoteDem ~ lag(voteDem) + diffPID + lag(pid) + diffIdeo + lag(ideo) + diffFemale + lag(female) + diffWhite + lag(white) + diffBlack + lag(black) + diffApproval + lag(approval) + diffEducation + lag(education), data = ts)

lmtest::bptest(ardl) # Bruesch-Pagan test for heteroskedasticity

        #** Just PID
ardlPID = tslm(diffVoteDem ~ lag(voteDem) + diffPID + lag(pid), data = ts_short)

lmtest::bptest(ardlPID) # Bruesch-Pagan test for heteroskedasticity

        #** Tables
models = list('Bias Reduction' = ardl, 'Parsimonious' = ardlPID) # put both models in a list

cm = c('lag(voteDem)' = '$\\text{Vote Democrat}_{t-1}$',
        'diffPID' = '$\\Delta \\text{PID}_t$',
        'lag(pid)' = '$\\text{PID}_{t-1}$',
        'diffIdeo' = '$\\Delta \\text{Ideology}_t$',
        'lag(ideo)' = '$\\text{Ideology}_{t-1}$',
        'diffFemale' = '$\\Delta \\text{Female}_t$',
        'lag(female)' = '$\\text{Female}_{t-1}$',
        'diffWhite' = '$\\Delta \\text{White}_t$',
        'lag(white)' = '$\\text{White}_{t-1}$',
        'diffBlack' = '$\\Delta \\text{Black}_t$',
        'lag(black)' = '$\\text{Black}_{t-1}$',
        'diffApproval' = '$\\Delta \\text{Approval}_t$',
        'lag(approval)' = '$\\text{Approval}_{t-1}$',
        'diffEducation' = '$\\Delta \\text{Education}_t$',
        'lag(education)' = '$\\text{Education}_{t-1}$',
        '(Intercept)' = 'Constant'
    ) # rename the variables to look pretty for my table of the models
gm = list(list('raw' = 'nobs', 'clean' = 'N', 'fmt' = 0),
            list('raw' = 'adj.r.squared', 'clean' = 'Adj. $\\text{R}^2$', 'fmt' = 3),
            list('raw' = 'statistic', 'clean' = 'F-Statistic', 'fmt' = 3)) # specify and relabel what model information I want at the bottom of the table
modelsummary(models, coef_map = cm, gof_map = gm, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), title = "Predictors' effect on vote choice over time", notes = c('Data source: ANES and CCES.', 'Estimates from fractionally integrated ARDL(1,1) model.', 'Standard errors in parentheses.'), escape = FALSE, output = 'tables_figures/models.tex') # make a table for the models using the prior adjustments and save it as a latex document

    #* Figures of residuals
        #** All variables
ardlResiduals = ardl$residuals # grab the residuals
ardlFitted = ardl$fitted.values # grab the fitted values

ardlResidualsPlot = ggplot() +
    geom_point(aes(x =ardlFitted, y = ardlResiduals)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    theme_bw() +
    labs(y = 'Residuals', x = 'Fitted', title = 'Bias reducing model') # plot the residuals on the y axis and fitted values on the x axis

        #** PID Only
ardlPIDResiduals = ardlPID$residuals # grab the residuals
ardlPIDFitted = ardl$fitted.values # grab the fitted values

ardlPIDResidualsPlot = ggplot() +
    geom_point(aes(x = ardlPIDFitted, y = ardlPIDResiduals)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    theme_bw() +
    labs(y = 'Residuals', x = 'Fitted', title = 'Parsimonious model') # plot the residuals on the y axis and fitted values on the x axis

        #** Combined Residual Plot ----
ardlCombinedResidualPlot = ardlPIDResidualsPlot + ardlResidualsPlot + plot_annotation(caption = 'Data Source: ANES and CCES.\n Fractionally integrated ARDL(1,1) models.') # combine the plots

ggsave(ardlCombinedResidualPlot, file = 'tables_figures/residuals.jpeg', dpi = 300) # save the plots

    #* Forecasts
        #** Setting up the datasets
ts_short = ts |>
    filter(year <= 2012) |>
    ts() # git rid of elections after 2012. Save as a ts object rather than data.frame
newdata = ts |>
    filter(year > 2012) |>
    select(year, voteDem, pid, ideo, female, white, black, approval, education, diffPID, diffIdeo, diffFemale, diffWhite, diffBlack, diffApproval, diffEducation) |>
    ts() # get table of post-2012 elections I want to predict the vote share of with the electorate's characteristics for that given election; save it as a ts object rather than a data.frame
        #** Forecasts
            #*** All Variables
ardl = tslm(diffVoteDem ~ lag(voteDem) + diffPID + lag(pid) + diffIdeo + lag(ideo) + diffFemale + lag(female) + diffWhite + lag(white) + diffBlack + lag(black) + diffApproval + lag(approval) + diffEducation + lag(education), data = ts_short) # rerun my model with only the pre-2012 electio ndata

forecast = forecast(ardl, newdata = newdata) # using the reran model, forecast post-2012 elections using the electorate's characteristics
            #*** PID Only
ardlPID = tslm(diffVoteDem ~ lag(voteDem) + diffPID + lag(pid), data = ts_short) # rerun my model withonly the pre-2012 election data

forecastPID = forecast(ardlPID, newdata = newdata) # using the reran model, forecast post-2012 elections using the electorate's characteristics

        #** Tables
forecast_PID_DF = tibble(
    Year = newdata[,1],
    `Predicted` = c(0, 0, 0),
    `True` = c(0, 0, 0),
    `Difference` = c(0, 0, 0)
) |> # make empty tibble object to eventually add my predicted and true values from the forecast , and the difference between them
    mutate(Predicted = forecastPID$mean,
            True = tail(ts$diffVoteDem, n = 3),
            Difference = True - Predicted) # fill out the tibble object with the pid only model results
forecast_DF = tibble(
    Year = newdata[,1],
    `Predicted` = c(0, 0, 0),
    `True` = c(0, 0, 0),
    `Difference` = c(0, 0, 0)
) |> # make empty tibble object to eventually add my predicted and true values from the forecast, and the difference between them
    mutate(Predicted = forecast$mean,
            True = tail(ts$diffVoteDem, n = 3),
            Difference = True - Predicted) # fill out the tibble object with the bias-reducing model results

forecastPIDTable = xtable(forecast_PID_DF) # make the pid only forecast tibble a table
print(forecastPIDTable, type = 'latex', file = 'tables_figures/forecast_pid_table.tex') # save the pid only forecast table as a latex document
forecastTable = xtable(forecast_DF) # make the bias reducing forecast tibble a table
print(forecastTable, type = 'latex', file = 'tables_figures/forecast_table.tex') # save the bias reducing forecast table as a latex document