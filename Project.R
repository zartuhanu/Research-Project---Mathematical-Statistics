library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(glmnet)
library(nortest)
library(lmtest)

#reading dataset
original_dataset <- read_csv(file.choose())
head(original_dataset)
glimpse(original_dataset)
names(original_dataset)

## data cleaning
#remove irrelevant columns
processed_dataset <- original_dataset %>%
  select(-artists, -total_tracks, -starts_with("t_name"), -starts_with("t_key"), -starts_with("t_mode"), 
         -starts_with("t_ins"), -starts_with("t_live"), -starts_with("t_sig"), -starts_with("t_acous"))

#remove rows with  missing values
processed_dataset <- processed_dataset %>%
  filter(complete.cases(.))
`
#remove rows with release dates in "YYYY-MM" format
processed_dataset <- processed_dataset %>%
  filter(!(nchar(as.character(release_date)) == 7)) 

#add placeholder date for year-only rows 
processed_dataset <- processed_dataset %>%
  mutate(
    release_date_temp = ifelse(nchar(as.character(release_date)) == 4,  
                               paste0(release_date, "-01-01"),  
                               release_date) 
  )

#continue processing with valid dates
processed_dataset <- processed_dataset %>%
  mutate(
    release_date = ymd(release_date_temp)  
  ) %>%
  filter(!is.na(release_date)) 

#extract release year
processed_dataset <- processed_dataset %>%
  mutate(release_year = year(release_date))  

#create the decade column
processed_dataset <- processed_dataset %>%
  mutate(decade = floor(release_year / 10) * 10)  

#remove temporary release date columns
processed_dataset <- processed_dataset %>%
  select(-release_date_temp,-release_date)

# Grouping columns
groups <- list(
  avg_dur = c("t_dur0", "t_dur1", "t_dur2"),
  avg_dance = c("t_dance0", "t_dance1", "t_dance2"),
  avg_energy = c("t_energy0", "t_energy1", "t_energy2"),
  avg_speech = c("t_speech0", "t_speech1", "t_speech2"),
  avg_valence = c("t_val0", "t_val1", "t_val2"),  
  avg_tempo = c("t_tempo0", "t_tempo1", "t_tempo2")  
)

#processing dataset (use the filtered dataset here)
processed_dataset <- reduce(names(groups), .init = processed_dataset, function(data, group) {
  data %>%
    mutate(!!group := rowMeans(select(., all_of(groups[[group]])), na.rm = TRUE)) %>%
    select(-all_of(groups[[group]]))  
})
#convert avg dur from milisenconds to minutes
processed_dataset <- processed_dataset %>%
  mutate(avg_dur = avg_dur / 1000 / 60)

#create interaction terms
processed_dataset <- processed_dataset %>%
  mutate(
    dance_energy = avg_dance * avg_energy,
    dance_valence = avg_dance * avg_valence,
    energy_valence = avg_energy * avg_valence
  )


#final dataset preview
glimpse(processed_dataset)

twentytens_dataset <- processed_dataset %>%
  filter(decade == 2010)

#filter dataset for albums with popularity >= 50
twentytens_dataset <- twentytens_dataset %>%
  filter(popularity >= 70)


##new section

#divide dataset into two
twentytens_dataset <- twentytens_dataset %>%
  mutate(half_decade = ifelse(release_year <= 2014, "2010-2014", "2015-2019"))

#split the dataset into two halves
first_half <- twentytens_dataset %>%
  filter(half_decade == "2010-2014")

second_half <- twentytens_dataset %>%
  filter(half_decade == "2015-2019")


##first plots
#function to create plots and store them in a list
create_plot_list <- function(dataset, half_label) {
  attributes <- c("avg_dur", "avg_dance", "avg_energy", "avg_speech", "avg_valence", "avg_tempo")
  plots <- list()
  
  for (attribute in attributes) {
    plots[[attribute]] <- ggplot(dataset, aes(x = .data[[attribute]], y = popularity)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE, color = "red")+
      labs(title = paste(half_label, "- Popularity vs", attribute), 
           x = attribute, y = "Popularity") +
      theme_minimal()
  }
  
  return(plots)
}

#create plot lists for each half
plots_2010_2014 <- create_plot_list(first_half, "2010-2014")
plots_2015_2019 <- create_plot_list(second_half, "2015-2019")


grid.arrange(grobs = plots_2010_2014, ncol = 2)
grid.arrange(grobs = plots_2015_2019, ncol = 2)

##plot for interaction

#interaction attributes
interaction_attributes <- c("dance_energy", "dance_valence", "energy_valence")

#function to create plots for interaction attributes
create_interaction_plot_list <- function(dataset, half_label) {
  plots <- list()
  
  for (attribute in interaction_attributes) {
    plots[[attribute]] <- ggplot(dataset, aes(x = .data[[attribute]], y = popularity)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE, color = "red")+
      labs(
        title = paste(half_label, "- Popularity vs", attribute),
        x = attribute,
        y = "Popularity"
      ) +
      theme_minimal()
  }
  
  return(plots)
}

#create interaction plot lists for each half
interaction_plots_2010_2014 <- create_interaction_plot_list(first_half, "2010-2014")
interaction_plots_2015_2019 <- create_interaction_plot_list(second_half, "2015-2019")

#arrange interaction plots in grids
grid.arrange(grobs = interaction_plots_2010_2014, ncol = 2)  # 2010-2014
grid.arrange(grobs = interaction_plots_2015_2019, ncol = 2)  # 2015-2019

#replaced the lm with loess plot

##regression analysis
##linear regression for first half
l_model_first <- lm(popularity ~ avg_dur + avg_dance + avg_energy + avg_speech + avg_valence + avg_tempo + 
                      dance_energy + dance_valence + energy_valence, data = first_half)
summary(l_model_first)

#residual diagnostics for linear model (first half)
plot(l_model_first$residuals, main = "Residual Plot (Linear - First Half)", ylab = "Residuals")
abline(h = 0, col = "red")
hist(l_model_first$residuals, main = "Residual Histogram (Linear - First Half)", xlab = "Residuals")

#add diagnostic tests
library(nortest)  # For normality tests
library(lmtest)   # For homoscedasticity tests

#qq for normality
qqnorm(l_model_first$residuals, main = "QQ-Plot (Linear - First Half)")
qqline(l_model_first$residuals, col = "red")

#Ks test for normality
ks_test_linear_first <- lillie.test(l_model_first$residuals)
print(ks_test_linear_first)

# Homoscedasticity Check using B-P Test
bp_test_linear_first <- bptest(l_model_first)
print(bp_test_linear_first)


##linear regression for second half
l_model_second <- lm(popularity ~ avg_dur + avg_dance + avg_energy + avg_speech + avg_valence + avg_tempo + 
                       dance_energy + dance_valence + energy_valence, data = second_half)
summary(l_model_second)

#residual diagnostics for linear model (second half)
plot(l_model_second$residuals, main = "Residual Plot (Linear - Second Half)", ylab = "Residuals")
abline(h = 0, col = "red")
hist(l_model_second$residuals, main = "Residual Histogram (Linear - Second Half)", xlab = "Residuals")

#add diagnostic tests
# qq-plot for normality
qqnorm(l_model_second$residuals, main = "QQ-Plot (Linear - Second Half)")
qqline(l_model_second$residuals, col = "red")

#Ks Test for normality
ks_test_linear_second <- lillie.test(l_model_second$residuals)
print(ks_test_linear_second)

# Homoscedasticity Check using BP Test
bp_test_linear_second <- bptest(l_model_second)
print(bp_test_linear_second)


##Non linear regression for first half
nl_model_first <- lm(popularity ~ poly(avg_dance, 2) + poly(avg_energy, 2) + poly(avg_valence, 2) +
                       dance_energy + dance_valence + energy_valence, data = first_half)
summary(nl_model_first)

#residual diagnostics for non-linear model (first half)
plot(nl_model_first$residuals, main = "Residual Plot (Non-Linear - First Half)", ylab = "Residuals")
abline(h = 0, col = "red")
hist(nl_model_first$residuals, main = "Residual Histogram (Non-Linear - First Half)", xlab = "Residuals")

# Add diagnostic tests
# qq for normality
qqnorm(nl_model_first$residuals, main = "QQ-Plot (Non-Linear - First Half)")
qqline(nl_model_first$residuals, col = "red")

# KS Test for normality
ks_test_nonlinear_first <- lillie.test(nl_model_first$residuals)
print(ks_test_nonlinear_first)

#homoscedasticity Check using Breusch-Pagan Test
bp_test_nonlinear_first <- bptest(nl_model_first)
print(bp_test_nonlinear_first)


##nL regression for second half
nl_model_second <- lm(popularity ~ poly(avg_dance, 2) + poly(avg_energy, 2) + poly(avg_valence, 2) +
                        dance_energy + dance_valence + energy_valence, data = second_half)
summary(nl_model_second)

#residual diagnostics for non-linear model (second half)
plot(nl_model_second$residuals, main = "Residual Plot (Non-Linear - Second Half)", ylab = "Residuals")
abline(h = 0, col = "red")
hist(nl_model_second$residuals, main = "Residual Histogram (Non-Linear - Second Half)", xlab = "Residuals")

# Add diagnostic tests
# qq for normality
qqnorm(nl_model_second$residuals, main = "QQ-Plot (Non-Linear - Second Half)")
qqline(nl_model_second$residuals, col = "red")

#KS Test for normality
ks_test_nonlinear_second <- lillie.test(nl_model_second$residuals)
print(ks_test_nonlinear_second)

#Homoscedasticity Check using Breusch-Pagan Test
bp_test_nonlinear_second <- bptest(nl_model_second)
print(bp_test_nonlinear_second)


##running tests for both hyphothesis
#first hyp

anova_first_hyp_first <- anova(l_model_first, nl_model_first)
anova_first_hyp_second <- anova(l_model_second, nl_model_second)

anova_first_hyp_first
anova_first_hyp_second
#second hyp


twentytens_dataset$half_decade <- as.factor(twentytens_dataset$half_decade)

#model without interaction terms
model_no_interaction <- lm(popularity ~ avg_dur + avg_dance + avg_energy + avg_speech + avg_valence +
                             avg_tempo + half_decade, data = twentytens_dataset)

#model with interaction terms
model_with_interaction <- lm(popularity ~ (avg_dur + avg_dance + avg_energy + avg_speech + avg_valence +
                                             avg_tempo) * half_decade, data = twentytens_dataset)


anova_results <- anova(model_no_interaction, model_with_interaction)
anova_results
