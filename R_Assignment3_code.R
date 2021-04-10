

## Load packages ---------------------------------------------------------------
library("dplyr")
library("tidyr")
library("ggplot2")
library("caret")
library("styler")

## Question 1 -----------------------------------------------------------------

loans_raw <- read.csv("input/loans.csv", stringsAsFactors = FALSE)
loans <- loans_raw
demo <- read.delim(file = "input/demographics.txt", sep = "\t", skip = 4)

loans$duration[loans$duration == 365] <- 12L 
loans <- loans %>%
  mutate(across(-c(where(is.numeric), where(is.integer)), ~ as.factor(.)))
# turning all non-integer, non-numerical vars into factors

demo <- demo %>% mutate(age = as.integer(ifelse(age == "thirty", 30, age)))
demo <- demo %>%
  mutate(across(-c(where(is.numeric), where(is.integer)), ~ as.factor(.)))

answer1A <- loans
answer1B <- demo

## Question 2 -----------------------------------------------------------------

risk_raw <- read.csv("input/credit_risk.csv", stringsAsFactors = FALSE)
risk <- risk_raw
replace_mistake_c <- function(curr_str) {
  if ((!grepl("-", curr_str) & !grepl("1", curr_str)) & !grepl(">= 6", curr_str)) {
    curr_str <- paste0(substr(curr_str,1,1), "-", substr(curr_str,2,2))
  }
  curr_str
}

risk <- risk %>%
  mutate(number_credits = apply(
    X = select(risk, number_credits),
    MARGIN = 1, FUN = replace_mistake_c
  ))

risk <- risk %>%
  mutate(across(-c(where(is.numeric), where(is.integer)), ~ as.factor(.)))

answer2 <- risk

## Question 3 -----------------------------------------------------------------

loans_fixed <- loans %>%
  mutate(amount = amount/1000, duration = duration/12) %>%
  pivot_longer(
    cols = c(duration, amount),
    names_to = "loan_attribute",
    values_to = "attribute_value"
  )
answer3 <- loans_fixed

ggplot(answer3, aes(x = loan_attribute, y = attribute_value)) +
  geom_violin() +
  scale_x_discrete(labels = c("Amount (x 1000 DM)", "Duration (years)")) +
  xlab("Loan Attribute") +
  ylab(element_blank())

## Question 4 -----------------------------------------------------------------

loans_summarised <- loans %>%
  mutate(type_of_expenditure = ifelse(grepl(pattern = "car", purpose), "Car",
    ifelse(purpose %in% c("domestic appliances", "furniture/equipment", 
"radio/television"),
      "Home", ifelse(
        purpose %in% c("business","retraining"),
        "Work", "Other"
      )
    )
  )) %>%
  mutate(Good_Credit_Fraction = ifelse(credit_risk == "good", T, F)) %>%
  group_by(type_of_expenditure) %>%
  summarize(across(c(amount, duration), list(Mean = mean, Max = max, 
Min = min)), across(Good_Credit_Fraction, mean), .groups = "keep") %>%
  arrange(desc(amount_Mean))

answer4 <- loans_summarised

## Question 5 -----------------------------------------------------------------

full_data_loans <- inner_join(loans, demo, by = c("credit_ID")) %>%
  semi_join(risk, by = c("credit_ID")) %>% drop_na(.) 
answer5 <- full_data_loans

## Question 6 -----------------------------------------------------------------

set.seed(1)
answer6_knn <- train(
  factor(credit_risk == "bad") ~ . -personal_status_sex -housing -credit_ID,
                     method = "knn", data = answer5_c,
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid = data.frame(k = seq(3,27,4))
)

answer6_backup <- train(
  factor(credit_risk == "bad") ~ . -personal_status_sex -housing -credit_ID,
                        method = "knn", data = answer5_backup_c,
                        trControl = trainControl(method = "cv", number = 5),
                        tuneGrid = data.frame(k = seq(3,27,4))
)


## Question 7 -----------------------------------------------------------------

set.seed(1)
answer7_df <- answer5_backup_c %>%
  mutate(credit_risk = factor(credit_risk) %>%
           relevel(ref = "bad"))
risk_glm <- train(credit_risk ~ (amount * employment_duration) + 
                    (amount * personal_status_sex),
                  method = "glm", data = answer7_df,
                  trControl = trainControl(
                    method = "cv", number = 3, classProbs = TRUE, 
                    summaryFunction = prSummary),
                  metric = "Recall",
                  family = binomial(link = "logit")
)

answer7 <- risk_glm

## Question 8 -----------------------------------------------------------------

full_combi  <- full_join(loans, demo, by = "credit_ID") %>%
  filter(personal_status_sex %in% c("male : married/widowed", 
                                    "male : divorced/separated")) %>%
  mutate(
    amount_bins = case_when(
      amount <= 1500 ~ "low",
      amount > 1500 & amount <= 3000 ~ "middle",
      amount > 3000 & amount <= 4500 ~ "high",
      amount > 4500 ~ "top"
    ) %>%
      factor(levels = c("top", "high", "middle", "low")),
    personal_status_sex = gsub(x = personal_status_sex, 
                               pattern = "male : ", 
                               replacement = "", 
                               fixed = T),
    credit_risk = relevel(credit_risk, "good")
  )


answer8 <- full_combi %>% drop_na() %>% ggplot(aes(
  x = amount_bins,
  fill = credit_risk)) + 
  geom_bar(position = "dodge") +
  facet_grid(. ~personal_status_sex ) +
  scale_fill_manual(values = c("good" = "green", "bad" = "red"))

## Question 9 -----------------------------------------------------------------

credit_data_all <- read.csv("input/credit_data_all.csv",
                            stringsAsFactors = T
)
add_prefix <- function(x,  prefix) {
  paste0(prefix, "_", x)
}
mean_v <- function(x) {
  ifelse(x > mean(x), "high", "low")
}

answer9 <- credit_data_all %>%
  filter(
    savings != "unknown/no savings account"
  ) %>%
  mutate(across(where(is.integer), mean_v)) %>%
  rename_if(names(credit_data_all) %in% names(loans), add_prefix,"loans") %>%
  rename_if(!names(credit_data_all) %in% c(names(risk),
                                           names(loans),names(demo)), 
            add_prefix,"unused") %>%
  select(starts_with(c("loans", "unused")))

## Question 10 -----------------------------------------------------------------

set.seed(1)
data_Q10 <- credit_data_all %>% select_if(!names(credit_data_all) %in% names(risk))
trn_index_c <- createDataPartition(
  y = data_Q10$credit_risk,
  p = 0.70, list = F
)
trn_data_c <- data_Q10[trn_index_c, ]
tst_data_c <- data_Q10[-trn_index_c, ]

set.seed(1)
Q10_lgr_c <- train(credit_risk ~ .,
                   method = "glm",
                   data = trn_data_c,
                   trControl = trainControl(
                     method = "cv",
                     number = 5,
                     classProbs = TRUE,
                     summaryFunction = prSummary,
                   ),
                   family = binomial(link = "logit"),
                   metric = "AUC", preProcess = c("center", "scale")
)

pred_Q10_c <- predict(Q10_lgr_c, tst_data_c)
Q10_confM_c <- confusionMatrix(
  pred_Q10_c,
  as.factor(tst_data_c$credit_risk)
)

answer10_c <- Q10_confM_c$byClass[c("Sensitivity", "Specificity")]



