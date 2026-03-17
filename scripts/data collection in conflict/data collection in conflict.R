# MENA MEL Workshop
# Data collection in conflict

# Randomized response ---- 

# Set the probability for the randomization step
# p = probability of answering the sensitive question truthfully
# 1-p = probability of answering "Yes" regardless of the truth

p <- 0.7  # You can adjust the probability

# Create a function to apply the randomized response technique
randomized_response <- function(truth, p) {
  # Generate a random number for each respondent
  random_num <- runif(length(truth))
  
  # If the random number is less than p, they answer truthfully
  # Otherwise, they answer "Yes" regardless of the truth
  response <- ifelse(random_num < p, truth, 1)
  
  return(response)
}

# Simulate data
# Assume we have a vector of true responses (1 = Yes, 0 = No)
set.seed(123)  # Set seed for reproducibility
n <- 100  # Number of respondents
true_responses <- rbinom(n, 1, 0.4)  # 40% of respondents say Yes

# Apply the randomized response technique
reported_responses <- randomized_response(true_responses, p)
reported_responses

# Let's estimate the proportion of 'Yes' responses in the population
# The formula for estimation based on the randomized response method is:
# Estimated True Proportion = (Observed Proportion - (1 - p)) / p

observed_proportion <- mean(reported_responses)
observed_proportion

estimated_true_proportion <- (observed_proportion - (1 - p)) / p

# Print the results
cat("Observed proportion of 'Yes' responses:", observed_proportion, "\n")
cat("Estimated true proportion of 'Yes' responses:", estimated_true_proportion, "\n")




# Endorsement ---- 

# Set random seed for reproducibility
set.seed(123)

# Define a sample of respondents
n <- 500  # Adjust the sample size as needed

# Simulate respondent IDs
endorse <- data.frame(ID = 1:n)

# Define experimental conditions (endorsement)
endorsements <- c("Foreign forces", "Taliban", "No Endorsement")

# Randomly assign endorsements to respondents
endorse <- endorse %>%
  mutate(Condition = sample(endorsements, n, replace = TRUE),
         Support = ifelse(Condition == "Foreign forces", rbinom(n, 1, 0.7),
                           ifelse(Condition == "Taliban", rbinom(n, 1, 0.2), 
                                  rbinom(n, 1, 0.45))))

# View the first few rows of the data
head(endorse)


# Summarize support by endorsement condition
byCond <- endorse %>%
  group_by(Condition) %>%
  summarise(se=std.error(Support),
            Support = mean(Support)) %>%
  mutate(lower=Support-1.96*se,
         upper=Support + 1.96*se) %>%
  select(Condition, Support, lower, upper) %>%
  arrange(desc(Support))

byCond

ggplot(byCond, aes(y=fct_reorder(Condition, Support), x=Support, xmin=lower, xmax=upper, color=Condition, fill=Condition)) +
#  geom_point(stat="identity") +
  geom_errorbar(width=0, size=1) +
  geom_label(aes(label=paste(round(Support*100,0), "%", sep="")),
             show.legend=F,
             label.padding=unit(.1, "cm"),
             size=3.4,
             #alpha=.8,
             color="white") + 
  scale_color_manual(values=c(usaid_blue, medium_grey, usaid_red)) +
#                     alpha=.4) +
  scale_fill_manual(values=c(usaid_blue, medium_grey, usaid_red)) +
#                    alpha=.4) +
  scale_x_continuous(limits=c(0,1),
                     labels=percent_format()) +
  labs(title="Policy Support by Endorsement Condition",
       x="",
       y="",
       caption="Not real") +
  theme(legend.position="none",
        plot.background=element_rect(fill="aliceblue"),
        panel.background=element_rect(fill="aliceblue"))

ggsave("scripts/Data collection in conflict/policy support toy.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

# Run a logistic regression to assess the effect of endorsements on support
model <- glm(Support ~ Condition, data = endorse, family = binomial)

# View the model summary
summary(model)


# Predict probabilities from the model
predicted_probs <- predict(model, type = "response")

# Add the predicted probabilities to the data
respondents$predicted_probs <- predicted_probs

# View the first few rows
head(respondents)

# Summary of predicted probabilities by endorsement
predicted_summary <- respondents %>%
  group_by(endorsement) %>%
  summarise(mean_pred_prob = mean(predicted_probs))

print(predicted_summary)


data(pakistan)
str(pakistan)

Y <- c("Polio.a", "Polio.b", "Polio.c", "Polio.d", "Polio.e")
yaxis <- c("Control", "Kashmir", "Afghan", "Al-Qaida", "Tanzeems")
endorse.plot(Y = Y, data = pakistan, scale = 5)


# List ---- 


# Sample data generation: Control group and Treatment group
set.seed(123) # For reproducibility
n <- 60  # Sample size

# Generate control group data (non-sensitive items)
control_group <- data.frame(
  item1 = rbinom(n, 1, 0.4),
  item2 = rbinom(n, 1, 0.4),
  item3 = rbinom(n, 1, 0.4),
  sensitive_item=NA,
  group = rep("Control", n)
)

head(control_group)

# Generate treatment group data (includes sensitive item)
treatment_group <- data.frame(
  item1 = rbinom(n, 1, 0.7),
  item2 = rbinom(n, 1, 0.7),
  item3 = rbinom(n, 1, 0.7),
  sensitive_item = rbinom(n, 1, 0.95),  # Sensitive item
  group = rep("Treatment", n)
)

head(treatment_group)

# Combine the datasets

lst <- rbind(control_group, treatment_group)

head(lst)

# Calculate total number of items selected in each group

lst$selected_items <- rowSums(lst[,1:3]) # Control group: sum non-sensitive items

# For the treatment group, include the sensitive item
lst$selected_items <- ifelse(lst$group == "treatment", rowSums(lst[,1:4]), lst$selected_items)


# Calculate mean number of items selected in each group
aggregate(selected_items ~ group, lst, mean)

# Difference in means is the estimate of the proportion endorsing the sensitive item
t.test(selected_items ~ group, data = lst)


# Summarize support by endorsement condition
listCond <- lst %>%
  group_by(group) %>%
  summarise(se=std.error(selected_items),
            Support = mean(selected_items)) %>%
  mutate(lower=Support-1.96*se,
         upper=Support + 1.96*se) %>%
  select(group, Support, lower, upper) %>%
  arrange(desc(Support))

listCond

ggplot(listCond, aes(y=fct_reorder(group, Support), x=Support, xmin=lower, xmax=upper, color=group, fill=group)) +
  #  geom_point(stat="identity") +
  geom_errorbar(width=0, size=1) +
  geom_label(aes(label=round(Support,1)),
             show.legend=F,
             label.padding=unit(.1, "cm"),
             size=3.4,
             #alpha=.8,
             color="white") + 
  scale_color_manual(values=c(usaid_blue, usaid_red)) +
  #                     alpha=.4) +
  scale_fill_manual(values=c(usaid_blue, usaid_red)) +
  #                    alpha=.4) +
  scale_x_continuous(limits=c(0,4)) +
#                     labels=percent_format()) +
  labs(title="Mean of item count by treatment condition",
       x="",
       y="",
       caption="Not real") +
  theme(legend.position="none",
        plot.background=element_rect(fill="aliceblue"),
        panel.background=element_rect(fill="aliceblue"))

ggsave("scripts/Data collection in conflict/list raw count mn.png",
       device="png",
       type="cairo",
       height=4,
       width=7)





data(affirm)
data(combinedListExps)
head(combinedListExps)

# complete case analysis
combinedListExps <- na.omit(combinedListExps)

# Conduct estimation without covariate adjustment
out.1 <- combinedListDirect(list1N ~ list1treat,
                            data = subset(combinedListExps, directsfirst==1),
                            treat = "list1treat", direct = "direct1")
summary(out.1)



# errata ---- 

ot <- data.frame(Condition=c("Control", "Actor A", "Actor B"),
                 Statement=c("It has recently been proposed to allow Afghans to vote in direct elections when selecting leaders for district councils. How strongly would you support this policy?",
                             "It has recently been proposed by foreign forces...",
                             "It has recently been proposed by the Taliban..."))



ot_flx <- ot %>%
  flextable() %>%
  align_text_col(align="center") %>%
  fontsize(size=14, part="header") %>%
  fontsize(size=14, part="body") %>%
  autofit()

#ot_flx

ot %>%
  gt() %>%
  opt_table_font(font="Gill Sans Mt",
                 size=16) %>%
  cols_align(align="center") %>%
  cols_width(Statement ~ px(380))

"I’m going to read you a list with the names of different groups and individuals on it. After I read the entire list, I’d like you to tell me how many of these groups and individuals you broadly support, meaning that you generally agree with the goals and policies of the group or individual. Please don’t
tell me which ones you generally agree with; only tell me how many groups or individuals you broadly support." 



