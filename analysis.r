data <- read.csv("hallucination_cases.csv")
head(data)
names(data)
str(data)



#visualizations

library(ggplot2)

data$AI.Tool <- trimws(data$AI.Tool)
data$AI.Tool[data$AI.Tool == "implied"] <- "Implied"
data$AI.Tool[data$AI.Tool == ""] <- "Not specified"

ai_counts <- sort(table(data$AI.Tool), decreasing = TRUE)

top_ai <- head(ai_counts, 6)

ai_plot <- data.frame(
  AI.Tool = names(top_ai),
  Cases = as.numeric(top_ai)
)

ggplot(ai_plot, aes(x = Cases, y = reorder(AI.Tool, Cases))) +
  geom_col() +
  labs(
    title = "Most Commonly Reported AI Tools",
    x = "Number of Cases",
    y = "AI Tool"
  )



data$Legal.Field.Primary[data$Legal.Field.Primary == ""] <- "Not specified"

legal_counts <- sort(table(data$Legal.Field.Primary), decreasing = TRUE)

legal_plot <- data.frame(
  Legal.Field.Primary = names(legal_counts),
  Cases = as.numeric(legal_counts)
)

ggplot(legal_plot, aes(x = Cases, y = reorder(Legal.Field.Primary, Cases))) +
  geom_col() +
  labs(
    title = "Reported Hallucination Cases by Primary Legal Field",
    x = "Number of Cases",
    y = "Primary Legal Field"
  )


sanction_counts <- sort(table(data$Professional.Sanction), decreasing = TRUE)

sanction_plot <- data.frame(
  Professional.Sanction = names(sanction_counts),
  Cases = as.numeric(sanction_counts)
)

ggplot(sanction_plot, aes(x = Cases, y = reorder(Professional.Sanction, Cases))) +
  geom_col() +
  labs(
    title = "Reported Hallucination Cases by Professional Sanction",
    x = "Number of Cases",
    y = "Professional Sanction"
  )



#Model1

data$Professional.Sanction <- factor(
  data$Professional.Sanction,
  levels = c("No", "Yes")
)

data$Legal.Field.Primary <- factor(data$Legal.Field.Primary)

data$Legal.Field.Primary <- relevel(
  data$Legal.Field.Primary,
  ref = "contract"
)

sanction_model <- glm(
  Professional.Sanction ~ Legal.Field.Primary,
  data = data,
  family = binomial
)

summary(sanction_model)

exp(coef(sanction_model))


#Model2

data$Legal.Field.Group <- as.character(data$Legal.Field.Primary)

data$Legal.Field.Group[
  data$Legal.Field.Group %in% c(
    "bankruptcy",
    "habeas",
    "IP",
    "landlord-tenant",
    "tax",
    "tort",
    "other"
  )
] <- "Other legal fields"

data$Legal.Field.Group <- factor(data$Legal.Field.Group)

data$Legal.Field.Group <- relevel(
  data$Legal.Field.Group,
  ref = "contract"
)

table(data$Legal.Field.Group, data$Professional.Sanction)

sanction_model_grouped <- glm(
  Professional.Sanction ~ Legal.Field.Group,
  data = data,
  family = binomial
)

summary(sanction_model_grouped)



#Regression Result
exp(cbind(
  Odds_Ratio = coef(sanction_model_grouped),
  confint(sanction_model_grouped)
))
