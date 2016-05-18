# Split up train and test data
train <- full[1:26729, ]
test  <- full[26730:nrow(full), ]

# Set a random seed
set.seed(731)

# Build the model
rf_mod <- randomForest(OutcomeType ~ AnimalType+AgeinDays+Intact+HasName+Hour+Weekday+TimeofDay+SimpleColor+IsMix+Sex+Month+Lifestage, 
                       data = train, 
                       ntree = 600, 
                       importance = TRUE)

# Show model error
plot(rf_mod, ylim=c(0,1))
legend('topright', colnames(rf_mod$err.rate), col=1:6, fill=1:6)

# Get importance
importance    <- importance(rf_mod)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,1],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance)) +
  geom_bar(stat='identity', colour = 'black') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'lavender',
            fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_mod, test, type = 'vote')

# Save the solution to a dataframe
solution <- data.frame('ID' = test$ID, prediction)

# Write it to file
write.csv(solution, "rf_two.csv", row.names = F)
