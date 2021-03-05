# Script for sampling 50 random articles from Journal of Memory and Language 
set.seed(125)
random_articles <- sample(1:114, 50, replace=FALSE)
random_articles <- sort(random_articles)
print(random_articles)