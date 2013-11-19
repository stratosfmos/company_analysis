# Rattle is Copyright (c) 2006-2013 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2013-11-19 19:45:58 x86_64-w64-mingw32 

# Rattle version 2.6.26 user 'STRATOS'

# Export this log textview to a file using the Export button or the Tools 
# menu to save a log of all activity. This facilitates repeatability. Exporting 
# to file 'myrf01.R', for example, allows us to the type in the R Console 
# the command source('myrf01.R') to repeat the process automatically. 
# Generally, we may want to edit the file to suit our needs. We can also directly 
# edit this current log textview to record additional information before exporting. 

# Saving and loading projects also retains this log.

library(rattle)

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building

# The colorspace package is used to generate the colours used in plots, if available.

library(colorspace)

# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2013-11-19 19:46:11 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///C:/Users/STRATOS/Desktop/company_analysis/company_analysis/sample_sample.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2013-11-19 19:46:11 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 168 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 117 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 25 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 26 observations

# The following variable selections have been noted.

crs$input <- c("v1", "v2", "v3", "v4")

crs$numeric <- c("v1", "v2", "v3", "v4")

crs$categoric <- NULL

crs$target  <- "v5"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2013-11-19 19:46:21 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- glm(v5 ~ .,
               data=crs$dataset[crs$train, c(crs$input, crs$target)],
               family=binomial(link="logit"))

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(crs$glm)[1],
            attr(logLik(crs$glm), "df")))
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            crs$glm$null.deviance-crs$glm$deviance,
            crs$glm$df.null-crs$glm$df.residual))
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(crs$glm$null.deviance-crs$glm$deviance,
                   crs$glm$df.null-crs$glm$df.residual)))
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
            cor(crs$glm$y, crs$glm$fitted.values)))
cat('\n==== ANOVA ====\n\n')
print(anova(crs$glm, test="Chisq"))
cat("\n")

# Time taken: 0.07 secs

#============================================================
# Rattle timestamp: 2013-11-19 19:46:29 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

print(crs$pr <- as.vector(ifelse(predict(crs$glm, type="response", newdata=crs$dataset[crs$validate, c(crs$input, crs$target)]) > 0.5, "1", "0")))

# Generate the confusion matrix showing counts.

print(table(crs$dataset[crs$validate, c(crs$input, crs$target)]$v5, crs$pr,
      dnn=c("Actual", "Predicted")))

# Generate the confusion matrix showing percentages.

print(round(100*table(crs$dataset[crs$validate, c(crs$input, crs$target)]$v5, crs$pr, 
                dnn=c("Actual", "Predicted"))/length(crs$pr)))

# Calucate the overall error percentage.

overall <- function(x)
{
  if (nrow(x) == 2) 
    cat((x[1,2] + x[2,1]) / sum(x)) 
  else
    cat(1 - (x[1,rownames(x)]) / sum(x))
} 
print(overall(table(crs$pr, crs$dataset[crs$validate, c(crs$input, crs$target)]$v5,  
              dnn=c("Predicted", "Actual"))))

library(knitr)
knit2html('analysis.Rmd')
knit2pdf('analysis.Rnw')
