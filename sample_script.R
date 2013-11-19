
 
library(rattle)
building <- TRUE
scoring  <- ! building
library(colorspace)
crv$seed <- 42 
mydatadataset <- sample
mydatadataset <- read.csv("file:///C:/Users/STRATOS/Desktop/company_analysis/company_analysis/sample.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
set.seed(crv$seed) 
mydatanobs <- nrow(mydatadataset) mydatasample <- mydatatrain <- sample(nrow(mydatadataset), 0.7*mydatanobs) mydatavalidate <- sample(setdiff(seq_len(nrow(mydatadataset)), mydatatrain), 0.15*mydatanobs) mydatatest <- setdiff(setdiff(seq_len(nrow(mydatadataset)), mydatatrain), mydatavalidate) 
mydatainput <- c("v1", "v2", "v3", "v4")
mydatanumeric <- c("v1", "v2", "v3", "v4")
mydatacategoric <- NULL
mydatatarget  <- "v5"
mydatarisk    <- NULL
mydataident   <- NULL
mydataignore  <- NULL
mydataweights <- NULL
mydataglm <- glm(v5 ~ .,
    data=mydatadataset[mydatatrain, c(mydatainput, mydatatarget)],
    family=binomial(link="logit"))
print(summary(mydataglm))
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(mydataglm)[1],
            attr(logLik(mydataglm), "df")))
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            mydataglm$null.deviance-mydataglm$deviance,
            mydataglm$df.null-mydataglm$df.residual))
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(mydataglm$null.deviance-mydataglm$deviance,
                   mydataglm$df.null-mydataglm$df.residual)))
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
             cor(mydataglm$y, mydataglm$fitted.values)))
cat('\n==== ANOVA ====\n\n')
print(anova(mydataglm, test="Chisq"))
cat("\n")
require(pmml, quietly=TRUE)
ttl <- genPlotTitleCmd("Linear Model",mydatadataname,vector=TRUE)
plot(mydataglm, main=ttl[1])
mydatapr <- as.vector(ifelse(predict(mydataglm, type="response", newdata=mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]) > 0.5, "1", "0"))
table(mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]$v5, mydatapr,
        dnn=c("Actual", "Predicted"))
round(100*table(mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]$v5, mydatapr, 
        dnn=c("Actual", "Predicted"))/length(mydatapr))
overall <- function(x)
{
  if (nrow(x) == 2) 
    cat((x[1,2] + x[2,1]) / sum(x)) 
  else
    cat(1 - (x[1,rownames(x)]) / sum(x))
} 
overall(table(mydatapr, mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]$v5,  
        dnn=c("Predicted", "Actual")))
require(pmml, quietly=TRUE)
require(pmml, quietly=TRUE)
saveXML(pmml(mydataglm), "C:\Users\STRATOS\Desktop\company_analysis\company_analysis\sample_glm.xml")
mydatapr <- as.vector(ifelse(predict(mydataglm, type="response", newdata=mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]) > 0.5, "1", "0"))
table(mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]$v5, mydatapr,
        dnn=c("Actual", "Predicted"))
round(100*table(mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]$v5, mydatapr, 
        dnn=c("Actual", "Predicted"))/length(mydatapr))
overall <- function(x)
{
  if (nrow(x) == 2) 
    cat((x[1,2] + x[2,1]) / sum(x)) 
  else
    cat(1 - (x[1,rownames(x)]) / sum(x))
} 
overall(table(mydatapr, mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]$v5,  
        dnn=c("Predicted", "Actual")))
mydatapr <- as.vector(ifelse(predict(mydataglm, type="response", newdata=mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]) > 0.5, "1", "0"))
table(mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]$v5, mydatapr,
        dnn=c("Actual", "Predicted"))
round(100*table(mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]$v5, mydatapr, 
        dnn=c("Actual", "Predicted"))/length(mydatapr))
overall <- function(x)
{
  if (nrow(x) == 2) 
    cat((x[1,2] + x[2,1]) / sum(x)) 
  else
    cat(1 - (x[1,rownames(x)]) / sum(x))
} 
overall(table(mydatapr, mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]$v5,  
        dnn=c("Predicted", "Actual")))
mydatapr <- as.vector(ifelse(predict(mydataglm, type="response", newdata=mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]) > 0.5, "1", "0"))
table(mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]$v5, mydatapr,
        dnn=c("Actual", "Predicted"))
round(100*table(mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]$v5, mydatapr, 
        dnn=c("Actual", "Predicted"))/length(mydatapr))
overall <- function(x)
{
  if (nrow(x) == 2) 
    cat((x[1,2] + x[2,1]) / sum(x)) 
  else
    cat(1 - (x[1,rownames(x)]) / sum(x))
} 
overall(table(mydatapr, mydatadataset[mydatavalidate, c(mydatainput, mydatatarget)]$v5,  
        dnn=c("Predicted", "Actual")))
