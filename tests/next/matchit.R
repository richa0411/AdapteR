#matchit function
tab <- FLTable("tblMatchitAlt","PERSON_ID")
# #do a in-db logistic regression (glm) ree test_glm_logistic.R as a reference 
# 2. scoring: combine scores and obsid, treatment and (optional) MatchOrderCol in a view/temp table (create a FLTable object for this) 
# 3. determine a temp output table name (see createTable and gen_unique_table_name functions for reference ) 
# 4. call FLMatchit 
# 5. constructing the result 
# 1. call: see lm implementation 
# 2. set formula to argument 
# 3. set model to result of glm logistic regression 
# 4. set match.matrix to NA 
# 5. set discarded to FLVector with matchit results 
# 6. set distance to NA 
# 7. set weights to NA 
# 8. set subclass an q.cut to same value as R matchit when doing nearest neighbor 
# 9. set treat,X to formula parts (see lm implementation for ref) 
# 10. reconstruct nn after instpecting R matchit results. 

require(MatchIt)
Renv = new.env(parent = globalenv())

logitinv <- function(l) exp(l)/(1+exp(l))
dataf<- data.frame(var1 = rnorm(200),
                   var2 = rnorm(200))
dataf$var3 = rbinom(nrow(dataf),1,prob=logitinv(dataf$var1+dataf$var2))
Renv$dataf <- dataf
FLenv <- as.FL(Renv)

dataf<-FLenv$dataf
#F denotes formula
F <- var3 ~ var1+var2
fit <- glm(F,data=dataf, family="binomial")

#predict
options(debugSQL = TRUE)
head(predict(fit,type="response"))
head(logitinv(predict(fit)))
score<-predict(fit)

predict.matchit <- function(object,
                              newdata=object@table,
                              scoreTable=""){
  return(predict.lmGeneric(object,newdata=newdata,
                           scoreTable=scoreTable))
}




#scoretable<-table()

FLtable<-function(table,
                  obs_id_colname,
                  var_id_colnames=character(0), 
                  cell_val_colname=character(0),
                  whereconditions=character(0),
                  connection=getFLConnection(),
                  type="double",
                  fetchIDs=TRUE,
                  ...)


#view
vviewName <- gen_view_name("matchit")
sqlstr <- paste0("SELECT p.vectorValueColumn AS Num_Val
                 FROM (",constructSelect(x),") AS p")
q <- createView(vviewName,sqlstr)

ret <- sqlStoredProc(connection,
                     "FLMatchIt",
                     TableName = 'tblMatchItAlt',
                     ObsIDColName = "PERSON_ID",
                     TreatmentColName = "exposure",
                     PropScoreCol = "prob",
                     MatchOrderCol = "prob",
                     TableOutput = 1,
                     outputParameter = c(OutTable = 'a')
)


