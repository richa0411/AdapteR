#matchit function
tab <- FLTable("tblMatchitAlt","PERSON_ID")
sqlQuery(connection,"show table tblMatchitAlt")

require(MatchIt)

Renv = new.env(parent = globalenv())

logitinv <- function(l) exp(l)/(1+exp(l))
dataf<- data.frame(var1 = rnorm(200),
                   var2 = rnorm(200))
dataf$var3 = rbinom(nrow(dataf),1,prob=logitinv(dataf$var1+dataf$var2))
Renv$dataf <- dataf
FLenv <- as.FL(Renv)

dataf<-FLenv$dataf
## 1. do a in-db logistic regression (glm) ree test_glm_logistic.R as a reference 
FORM <- var3 ~ var1+var2
fit <- glm(FORM,data=dataf, family="binomial")
summary(fit)

# 2. scoring: combine scores and obsid, treatment and (optional) MatchOrderCol in a view/temp table (create a FLTable object for this) 
outWideTable <- gen_view_name("regprep")
ret <- sqlStoredProc(connection,
                     "FLLogRegrScore",
                     TableName = fit@deeptable@select@table_name,
                     ObsIDCol = fit@deeptable@select@variables$obs_id_colname,
                     VarIDCol = fit@deeptable@select@variables$var_id_colname,
                     ValueCol = fit@deeptable@select@variables$cell_val_colname,
                     WhereClause = "NULL",
                     RegrAnalysisID = fit@AnalysisID,
                     ScoreTable = outWideTable,
                     Note = "Matchit scoring table",
                     outputParameter = c(OutTable = 'a')
)

# 3. determine a temp output table name (see createTable and gen_unique_table_name functions for reference ) and amend with exposure col
getDepVarname <- function(fit) rownames(attr(terms(fit@formula),"factors"))[[1]]
getObsIDColColname <- function(dataf) dataf@select@variables$obs_id_colname # getIndexSQLExpression(dataf,1)
getTable <- function(dataf) dataf@select@table_name

sqlSendUpdate(connection,paste0("alter table ",outWideTable," add exposure INTEGER"))
sqlSendUpdate(connection,paste0("update a from ",outWideTable," a, ",getTable(dataf), " b\n",
                                " set exposure=b.",getDepVarname(fit),"\n",
                                " where a.",getObsIDColColname(fit@deeptable),"=b.",getObsIDColColname(dataf)))

## Try to create a view instead of alter table
#sqlSendUpdate(connection,paste0("create or replace view",tempoutput, "AS","select a from",outWideTbale,"a","getTable(dataf),"b\n"),
#" set exposure=b.",getDepVarname(fit),"\n",
#" where a.",getObsIDColColname(fit@deeptable),"=b.",getObsIDColColname(dataf))



# 4. call FLMatchit 
ret <- sqlStoredProc(connection,
                     "FLMatchIt",
                     TableName = outWideTable,
                     ObsIDColName = "obs_id_colname",
                     TreatmentColName = "exposure",
                     PropScoreCol = "Y",
                     MatchOrderCol = "Y",
                     TableOutput = 1,
                     outputParameter = c(OutTable = 'a')
)

## 5. constructing the result 
##    1. call: see lm implementation 
##    2. set formula to argument 
##    3. set model to result of glm logistic regression 
##    4. set match.matrix to NA 
##    5. set discarded to FLVector with matchit results 
##    6. set distance to NA 
##    7. set weights to NA 
##    8. set subclass an q.cut to same value as R matchit when doing nearest neighbor 
##    9. set treat,X to formula parts (see lm implementation for ref) 
##    10. reconstruct nn after instpecting R matchit results. 
result <- matchit(FORM,Renv$dataf)
str(result)
structure(list(formula=FORM,
    model=fit,
    match.matrix="NA",
    distance="NA",
    weights="NA",
    #subclass=
    treat=FORM,
    x=FORM
    ),
class="matchit")
          
          
          sqlQuery(connection,"select * from A411737_UnMatched")
          
          