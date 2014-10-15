FLLogRegr <- function( 	x,
						DepCol,
						PrimaryKey,
                  		MaxIterations,
						pThreshold,
						Note     = "From RWrapper For DBLytix",
						Exclude      = c(),
						ClassSpec    = list(),
						WhereClause  = ""){

	ObsIDColName  <- "ObsID";
	VarIDColName  <- "VarID";
	ValueColName  <- "Num_Val";
	
	DataPrepRes <- FLRegrDataPrep( 	x,
									DepCol,
									ObsIDColName = ObsIDColName,
									VarIDColName = VarIDColName,
									ValueColName = ValueColName,
									PrimaryKey   = PrimaryKey,
									Exclude      = Exclude,
									ClassSpec    = ClassSpec,
									WhereClause  = WhereClause);
	
	DeepTableName        <- DataPrepRes$DeepTableName
	WidetoDeepAnalysisID <- DataPrepRes$WidetoDeepAnalysisID
	DBConnection         <- x@ODBCConnection;
	
	SQLStr        <- "CALL WorkaroundLogRegr('";
	SQLParameters <- paste(	DeepTableName,
							ObsIDColName,  
							VarIDColName, 
							ValueColName, 
							toString(MaxIterations),  
							toString(pThreshold),
							Note, sep="','")
	SQLStr           <- paste(SQLStr, SQLParameters,"')", sep="")
	#print(SQLStr)
		
	#run FLLogRegr
	LogRegrRes  <- sqlQuery(DBConnection, SQLStr);
	#print(LogRegrRes)
	AnalysisID <- toString(LogRegrRes[[1,"ANALYSISID"]]);

	RetData = new("FLLogRegr",AnalysisID = AnalysisID, ODBCConnection = DBConnection);
	
	return(RetData);
}