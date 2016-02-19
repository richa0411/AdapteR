REPLACE PROCEDURE WorkaroundLDA (	IN TableName VARCHAR(100), 
											IN ObsIDColName VARCHAR(100),
											IN VarIDColName VARCHAR(100),
											IN ValueColName VARCHAR(100),
										    IN Note VARCHAR(255) )
DYNAMIC RESULT SETS 1
BEGIN
	DECLARE AnalysisID VARCHAR(30);
	DECLARE statement1_str VARCHAR(500);
	DECLARE result_set CURSOR WITH RETURN ONLY FOR stmt1;
	
	CALL FLLDA(TableName,ObsIDColName,VarIDColName,ValueColName,Note,AnalysisID);    
	SET statement1_str = 'SELECT AnalysisID FROM fzzlLDAInfo WHERE AnalysisID = ?';
	PREPARE stmt1 FROM statement1_str;
	OPEN result_set USING AnalysisID;
	DEALLOCATE PREPARE stmt1;
END;