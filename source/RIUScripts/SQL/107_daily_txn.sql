SET NOCOUNT ON
/*
You can omit the TenderCode, SeqNo.
The SeqNo is genearted at line 21-25
*/
SELECT "TxnNo", "TxnDate", "MemberID", "CardNumber", "TenderCode", "TotalAmount",
		"SeqNo", "RetailPrice", "NetPrice", "Qty", "ProdCode", "ProdName1", "ProdName3",
		"DepartCode", "DepartName1", "StoreID";
SELECT 
[Sales_H].[TxnNo], [Sales_H].[TxnDate], [Sales_H].[MemberID], [Sales_H].[CardNumber], [Sales_T].[TenderCode], [Sales_H].[TotalAmount], 
[Sales_D].[SeqNo], [Sales_D].[RetailPrice], [Sales_D].[NetPrice], [Sales_D].[Qty], [Sales_D].[ProdCode],
'"' + CAST([Product].[ProdName1] AS NVARCHAR(MAX)) + '"' AS [ProdName1], 
'"' + CAST([Product].[ProdName3] AS NVARCHAR(MAX)) + '"' AS [ProdName3], [Product].[DepartCode],
[Department].[DepartName1], [Store].[StoreID]
FROM
[Sales_H]
JOIN [Sales_D]
ON [Sales_H].TxnNo = [Sales_D].TxnNo
JOIN
(
	SELECT * FROM
	(
		SELECT *, ROW_NUMBER() OVER (PARTITION BY [TXNNO] ORDER BY [SEQNO]) AS [RowNumber]
		FROM [Sales_T]
	) AS [Sales_T] WHERE [Sales_T].[RowNumber] = 1
) AS [Sales_T]
ON [Sales_H].TxnNo = [Sales_T].TxnNo
JOIN [Product]
ON [Sales_D].ProdCode = [Product].ProdCode
JOIN [Department]
ON [Product].DepartCode = [Department].DepartCode
JOIN [Store]
ON [Sales_H].StoreCode=[Store].StoreCode
WHERE [Sales_H].[TxnDate] >= CAST(DATEADD(day, -1, GETDATE()) AS date) AND [Sales_H].[TxnDate] < CAST(GETDATE() AS date);