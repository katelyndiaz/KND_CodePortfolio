-- Katelyn N Diaz
-- Data Pulling in SQL, from EHR (Caboodle)

SELECT DISTINCT m.redcapID
, v.EncounterType
, d.DateValue
, v.Height,v.BodyMassIndex
, (v.WeightInOunces/16.0) AS WeightInPounds

FROM CDWMMCDB.dbo.DFM_KND_PowerUpMrn AS m

JOIN clrpt.fullaccess.PatientDim AS pat ON m.monte_mrn = pat.PrimaryMrn
JOIN clrpt.fullaccess.VisitFact AS v ON pat.DurableKey = v.PatientDurableKey
JOIN clrpt.fullaccess.ProviderDim AS prov ON v.PrimaryVisitProviderDurableKey = prov.DurableKey
JOIN clrpt.fullaccess.DateDim AS d ON d.DateKey = v.EncounterDateKey

WHERE d.DateValue>= '06/23/2021'
AND v.WeightInOunces IS NOT NULL

AND pat.iscurrent=1
AND prov.iscurrent=1
AND v.Complete_YesNo = 'yes'
;



SELECT DISTINCT m.redcapID
, d.DateValue
, lab.Value
, proce.PatientFriendlyName

FROM CDWMMCDB.dbo.DFM_KND_PowerUpMrn AS m

JOIN clrpt.fullaccess.PatientDim AS pat ON m.monte_mrn = pat.PrimaryMrn
JOIN clrpt.fullaccess.VisitFact AS v ON pat.DurableKey = v.PatientDurableKey
JOIN clrpt.fullaccess.ProviderDim AS prov ON v.PrimaryVisitProviderDurableKey = prov.DurableKey
LEFT JOIN clrpt.fullaccess.LabComponentResultFact AS lab ON pat.DurableKey = lab.PatientDurableKey
LEFT JOIN clrpt.fullaccess.ProcedureDim AS proce ON lab.ProcedureDurableKey = proce.DurableKey
JOIN clrpt.fullaccess.DateDim AS d ON d.DateKey = lab.OrderedDateKey

WHERE d.DateValue>= '06/23/2021'
AND proce.ProcedureEpicId = 828

AND pat.iscurrent=1
AND prov.iscurrent=1
AND v.Complete_YesNo = 'yes'
AND IsFinal_YesNo='yes'
AND IsReportable=1
;
