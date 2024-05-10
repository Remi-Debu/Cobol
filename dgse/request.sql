/* SELECT MAX(age) AS AGE_MAX FROM databank; */
/* SELECT MIN(age) AS AGE_MAX FROM databank; */
/* SELECT age, COUNT(*) AS nombre_individus 
FROM databank
GROUP BY age
ORDER BY age; */
/* SELECT db.first_name, db.last_name, db.email, ph.phrase
FROM databank as db
JOIN phrase as ph ON db.country_code = ph.country_code
WHERE db.country = 'Belgium'; */
/* SELECT MIN(age) AS age_min, MAX(age) AS age_max, 
       PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY age) AS age_median
FROM databank; */
/* SELECT pays, 
       SUM(CASE WHEN genre = 'homme' THEN 1 ELSE 0 END) / COUNT(*) AS proportion_hommes,
       SUM(CASE WHEN genre = 'femme' THEN 1 ELSE 0 END) / COUNT(*) AS proportion_femmes
FROM databank
GROUP BY pays; */
/*
SELECT country, 
			 count(gender) AS total,
       ROUND((AVG(CASE WHEN gender = 'Agender' THEN 1 ELSE 0 END) * 100),2) AS Agender,
       ROUND((AVG(CASE WHEN gender = 'Bigender' THEN 1 ELSE 0 END) * 100),2) AS Bigender,
       ROUND((AVG(CASE WHEN gender = 'Female' THEN 1 ELSE 0 END) * 100),2) AS Female,
       ROUND((AVG(CASE WHEN gender = 'Genderfluid' THEN 1 ELSE 0 END) * 100),2) AS Genderfluid,
       ROUND((AVG(CASE WHEN gender = 'Genderqueer' THEN 1 ELSE 0 END) * 100),2) AS Genderqueer,
       ROUND((AVG(CASE WHEN gender = 'Male' THEN 1 ELSE 0 END) * 100),2) AS Male,
       ROUND((AVG(CASE WHEN gender = 'Non-binary' THEN 1 ELSE 0 END) * 100),2) AS NonBinary,
       ROUND((AVG(CASE WHEN gender = 'Polygender' THEN 1 ELSE 0 END) * 100),2) AS Polygender
FROM databank
GROUP BY country ORDER BY country ASC;
*/
