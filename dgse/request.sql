/* SELECT MAX(age) AS AGE_MAX FROM databank; */
/* SELECT MIN(age) AS AGE_MAX FROM databank; */
SELECT age, COUNT(*) AS nombre_individus 
FROM databank
GROUP BY age
ORDER BY age;