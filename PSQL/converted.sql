CREATE TABLE CONVERTED (
	conv_id SMALLINT,
	conv_prospect_id SMALLINT,
	conv_operator_id SMALLINT,
	conv_call_date DATE,
	conv_mobile_taken INT,
	conv_web_taken INT,
	CONSTRAINT id_converted PRIMARY KEY (conv_id),
	FOREIGN KEY (conv_prospect_id) REFERENCES PROSPECT(pros_id),
	FOREIGN KEY (conv_operator_id) REFERENCES OPERATOR(ope_id)
	);

INSERT INTO CONVERTED VALUES (1, 88, 6, '2023-11-04', 1, 0);
INSERT INTO CONVERTED VALUES (2, 30, 1, '2024-01-28', 0, 0);
INSERT INTO CONVERTED VALUES (3, 143, 10, '2023-12-03', 1, 0);
INSERT INTO CONVERTED VALUES (4, 109, 10, '2023-10-27', 1, 0);
INSERT INTO CONVERTED VALUES (5, 93, 7, '2024-04-12', 0, 1);
INSERT INTO CONVERTED VALUES (6, 74, 5, '2024-02-12', 0, 1);
INSERT INTO CONVERTED VALUES (7, 95, 9, '2023-12-18', 1, 0);
INSERT INTO CONVERTED VALUES (8, 25, 7, '2024-02-29', 1, 1);
INSERT INTO CONVERTED VALUES (9, 11, 9, '2023-11-16', 0, 1);
INSERT INTO CONVERTED VALUES (10, 135, 2, '2024-02-12', 1, 0);
INSERT INTO CONVERTED VALUES (11, 13, 6, '2023-10-27', 0, 1);
INSERT INTO CONVERTED VALUES (12, 130, 2, '2024-03-20', 0, 0);
INSERT INTO CONVERTED VALUES (13, 142, 2, '2024-01-28', 1, 1);
INSERT INTO CONVERTED VALUES (14, 128, 2, '2023-10-23', 1, 0);
INSERT INTO CONVERTED VALUES (15, 140, 3, '2023-11-02', 0, 1);
INSERT INTO CONVERTED VALUES (16, 3, 10, '2024-03-22', 1, 0);
INSERT INTO CONVERTED VALUES (17, 7, 5, '2023-12-21', 1, 0);
INSERT INTO CONVERTED VALUES (18, 52, 7, '2023-10-27', 0, 1);
INSERT INTO CONVERTED VALUES (19, 89, 7, '2024-01-01', 1, 1);
INSERT INTO CONVERTED VALUES (20, 103, 1, '2024-03-04', 1, 1);
INSERT INTO CONVERTED VALUES (21, 116, 3, '2023-12-02', 1, 1);
INSERT INTO CONVERTED VALUES (22, 71, 6, '2024-03-19', 1, 0);
INSERT INTO CONVERTED VALUES (23, 65, 1, '2024-02-09', 1, 0);
INSERT INTO CONVERTED VALUES (24, 112, 8, '2024-01-09', 1, 0);
INSERT INTO CONVERTED VALUES (25, 98, 10, '2023-10-15', 0, 1);
INSERT INTO CONVERTED VALUES (26, 14, 6, '2023-10-22', 1, 1);
INSERT INTO CONVERTED VALUES (27, 96, 8, '2023-11-06', 0, 1);
INSERT INTO CONVERTED VALUES (28, 51, 10, '2023-10-22', 0, 0);
INSERT INTO CONVERTED VALUES (31, 124, 8, '2023-10-16', 1, 0);
INSERT INTO CONVERTED VALUES (32, 47, 9, '2023-11-28', 0, 0);
INSERT INTO CONVERTED VALUES (33, 129, 2, '2024-02-19', 0, 1);
INSERT INTO CONVERTED VALUES (36, 10, 8, '2023-11-26', 1, 1);
INSERT INTO CONVERTED VALUES (37, 69, 1, '2024-01-18', 1, 0);
INSERT INTO CONVERTED VALUES (39, 55, 6, '2023-10-28', 1, 0);
INSERT INTO CONVERTED VALUES (40, 45, 2, '2023-12-07', 0, 0);
INSERT INTO CONVERTED VALUES (41, 127, 2, '2024-02-06', 0, 1);
INSERT INTO CONVERTED VALUES (42, 76, 3, '2024-04-01', 1, 0);
INSERT INTO CONVERTED VALUES (43, 101, 1, '2023-12-31', 0, 1);
INSERT INTO CONVERTED VALUES (47, 43, 8, '2023-12-24', 1, 0);
INSERT INTO CONVERTED VALUES (48, 78, 5, '2024-02-28', 1, 0);