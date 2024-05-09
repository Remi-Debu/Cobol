CREATE TABLE PROSPECT (
	pros_id SMALLINT,
	pros_dateregistration CHAR(10) NOT NULL DEFAULT '2024-01-01',
	pros_namelast CHAR(30) NOT NULL DEFAULT 'DOE',
	pros_namefirst CHAR(30) NOT NULL DEFAULT 'JOHN',
	pros_emailname CHAR(30) NOT NULL DEFAULT 'N/A',
	pros_emaildomain CHAR(30) NOT NULL DEFAULT 'N/A',
	pros_gender CHAR(20) ,
	pros_datebirth CHAR(10),
	pros_phone CHAR(20) NOT NULL DEFAULT 'N/A',
	pros_addstreet CHAR(100),
	pros_addcity CHAR(35),
	pros_addcountry CHAR(35),
	pros_ownmobilephone CHAR(03),
	pros_ownwebathome CHAR(03),
	pros_ownfamilymembers CHAR(03),
	pros_durationmonthcall CHAR(04),
	CONSTRAINT id_prospect PRIMARY KEY (pros_id)
);

INSERT INTO PROSPECT VALUES (1, '2012-05-19', 'Prowting', 'Maena', 'nprowting0','behance.net', 'Female', '1976-06-19', '+33-552-949-1326', '0 Dwight Trail', 'Paris 06', 'France', 1, 'N/A', 'N/A', 116);
INSERT INTO PROSPECT VALUES (2, '2017-05-05', 'Steele', 'Faites', 'bsteele1','google.co.uk', 'Male', '1974-08-25', '+33-620-877-4374', '9 Mockingbird Hill', 'Tours', 'France', 1, 0, 1, 38);
INSERT INTO PROSPECT VALUES (3, '2019-05-01', 'Macknish', 'Adelie', 'mmacknish2','wordpress.com', 'Polygender', '1982-08-02', '+33-664-546-1807', '9 Katie Parkway', 'Fresnes', 'France', 1, 0, 4, 99);
INSERT INTO PROSPECT VALUES (4, '2010-01-21', 'Sedgebeer', 'Veronique', 'usedgebeer3','tripod.com', 'Male', '1997-12-05', '+33-881-864-0574', '9 Sommers Place', 'Paris 19', 'France', 0, 0, 'N/A', 76);
INSERT INTO PROSPECT VALUES (5, '2016-04-07', 'Frost', 'Faites', 'rfrost4','ed.gov', 'Male', '1974-08-12', '+33-250-772-5390', '8 Ridge Oak Hill', 'Castelnaudary', 'France', 0, 0, 'N/A', 38);
INSERT INTO PROSPECT VALUES (6, '2014-05-19', 'Adamczewski', 'Judicael', 'oadamczewski5','biglobe.ne.jp', 'Male', '1980-10-31', '+33-127-890-3261', '1124 Corscot Road', 'Levallois-Perret', 'France', 0, 0, 2, 125);
INSERT INTO PROSPECT VALUES (7, '2014-06-14', 'Bartholin', 'Maeline', 'lbartholin6','addthis.com', 'Female', '1993-07-31', '+33-386-676-4692', '7 Novick Parkway', 'Bressuire', 'France', 0, 1, 2, 115);
INSERT INTO PROSPECT VALUES (8, '2015-05-28', 'Heaslip', 'Pal', 'N/A','N/A', 'Female', '1994-04-06', '+33-630-673-0075', '3372 Mariners Cove Drive', 'Lomme', 'France', 0, 0, 1, 27);
INSERT INTO PROSPECT VALUES (9, '2014-09-24', 'Garry', 'Berangere', 'lgarry8','go.com', 'Male', '1957-09-23', '+33-905-153-0663', '250 Banding Lane', 'Belfort', 'France', 1, 0, 'N/A', 64);
INSERT INTO PROSPECT VALUES (10, '2010-05-11', 'Ojeda', 'Stevina', 'mojeda9','foxnews.com', 'Genderqueer', '1991-03-19', '+33-186-200-4764', '97 Loeprich Trail', 'Caen', 'France', 1, 0, 5, 'N/A');
INSERT INTO PROSPECT VALUES (11, '2019-02-03', 'Enoksson', 'Therese', 'henokssona','amazon.co.jp', 'Male', '1963-05-09', '+33-648-227-4177', '02302 Independence Point', 'Guebwiller', 'France', 'N/A', 0, 2, 'N/A');
INSERT INTO PROSPECT VALUES (12, '2015-07-20', 'Roaf', 'Maïlis', 'wroafb','bigcartel.com', 'Male', '1971-08-15', '+33-588-995-1018', '6074 Old Shore Junction', 'Strasbourg', 'France', 0, 1, 1, 54);
INSERT INTO PROSPECT VALUES (13, '2013-08-12', 'Mattaus', 'Faites', 'N/A','N/A', 'Male', '2000-04-27', '+33-548-750-9294', '46 Butterfield Way', 'Paris 15', 'France', 1, 0, 'N/A', 102);
INSERT INTO PROSPECT VALUES (14, '2018-09-13', 'Serjent', 'Irene', 'rserjentd','comcast.net', 'Female', '1995-04-23', '+33-669-605-8853', '67 Annamark Point', 'Niort', 'France', 'N/A', 0, 5, 41);
INSERT INTO PROSPECT VALUES (15, '2016-03-22', 'Wakeham', 'Anaelle', 'dwakehame','answers.com', 'Male', '1971-07-31', '+33-734-960-5457', '736 Novick Center', 'Avon', 'France', 1, 0, 1, 144);
INSERT INTO PROSPECT VALUES (16, '2015-12-09', 'Maruska', 'Gwenaelle', 'N/A','N/A', 'Female', '2001-07-25', '+33-728-491-6080', '78764 Kim Circle', 'Vanves', 'France', 0, 0, 5, 11);
INSERT INTO PROSPECT VALUES (17, '2014-06-01', 'Buxton', 'Desiree', 'cbuxtong','dailymail.co.uk', 'Female', '1958-06-29', '+33-170-493-1631', '3 Anhalt Park', 'La Seyne-sur-Mer', 'France', 0, 0, 'N/A', 23);
INSERT INTO PROSPECT VALUES (18, '2019-09-17', 'Worge', 'Beatrice', 'N/A','N/A', 'Male', '2004-08-10', '+33-815-632-5079', '33 Lukken Crossing', 'Valenciennes', 'France', 0, 1, 'N/A', 149);
INSERT INTO PROSPECT VALUES (19, '2012-07-09', 'Purdey', 'Long', 'bpurdeyi','goodreads.com', 'Genderfluid', '1974-10-21', '+33-480-472-6358', '1274 Hoard Junction', 'Saint-Avold', 'France', 1, 1, 1, 'N/A');
INSERT INTO PROSPECT VALUES (20, '2014-12-29', 'Dibden', 'Hakan', 'cdibdenj','alibaba.com', 'Male', '1965-12-25', '+33-969-304-2763', '7833 Brown Avenue', 'Paris 17', 'France', 'N/A', 0, 5, 135);
INSERT INTO PROSPECT VALUES (21, '2019-10-27', 'Dupey', 'Andrea', 'N/A','N/A', 'Female', '1992-08-13', '+33-169-848-3199', '92722 Johnson Lane', 'Avallon', 'France', 0, 1, 5, 2);
INSERT INTO PROSPECT VALUES (22, '2013-12-09', 'Finci', 'Borje', 'ffincil','loc.gov', 'Female', '1988-06-01', '+33-390-793-2749', '52 Service Crossing', 'Reims', 'France', 1, 0, 'N/A', 7);
INSERT INTO PROSPECT VALUES (23, '2013-03-27', 'Realy', 'Ye', 'N/A','N/A', 'Female', '1987-08-05', '+33-246-563-4685', '61397 Becker Park', 'Nimes', 'France', 1, 1, 1, 6);
INSERT INTO PROSPECT VALUES (24, '2018-04-16', 'Cumo', 'Rejane', 'lcumon','mac.com', 'Male', '1983-12-23', '+33-782-426-7451', '3476 Lindbergh Junction', 'Carrieres-sur-Seine', 'France', 1, 0, 4, 33);
INSERT INTO PROSPECT VALUES (25, '2017-03-22', 'Probate', 'Françoise', 'nprobateo','nifty.com', 'Male', '1959-11-23', '+33-317-787-4978', '193 Caliangt Junction', 'Istres', 'France', 0, 0, 'N/A', 137);
INSERT INTO PROSPECT VALUES (26, '2019-12-15', 'McRorie', 'Benedicte', 'tmcroriep','state.gov', 'Female', '1968-03-21', '+33-864-969-4560', '62637 Myrtle Park', 'Lille', 'France', 1, 0, 1, 57);
INSERT INTO PROSPECT VALUES (27, '2010-10-02', 'Clamp', 'Kalliste', 'N/A','N/A', 'Female', '1965-10-23', '+33-354-905-8588', '4229 Scott Junction', 'Alençon', 'France', 1, 0, 5, 'N/A');
INSERT INTO PROSPECT VALUES (28, '2016-02-25', 'Rodgerson', 'Gosta', 'drodgersonr','google.pl', 'Female', '1989-09-14', '+33-374-797-2656', '33 Stang Place', 'Ales', 'France', 1, 0, 1, 31);
INSERT INTO PROSPECT VALUES (29, '2011-06-09', 'Smeath', 'Leandre', 'csmeaths','newsvine.com', 'Female', '1986-06-19', '+33-271-412-9385', '85 Mallory Junction', 'Vedene', 'France', 0, 0, 'N/A', 'N/A');
INSERT INTO PROSPECT VALUES (30, '2011-09-23', 'Amy', 'Celia', 'aamyt','mapy.cz', 'Female', '1990-03-29', '+33-721-162-5655', '169 Sachtjen Circle', 'Parthenay', 'France', 1, 0, 3, 75);
INSERT INTO PROSPECT VALUES (31, '2014-10-30', 'Daugherty', 'Andrea', 'N/A','N/A', 'Male', '1951-07-08', '+33-874-510-6301', '8 Duke Center', 'Toulon', 'France', 0, 1, 2, 87);
INSERT INTO PROSPECT VALUES (32, '2014-08-14', 'Segrott', 'Severine', 'hsegrottv','vkontakte.ru', 'Polygender', '1989-02-10', '+33-221-574-4065', '620 Schlimgen Street', 'Ivry-sur-Seine', 'France', 'N/A', 1, 5, 116);
INSERT INTO PROSPECT VALUES (33, '2017-06-24', 'Mirando', 'Edmee', 'tmirandow','mtv.com', 'Male', '1982-10-13', '+33-985-573-0493', '7635 Judy Park', 'Saint-Jean-de-Luz', 'France', 0, 'N/A', 4, 16);
INSERT INTO PROSPECT VALUES (34, '2018-03-11', 'Forten', 'Selene', 'afortenx','people.com.cn', 'Male', '2001-08-08', '+33-571-987-3940', '455 Sundown Plaza', 'Sotteville-les-Rouen', 'France', 0, 0, 3, 20);
INSERT INTO PROSPECT VALUES (35, '2015-08-28', 'Thys', 'Interessant', 'vthysy','upenn.edu', 'Male', '1959-09-23', '+33-235-430-3955', '4 Commercial Junction', 'Montpellier', 'France', 0, 0, 3, 149);
INSERT INTO PROSPECT VALUES (36, '2010-08-24', 'Whisby', 'Gwenaelle', 'iwhisbyz','dot.gov', 'Female', '1982-09-09', '+33-940-462-2654', '422 International Crossing', 'Pau', 'France', 0, 'N/A', 1, 148);
INSERT INTO PROSPECT VALUES (37, '2019-01-10', 'Worner', 'Styrbjorn', 'N/A','N/A', 'Female', '1967-11-27', '+33-588-695-9355', '949 Green Ridge Alley', 'Paris 19', 'France', 'N/A', 0, 'N/A', 135);
INSERT INTO PROSPECT VALUES (38, '2015-11-14', 'Mapam', 'asa', 'fmapam11','ftc.gov', 'Male', '1964-07-13', '+33-601-492-3116', '8 Cascade Terrace', 'Bagnolet', 'France', 1, 1, 4, 39);
INSERT INTO PROSPECT VALUES (39, '2010-08-28', 'Cuzen', 'Melinda', 'N/A','N/A', 'Male', '1967-07-15', '+33-469-840-1068', '80 Atwood Lane', 'Paris 16', 'France', 0, 'N/A', 2, 71);
INSERT INTO PROSPECT VALUES (40, '2011-11-17', 'Verny', 'Lai', 'overny13','php.net', 'Male', '1961-11-18', '+33-848-235-9442', '4 Bashford Alley', 'Cergy-Pontoise', 'France', 0, 1, 5, 111);
INSERT INTO PROSPECT VALUES (41, '2011-09-06', 'Giampietro', 'Becassine', 'agiampietro14','accuweather.com', 'Male', '1992-11-23', '+33-669-959-3543', '8 Bonner Trail', 'Carcassonne', 'France', 0, 0, 2, 15);
INSERT INTO PROSPECT VALUES (42, '2015-06-10', 'Feitosa', 'Lie', 'kfeitosa15','umich.edu', 'Female', '1951-10-21', '+33-276-372-8769', '4826 Fairview Street', 'Valenciennes', 'France', 0, 0, 1, 20);
INSERT INTO PROSPECT VALUES (43, '2010-09-15', 'Palatini', 'Marta', 'cpalatini16','bloomberg.com', 'Female', '1951-10-30', '+33-109-444-6138', '585 Mcbride Place', 'Paris 08', 'France', 1, 1, 1, 'N/A');
INSERT INTO PROSPECT VALUES (44, '2019-05-06', 'Lockwood', 'Creez', 'N/A','N/A', 'Male', '1954-01-13', '+33-900-276-0459', '66387 Pierstorff Pass', 'Paris 15', 'France', 1, 1, 'N/A', 'N/A');
INSERT INTO PROSPECT VALUES (45, '2015-12-03', 'MacMaster', 'Marlene', 'mmacmaster18','who.int', 'Female', '1986-03-16', '+33-509-195-3769', '40 Gina Hill', 'Saint-Die-des-Vosges', 'France', 1, 1, 1, 17);
INSERT INTO PROSPECT VALUES (46, '2015-01-19', 'Tacon', 'Marta', 'N/A','N/A', 'Female', '1950-06-09', '+33-421-784-2230', '340 Pennsylvania Lane', 'Landivisiau', 'France', 'N/A', 1, 5, 139);
INSERT INTO PROSPECT VALUES (47, '2018-09-29', 'Featherston', 'Laurena', 'gfeatherston1a','booking.com', 'Male', '1956-12-03', '+33-912-866-6240', '51 Westerfield Hill', 'Altkirch', 'France', 1, 0, 4, 'N/A');
INSERT INTO PROSPECT VALUES (48, '2017-09-23', 'Schwaiger', 'Andre', 'rschwaiger1b','topsy.com', 'Female', '2004-09-28', '+33-532-486-8529', '7 Macpherson Plaza', 'Toulouse', 'France', 0, 1, 2, 'N/A');
INSERT INTO PROSPECT VALUES (49, '2013-12-14', 'Owbrick', 'Gerald', 'rowbrick1c','multiply.com', 'Male', '1982-07-09', '+33-597-384-0244', '4444 Nobel Plaza', 'Versailles', 'France', 1, 1, 5, 143);
INSERT INTO PROSPECT VALUES (50, '2017-09-24', 'Beales', 'Annotee', 'N/A','N/A', 'Female', '1971-11-22', '+33-269-808-2352', '629 Judy Drive', 'Saint-Amand-les-Eaux', 'France', 0, 1, 3, 7);
INSERT INTO PROSPECT VALUES (51, '2015-05-04', 'Goadbie', 'Ines', 'cgoadbie1e','fema.gov', 'Male', '1975-05-16', '+33-435-307-1054', '31 Spenser Pass', 'Le Raincy', 'France', 'N/A', 'N/A', 2, 70);
INSERT INTO PROSPECT VALUES (52, '2019-04-17', 'Croney', 'Leana', 'jcroney1f','yellowpages.com', 'Genderfluid', '1993-03-09', '+33-949-221-6847', '02 Hanover Street', 'Marseille', 'France', 0, 0, 1, 129);
INSERT INTO PROSPECT VALUES (53, '2016-09-15', 'Petrovykh', 'Melisandre', 'N/A','N/A', 'Male', '1985-11-20', '+33-200-953-6282', '586 Ilene Center', 'Montelimar', 'France', 1, 0, 1, 144);
INSERT INTO PROSPECT VALUES (54, '2019-06-25', 'Rhymes', 'Borje', 'N/A','N/A', 'Female', '1985-04-10', '+33-966-631-1578', '0 Bashford Parkway', 'Saint-etienne', 'France', 1, 0, 4, 109);
INSERT INTO PROSPECT VALUES (55, '2012-04-27', 'Derl', 'Ruo', 'cderl1i','cloudflare.com', 'Female', '1971-09-16', '+33-583-737-4518', '38678 Bayside Parkway', 'Orleans', 'France', 0, 1, 5, 'N/A');
INSERT INTO PROSPECT VALUES (56, '2014-07-04', 'Fortnum', 'Clea', 'N/A','N/A', 'Male', '1996-06-25', '+33-272-574-4095', '61 Vernon Terrace', 'Strasbourg', 'France', 1, 'N/A', 4, 72);
INSERT INTO PROSPECT VALUES (57, '2020-03-11', 'Eller', 'Angelique', 'jeller1k','cdbaby.com', 'Female', '1988-06-12', '+33-488-186-9968', '6 Hanson Parkway', 'Hayange', 'France', 1, 1, 2, 58);
INSERT INTO PROSPECT VALUES (58, '2017-04-17', 'Purviss', 'Zoe', 'mpurviss1l','qq.com', 'Female', '1999-11-26', '+33-393-694-4529', '7 Kingsford Avenue', 'Nogent-sur-Marne', 'France', 0, 0, 3, 'N/A');
INSERT INTO PROSPECT VALUES (59, '2014-03-17', 'Blei', 'Pal', 'eblei1m','drupal.org', 'Female', '1969-11-03', '+33-257-367-9639', '950 Knutson Road', 'Toulouse', 'France', 0, 1, 4, 84);
INSERT INTO PROSPECT VALUES (60, '2017-02-10', 'Folkes', 'Naeva', 'efolkes1n','slate.com', 'Female', '1965-09-02', '+33-758-124-7714', '238 Scofield Drive', 'Laon', 'France', 0, 'N/A', 4, 21);
INSERT INTO PROSPECT VALUES (61, '2017-06-07', 'Leckey', 'Annotee', 'rleckey1o','webeden.co.uk', 'Female', '1960-10-17', '+33-203-101-8000', '12584 Hooker Parkway', 'Saint-Die-des-Vosges', 'France', 'N/A', 0, 1, 62);
INSERT INTO PROSPECT VALUES (62, '2010-07-14', 'Murrish', 'Ku', 'dmurrish1p','google.fr', 'Genderfluid', '1991-08-22', '+33-228-338-8142', '63 American Ash Crossing', 'Paris 09', 'France', 0, 0, 5, 114);
INSERT INTO PROSPECT VALUES (63, '2015-06-21', 'Shapter', 'Marie-françoise', 'lshapter1q','themeforest.net', 'Male', '1993-10-07', '+33-276-725-3971', '10 Killdeer Lane', 'Rouen', 'France', 1, 1, 3, 119);
INSERT INTO PROSPECT VALUES (64, '2012-06-16', 'Mizzen', 'Lie', 'wmizzen1r','un.org', 'Male', '1962-03-24', '+33-661-311-4658', '0241 Lakeland Junction', 'La Ravoire', 'France', 1, 1, 3, 67);
INSERT INTO PROSPECT VALUES (65, '2016-11-22', 'Matys', 'Lysea', 'imatys1s','technorati.com', 'Male', '1980-10-28', '+33-174-724-9280', '23680 Linden Hill', 'Paris 02', 'France', 1, 1, 1, 15);
INSERT INTO PROSPECT VALUES (66, '2011-07-28', 'Bruhn', 'Cecilia', 'sbruhn1t','cargocollective.com', 'Male', '2003-06-26', '+33-927-540-2643', '26823 Comanche Trail', 'Tarbes', 'France', 'N/A', 1, 5, 'N/A');
INSERT INTO PROSPECT VALUES (67, '2016-11-08', 'Buzine', 'Hakan', 'ebuzine1u','ow.ly', 'Genderqueer', '1982-09-22', '+33-436-807-7101', '07 Messerschmidt Plaza', 'Lens', 'France', 1, 1, 1, 122);
INSERT INTO PROSPECT VALUES (68, '2010-09-24', 'Weins', 'Felicie', 'N/A','N/A', 'Male', '1998-07-12', '+33-906-689-5127', '53 Hovde Alley', 'Boe', 'France', 1, 0, 3, 123);
INSERT INTO PROSPECT VALUES (69, '2018-03-18', 'Swayne', 'osten', 'vswayne1w','adobe.com', 'Female', '1958-05-27', '+33-102-592-1449', '9968 Jana Hill', 'Paris 01', 'France', 1, 'N/A', 2, 89);
INSERT INTO PROSPECT VALUES (70, '2017-10-22', 'Jeays', 'Melys', 'kjeays1x','exblog.jp', 'Male', '1981-03-09', '+33-697-301-8548', '95 John Wall Road', 'Lyon', 'France', 1, 'N/A', 4, 'N/A');
INSERT INTO PROSPECT VALUES (71, '2016-04-27', 'Holt', 'Simplifies', 'mholt1y','nih.gov', 'Female', '1955-11-15', '+33-505-719-2820', '8470 Bartelt Alley', 'Le Blanc-Mesnil', 'France', 0, 'N/A', 5, 47);
INSERT INTO PROSPECT VALUES (72, '2018-02-07', 'Giffaut', 'Goran', 'N/A','N/A', 'Male', '1990-03-23', '+33-343-789-9180', '6 Oxford Crossing', 'Annecy-le-Vieux', 'France', 'N/A', 0, 5, 40);
INSERT INTO PROSPECT VALUES (73, '2016-09-12', 'Anderbrugge', 'Len', 'banderbrugge20','shop-pro.jp', 'Female', '1952-11-23', '+33-250-576-1316', '36469 Main Park', 'Creil', 'France', 1, 0, 5, 26);
INSERT INTO PROSPECT VALUES (74, '2019-09-15', 'Zuker', 'Leandre', 'mzuker21','paypal.com', 'Male', '1971-05-13', '+33-950-448-6688', '9189 Packers Street', 'Marseille', 'France', 1, 1, 1, 3);
INSERT INTO PROSPECT VALUES (75, '2011-08-02', 'Elie', 'Mahelie', 'aelie22','nytimes.com', 'Female', '1977-02-06', '+33-760-752-6005', '584 Bonner Plaza', 'Lyon', 'France', 1, 0, 4, 112);
INSERT INTO PROSPECT VALUES (76, '2011-04-10', 'Lukins', 'Interessant', 'tlukins23','stanford.edu', 'Bigender', '1997-08-28', '+33-285-270-6314', '63531 Heath Junction', 'Issy-les-Moulineaux', 'France', 0, 0, 2, 100);
INSERT INTO PROSPECT VALUES (77, '2013-01-23', 'Gaythorpe', 'Cinema', 'ggaythorpe24','cisco.com', 'Female', '1991-03-24', '+33-441-955-0486', '4044 Maple Point', 'Nevers', 'France', 'N/A', 1, 4, 120);
INSERT INTO PROSPECT VALUES (78, '2018-03-29', 'Bielefeld', 'Desiree', 'dbielefeld25','about.com', 'Female', '1983-05-20', '+33-248-558-0210', '9 Park Meadow Junction', 'Saint-Quentin', 'France', 'N/A', 1, 5, 'N/A');
INSERT INTO PROSPECT VALUES (79, '2015-08-11', 'Clayhill', 'Angele', 'N/A','N/A', 'Male', '2004-01-17', '+33-815-800-2977', '60 Wayridge Circle', 'Nice', 'France', 'N/A', 1, 'N/A', 54);
INSERT INTO PROSPECT VALUES (80, '2014-07-25', 'Eddoes', 'Long', 'geddoes27','spotify.com', 'Female', '1973-06-16', '+33-203-773-4220', '610 Pepper Wood Hill', 'Nice', 'France', 0, 'N/A', 3, 38);
INSERT INTO PROSPECT VALUES (81, '2020-02-03', 'Matonin', 'Venus', 'amatonin28','buzzfeed.com', 'Female', '1976-04-01', '+33-796-672-5691', '5 Service Trail', 'Bonneuil-sur-Marne', 'France', 1, 0, 5, 121);
INSERT INTO PROSPECT VALUES (82, '2015-12-15', 'Stonier', 'Marie-therese', 'cstonier29','va.gov', 'Male', '1953-08-14', '+33-417-338-0339', '46670 Stephen Junction', 'Privas', 'France', 1, 0, 'N/A', 65);
INSERT INTO PROSPECT VALUES (83, '2019-04-04', 'Rayworth', 'Leone', 'hrayworth2a','europa.eu', 'Female', '1999-05-25', '+33-867-727-0743', '37 Badeau Place', 'Aix-en-Provence', 'France', 1, 1, 5, 110);
INSERT INTO PROSPECT VALUES (84, '2014-09-23', 'Crosson', 'osten', 'ccrosson2b','hud.gov', 'Female', '1951-02-25', '+32-948-630-4658', '657 Burrows Hill', 'Namur', 'Belgium', 0, 0, 4, 150);
INSERT INTO PROSPECT VALUES (85, '2014-02-06', 'Philpot', 'Marylene', 'fphilpot2c','exblog.jp', 'Female', '1972-05-22', '+33-986-613-5205', '8501 Center Point', 'Bourgoin-Jallieu', 'France', 1, 0, 3, 77);
INSERT INTO PROSPECT VALUES (86, '2019-04-26', 'Veasey', 'Erwei', 'nveasey2d','craigslist.org', 'Female', '1973-08-30', '+33-917-924-5650', '6 Susan Circle', 'Luneville', 'France', 1, 0, 2, 55);
INSERT INTO PROSPECT VALUES (87, '2015-09-18', 'Halliberton', 'Oceanne', 'mhalliberton2e','weather.com', 'Male', '1998-03-16', '+33-968-306-8686', '6834 Merry Lane', 'Tournon-sur-Rhône', 'France', 1, 1, 4, 21);
INSERT INTO PROSPECT VALUES (88, '2017-12-09', 'Baiss', 'Anaelle', 'N/A','N/A', 'Genderfluid', '1952-12-19', '+33-967-323-9028', '0615 Sachtjen Park', 'Saumur', 'France', 1, 0, 'N/A', 86);
INSERT INTO PROSPECT VALUES (89, '2013-08-20', 'Yewdale', 'Solene', 'dyewdale2g','livejournal.com', 'Female', '1987-11-25', '+33-541-376-4872', '9556 Del Mar Pass', 'Lyon', 'France', 0, 1, 'N/A', 149);
INSERT INTO PROSPECT VALUES (90, '2015-10-23', 'Lunam', 'Felicie', 'mlunam2h','storify.com', 'Male', '1972-02-13', '+33-200-123-7525', '663 Elka Street', 'Haguenau', 'France', 'N/A', 0, 3, 98);
INSERT INTO PROSPECT VALUES (91, '2013-09-01', 'Cornhill', 'Yú', 'ccornhill2i','vinaora.com', 'Female', '1989-09-05', '+33-339-473-7061', '1 Anthes Crossing', 'Beziers', 'France', 1, 0, 5, 72);
INSERT INTO PROSPECT VALUES (92, '2017-01-12', 'Eastmead', 'Rejane', 'meastmead2j','go.com', 'Female', '1993-07-02', '+33-346-673-9905', '81 Buell Circle', 'Angoulême', 'France', 1, 1, 3, 121);
INSERT INTO PROSPECT VALUES (93, '2016-05-10', 'Solon', 'Ophelie', 'N/A','N/A', 'Female', '1954-08-18', '+33-930-796-7084', '04410 Erie Pass', 'Marseille', 'France', 'N/A', 0, 4, 2);
INSERT INTO PROSPECT VALUES (94, '2015-06-02', 'Sessuns', 'Ye', 'dsessuns2l','nbcnews.com', 'Female', '1965-01-08', '+33-476-199-2766', '6 Fisk Crossing', 'Orleans', 'France', 0, 'N/A', 3, 'N/A');
INSERT INTO PROSPECT VALUES (95, '2016-08-09', 'Linnitt', 'Maely', 'klinnitt2m','sciencedirect.com', 'Female', '1955-02-15', '+33-168-900-3119', '1 Sunbrook Way', 'Suresnes', 'France', 1, 1, 3, 'N/A');
INSERT INTO PROSPECT VALUES (96, '2019-02-14', 'Shelbourne', 'Garçon', 'eshelbourne2n','163.com', 'Female', '1988-09-22', '+33-505-320-6259', '492 Service Street', 'Dijon', 'France', 0, 1, 'N/A', 79);
INSERT INTO PROSPECT VALUES (97, '2012-10-07', 'Schaumann', 'Naelle', 'kschaumann2o','abc.net.au', 'Female', '1957-08-30', '+33-851-820-1002', '224 Westridge Alley', 'Billere', 'France', 0, 0, 5, 146);
INSERT INTO PROSPECT VALUES (98, '2019-01-26', 'Pitrelli', 'Athena', 'npitrelli2p','live.com', 'Female', '1971-04-16', '+33-637-536-5444', '79694 Weeping Birch Junction', 'Clichy', 'France', 0, 1, 1, 103);
INSERT INTO PROSPECT VALUES (99, '2014-11-20', 'Altham', 'Venus', 'zaltham2q','wikispaces.com', 'Female', '1955-10-16', '+33-475-544-0586', '319 Gateway Junction', 'evry', 'France', 'N/A', 0, 2, 'N/A');
INSERT INTO PROSPECT VALUES (100, '2015-03-14', 'Feechum', 'Anaïs', 'ufeechum2r','ezinearticles.com', 'Female', '1988-10-31', '+33-953-286-6499', '331 Esch Parkway', 'Troyes', 'France', 1, 0, 1, 36);
INSERT INTO PROSPECT VALUES (101, '2013-02-26', 'Brannigan', 'aslog', 'pbrannigan2s','ebay.co.uk', 'Female', '1951-12-18', '+32-890-196-4192', '6448 Kings Circle', 'Ninove', 'Belgium', 1, 0, 5, 14);
INSERT INTO PROSPECT VALUES (102, '2016-12-14', 'Basketter', 'Mans', 'ebasketter2t','paypal.com', 'Genderqueer', '1955-03-01', '+33-376-437-8713', '02 Linden Trail', 'Nantes', 'France', 1, 1, 5, 'N/A');
INSERT INTO PROSPECT VALUES (103, '2016-11-19', 'Okie', 'Maïte', 'lokie2u','abc.net.au', 'Male', '1966-07-28', '+33-497-167-5051', '81168 Welch Crossing', 'Antony', 'France', 1, 1, 1, 'N/A');
INSERT INTO PROSPECT VALUES (104, '2010-05-09', 'Diwell', 'Gwenaelle', 'ldiwell2v','ow.ly', 'Male', '1998-07-15', '+33-837-768-3654', '5456 Claremont Street', 'Le Havre', 'France', 1, 0, 4, 'N/A');
INSERT INTO PROSPECT VALUES (105, '2010-04-22', 'Cowling', 'Ophelie', 'gcowling2w','cam.ac.uk', 'Female', '1963-06-01', '+33-469-251-4903', '5381 Dottie Parkway', 'Aix-en-Provence', 'France', 'N/A', 1, 'N/A', 'N/A');
INSERT INTO PROSPECT VALUES (106, '2015-01-18', 'Vaneev', 'Naelle', 'avaneev2x','soundcloud.com', 'Female', '1992-05-24', '+33-835-429-7164', '8 Badeau Drive', 'Angers', 'France', 0, 1, 5, 101);
INSERT INTO PROSPECT VALUES (107, '2019-07-02', 'Allbrook', 'Laurelie', 'lallbrook2y','prweb.com', 'Male', '1990-02-24', '+33-775-187-6123', '9295 Caliangt Plaza', 'Suresnes', 'France', 0, 1, 2, 47);
INSERT INTO PROSPECT VALUES (108, '2018-04-18', 'Enrich', 'Eugenie', 'senrich2z','last.fm', 'Male', '2001-12-12', '+33-661-884-2120', '2 Sugar Lane', 'Nimes', 'France', 0, 1, 2, 106);
INSERT INTO PROSPECT VALUES (109, '2014-06-19', 'Robbert', 'Len', 'N/A','N/A', 'Polygender', '1956-09-23', '+33-732-887-0098', '8318 Surrey Avenue', 'Poitiers', 'France', 0, 0, 'N/A', 'N/A');
INSERT INTO PROSPECT VALUES (110, '2018-12-12', 'Schreiner', 'Dafnee', 'aschreiner31','merriam-webster.com', 'Genderfluid', '1965-01-10', '+33-148-559-3342', '01770 Upham Crossing', 'Massy', 'France', 0, 1, 3, 4);
INSERT INTO PROSPECT VALUES (111, '2017-06-14', 'Dight', 'Leonie', 'tdight32','telegraph.co.uk', 'Agender', '1954-09-25', '+33-536-970-2804', '40 Buhler Place', 'Grenoble', 'France', 0, 1, 4, 111);
INSERT INTO PROSPECT VALUES (112, '2018-04-21', 'Seymer', 'Maïwenn', 'N/A','N/A', 'Female', '1969-08-03', '+33-980-412-7004', '99 Mayer Drive', 'L''Aigle', 'France', 1, 'N/A', 5, 85);
INSERT INTO PROSPECT VALUES (113, '2014-03-08', 'Brownlea', 'Gosta', 'N/A','N/A', 'Non-binary', '1988-04-03', '+33-587-134-5407', '6 Gulseth Circle', 'Firminy', 'France', 0, 1, 1, 25);
INSERT INTO PROSPECT VALUES (114, '2011-07-05', 'Teesdale', 'Kalliste', 'nteesdale35','nba.com', 'Female', '1962-04-04', '+33-289-851-8266', '8 Moland Road', 'Quimperle', 'France', 'N/A', 0, 2, 33);
INSERT INTO PROSPECT VALUES (115, '2012-06-25', 'Bradnock', 'Lie', 'N/A','N/A', 'Male', '1979-12-10', '+33-298-555-4282', '596 Gerald Hill', 'Lorient', 'France', 1, 1, 4, 124);
INSERT INTO PROSPECT VALUES (116, '2019-07-06', 'Calderwood', 'Cinema', 'icalderwood37','linkedin.com', 'Male', '1974-02-16', '+33-491-802-8381', '924 Buhler Junction', 'Rungis', 'France', 0, 0, 1, 7);
INSERT INTO PROSPECT VALUES (117, '2018-10-28', 'Tregear', 'Gwenaelle', 'N/A','N/A', 'Male', '2001-02-02', '+33-112-849-7626', '80 Bluestem Pass', 'Briey', 'France', 'N/A', 0, 2, 36);
INSERT INTO PROSPECT VALUES (118, '2016-03-23', 'Clouter', 'Mylene', 'kclouter39','mit.edu', 'Male', '2002-02-18', '+33-580-154-2634', '2853 Dennis Circle', 'Altkirch', 'France', 1, 1, 'N/A', 45);
INSERT INTO PROSPECT VALUES (119, '2012-11-19', 'Crane', 'Geraldine', 'N/A','N/A', 'Female', '1959-06-04', '+33-501-864-6809', '23 Anniversary Way', 'Saint-Quentin', 'France', 'N/A', 0, 'N/A', 45);
INSERT INTO PROSPECT VALUES (120, '2016-01-19', 'Rames', 'Tan', 'mrames3b','ihg.com', 'Male', '1954-01-07', '+33-782-334-6025', '237 Del Sol Junction', 'Rungis', 'France', 0, 0, 5, 23);
INSERT INTO PROSPECT VALUES (121, '2013-09-20', 'Bellany', 'Melys', 'rbellany3c','google.co.uk', 'Male', '1976-01-26', '+33-461-832-2638', '0087 High Crossing Street', 'Chantepie', 'France', 1, 0, 3, 23);
INSERT INTO PROSPECT VALUES (122, '2015-01-24', 'Brouncker', 'Anael', 'N/A','N/A', 'Female', '1997-09-15', '+33-212-747-9519', '9 Sutherland Pass', 'Paris La Defense', 'France', 0, 1, 5, 'N/A');
INSERT INTO PROSPECT VALUES (123, '2013-06-10', 'Klaggeman', 'Anaïs', 'sklaggeman3e','boston.com', 'Female', '1975-09-06', '+33-206-196-6093', '89489 Red Cloud Parkway', 'Noisy-le-Grand', 'France', 0, 'N/A', 'N/A', 51);
INSERT INTO PROSPECT VALUES (124, '2019-12-16', 'Haylett', 'Andree', 'ehaylett3f','psu.edu', 'Male', '1959-12-07', '+33-697-637-6486', '0 Merrick Trail', 'Corbeil-Essonnes', 'France', 0, 0, 3, 'N/A');
INSERT INTO PROSPECT VALUES (125, '2014-01-25', 'Seel', 'Leonore', 'wseel3g','exblog.jp', 'Female', '1986-07-05', '+33-378-239-4277', '283 Texas Alley', 'Paris 17', 'France', 1, 0, 4, 104);
INSERT INTO PROSPECT VALUES (126, '2016-04-15', 'Tremblet', 'Marie-noel', 'ktremblet3h','miibeian.gov.cn', 'Male', '1951-10-16', '+33-819-302-1436', '940 Coolidge Way', 'Nice', 'France', 0, 'N/A', 2, 40);
INSERT INTO PROSPECT VALUES (127, '2015-08-04', 'MacCarlich', 'Cecilia', 'gmaccarlich3i','vistaprint.com', 'Male', '1990-07-16', '+33-175-792-6605', '8 Victoria Point', 'Grenoble', 'France', 0, 'N/A', 5, 126);
INSERT INTO PROSPECT VALUES (128, '2016-11-26', 'MacVanamy', 'Marta', 'emacvanamy3j','last.fm', 'Male', '1954-06-29', '+33-866-648-6149', '933 Tomscot Center', 'Paris 08', 'France', 0, 1, 3, 122);
INSERT INTO PROSPECT VALUES (129, '2017-10-19', 'Criag', 'Marylene', 'acriag3k','apache.org', 'Female', '1971-11-18', '+33-408-567-1996', '19 Waywood Crossing', 'Courtaboeuf', 'France', 1, 1, 2, 35);
INSERT INTO PROSPECT VALUES (130, '2015-04-04', 'Dissman', 'Judicael', 'edissman3l','si.edu', 'Female', '2000-07-13', '+33-528-381-9407', '8 Twin Pines Road', 'Boulogne-Billancourt', 'France', 1, 1, 4, 143);
INSERT INTO PROSPECT VALUES (131, '2014-03-15', 'Kidner', 'Selene', 'jkidner3m','abc.net.au', 'Female', '2000-10-17', '+33-614-811-6333', '0619 Manitowish Plaza', 'Berck', 'France', 1, 0, 5, 18);
INSERT INTO PROSPECT VALUES (132, '2014-12-03', 'Grinyov', 'Alizee', 'N/A','N/A', 'Genderqueer', '1962-03-04', '+33-674-792-4777', '4 Rockefeller Junction', 'La Roche-sur-Yon', 'France', 1, 1, 2, 139);
INSERT INTO PROSPECT VALUES (133, '2018-09-23', 'Meynell', 'Kevina', 'rmeynell3o','comcast.net', 'Male', '1967-03-20', '+33-803-498-4879', '218 Badeau Hill', 'Lyon', 'France', 0, 'N/A', 1, 54);
INSERT INTO PROSPECT VALUES (134, '2012-07-04', 'Lippini', 'Wa', 'N/A','N/A', 'Male', '1957-08-13', '+33-536-790-6118', '27376 Division Trail', 'Alfortville', 'France', 1, 0, 2, 114);
INSERT INTO PROSPECT VALUES (135, '2015-09-22', 'Delaney', 'Oceanne', 'N/A','N/A', 'Female', '1984-04-25', '+33-357-454-7460', '88 Shopko Trail', 'Reze', 'France', 'N/A', 1, 5, 71);
INSERT INTO PROSPECT VALUES (136, '2011-09-10', 'Braycotton', 'Penelope', 'N/A','N/A', 'Male', '1954-03-08', '+33-591-329-3174', '681 Hanson Road', 'Saint-Remy-de-Provence', 'France', 1, 0, 3, 'N/A');
INSERT INTO PROSPECT VALUES (137, '2013-06-13', 'Merwede', 'Lucrece', 'pmerwede3s','yahoo.co.jp', 'Female', '1977-07-03', '+33-273-835-1354', '0709 Mitchell Plaza', 'Strasbourg', 'France', 0, 'N/A', 'N/A', 'N/A');
INSERT INTO PROSPECT VALUES (138, '2011-07-22', 'McTaggart', 'Gisele', 'amctaggart3t','freewebs.com', 'Male', '1971-08-01', '+33-846-795-8503', '17 Kensington Road', 'Paris 18', 'France', 1, 'N/A', 'N/A', 60);
INSERT INTO PROSPECT VALUES (139, '2017-09-01', 'Wiggall', 'Judicael', 'awiggall3u','biglobe.ne.jp', 'Agender', '1965-11-29', '+33-867-480-0382', '9 Straubel Point', 'Meudon', 'France', 0, 0, 2, 88);
INSERT INTO PROSPECT VALUES (140, '2013-06-15', 'Scholtis', 'Becassine', 'sscholtis3v','github.com', 'Female', '2001-04-23', '+33-717-961-9683', '591 Heath Parkway', 'Mâcon', 'France', 1, 0, 3, 77);
INSERT INTO PROSPECT VALUES (141, '2020-03-27', 'Sacker', 'Naeva', 'hsacker3w','ask.com', 'Male', '1974-07-14', '+33-583-132-5232', '48741 Claremont Avenue', 'Aubenas', 'France', 0, 0, 4, 21);
INSERT INTO PROSPECT VALUES (142, '2014-10-31', 'Galsworthy', 'Aloïs', 'kgalsworthy3x','oaic.gov.au', 'Female', '1964-03-19', '+33-137-466-2580', '536 Kim Hill', 'Nice', 'France', 0, 1, 'N/A', 'N/A');
INSERT INTO PROSPECT VALUES (143, '2017-07-15', 'Hayth', 'Agnes', 'N/A','N/A', 'Genderfluid', '1957-06-24', '+33-115-547-0399', '80305 Cardinal Circle', 'Pontivy', 'France', 0, 'N/A', 2, 118);
INSERT INTO PROSPECT VALUES (144, '2010-09-20', 'Brownlie', 'Marie-josee', 'dbrownlie3z','tinypic.com', 'Female', '1983-05-22', '+33-881-314-5274', '98 Fremont Drive', 'Metz', 'France', 1, 1, 4, 66);
INSERT INTO PROSPECT VALUES (145, '2011-09-29', 'Collough', 'Marten', 'ecollough40','nifty.com', 'Male', '1961-08-21', '+33-993-896-7102', '36433 Kedzie Way', 'Lescar', 'France', 0, 1, 3, 107);
INSERT INTO PROSPECT VALUES (146, '2015-07-08', 'Pickerin', 'Noemie', 'cpickerin41','washingtonpost.com', 'Male', '1994-11-23', '+33-589-310-9488', '6 International Place', 'Vincennes', 'France', 0, 'N/A', 'N/A', 'N/A');
INSERT INTO PROSPECT VALUES (147, '2018-05-28', 'Lympenie', 'Clea', 'alympenie42','epa.gov', 'Female', '1954-11-17', '+33-999-318-2717', '27915 Bowman Place', 'Bezons', 'France',0, 1, 2, 'N/A');
INSERT INTO PROSPECT VALUES (148, '2010-04-12', 'Sidwell', 'Methode', 'hsidwell43','icq.com', 'Male', '1951-04-01', '+33-536-624-1979', '9967 American Ash Alley', 'Vitrolles', 'France', 'N/A', 0, 'N/A', 94);
INSERT INTO PROSPECT VALUES (149, '2013-04-14', 'Usborn', 'Andre', 'susborn44','flickr.com', 'Female', '1994-09-07', '+33-826-888-0348', '4 Sachtjen Parkway', 'Saint-Dizier', 'France', 1, 1, 1, 4);
INSERT INTO PROSPECT VALUES (150, '2019-09-15', 'Aspell', 'Nadege', 'baspell45','elegantthemes.com', 'Female', '1965-01-20', '+33-771-537-2414', '0 Lunder Court', 'Rungis', 'France', 1, 'N/A', 2, 112);

