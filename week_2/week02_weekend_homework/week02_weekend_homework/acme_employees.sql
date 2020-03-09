--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.9
-- Dumped by pg_dump version 9.6.9

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: employees; Type: TABLE; Schema: public
--

CREATE TABLE public.employees (
    id integer NOT NULL,
    first_name character varying(100),
    last_name character varying(100),
    email character varying(100),
    team_id integer NOT NULL
);


--
-- Name: feedbacks; Type: TABLE; Schema: public
--

CREATE TABLE public.feedbacks (
    id integer NOT NULL,
    employee_id integer NOT NULL,
    message text,
    date date
);


--
-- Name: teams; Type: TABLE; Schema: public
--

CREATE TABLE public.teams (
    id integer NOT NULL,
    name character varying(50),
    charge_cost integer
);


--
-- Data for Name: employees; Type: TABLE DATA; Schema: public
--

COPY public.employees (id, first_name, last_name, email, team_id) FROM stdin;
1	Tabby	Lesley	tlesley0@shinystat.com	4
2	Wallie	Edson	\N	1
3	Bunnie	Di Maria	bdimaria2@list-manage.com	3
4	Harris	Condliffe	hcondliffe3@apache.org	1
5	Lydia	Shout	lshout4@flavors.me	10
6	Gayel	Fear	gfear5@columbia.edu	7
7	Mildrid	Pinney	mpinney6@hubpages.com	6
8	S	Mathevet	smathevet7@state.tx.us	3
9	Crin	Hollyar	chollyar8@vistaprint.com	1
10	Palmer	Di Nisco	pdinisco9@behance.net	3
11	Adelind	Shout	ashouta@tripod.com	4
12	Trina	Polet	tpoletb@businessweek.com	10
13	Jasmine	Tuminelli	jtuminellic@etsy.com	1
14	Sarita	Farenden	sfarendend@topsy.com	1
15	Davina	Cory	dcorye@w3.org	10
16	Huberto	Blockley	hblockleyf@fema.gov	1
17	Sauncho	Peake	speakeg@digg.com	9
18	Orelia	Passfield	opassfieldh@chicagotribune.com	1
19	Wanda	Lardiner	wlardineri@amazon.de	7
20	Bennett	Harvatt	bharvattj@shutterfly.com	1
21	D	Proven	dprovenk@moonfruit.com	3
22	Pyotr	Lantry	\N	2
23	Brenna	Belden	bbeldenm@nasa.gov	8
24	Dorette	Angel	dangeln@weibo.com	3
25	Gregorio	Brymham	gbrymhamo@list-manage.com	3
26	Jayne	Crankhorn	jcrankhornp@chronoengine.com	7
27	Hildy	Main	hmainq@over-blog.com	8
28	Lionel	Noli	lnolir@joomla.org	9
29	Rockwell	Chasen	rchasens@cornell.edu	3
30	Rianon	Fulkes	rfulkest@deviantart.com	9
31	Annecorinne	Merton	amertonu@clickbank.net	8
32	Imogen	Hovington	ihovingtonv@hubpages.com	1
33	Dorella	Crowden	dcrowdenw@pbs.org	9
34	Montgomery	Lyard	mlyardx@washington.edu	1
35	Noach	Hollyer	nhollyery@i2i.jp	5
36	Asia	Hatherill	ahatherillz *at* java.com	9
37	Wilfrid	Christal	wchristal10@oracle.com	9
38	Jenica	Van Der Hoog	jvanderhoog11@photobucket.com	10
39	Hollis	Sevior	hsevior12@archive.org	4
40	My	Scripture	mscripture13@geocities.com	1
41	Neville	Rushworth	nrushworth14@harvard.edu	6
42	Tracee	Frankes	tfrankes15@squidoo.com	9
43	Lilla	Ellor	lellor16@gnu.org	9
44	Brittne	Brislawn	bbrislawn17@webnode.com	1
45	Tallulah	Canedo	tcanedo18@wunderground.com	8
46	Grannie	Iskowicz	giskowicz19@un.org	9
47	Barde	Tourot	btourot1a@123-reg.co.uk	2
48	Sal	Rockcliff	srockcliff1b@china.com.cn	3
49	Cheston	Reichardt	creichardt1c@ted.com	8
50	Rickert	Fearns	rfearns1d@yahoo.co.jp	7
51	Wash	Hannaford	whannaford1e@cnet.com	7
52	Zondra	Elphick	zelphick1f@soundcloud.com	9
53	Cornall	Tempest	ctempest1g@geocities.jp	6
54	Marcelline	Jahnke	mjahnke1h@fc2.com	4
55	Linnea	Berger	lberger1i@google.fr	2
56	J	Satcher	jsatcher1j@networkadvertising.org	3
57	Lilia	Ducarne	lducarne1k@who.int	10
58	Ingaborg	Pocklington	ipocklington1l@ocn.ne.jp	7
59	Guendolen	Morshead	gmorshead1m@blog.com	1
60	Osbourn	Danzelman	odanzelman1n@google.cn	10
61	Sacha	Meredyth	smeredyth1o@state.tx.us	1
62	Duky	Shord	dshord1p@edublogs.org	10
63	Dorie	Bains	dbains1q@buzzfeed.com	5
64	Adrian	McQuillin	amcquillin1r@soundcloud.com	8
65	Albertina	Anespie	aanespie1s@buzzfeed.com	4
66	Crystal	Cotton	ccotton1t@cpanel.net	2
67	Leese	Linstead	llinstead1u@github.com	3
68	Lori	Giorgioni	lgiorgioni1v@infoseek.co.jp	6
69	Sammy	Debrick	sdebrick1w@usgs.gov	5
70	Isador	Sneaker	isneaker1x@amazon.co.uk	5
71	Evania	Robjents	erobjents1y@cpanel.net	10
72	Sammy	Muggleston	smuggleston1z@dion.ne.jp	5
73	Stearne	Eckert	seckert20@shinystat.com	5
74	Janenna	Gallaher	jgallaher21@walmart.com	2
75	Vita	Munnery	vmunnery22@tumblr.com	3
76	Shannon	Spalls	sspalls23@qq.com	9
77	Danella	Renzullo	drenzullo24@soup.io	5
78	Roman	Abazi	rabazi25@google.it	2
79	Arline	Upton	aupton26@domainmarket.com	3
80	Bonny	Rawsen	brawsen27@pcworld.com	1
81	Christyna	Calbreath	ccalbreath28@free.fr	3
82	Maxie	Risebarer	mrisebarer29@jugem.jp	1
83	Ian	Blonfield	iblonfield2a admin.ch	2
84	Cullen	McGown	cmcgown2b@sciencedaily.com	8
85	Persis	Tolhurst	ptolhurst2c@plala.or.jp	6
86	Norene	Filchagin	nfilchagin2d@huffingtonpost.com	4
87	Latia	McKeggie	lmckeggie2e@oakley.com	9
88	Joelie	Ruberry	jruberry2f@yandex.ru	1
89	Krista	Lebreton	klebreton2g@clickbank.net	6
90	Titus	Spurgeon	tspurgeon2h@sohu.com	5
91	Jonathon	McGlynn	jmcglynn2i@fc2.com	9
92	Astrix	Geldard	ageldard2j@behance.net	9
93	T	Regina	tregina2k@about.com	1
94	Emanuele	Georgescu	egeorgescu2l@1688.com	10
95	Jozef	Yerrill	jyerrill2m@yolasite.com	6
96	Jana	Bygrave	jbygrave2n@surveymonkey.com	5
97	Rabi	Ellsbury	rellsbury2o@facebook.com	6
98	Westbrook	Ubanks	wubanks2p@youtube.com	8
99	Jeanne	Dimmick	jdimmick2q@wikia.com	6
100	Toby	Blaisdell	\N	7
101	Kathryne	Toomey	ktoomey2s@businessinsider.com	8
102	Iggie	Griffey	igriffey2t@toplist.cz	4
103	Concettina	Scown	cscown2u@bandcamp.com	9
104	Giulio	Haliday	ghaliday2v@moonfruit.com	5
105	Maire	Skiplorne	mskiplorne2w@com.com	6
106	Ilse	Barltrop	ibarltrop2xmacromedia.com	4
107	Alejoa	Broomhead	abroomhead2y@meetup.com	9
108	Gretchen	Tidswell	gtidswell2z@cornell.edu	9
109	Randene	Armall	rarmall30@myspace.com	6
110	Kari	Kobiela	kkobiela31@people.com.cn	6
111	Salvidor	Freeth	sfreeth32@123-reg.co.uk	2
112	Agnes	Gorgl	agorgl33@oakley.com	4
113	Donelle	Dollard	ddollard34@slate.com	10
114	Garland	Matusiak	gmatusiak35@mapy.cz	3
115	Flynn	Towlson	ftowlson36@huffingtonpost.com	6
116	Ellen	Massel	emassel37@whitehouse.gov	7
117	Harwell	Cheine	hcheine38@wiley.com	6
118	Blanche	Gawthorpe	bgawthorpe39@google.it	2
119	Lorri	Pesak	lpesak3a@cnn.com	1
120	Brok	Tilling	btilling3b@php.net	1
121	Gracia	Wandtke	gwandtke3c@networksolutions.com	3
122	Raimund	Fishlock	rfishlock3d@dailymail.co.uk	2
123	Dulcie	Gilbey	dgilbey3e@cbsnews.com	9
124	Alberik	Shill	ashill3f@house.gov	8
125	Britt	Croot	bcroot3g@va.gov	3
126	Mireille	Fideler	mfideler3h@w3.org	5
127	Knox	McSharry	kmcsharry3i@fda.gov	2
128	Emmalyn	Gallon	egallon3j@cbc.ca	7
129	Benjy	Eskrigg	beskrigg3k@businessweek.com	6
130	Vallie	Dyott	vdyott3l@simplemachines.org	9
131	Shurwood	Bengle	sbengle3m@posterous.com	6
132	Amalea	Rigts	arigts3n@i2i.jp	7
133	Dianemarie	Bennit	dbennit3o@businessinsider.com	8
134	Abbey	Threadgill	athreadgill3p@pagesperso-orange.fr	4
135	Cale	Crosser	ccrosser3q@omniture.com	9
136	Brant	Winscum	bwinscum3r@salon.com	8
137	H	Gayler	hgayler3s@youtube.com	3
138	Dagmar	Mathis	dmathis3t@plala.or.jp	10
139	Salli	Caddy	scaddy3u@mozilla.org	6
140	Konstanze	Floodgate	kfloodgate3v@nba.com	10
141	Archy	Pocknoll	apocknoll3w@imageshack.us	8
142	Scotti	Magson	smagson3x@utexas.edu	3
143	Jody	Jina	jjina3y@netscape.com	6
144	Renaldo	Kneeshaw	rkneeshaw3z@amazon.co.uk	9
145	Avie	Megroff	amegroff40@hhs.gov	9
146	Carey	DelaField	cdelafield41@comsenz.com	7
147	Odella	O'Fearguise	oofearguise42@multiply.com	1
148	Willi	McBoyle	wmcboyle43@dot.gov	4
149	Dolf	Ghelerdini	dghelerdini44@ifeng.com	8
150	Hieronymus	Cisneros	hcisneros45@bloomberg.com	10
151	Derwin	Rings	drings46@zdnet.com	4
152	Claudia	Gresham	cgresham47@google.com.au	9
153	Modestia	Uttley	muttley48@hatena.ne.jp	7
154	Arnaldo	Gingles	agingles49@state.tx.us	8
155	Wallie	Castellino	wcastellino4a@cnn.com	8
156	Arne	Slides	aslides4b@dropbox.com	4
157	P	Prescote	pprescote4c@fc2.com	3
158	Karolina	Elvin	kelvin4d@amazon.de	4
159	Linnell	Merritt	lmerritt4e@constantcontact.com	7
160	Jess	Purkess	jpurkess4f@soup.io	7
161	Leopold	Childs	lchilds4g@digg.com	2
162	Jan	Loughnan	jloughnan4h@admin.ch	6
163	Pippy	Scotchford	pscotchford4i@opensource.org	9
164	Norris	Soffe	nsoffe4jstate.gov	4
165	Miran	Poyzer	mpoyzer4k@amazon.com	1
166	Tucky	Bulger	tbulger4l@dmoz.org	7
167	Elvis	Lendon	elendon4m@columbia.edu	2
168	Nettie	Filyashin	nfilyashin4n@hexun.com	1
169	Jinny	Tregenza	jtregenza4o@google.pl	9
170	Aldwin	Hurler	ahurler4p@nationalgeographic.com	3
171	Cordelie	Beckles	cbeckles4q@tumblr.com	9
172	King	MacAlees	kmacalees4r@nba.com	4
173	Vinson	Tuckley	vtuckley4s@xrea.com	1
174	Hyatt	Mulvany	hmulvany4t@japanpost.jp	6
175	Noll	Yu	nyu4u@nationalgeographic.com	1
176	Ash	Grabban	agrabban4v@fda.gov	4
177	Lin	Shutler	lshutler4w@delicious.com	8
178	Kevan	Scotchbourouge	kscotchbourouge4x@goo.ne.jp	2
179	Brion	Sebastian	bsebastian4y@rambler.ru	6
180	Leigh	Mc Gaughey	lmcgaughey4z@de.vu	7
181	Art	Feehely	afeehely50@webnode.com	9
182	Vivia	Birden	vbirden51@webs.com	8
183	Ranice	Beckey	rbeckey52@goo.gl	2
184	Bibby	Jurasek	bjurasek53@usa.gov	10
185	J	Spir	jspir54@example.com	3
186	Tobey	Dulinty	tdulinty55@illinois.edu	3
187	Marijn	Hessenthaler	mhessenthaler56@fastcompany.com	2
188	Melany	O'Finan	mofinan57@ustream.tv	10
189	Anderson	Bergstrand	abergstrand58@canalblog.com	10
190	Laurie	Lanchester	llanchester59@mapy.cz	5
191	Bary	Arrigucci	barrigucci5a@slate.com	1
192	Welsh	Tourne	wtourne5b@twitpic.com	9
193	Susanetta	Pulhoster	spulhoster5c@i2i.jp	4
194	B	Horning	bhorning5d@oracle.com	3
195	Emlen	Blackburn	eblackburn5e@ow.ly	6
196	Arlin	Chismon	achismon5f@last.fm	10
197	Dun	Haward	dhaward5g@illinois.edu	2
198	Cart	Hadlee	chadlee5h@livejournal.com	3
199	Deny	Nutbeem	dnutbeem5i@weebly.com	4
200	Brett	Fasset	bfasset5j@canalblog.com	7
\.


--
-- Data for Name: feedbacks; Type: TABLE DATA; Schema: public
--

COPY public.feedbacks (id, employee_id, message, date) FROM stdin;
1	33	Slow advancement, political, and inconsistent expectations.	2017-11-15
2	186	work helps me to indulge into something which helps me in achieving something and prove myself.i like working smart	2016-08-09
3	74	Great management, decently quick training for greater responsibility, seasonal jobs that turn into full time, and a great crew of people that are sticking with the company. If you were RJ hard, you can take your pick of positions.	2018-03-06
4	171	Great benefits and perks Good Salary Great WLB	2018-07-18
5	55	Slow decision making as the company continues to grow at an amazing pace.	2016-11-01
6	85	They don’t provide paid trainning because you work for a third partier... but I believe Acme should get involved there and provided various paid training	2016-04-02
7	121	The initial time after training is difficult because much of the skills needed will be learned on the job. Honestly there are not a lot of cons. People who like face-to-face interactions may go stir crazy but otherwise it’s no problem at all.	2017-03-13
8	86	The perks for the employees are amazing	2016-09-21
9	33	Corporate housing might be quite far from the office location, which may cause problems for students who plan to bike. Unlike Acme, no free food (unless there is a meeting) for interns.	2016-11-28
10	71	boring projects but it's fine	2017-05-03
11	130	Big company, grow fast and lot of opportunities	2017-04-18
12	143	Great Pay. Great Benefits. Great way to lose weight	2019-05-12
13	98	My experience was that of an intern so it may not be typical, but I ended up doing work I had absolutely no interest in and didn't envision getting my degree for without much opportunity to do something different. When interns are recruited they don't have much of a say on what type of work they'd like to do, so if you're stuck with something you don't like and want to work on more interesting things, that's what evenings and weekends are for.  If you have a PhD you can do some fascinating things at Acme, but if you just have an undergrad degree you might be better off at a smaller company where you will get more opportunities to do different kinds of work that span several job positions at Acme.	2015-11-28
14	136	Work friendly environment Young working crowd No work pressure Good facilities and perks	2016-09-13
15	55	Good culture, great people, good benefits, visa support.	2017-10-25
16	172	Managers are NOT in tune with employees and are too busy trying to think up of new sales processes to understand their products, employees and are often patronzing. Management are not true to their word and don't do anything to address the negative feedback from staff	2018-04-14
17	122	Nice people around. Good work life balance.	2018-05-01
18	33	Growth is little slow for associates.	2015-02-10
19	22	Resource rich in funds and talent, lots of hours to interact and leverage synergies, work on all aspect of project and interact with the leadership and contributors	2017-05-09
20	95	Limited stock bonus, move slow	2016-11-27
21	33	Review model doesn't encourage real teamwork Benefits steadily worsening Uncertain future Closed culture	2019-05-25
22	10	pay is alright, a bit edgey at times.	2019-05-27
23	122	it didn't pay very well and required a lot of training so it was a lot of work.	2018-11-13
24	190	Great management and team to work with. Acme does take care of its employees and the benefits are great.	2016-03-28
25	118	Cant think of many cons.	2015-09-20
26	97	Bureaucratic, Leadership says things public but often has different guidance to the managers	2017-04-16
27	157	Time zone challenges are existing in some projects. US Team is more exposed when compared to India team to the career growth.	2018-07-22
28	110	Need a lot of travels.	2015-05-27
29	81	Great benefits, great people, good pay. Great product discounts. Opportunities for advancement	2018-06-26
30	122	Great brand recognition and a good resume builder. Decent level of autonomy and a high level of responsibility. They hire smart people. New businesses are always under development.	2016-05-07
31	6	Fast paced, friendly people, helpful, fun	2018-05-31
32	55	pretty much everything corporate but to be expected in its stage in life. no longer is everyone allowed to be a pioneer and it was just sad to watch the new ones some in and just get frustrated.	2017-12-03
33	2	Work /life balance, attrition, peak!	2016-08-29
34	7	everyone focuses on immediate impact. this can prove to be a bane in the long run, if one does not occasionally stop, slow down to pay the technical debt	2016-08-23
35	118	You will get to learn new stuff. Good salary.	2017-06-22
36	190	Ten hour days could be eight	2015-01-27
37	148	- Great benefits - Great managers	2018-11-17
38	122	Great place to work at Friendly enviornment	2015-09-04
39	86	- Motivating work culture - Very international and diverse - Strong peers to learn from - Competitive salaries and perks	2016-04-21
40	53	So much to learn all of the time, very fast paced, and given latitude to think on your feet.	2015-02-01
41	46	Lack of formal training or certifications offered	2016-04-22
42	133	Very fun people to work with	2019-05-06
43	99	Dans zones éloigné , travail très dur ,la pause est insuffisante je propose 2 fois 30 mn et une fois 15 mn .	2017-09-04
44	13	A high level of ownership over your business also comes with great responsibility. The heavy workload in such an ambiguous environment can be difficult to manage at times. You need to draw boundaries for yourself, no one will do it for you.	2017-08-21
45	55	Not any cons from my observation.	2019-06-26
46	70	All benefits of working in the corporate world.	2016-12-03
47	110	Work life balance, too many teams to consider to make a difference.	2018-12-12
48	183	Challenging work, perks, enlightened senior management	2015-03-20
49	194	Benefits, Work/life balance, opportunities to grow, caring atmosphere.	2018-07-14
50	148	Certain Teams has minute management with the employees, which always depends manager to manager	2015-08-23
51	110	Great team. Very smart and with a great energy.	2015-06-04
52	138	none at all writing this bothers me	2018-12-03
53	12	Schedules, cafeteria, transportation and supplies.	2017-07-10
54	21	Lots of free food. Lots of cafes. Lots of trees. Lots of fresh air. Lots of buildings. Lots of smart people to work with and hard problems to solve.	2018-08-11
55	131	Even with a new, revised review model, Acme is a terrible place to fail or make a mistake. You get flushed out of the toilet pretty quickly. Managers play favorites on who they hire and promote, and who they don't.	2018-12-09
56	134	Smart people, many very motivated. Working with them can be humbling.  In Redmond at least where I work: medical center, library, nice campus, amenities for cyclists (showers towels bike tools), the makers garage (free use of 3D printers, laser cutters, etc.)	2018-12-29
57	92	Not suitable for slow-lerners, very agile, highly competetive	2016-06-09
58	37	A great company to work with.	2015-05-23
59	61	Hard to innovate new products	2018-03-18
60	32	The easy work can become repetitive very quickly. The equipment always seems to be broken and with as much money as Acme has coming in, there's no excuse for it.	2018-07-12
\.


--
-- Data for Name: teams; Type: TABLE DATA; Schema: public
--

COPY public.teams (id, name, charge_cost) FROM stdin;
1	Audit Team 1	30
2	Audit Team 2	30
3	Risk Team 1	50
4	Risk Team 2	50
5	Audit Escalate	60
6	Risk Escalate	100
7	Data Team 1	40
8	Data Team 2	40
9	Data Escalate	80
10	Corporate	100
11	Uncommitted	0
\.


--
-- Name: employees employees_pkey; Type: CONSTRAINT; Schema: public
--

ALTER TABLE ONLY public.employees
    ADD CONSTRAINT employees_pkey PRIMARY KEY (id);


--
-- Name: feedbacks feedbacks_pkey; Type: CONSTRAINT; Schema: public
--

ALTER TABLE ONLY public.feedbacks
    ADD CONSTRAINT feedbacks_pkey PRIMARY KEY (id);


--
-- Name: teams teams_pkey; Type: CONSTRAINT; Schema: public
--

ALTER TABLE ONLY public.teams
    ADD CONSTRAINT teams_pkey PRIMARY KEY (id);


--
-- Name: employees employees_team_id_fkey; Type: FK CONSTRAINT; Schema: public
--

ALTER TABLE ONLY public.employees
    ADD CONSTRAINT employees_team_id_fkey FOREIGN KEY (team_id) REFERENCES public.teams(id);


--
-- Name: feedbacks feedbacks_employee_id_fkey; Type: FK CONSTRAINT; Schema: public
--

ALTER TABLE ONLY public.feedbacks
    ADD CONSTRAINT feedbacks_employee_id_fkey FOREIGN KEY (employee_id) REFERENCES public.employees(id);


--
-- PostgreSQL database dump complete
--

