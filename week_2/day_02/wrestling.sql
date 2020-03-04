--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.9
-- Dumped by pg_dump version 9.6.9

DROP TABLE IF EXISTS wrestlers;

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
-- Name: wrestlers; Type: TABLE; Schema: public
--

CREATE TABLE public.wrestlers (
    id integer NOT NULL,
    born character varying(50),
    name character varying(50),
    gender character varying(50),
    win_percent real,
    total_matches integer
);


--
-- Name: wrestlers_id_seq; Type: SEQUENCE; Schema: public
--

CREATE SEQUENCE public.wrestlers_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: wrestlers_id_seq; Type: SEQUENCE OWNED BY; Schema: public
--

ALTER SEQUENCE public.wrestlers_id_seq OWNED BY public.wrestlers.id;


--
-- Name: wrestlers id; Type: DEFAULT; Schema: public
--

ALTER TABLE ONLY public.wrestlers ALTER COLUMN id SET DEFAULT nextval('public.wrestlers_id_seq'::regclass);


--
-- Data for Name: wrestlers; Type: TABLE DATA; Schema: public
--

COPY public.wrestlers (id, born, name, gender, win_percent, total_matches) FROM stdin;
1	Mexico	El Torito	Male	92.5400009	67
2	USA	Rick Steiner	Male	91.0699997	56
3	USA	Tom Zenk	Male	90.3199997	31
4	USA	Warrior	Male	89.3600006	141
5	United Kingdom	Adrian Neville	Male	89.3300018	75
6	Finland	Tony Halme	Male	87.0999985	31
7	USA	Fred Ottman	Male	84.2099991	114
8	USA	Lex Luger	Male	83.3300018	102
9	USA	Bad News Brown	Male	83	100
10	USA	Kerry Von Erich	Male	82.8600006	70
11	Argentina	Argentina Apollo	Male	81.5800018	38
12	USA	Curtis Hughes	Male	80	30
13	USA	Jerry Sags	Male	80	85
14	USA	Ricky Steamboat	Male	79.5500031	88
15	USA	Samu	Male	78.9499969	95
16	USA	Naomi	Female	78.6399994	220
17	USA	Bill Eadie	Male	78.6200027	159
18	USA	Brian Knobbs	Male	78.5699997	84
19	New Zealand	Luke Williams	Male	78.4700012	144
20	USA	Jake Roberts	Male	78.3799973	222
21	Australia	Emma	Female	77.9700012	118
22	New Zealand	Butch Miller	Male	77.6200027	143
23	USA	Hulk Hogan	Male	77.0400009	196
24	USA	Mike Rotundo	Male	76.8399963	190
25	Canada	Raymond Rougeau	Male	76.4000015	89
26	USA	Dusty Rhodes	Male	76.3899994	72
27	USA	Butch Reed	Male	76.3600006	55
28	Canada	Rick Martel	Male	76.2799988	215
29	USA	The Junkyard Dog	Male	76.1900024	84
30	USA	Bam Bam Bigelow	Male	76.1100006	180
31	USA	Ed Leslie	Male	76.0199966	171
32	USA	Bobby Lashley	Male	75.8300018	120
33	USA	Rick Rude	Male	75.7799988	128
34	USA	Jim Duggan	Male	75.6200027	283
35	USA	The Fabulous Moolah	Female	75.5100021	49
36	USA	John Cena	Male	75.4899979	967
37	USA	Wayne Bloom	Male	75.4700012	53
38	USA	The Warlord	Male	75.2099991	121
39	USA	Robert Horne	Male	75	72
40	USA	Scott Hall	Male	74.75	202
41	USA	Tony Norris	Male	74.6999969	83
42	Canada	John Tenta	Male	74.5599976	169
43	USA	Jimmy Del Ray	Male	74.4700012	47
44	USA	Bryan Clark	Male	74.0699997	108
45	USA	Scott Steiner	Male	74.0699997	108
46	Italy	Dino Bravo	Male	73.75	160
47	USA	Bill Goldberg	Male	73.6800003	38
48	Italy	Bruno Sammartino	Male	73.3700027	169
49	USA	Kamala	Male	73.1200027	93
50	USA	John Nord	Male	73.0800018	52
51	Ireland	Sheamus	Male	73.0400009	638
52	USA	Barry Darsow	Male	73.0400009	204
53	USA	Curt Hennig	Male	72.8099976	217
54	USA	Harley Race	Male	72.3700027	76
55	USA	Ted DiBiase	Male	72.1999969	241
56	United Kingdom	Dynamite Kid	Male	71.9499969	82
57	Canada	Bret Hart	Male	71.9400024	360
58	USA	Brian Adams	Male	71.8399963	174
59	Tonga	The Barbarian	Male	71.1999969	125
60	USA	Mike Barton	Male	71.1900024	177
61	Canada	Jacques Rougeau, Jr.	Male	71.0800018	204
62	USA	Tyrus	Male	71.0500031	304
63	USA	Road Warrior Hawk	Male	70.9499969	148
64	USA	Randy Savage	Male	70.8499985	199
65	USA	Yokozuna	Male	70.8300018	120
66	Bulgaria	Rusev	Male	70.7200012	181
67	USA	One Man Gang	Male	70.4300003	115
68	USA	Koko B. Ware	Male	70.3399963	145
69	USA	Ray Apollo	Male	70.3099976	64
70	USA	Arn Anderson	Male	70.2099991	47
71	USA	Tully Blanchard	Male	70.2099991	47
72	USA	Tatanka	Male	69.9100037	216
73	USA	Ron Bass	Male	69.6399994	56
74	USA	Brock Lesnar	Male	69.6299973	135
75	USA	Billy Jack Haynes	Male	69.3899994	49
76	USA	Sean O'Haire	Male	69.3899994	49
77	USA	Terry Taylor	Male	68.6600037	67
78	USA	Mike Droese	Male	68.25	63
79	Puerto Rico	Pedro Morales	Male	68.0299988	147
80	USA	Tito Santana	Male	67.9700012	256
81	USA	Dan Spivey	Male	67.6500015	34
82	USA	The Tonga Kid	Male	67.6500015	68
83	United Kingdom	Davey-Boy Smith	Male	67.6299973	380
84	USA	The Undertaker	Male	67.6200027	738
85	USA	Shawn Michaels	Male	67.4800034	575
86	USA	Honky Tonk Man	Male	67.3099976	156
87	USA	Jim Neidhart	Male	67.2699966	220
88	USA	Jon Heidenreich	Male	67.2399979	58
89	Mexico	Mistico	Male	67.0999985	155
90	USA	Ray Fernandez	Male	67.0500031	173
91	USA	Rodney Mack	Male	66.6699982	48
92	USA	Daniel Bryan	Male	66.5	597
93	USA	Road Warrior Animal	Male	66.4899979	185
94	France	Andre The Giant	Male	66.3899994	119
95	Fiji	Jimmy Snuka	Male	66.3399963	101
96	USA	King Kong Bundy	Male	66.3000031	92
97	USA	Mike Enos	Male	65.5699997	61
98	USA	Jimmy Uso	Male	64.8799973	504
99	USA	Sid	Male	64.7699966	88
100	USA	Marty Jannetty	Male	64.7300034	224
101	USA	Johnny Barend	Male	64.7099991	34
102	USA	Matt Borne	Male	64.7099991	51
103	Australian	Roy Heffernan	Male	64.5199966	31
104	USA	Tony Atlas	Male	64.4400024	45
105	Mexico	Victor Rivera	Male	64.1800003	67
106	USA	Jey Uso	Male	64.0199966	503
107	Japan	Jinsei Shinzaki	Male	64	50
108	USA	Bob Backlund	Male	63.8899994	144
109	USA	Larry Zbyszko	Male	63.8300018	47
110	India	The Great Khali	Male	63.6599998	366
111	Canada	Owen Hart	Male	63.6100006	371
112	Poland	Ivan Putski	Male	63.5099983	74
113	USA	Hillbilly Jim	Male	63.4599991	52
114	USA	The Boogeyman	Male	63.1599998	38
115	Canada	Roddy Piper	Male	62.9199982	89
116	Japan	Killer Khan	Male	62.8600006	35
117	Canada	Ron Garvin	Male	62.8600006	35
118	Greece	Spiros Arion	Male	62.7900009	43
119	USA	Rey Mysterio	Male	62.6399994	554
120	USA	Kaitlyn	Female	61.5400009	221
121	USA	Sergeant Slaughter	Male	61.2599983	111
122	USA	Lita	Female	61.0800018	203
123	USA	Marco Corleone	Male	60.9500008	105
124	USA	Kevin Nash	Male	60.8699989	115
125	USA	Dr. Tom Prichard	Male	60.7099991	84
126	USA	Steve Austin	Male	60.5299988	342
127	USA	Adrian Adonis	Male	60.4199982	48
128	United Kingdom	Paige	Female	60.3400002	174
129	Canada	Sami Zayn	Male	60.2900009	68
130	USA	Billy Gunn	Male	60.2700005	667
131	Russia	Alexander Kozlov	Male	60	145
132	USA	Billy Graham	Male	60	45
133	Canada	Natalya	Female	59.7400017	467
134	USA	Eve Torres	Female	59.6699982	181
135	Canada	Robert Maillet	Male	59.6500015	57
136	USA	Pat Tanaka	Male	59.6199989	52
137	Italy	Al Costello	Male	59.4599991	37
138	USA	Steve Blackman	Male	59.3800011	256
139	USA	Rob Van Dam	Male	58.9700012	507
140	USA	Barry Windham	Male	58.8199997	68
141	USA	Bobo Brazil	Male	58.6199989	87
142	Canada	Phil Lafon	Male	58.6199989	29
143	Canada	Chris Benoit	Male	58.5400009	480
144	USA	Greg Valentine	Male	58.5299988	217
145	USA	Mickie James	Female	58.4700012	183
146	USA	B. Brian Blair	Male	58.4599991	65
147	USA	The Rock	Male	58.2400017	455
148	USA	Shane Douglas	Male	58.0600014	31
149	USA	Batista	Male	58.0200005	374
150	Canada	Carl Ouellet	Male	57.6500015	85
151	USA	Kelly Kelly	Female	57.2799988	206
152	Tonga	Haku	Male	57.2099991	208
153	USA	Doug Furnas	Male	57.1399994	28
154	USA	Mike Shaw	Male	57.1399994	35
155	USA	Candice Michelle	Female	56.9399986	72
156	USA	Jerry Lawler	Male	56.9099998	123
157	USA	Ryback	Male	56.6199989	521
158	USA	Paul Orndorff	Male	56.3600006	55
159	USA	Jeff Jarrett	Male	56.1699982	235
160	USA	Sean Waltman	Male	56.1599998	495
161	USA	Big E	Male	56.0400009	323
162	USA	Percy Watson	Male	55.7099991	70
163	USA	Sam Houston	Male	55.5600014	27
164	USA	Beth Phoenix	Female	55.4500008	202
165	USA	Eva Marie	Female	55.3199997	47
166	USA	Ken Patera	Male	55.2599983	76
167	Ghana	Kofi Kingston	Male	55.2599983	742
168	USA	Rikishi	Male	55.1199989	459
169	USA	Ray Traylor	Male	55.0900002	403
170	USA	C. M. Punk	Male	55.0299988	587
171	USA	R-Truth	Male	54.9700012	604
172	USA	Viscera	Male	54.8800011	328
173	USA	Don Muraco	Male	54.8699989	113
174	USA	Lance Cade	Male	54.8600006	175
175	Iran	The Iron Sheik	Male	54.8400002	93
176	USA	Dean Ambrose	Male	54.6199989	487
177	Canada	Santino Marella	Male	54.5	411
178	USA	Nikki Bella	Female	54.4799995	268
179	USA	Mark Canterbury	Male	54.3199997	162
180	USA	Haystacks Calhoun	Male	54.2900009	35
181	USA	Ken Shamrock	Male	54.1899986	155
182	USA	The Big Show	Male	54.1300011	1149
183	USA	Chief Jay Strongbow	Male	54.1300011	109
184	USA	Road Dogg	Male	54.0499992	346
185	USA	Chuck Palumbo	Male	54.0400009	235
186	Canada	Trish Stratus	Female	53.9300003	267
187	USA	Chyna	Female	53.8499985	91
188	Yugoslavia	Nikolai Volkoff	Male	53.8499985	169
189	USA	Vader	Male	53.6599998	123
190	USA	Sin Cara	Male	53.1399994	239
191	USA	Virgil	Male	53.0600014	98
192	USA	Roman Reigns	Male	53.0099983	415
193	USA	Matt Sydal	Male	52.9799995	168
194	USA	Kevin Fertig	Male	52.9399986	51
195	USA	Shad Gaspard	Male	52.8800011	104
196	USA	Ron Simmons	Male	52.7700005	415
197	USA	Umaga	Male	52.3499985	149
198	USA	Randy Orton	Male	52.2999992	958
199	USA	The Pimp Fatha	Male	52.1300011	376
200	USA	Blackjack Mulligan	Male	51.7200012	29
201	USA	Triple H	Male	51.6500015	788
202	USA	Torrie Wilson	Female	51.6399994	122
203	Japan	Ultimo Dragon	Male	51.6100006	31
204	USA	Kurt Angle	Male	51.5800018	539
205	USA	Dallas Page	Male	51.5200005	66
206	USA	Xavier Woods	Male	51.3499985	111
207	Canada	Pat Patterson	Male	51.2799988	39
208	Canada	Edge	Male	51.2599983	794
209	USA	Bo Dallas	Male	50.9599991	157
210	USA	Eddie Guerrero	Male	50.9300003	322
211	USA	A.J. Lee	Female	50.9199982	271
212	USA	Brie Bella	Female	50.8800011	283
213	USA	Chris Candido	Male	50.8800011	57
214	USA	Paul Roma	Male	50.8199997	122
215	USA	Taz	Male	50.7200012	138
216	USA	Jazz	Female	50.5200005	97
217	USA	John Layfield	Male	50.4099998	607
218	Spain	Kane	Male	50.1800003	1355
219	USA	Ashley Massaro	Female	50	44
220	USA	Dean Malenko	Male	50	132
221	USA	Jeff Hardy	Male	50	672
222	USA	Jerry Lynn	Male	50	36
223	USA	Kid Kash	Male	50	38
224	USA	Ray Gordy	Male	50	44
225	USA	Booker T.	Male	49.8800011	401
226	USA	Mark Henry	Male	49.7599983	617
227	United Kingdom	Bad News Barrett	Male	49.5699997	464
228	USA	Devon	Male	49.5699997	464
229	USA	Mick Foley	Male	49.4300003	263
230	South Africa	Adam Rose	Male	49.3199997	146
231	USA	Chris Jericho	Male	49.1100006	894
232	Puerto Rico	Savio Vega	Male	48.9700012	194
233	USA	Bubba Ray Dudley	Male	48.9599991	482
234	USA	Terry Funk	Male	48.8400002	43
235	USA	Matt Hardy	Male	48.7200012	821
236	USA	Steve Keirn	Male	48.6500015	37
237	USA	Melina	Female	48.6300011	183
238	Australia	Tony Garea	Male	48.5999985	107
239	United Kingdom	Mason Ryan	Male	48.5699997	35
240	American Samoa	Sika	Male	48.3300018	60
241	USA	Perry Saturn	Male	48.3199997	238
242	USA	Arnold Skaaland	Male	48.2799988	29
243	Guyana	Big Ryck	Male	48.2799988	116
244	USA	Ernie Ladd	Male	48.2799988	29
245	USA	Sable	Female	48.2799988	29
246	Canada	Test	Male	48.25	487
247	USA	Snitsky	Male	48.2299995	141
248	USA	Bob Holly	Male	48.0600014	541
249	USA	Goldust	Male	48.0499992	614
250	USA	Hornswoggle	Male	48.0299988	127
\.


--
-- Name: wrestlers_id_seq; Type: SEQUENCE SET; Schema: public; Owner
--

SELECT pg_catalog.setval('public.wrestlers_id_seq', 250, true);


--
-- Name: wrestlers wrestlers_pkey; Type: CONSTRAINT; Schema: public
--

ALTER TABLE ONLY public.wrestlers
    ADD CONSTRAINT wrestlers_pkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--

