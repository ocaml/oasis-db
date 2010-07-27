--
-- PostgreSQL database dump
--

-- Started on 2010-07-15 18:17:09 CEST

SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- TOC entry 1759 (class 1262 OID 16429)
-- Dependencies: 1758
-- Name: account_ext; Type: COMMENT; Schema: -; Owner: account_ext_admin
--

COMMENT ON DATABASE account_ext IS 'External account management database';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 1471 (class 1259 OID 16450)
-- Dependencies: 3
-- Name: tokens; Type: TABLE; Schema: public; Owner: account_ext_admin; Tablespace: 
--

CREATE TABLE tokens (
    token character(128) NOT NULL,
    user_id integer NOT NULL,
    expire timestamp with time zone NOT NULL
);


ALTER TABLE public.tokens OWNER TO account_ext_admin;

--
-- TOC entry 1469 (class 1259 OID 16430)
-- Dependencies: 1740 1741 1742 1743 3
-- Name: users; Type: TABLE; Schema: public; Owner: account_ext_admin; Tablespace: 
--

CREATE TABLE users (
    user_id integer NOT NULL,
    user_name text DEFAULT ''::text NOT NULL,
    realname character varying(32) DEFAULT ''::character varying NOT NULL,
    timezone character varying(64) DEFAULT 'GMT'::character varying,
    language integer DEFAULT 1 NOT NULL,
    firstname character varying(60),
    lastname character varying(60)
);


ALTER TABLE public.users OWNER TO account_ext_admin;

--
-- TOC entry 1472 (class 1259 OID 16459)
-- Dependencies: 1548 3
-- Name: account_ext; Type: VIEW; Schema: public; Owner: account_ext_admin
--

CREATE VIEW account_ext AS
    SELECT tokens.token, tokens.expire, tokens.user_id, users.user_name, users.realname, users.timezone, users.language FROM tokens, users WHERE (tokens.user_id = users.user_id);


ALTER TABLE public.account_ext OWNER TO account_ext_admin;

--
-- TOC entry 1470 (class 1259 OID 16442)
-- Dependencies: 1744 1745 1746 3
-- Name: user_group; Type: TABLE; Schema: public; Owner: account_ext_admin; Tablespace: 
--

CREATE TABLE user_group (
    user_group_id integer NOT NULL,
    user_id integer DEFAULT 0 NOT NULL,
    group_id integer DEFAULT 0 NOT NULL,
    role_id integer DEFAULT 1
);


ALTER TABLE public.user_group OWNER TO account_ext_admin;

--
-- TOC entry 1755 (class 0 OID 16450)
-- Dependencies: 1471
-- Data for Name: tokens; Type: TABLE DATA; Schema: public; Owner: account_ext_admin
--

COPY tokens (token, user_id, expire) FROM stdin;
5ZwQ1+x2EjbgXUf1IfkIVLmhN+EOrIeNU3lrSekH                                                                                        	0	2010-07-15 18:43:25.063566+02
\.


--
-- TOC entry 1754 (class 0 OID 16442)
-- Dependencies: 1470
-- Data for Name: user_group; Type: TABLE DATA; Schema: public; Owner: account_ext_admin
--

COPY user_group (user_group_id, user_id, group_id, role_id) FROM stdin;
0	0	0	0
1	1	0	0
2	2	0	1
3	3	0	1
\.


--
-- TOC entry 1753 (class 0 OID 16430)
-- Dependencies: 1469
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: account_ext_admin
--

COPY users (user_id, user_name, realname, timezone, language, firstname, lastname) FROM stdin;
0	admin1	Foo	CEST	1	Foo	Foo
1	admin2	Bar	GMT	1	Bar	Bar
2	user1	Foo user	GMT	1	Foo	Foo
3	user2	Bar user	GMT	1	Bar	Bar
\.


--
-- TOC entry 1752 (class 2606 OID 16454)
-- Dependencies: 1471 1471
-- Name: token_pkey; Type: CONSTRAINT; Schema: public; Owner: account_ext_admin; Tablespace: 
--

ALTER TABLE ONLY tokens
    ADD CONSTRAINT token_pkey PRIMARY KEY (token);


--
-- TOC entry 1750 (class 2606 OID 16449)
-- Dependencies: 1470 1470
-- Name: user_group_pkey; Type: CONSTRAINT; Schema: public; Owner: account_ext_admin; Tablespace: 
--

ALTER TABLE ONLY user_group
    ADD CONSTRAINT user_group_pkey PRIMARY KEY (user_group_id);


--
-- TOC entry 1748 (class 2606 OID 16441)
-- Dependencies: 1469 1469
-- Name: users_pkey; Type: CONSTRAINT; Schema: public; Owner: account_ext_admin; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (user_id);


--
-- TOC entry 1761 (class 0 OID 0)
-- Dependencies: 3
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- TOC entry 1762 (class 0 OID 0)
-- Dependencies: 1471
-- Name: tokens; Type: ACL; Schema: public; Owner: account_ext_admin
--

REVOKE ALL ON TABLE tokens FROM PUBLIC;
REVOKE ALL ON TABLE tokens FROM account_ext_admin;
GRANT ALL ON TABLE tokens TO account_ext_admin;
GRANT SELECT ON TABLE tokens TO PUBLIC;


--
-- TOC entry 1763 (class 0 OID 0)
-- Dependencies: 1469
-- Name: users; Type: ACL; Schema: public; Owner: account_ext_admin
--

REVOKE ALL ON TABLE users FROM PUBLIC;
REVOKE ALL ON TABLE users FROM account_ext_admin;
GRANT ALL ON TABLE users TO account_ext_admin;
GRANT SELECT ON TABLE users TO account_ext_3rdparty;


--
-- TOC entry 1764 (class 0 OID 0)
-- Dependencies: 1472
-- Name: account_ext; Type: ACL; Schema: public; Owner: account_ext_admin
--

REVOKE ALL ON TABLE account_ext FROM PUBLIC;
REVOKE ALL ON TABLE account_ext FROM account_ext_admin;
GRANT ALL ON TABLE account_ext TO account_ext_admin;
GRANT SELECT ON TABLE account_ext TO PUBLIC;


--
-- TOC entry 1765 (class 0 OID 0)
-- Dependencies: 1470
-- Name: user_group; Type: ACL; Schema: public; Owner: account_ext_admin
--

REVOKE ALL ON TABLE user_group FROM PUBLIC;
REVOKE ALL ON TABLE user_group FROM account_ext_admin;
GRANT ALL ON TABLE user_group TO account_ext_admin;
GRANT SELECT ON TABLE user_group TO account_ext_3rdparty;


-- Completed on 2010-07-15 18:17:09 CEST

--
-- PostgreSQL database dump complete
--

