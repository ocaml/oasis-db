--
-- PostgreSQL database dump
--

-- Started on 2010-07-16 18:58:04 CEST

SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

ALTER TABLE ONLY public.account_ext DROP CONSTRAINT account_ext_pkey;
DROP TABLE public.account_ext;
DROP SCHEMA public;
--
-- TOC entry 1736 (class 1262 OID 16464)
-- Dependencies: 1735
-- Name: oasis_db_dev; Type: COMMENT; Schema: -; Owner: oasis_db
--

COMMENT ON DATABASE oasis_db_dev IS 'OASIS-DB persistent data';


--
-- TOC entry 3 (class 2615 OID 2200)
-- Name: public; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA public;


ALTER SCHEMA public OWNER TO postgres;

--
-- TOC entry 1737 (class 0 OID 0)
-- Dependencies: 3
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS 'standard public schema';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 1463 (class 1259 OID 16465)
-- Dependencies: 3
-- Name: account_ext; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE account_ext (
    token character(128)[] NOT NULL,
    expire timestamp with time zone[],
    user_id integer,
    user_name text,
    realname character varying(32)[],
    timezone character varying(64)[],
    language integer
);


ALTER TABLE public.account_ext OWNER TO postgres;

--
-- TOC entry 1732 (class 0 OID 16465)
-- Dependencies: 1463
-- Data for Name: account_ext; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY account_ext (token, expire, user_id, user_name, realname, timezone, language) FROM stdin;
\.


--
-- TOC entry 1731 (class 2606 OID 16472)
-- Dependencies: 1463 1463
-- Name: account_ext_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY account_ext
    ADD CONSTRAINT account_ext_pkey PRIMARY KEY (token);


--
-- TOC entry 1738 (class 0 OID 0)
-- Dependencies: 3
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


-- Completed on 2010-07-16 18:58:04 CEST

--
-- PostgreSQL database dump complete
--

