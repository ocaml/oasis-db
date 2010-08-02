--
-- PostgreSQL database dump
--

-- Started on 2010-07-29 19:58:24 CEST

SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- TOC entry 1734 (class 1262 OID 16464)
-- Dependencies: 1733
-- Name: oasis_db_dev; Type: COMMENT; Schema: -; Owner: oasis_db
--

COMMENT ON DATABASE oasis_db_dev IS 'OASIS-DB persistent data';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 1463 (class 1259 OID 16465)
-- Dependencies: 3
-- Name: account_ext; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE account_ext (
    user_id integer,
    user_name text,
    language integer,
    realname character varying(32),
    timezone character varying(64),
    token character(128),
    expire timestamp with time zone
);


--
-- TOC entry 1730 (class 0 OID 16465)
-- Dependencies: 1463
-- Data for Name: account_ext; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY account_ext (user_id, user_name, language, realname, timezone, token, expire) FROM stdin;
\.


-- Completed on 2010-07-29 19:58:24 CEST

--
-- PostgreSQL database dump complete
--

