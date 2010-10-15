--
-- PostgreSQL database dump
--

-- Started on 2010-08-24 18:34:20 CEST

SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

--
-- TOC entry 1743 (class 1262 OID 16464)
-- Dependencies: 1742
-- Name: oasis_db_dev; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON DATABASE oasis_db_dev IS 'OASIS-DB persistent data';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 1467 (class 1259 OID 16465)
-- Dependencies: 3
-- Name: account_ext; Type: TABLE; Schema: public; Owner: -; Tablespace: 
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
-- TOC entry 1469 (class 1259 OID 16549)
-- Dependencies: 1737 3
-- Name: log; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE log (
    id integer NOT NULL,
    level character(1) NOT NULL,
    what text NOT NULL,
    message text NOT NULL,
    replay text NOT NULL,
    path text NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now() NOT NULL
);


--
-- TOC entry 1468 (class 1259 OID 16547)
-- Dependencies: 3 1469
-- Name: log_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE log_id_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- TOC entry 1746 (class 0 OID 0)
-- Dependencies: 1468
-- Name: log_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE log_id_seq OWNED BY log.id;


--
-- TOC entry 1736 (class 2604 OID 16552)
-- Dependencies: 1469 1468 1469
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE log ALTER COLUMN id SET DEFAULT nextval('log_id_seq'::regclass);


--
-- TOC entry 1739 (class 2606 OID 16557)
-- Dependencies: 1469 1469
-- Name: key_id; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY log
    ADD CONSTRAINT key_id PRIMARY KEY (id);


--
-- TOC entry 1745 (class 0 OID 0)
-- Dependencies: 3
-- Name: public; Type: ACL; Schema: -; Owner: -
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


-- Completed on 2010-08-24 18:34:20 CEST

--
-- PostgreSQL database dump complete
--

