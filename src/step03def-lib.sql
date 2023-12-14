--
--  Grade scientific/logistics
--

CREATE EXTENSION IF NOT EXISTS postgis;

DROP SCHEMA IF EXISTS osmc CASCADE;
CREATE SCHEMA osmc;

------------------
-- Helper functions:

CREATE or replace FUNCTION osmc.ij_to_xy(
  i  int, -- coluna
  j  int, -- linha
  x0 int, -- referencia de inicio do eixo x [x0,y0]
  y0 int, -- referencia de inicio do eixo y [x0,y0]
  s  int  -- lado da célula
) RETURNS int[] AS $f$
  SELECT array[
    x0 + i*s,
    y0 + j*s
  ]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.ij_to_xy(int,int,int,int,int)
 IS 'Retorna canto inferior esquerdo de célula na matriz.'
;
--SELECT osmc.ij_to_xy(1,1,4180000,1035500,262144);

CREATE or replace FUNCTION osmc.ij_to_geom(
  i  int, -- coluna
  j  int, -- linha
  x0 int, -- referencia de inicio do eixo x [x0,y0]
  y0 int, -- referencia de inicio do eixo y [x0,y0]
  s  int, -- lado da célula
  sr int  -- srid
) RETURNS geometry AS $f$
  SELECT ggeohash.draw_cell_bycenter(v[1]+s/2,v[2]+s/2,s/2,false,sr)
  FROM
  (
    SELECT osmc.ij_to_xy(i,j,x0,y0,s) v
  ) t
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.ij_to_geom(int,int,int,int,int,int)
 IS 'Retorna geometria de célula na matriz.'
;
--SELECT osmc.ij_to_geom(0,0,4180000,1035500,262144,9377);

CREATE or replace FUNCTION osmc.ij_to_bbox(
  i  int, -- coluna
  j  int, -- linha
  x0 int, -- referencia de inicio do eixo x [x0,y0]
  y0 int, -- referencia de inicio do eixo y [x0,y0]
  s  int  -- lado da célula
) RETURNS int[] AS $f$
  SELECT array[ x0+i*s, y0+j*s, x0+i*s+s, y0+j*s+s ]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.ij_to_bbox(int,int,int,int,int)
 IS 'Retorna bbox de célula da matriz.'
;
-- SELECT osmc.ij_to_bbox(0,0,4180000,1035500,262144);

CREATE or replace FUNCTION osmc.ij_to_box2d(
  i  int, -- coluna
  j  int, -- linha
  x0 int, -- referencia de inicio do eixo x [x0,y0]
  y0 int, -- referencia de inicio do eixo y [x0,y0]
  s  int  -- lado da célula
) RETURNS BOX2D AS $f$
  SELECT ('BOX(' || x0+i*s || ' ' || y0+j*s || ',' || x0+i*s+s || ' ' || y0+j*s+s || ')')::BOX2D
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.ij_to_box2d(int,int,int,int,int)
 IS 'Retorna box2d de célula da matriz.'
;
-- SELECT osmc.ij_to_box2d(0,0,4180000,1035500,262144);

CREATE or replace FUNCTION osmc.xy_to_ij(
  x  int,
  y  int,
  x0 int, -- referencia de inicio do eixo x [x0,y0]
  y0 int, -- referencia de inicio do eixo y [x0,y0]
  s  int  -- lado da célula
) RETURNS int[] AS $f$
  SELECT array[ (x-x0)/s, (y-y0)/s, x0, y0, s ]
  WHERE (x-x0) >= 0 AND (y-y0) >= 0
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.xy_to_ij(int,int,int,int,int)
 IS 'Retorna célula da matriz que contem x,y.'
;
--SELECT osmc.xy_to_ij(4442144,1297644,4180000,1035500,262144);

CREATE or replace  FUNCTION osmc.xy_to_ij(
  a int[]
) RETURNS int[] AS $wrap$
  SELECT osmc.xy_to_ij(a[1],a[2],a[3],a[4],a[5])
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.xy_to_ij(int[])
 IS 'Retorna célula da matriz que contem x,y.'
;
--SELECT osmc.xy_to_ij(array[4442144,1297644,4180000,1035500,262144]);

CREATE or replace  FUNCTION osmc.xy_to_quadrant(
  x  int,
  y  int,
  x0 int, -- referencia de inicio do eixo x [x0,y0]
  y0 int, -- referencia de inicio do eixo y [x0,y0]
  s  int, -- lado da célula
  cl int  -- número de colunas da matriz
) RETURNS int AS $f$
  SELECT cl*ij[2] + ij[1]
  FROM ( SELECT osmc.xy_to_ij(x,y,x0,y0,s) ) t(ij)
  WHERE ij[1] < cl
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.xy_to_quadrant(int,int,int,int,int,int)
 IS 'Retorna número do quandrante que contém x,y.'
;
--SELECT osmc.xy_to_quadrant(4442144,1297644,4180000,1035500,262144,6);

CREATE or replace  FUNCTION osmc.xy_to_quadrant(
  a int[]
) RETURNS int AS $wrap$
  SELECT osmc.xy_to_quadrant(a[1],a[2],a[3],a[4],a[5],a[6])
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.xy_to_quadrant(int[])
 IS 'Retorna número do quandrante que contém x,y.'
;
--SELECT osmc.xy_to_quadrant(array[4442144,1297644,4180000,1035500,262144,6]);

------------------
-- Uncertain level defaults:

CREATE or replace FUNCTION osmc.uncertain_base16h(u float) RETURNS int AS $f$
  -- GeoURI's uncertainty value "is the radius of the disk that represents uncertainty geometrically"
  SELECT CASE -- discretization by "snap to size-levels bits"
    WHEN s < 1.2 THEN 40
    WHEN s < 1.7 THEN 39
    WHEN s < 2.4 THEN 38
    WHEN s < 3.4 THEN 37
    WHEN s < 4.8 THEN 36
    WHEN s < 6.8 THEN 35
    WHEN s < 9.7 THEN 34
    WHEN s < 13.7 THEN 33
    WHEN s < 19.3 THEN 32
    WHEN s < 27 THEN 31
    WHEN s < 39 THEN 30
    WHEN s < 55 THEN 29
    WHEN s < 77 THEN 28
    WHEN s < 109 THEN 27
    WHEN s < 155 THEN 26
    WHEN s < 219 THEN 25
    WHEN s < 309 THEN 24
    WHEN s < 437 THEN 23
    WHEN s < 618 THEN 22
    WHEN s < 874 THEN 21
    WHEN s < 1236 THEN 20
    WHEN s < 1748 THEN 19
    WHEN s < 2472 THEN 18
    WHEN s < 3496 THEN 17
    WHEN s < 4944 THEN 16
    WHEN s < 6992 THEN 15
    WHEN s < 9889 THEN 14
    WHEN s < 13985 THEN 13
    WHEN s < 19777 THEN 12
    WHEN s < 27969 THEN 11
    WHEN s < 39554 THEN 10
    WHEN s < 55938 THEN 9
    WHEN s < 79109 THEN 8
    WHEN s < 111877 THEN 7
    WHEN s < 158218 THEN 6
    WHEN s < 223754 THEN 5
    WHEN s < 316436 THEN 4
    WHEN s < 447508 THEN 3
    WHEN s < 632872 THEN 2
    WHEN s < 895016 THEN 1
    ELSE 0
    END
  FROM (SELECT CASE WHEN u > 9 THEN (ROUND(u,0))*2 ELSE (ROUND(u,1))*2 END) t(s)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.uncertain_base16h(float)
  IS 'Uncertain base 16h.'
;

------------------
-- Others helper functions::

CREATE or replace FUNCTION osmc.ggeohash_GeomsFromVarbit(
  p_code      varbit,
  p_l0code    varbit,
  p_translate boolean DEFAULT false, -- true para converter em LatLong (WGS84 sem projeção)
  p_srid      int     DEFAULT 4326,  -- WGS84
  p_base      int     DEFAULT 16,
  p_grid_size int     DEFAULT 2,
  p_bbox      int[] DEFAULT array[0.,0.,0.,0.],
  p_lonlat    boolean DEFAULT false  -- false: latLon, true: lonLat
) RETURNS TABLE(ghs text, geom geometry) AS $f$
  SELECT
    CASE
    WHEN p_base = 32 THEN natcod.vbit_to_strstd(p_l0code || p_code || x,'32nvu')
    ELSE natcod.vbit_to_baseh(p_l0code || p_code || x,p_base,true)
    END,
    ggeohash.draw_cell_bybox(ggeohash.decode_box2(p_code || x,p_bbox,p_lonlat),p_translate,p_srid)
  FROM
  unnest(
  CASE
  WHEN p_base = 16 AND p_grid_size = 2  THEN '{0,1}'::bit[] --'{G,H}'
  WHEN p_base = 16 AND p_grid_size = 4  THEN '{00,01,11,10}'::varbit[] --'{J,K,L,M}'
  WHEN p_base = 16 AND p_grid_size = 8  THEN '{000,001,010,011,100,101,110,111}'::varbit[] --'{N,P,Q,R,S,T,V,Z}'
  WHEN p_base = 16 AND p_grid_size = 16 THEN '{0000,0001,0010,0011,0100,0101,0110,0111,1000,1001,1010,1011,1100,1101,1110,1111}'::varbit[]
  WHEN p_base = 32 AND p_grid_size = 32 THEN '{00000,00001,00010,00011,00100,00101,00110,00111,01000,01001,01010,01011,01100,01101,01110,01111,10000,10001,10010,10011,10100,10101,10110,10111,11000,11001,11010,11011,11100,11101,11110,11111}'::varbit[]
  END
  ) t(x)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.ggeohash_GeomsFromVarbit
  IS 'Return grid child-cell of OSMcode. The parameter is the ggeohash the parent-cell, that will be a prefix for all child-cells.'
;

------------------
-- Table coverage:

CREATE TABLE osmc.coverage (
  cbits          varbit,
  isolabel_ext   text,
  cindex         text,
  bbox           int[],
  status         smallint DEFAULT 0 CHECK (status IN (0,1,2)), -- 0: generated, 1: revised, 2: homologated
  is_country     boolean  DEFAULT FALSE,
  is_contained   boolean  DEFAULT FALSE,
  is_overlay     boolean  DEFAULT FALSE,
  kx_prefix      text,
  geom           geometry,
  geom_srid4326  geometry
);
CREATE INDEX osm_coverage_geom_idx1              ON osmc.coverage USING gist (geom);
CREATE INDEX osm_coverage_geom4326_idx1          ON osmc.coverage USING gist (geom_srid4326);
CREATE INDEX osm_coverage_isolabel_ext_idx1      ON osmc.coverage USING btree (isolabel_ext);
CREATE INDEX osm_coverage_cbits10true_idx        ON osmc.coverage ((cbits::bit(8))) WHERE is_country IS TRUE;
CREATE INDEX osm_coverage_isolabel_ext_true_idx  ON osmc.coverage (isolabel_ext) WHERE is_country IS TRUE;
CREATE INDEX osm_coverage_isolabel_ext_false_idx ON osmc.coverage (isolabel_ext) WHERE is_country IS FALSE;
CREATE INDEX osm_coverage_cbits15false_idx       ON osmc.coverage ((cbits::bit(12)),isolabel_ext) WHERE is_country IS FALSE;

COMMENT ON COLUMN osmc.coverage.cbits            IS 'Coverage cell identifier.';
COMMENT ON COLUMN osmc.coverage.isolabel_ext     IS 'ISO 3166-1 alpha-2 code and name (camel case); e.g. BR-SP-SaoPaulo.';
COMMENT ON COLUMN osmc.coverage.cindex           IS 'Coverage cell prefix in 32nvu.  Used only case is_country=false.';
COMMENT ON COLUMN osmc.coverage.bbox             IS 'Coverage cell bbox.';
COMMENT ON COLUMN osmc.coverage.status           IS 'Coverage status. Convention: 0: generated, 1: revised, 2: homologated.';
COMMENT ON COLUMN osmc.coverage.is_country       IS 'True if it is a cell of national coverage..';
COMMENT ON COLUMN osmc.coverage.is_contained     IS 'True if it is a cell contained in the jurisdiction..';
COMMENT ON COLUMN osmc.coverage.is_overlay       IS 'True if it is an overlay cell.';
COMMENT ON COLUMN osmc.coverage.kx_prefix        IS 'Coverage cell prefix in 32nvu.';
COMMENT ON COLUMN osmc.coverage.geom             IS 'Coverage cell geometry on default srid.';
COMMENT ON COLUMN osmc.coverage.geom_srid4326    IS 'Coverage cell geometry on 4326 srid. Used only case is_country=true.';

COMMENT ON TABLE osmc.coverage IS 'Jurisdictional coverage.';

CREATE or replace VIEW osmc.jurisdictions_select AS
  SELECT jsonb_object_agg(isolabel_ext,ll) AS gg
  FROM
  (
    SELECT split_part(z.isolabel_ext,'-',1) AS isolabel_ext, jsonb_object_agg(split_part(z.isolabel_ext,'-',2),jsonb_build_object('draft', draft, 'work', work, 'name', x.name)) AS ll
    FROM
    (
      SELECT CASE WHEN b.isolabel_ext IS NULL THEN c.isolabel_ext ELSE b.isolabel_ext END AS isolabel_ext, draft, work
      FROM
      (
        SELECT split_part(isolabel_ext,'-',1) || '-' || split_part(isolabel_ext,'-',2) AS isolabel_ext, jsonb_agg(split_part(isolabel_ext,'-',3)) AS work
        FROM
        (
          SELECT DISTINCT isolabel_ext, status
          FROM osmc.coverage
          WHERE is_country IS FALSE AND status <> 0
          ORDER BY 1
        ) a
        GROUP BY split_part(isolabel_ext,'-',1) || '-' || split_part(isolabel_ext,'-',2), status
        ORDER BY 1
      ) b
      FULL OUTER JOIN
      (
        SELECT split_part(isolabel_ext,'-',1) || '-' || split_part(isolabel_ext,'-',2) AS isolabel_ext, jsonb_agg(split_part(isolabel_ext,'-',3)) AS draft
        FROM
        (
          SELECT DISTINCT isolabel_ext, status
          FROM osmc.coverage
          WHERE is_country IS FALSE AND status = 0
          ORDER BY 1
        ) a
        GROUP BY split_part(isolabel_ext,'-',1) || '-' || split_part(isolabel_ext,'-',2), status
        ORDER BY 1
      ) c
      ON b.isolabel_ext = c.isolabel_ext
    ) z
    LEFT JOIN optim.jurisdiction x
    ON z.isolabel_ext = x.isolabel_ext
    GROUP BY split_part(z.isolabel_ext,'-',1)
  ) c
;
COMMENT ON VIEW osmc.jurisdictions_select
  IS 'Generates json for select from AFA.codes website.'
;

------------------
-- encode/decode 16h1c:

CREATE or replace FUNCTION osmc.encode_16h1c_br(
  p_code text
) RETURNS text AS $wrap$
  SELECT
    CASE
      -- tr g->F, q->F
      WHEN length(p_code) > 2
      THEN
      (
        ('{"00": "0", "01": "1", "02": "2", "03": "3", "04": "4", "05": "5", "06": "6", "07": "7",
           "08": "8", "09": "9", "0a": "a", "0b": "b", "0c": "c", "0d": "d", "0e": "e", "0f": "f",
           "10": "f", "11": "f", "12": "h", "13": "m", "14": "r", "15": "v", "16": "j", "17": "k",
           "18": "n", "19": "p", "1a": "s", "1b": "t", "1c": "z", "1d": "y"}'::jsonb)->>(substring(p_code,1,2))
      )
      WHEN length(p_code) = 2
      THEN
      (
        ('{"00": "0", "01": "1", "02": "2", "03": "3", "04": "4", "05": "5", "06": "6", "07": "7",
          "08": "8", "09": "9", "0a": "a", "0b": "b", "0c": "c", "0d": "d", "0e": "e", "0f": "f",
          "10": "g", "11": "q", "12": "h", "13": "m", "14": "r", "15": "v", "16": "j", "17": "k",
          "18": "n", "19": "p", "1a": "s", "1b": "t", "1c": "z", "1d": "y"}'::jsonb)->>(substring(p_code,1,2))
      )
    END || substring(p_code,3)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_16h1c_br(text)
  IS 'Encodes ghosts in BR.'
;

CREATE or replace FUNCTION osmc.encode_16h1c_uy(
  p_code text
) RETURNS text AS $wrap$
  SELECT
    CASE
      -- tr g->E, q->5, h->0
      WHEN length(p_code) > 2 AND substring(p_code,1,3) IN ('100','101','102','10h','10j','10k', '12a','12b','12t', '11v','11z','11y','11c','11d','11e','11f')
      THEN
      (
        ('{"10": "e", "11": "5", "12": "0"}'::jsonb)->>(substring(p_code,1,2))
      )
      WHEN length(p_code) > 2 AND substring(p_code,1,3) NOT IN (
      '0e0','0e1','0e2','0ej','0eh','0ek',
      '00a','00b','00t',
      '05v','05z','05y','05c','05d','05e','05f',

      '100','101','102','10h','10j','10k',
      '12a','12b','12t',
      '11v','11z','11y','11c','11d','11e','11f'
      )
      THEN
      (
        ('{"00": "0", "01": "1", "02": "2", "03": "3", "04": "4", "05": "5", "06": "6", "07": "7",
          "08": "8", "09": "9", "0a": "a", "0b": "b", "0c": "c", "0d": "d", "0e": "e", "0f": "f"}'::jsonb)->>(substring(p_code,1,2))
      )
      WHEN length(p_code) = 2
      THEN
      (
        ('{"00": "0", "01": "1", "02": "2", "03": "3", "04": "4", "05": "5", "06": "6", "07": "7",
          "08": "8", "09": "9", "0a": "a", "0b": "b", "0c": "c", "0d": "d", "0e": "e", "0f": "f",
          "10": "g", "11": "q", "12": "h", "13": "m", "14": "r", "15": "v", "16": "j", "17": "k",
          "18": "n", "19": "p", "1a": "s", "1b": "t", "1c": "z", "1d": "y"}'::jsonb)->>(substring(p_code,1,2))
      )
    END || substring(p_code,3)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_16h1c_uy(text)
  IS 'Encodes ghosts in UY.'
;

CREATE or replace FUNCTION osmc.encode_16h1c(
  p_code      text,
  p_jurisd_id int
) RETURNS text AS $wrap$
  SELECT
    CASE
      WHEN p_jurisd_id = 1 THEN osmc.encode_16h1c_br(p_code)
      WHEN p_jurisd_id = 4 THEN osmc.encode_16h1c_uy(p_code)
    END
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_16h1c(text,int)
  IS 'Encodes ghosts in BR and UY.'
;

CREATE or replace FUNCTION osmc.decode_16h1c_br(
  p_code text
) RETURNS text AS $wrap$
  SELECT
    CASE
      -- fr,ft,fs,fa,fb,f8,f9: tr f -> 0f
      WHEN substring(p_code,1,2) IN ('fr','ft','fs','fa','fb','f8','f9') THEN ('0f')
      -- fn,f4,f5: tr f -> q
      WHEN substring(p_code,1,2) IN ('fn','f4','f5')                     THEN ('11')
      -- fp,f6,f7: tr f -> g
      WHEN substring(p_code,1,2) IN ('fp','f6','f7')                     THEN ('10')
      ELSE
      (
        ('{"0": "00", "1": "01", "2": "02", "3": "03", "4": "04", "5": "05", "6": "06", "7": "07",
        "8": "08", "9": "09", "a": "0a", "b": "0b", "c": "0c", "d": "0d", "e": "0e", "f": "0f",
        "g": "10", "q": "11", "h": "12", "m": "13", "r": "14", "v": "15", "j": "16", "k": "17",
        "n": "18", "p": "19", "s": "1a", "t": "1b", "z": "1c", "y": "1d"}'::jsonb)->>(substring(p_code,1,1))
      )
    END || substring(p_code,2)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.decode_16h1c_br(text)
  IS 'Decode ghosts in BR.'
;

CREATE or replace FUNCTION osmc.decode_16h1c_uy(
  p_code text
) RETURNS text AS $wrap$
  SELECT
    CASE
      -- e0,e1,e2,eh,ej,ek: tr f -> g
      WHEN substring(p_code,1,2) IN ('e0','e1','e2','eh','ej','ek')      THEN ('10')
      -- 0a,0b,0t: tr 0 -> h
      WHEN substring(p_code,1,2) IN ('0a','0b','0t')                     THEN ('12')
      -- 5v,5z,5y,5c,5d,5e,5f: tr 5 -> q
      WHEN substring(p_code,1,2) IN ('5v','5z','5y','5c','5d','5e','5f') THEN ('11')
      ELSE
      (
        ('{"0": "00", "1": "01", "2": "02", "3": "03", "4": "04", "5": "05", "6": "06", "7": "07",
        "8": "08", "9": "09", "a": "0a", "b": "0b", "c": "0c", "d": "0d", "e": "0e", "f": "0f",
        "g": "10", "q": "11", "h": "12", "m": "13", "r": "14", "v": "15", "j": "16", "k": "17",
        "n": "18", "p": "19", "s": "1a", "t": "1b", "z": "1c", "y": "1d"}'::jsonb)->>(substring(p_code,1,1))
      )
    END || substring(p_code,2)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.decode_16h1c_uy(text)
  IS 'Decode ghosts in UY.'
;

CREATE or replace FUNCTION osmc.decode_16h1c(
  p_code      text,
  p_jurisd_id int
) RETURNS text AS $wrap$
  SELECT
    CASE
      WHEN p_jurisd_id = 1 THEN osmc.decode_16h1c_br(p_code)
      WHEN p_jurisd_id = 4 THEN osmc.decode_16h1c_uy(p_code)
    END
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.decode_16h1c(text,int)
  IS 'Encodes ghosts in BR and UY.'
;

CREATE or replace FUNCTION osmc.extract_cellbits(
  p_x  varbit
) RETURNS varbit AS $f$
  SELECT substring(p_x from 9);
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.extract_cellbits(varbit)
  IS 'Return cell bits. Discard jurisdiction bits.'
;

CREATE or replace FUNCTION osmc.extract_jurisdbits(
  p_x  varbit
) RETURNS int AS $f$
  SELECT (p_x::bit(8))::int;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.extract_jurisdbits(varbit)
  IS 'Return jurisdiction bits. Discard cell bits.'
;

CREATE or replace FUNCTION osmc.extract_L0bits4(
  p_x varbit
) RETURNS varbit AS $f$
  SELECT (osmc.extract_cellbits(p_x))::bit(4);
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.extract_L0bits4(varbit)
  IS 'Returns 4 bits of L0. Discard 8 bits from the country.'
;

CREATE or replace FUNCTION osmc.extract_L0bits8(
  p_x varbit
) RETURNS varbit AS $f$
  SELECT (osmc.extract_cellbits(p_x))::bit(8);
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.extract_L0bits8(varbit)
  IS 'Returns 8 bits of L0. Discard 8 bits from the country.'
;

CREATE or replace FUNCTION osmc.extract_L0bits(
  p_x varbit
) RETURNS varbit AS $wrap$
  SELECT
    CASE
    WHEN y IN (1,4,5) THEN osmc.extract_L0bits8(p_x)
    WHEN y IN (2,3)   THEN osmc.extract_L0bits4(p_x)
    END
  FROM osmc.extract_jurisdbits(p_x) y
    ;
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.extract_L0bits(varbit)
  IS 'Returns the bits of the L0 cell.'
;

CREATE or replace FUNCTION osmc.vbit_withoutL0(
  p_x  varbit,
  p_iso int
) RETURNS varbit AS $wrap$
  SELECT
    CASE
    WHEN p_iso IN (1,4,5) THEN substring(p_x from 9) -- Remove 8 bits MSb
    WHEN p_iso IN (2,3)   THEN substring(p_x from 5) -- Remove 4 bits MSb
    END
    ;
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.vbit_withoutL0(varbit,int)
  IS 'Return cell bits. Discard L0 bits.'
;

CREATE or replace FUNCTION osmc.cbits_b32nvu_to_16h(
  p_x  varbit,
  p_iso int
) RETURNS varbit AS $wrap$
  SELECT
    CASE
    WHEN p_iso IN (1,4,5) THEN b'000' || p_x         -- 5bits MSb viram 8
    WHEN p_iso IN (2)     THEN substring(p_x,2,4) || substring(p_x from 8) -- 5bits MSb viram 4. eg.: abcdefghijk -> bcdehijk
    WHEN p_iso IN (3)     THEN substring(p_x,2,4) || substring(p_x from 10)
    END
    ;
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.cbits_b32nvu_to_16h(varbit,int)
  IS 'Convert 5-bit L0 to 4-bit L0.'
;

CREATE or replace FUNCTION osmc.cbits_16h_to_b32nvu(
  p_x  varbit,
  p_iso int
) RETURNS varbit AS $wrap$
  SELECT
    CASE
    WHEN p_iso IN (1,4,5) THEN substring(p_x from 4) -- 8bits MSb viram 5
    WHEN p_iso IN (2)     THEN b'0' || substring(p_x,1,4) || b'00' || substring(p_x from 5) -- 4bits MSb viram 5. eg.: xxxxxxxx -> 0xxxx00xxxx
    WHEN p_iso IN (3)     THEN b'0' || substring(p_x,1,4) || b'0000' || substring(p_x from 5)
    END
    ;
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.cbits_16h_to_b32nvu(varbit,int)
  IS 'Convert 4-bit L0 to 5-bit L0.'
;

-- specific encode for b32nvu and 8 digits:

CREATE or replace FUNCTION osmc.encode_point_brazil(
  p_geom       geometry(POINT),
  p_bit_length int DEFAULT 35
) RETURNS text AS $wrap$
  SELECT
    (
      SELECT (natcod.vbit_to_strstd(osmc.cbits_16h_to_b32nvu(osmc.extract_L0bits(cbits),1) || ggeohash.encode3(ST_X(x),ST_Y(x),bbox,(p_bit_length/5)*5,false),'32nvu'))

      FROM osmc.coverage
      WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 1 AND ST_X(x) BETWEEN bbox[1] AND bbox[3] AND ST_Y(x) BETWEEN bbox[2] AND bbox[4]
    )
  FROM (SELECT ST_Transform(p_geom,952019)) t(x)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_point_brazil(geometry(POINT),int)
  IS 'Encode Point for Brazil: base 32nvu, default 8 digits'
;

CREATE or replace FUNCTION osmc.encode_point_colombia(
  p_geom       geometry(POINT),
  p_bit_length int DEFAULT 35
) RETURNS text AS $wrap$
  SELECT
    (
      SELECT (natcod.vbit_to_strstd(osmc.cbits_16h_to_b32nvu(osmc.extract_L0bits4(cbits) || ggeohash.encode3(ST_X(x),ST_Y(x),bbox,((p_bit_length-2)/5)*5 +3,false),2),'32nvu'))
      FROM osmc.coverage
      WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 2 AND ST_X(x) BETWEEN bbox[1] AND bbox[3] AND ST_Y(x) BETWEEN bbox[2] AND bbox[4]
    )
  FROM (SELECT ST_Transform(p_geom,9377)) t(x)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_point_colombia(geometry(POINT),int)
  IS 'Encode Point for Colombia: base 32nvu, default 8 digits'
;

CREATE or replace FUNCTION osmc.encode_point_cm(
  p_geom       geometry(POINT),
  p_bit_length int DEFAULT 35
) RETURNS text AS $wrap$
  SELECT
    (
      SELECT (natcod.vbit_to_strstd(osmc.cbits_16h_to_b32nvu(osmc.extract_L0bits4(cbits) || ggeohash.encode3(ST_X(x),ST_Y(x),bbox,((p_bit_length-4)/5)*5 +1,false),3),'32nvu'))
      FROM osmc.coverage
      WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 3 AND ST_X(x) BETWEEN bbox[1] AND bbox[3] AND ST_Y(x) BETWEEN bbox[2] AND bbox[4]
    )
  FROM (SELECT ST_Transform(p_geom,102022)) t(x)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_point_colombia(geometry(POINT),int)
  IS 'Encode Point for Colombia: base 32nvu, default 8 digits'
;

CREATE or replace FUNCTION osmc.string_base(
  p_base int
) RETURNS text AS $f$
  SELECT
      CASE
           WHEN p_base = 16 THEN '16h'
           WHEN p_base = 17 THEN '16'
           WHEN p_base = 18 THEN '16h1c'
           WHEN p_base = 32 THEN '32nvu'
      END
      ;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.string_base(int)
  IS 'Returns string name of base.'
;

-- encode scientific:

CREATE or replace FUNCTION osmc.encode(
  p_x          float,
  p_y          float,
  p_bit_length int     DEFAULT 40,
  p_srid       int     DEFAULT 9377,
  p_bbox       int[]   DEFAULT array[0,0,0,0],
  p_l0code     varbit  DEFAULT b'0',
  p_lonlat     boolean DEFAULT FALSE  -- false: latLon, true: lonLat
) RETURNS TABLE (bit_string varbit, geom geometry) AS $f$
  SELECT bit_string, ggeohash.draw_cell_bybox((CASE WHEN p_bit_length = 0 THEN p_bbox ELSE ggeohash.decode_box2(bit_string,p_bbox,p_lonlat) END),false,p_srid) AS geom
  FROM ggeohash.encode3(p_x,p_y,p_bbox,p_bit_length,p_lonlat) r(bit_string)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode(float,float,int,int,int[],varbit,boolean)
  IS ''
;

CREATE or replace FUNCTION osmc.osmcode_encode_scientific(
  p_x          float,
  p_y          float,
  p_base       int     DEFAULT 16,
  p_bit_length int     DEFAULT 40,
  p_srid       int     DEFAULT 9377,
  p_grid_size  int     DEFAULT 2,
  p_bbox       int[]   DEFAULT array[0,0,0,0],
  p_l0code     varbit  DEFAULT b'0',
  p_jurisd_id  int     DEFAULT 170,
  p_lonlat     boolean DEFAULT false  -- false: latLon, true: lonLat
) RETURNS jsonb AS $f$
    SELECT jsonb_build_object(
      'type', 'FeatureCollection',
      'features',
        (
          (ST_AsGeoJSONb(ST_Transform_resilient(c.geom,4326,0.005),8,0,null,
              jsonb_strip_nulls(jsonb_build_object(
                  'code', CASE WHEN p_base IN (18) THEN osmc.encode_16h1c(c.code,p_jurisd_id) ELSE c.code END,
                  'area', ST_Area(c.geom),
                  'side', SQRT(ST_Area(c.geom)),
                  'base', osmc.string_base(p_base),
                  'jurisd_base_id',p_jurisd_id -- ***
                  ))
          )::jsonb) || m.subcells
        )
      )
    FROM
    (
      SELECT bit_string, natcod.vbit_to_baseh((CASE WHEN p_bit_length = 0 THEN p_l0code ELSE p_l0code||bit_string END),16,true) AS code, geom
      FROM  osmc.encode(p_x,p_y,p_bit_length,p_srid,p_bbox,p_l0code,p_lonlat)
    ) c
    -- responsável por subcélulas
    LEFT JOIN LATERAL
    (
      SELECT
        CASE
        WHEN p_grid_size > 0
        THEN
          (
            SELECT jsonb_agg(
                ST_AsGeoJSONb(ST_Transform_resilient((CASE WHEN p_grid_size % 2 = 1 THEN ST_Centroid(geom) ELSE geom END),4326,0.005),8,0,null,
                    jsonb_strip_nulls(jsonb_build_object(
                        'code', ghs2,
                        'code_subcell', (CASE WHEN length(code_sci) = length(ghs2) THEN substring(ghs2 FROM length(code_sci)) ELSE substring(ghs2 FROM length(code_sci)+1) END) ,
                        'prefix', code_sci,
                        'area', geom_area,
                        'side', SQRT(geom_area),
                        'base', osmc.string_base(p_base)
                        ))
                    )::jsonb)
              FROM
             (
              SELECT geom, ghs,
                  CASE WHEN p_base = 18 THEN osmc.encode_16h1c(ghs,p_jurisd_id) ELSE ghs  END AS ghs2,
                  CASE WHEN p_base = 18 THEN osmc.encode_16h1c(c.code,p_jurisd_id) ELSE c.code END AS code_sci,
                  ST_Area(geom) AS geom_area
                FROM osmc.ggeohash_GeomsFromVarbit(
                      c.bit_string,p_l0code,false,p_srid,16,
                      CASE
                        WHEN p_grid_size % 2 = 1 THEN p_grid_size - 1
                        ELSE p_grid_size
                      END,
                      p_bbox,
                      p_lonlat
                      )
             ) xx
          )
        ELSE '[]'::jsonb
        END AS subcells
    ) m
    ON TRUE

    WHERE CASE WHEN p_jurisd_id = 4 THEN c.code NOT IN ('0eg','10g','12g','00r','12r','0eh','05q','11q') ELSE TRUE  END

$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.osmcode_encode_scientific(float,float,int,int,int,int,int[],varbit,int,boolean)
  IS 'Encodes geometry to OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_scientific_br(
  p_geom        geometry(POINT),
  p_uncertainty float  DEFAULT -1,
  p_grid_size   int    DEFAULT 0
) RETURNS jsonb AS $f$
    SELECT osmc.osmcode_encode_scientific(x,y,18,
      CASE
      WHEN p_uncertainty > -1 THEN u
      ELSE 40
      END,
      952019,
      CASE
        WHEN u = 40 THEN 0
        WHEN p_grid_size > 0 AND u = 39 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 3 ELSE 2 END))
        WHEN p_grid_size > 0 AND u = 38 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 5 ELSE 4 END))
        WHEN p_grid_size > 0 AND u = 37 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 9 ELSE 8 END))
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits8(cbits),76,FALSE)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty), ST_X(p_geom), ST_Y(p_geom)) t(u,x,y)
    WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 1 AND x BETWEEN bbox[1] AND bbox[3] AND y BETWEEN bbox[2] AND bbox[4]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_scientific_br(geometry(POINT),float,int)
  IS 'Encodes geometry to BR Scientific OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_scientific_cm(
  p_geom        geometry(POINT),
  p_uncertainty float  DEFAULT -1,
  p_grid_size   int    DEFAULT 0
) RETURNS jsonb AS $f$
    SELECT osmc.osmcode_encode_scientific(x,y,16,
      CASE
      WHEN p_uncertainty > -1 AND u >  4 THEN u-4
      WHEN p_uncertainty > -1 AND u <= 4 THEN 0
      ELSE 40
      END,
      102022,
      CASE
        WHEN u > 37 THEN 0
        WHEN p_grid_size > 0 AND u = 37 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 3 ELSE 2 END))
        WHEN p_grid_size > 0 AND u = 36 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 5 ELSE 4 END))
        WHEN p_grid_size > 0 AND u = 35 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 9 ELSE 8 END))
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits4(cbits),120,FALSE)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty), ST_X(p_geom), ST_Y(p_geom)) t(u,x,y)
    WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 3 AND x BETWEEN bbox[1] AND bbox[3] AND y BETWEEN bbox[2] AND bbox[4]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_scientific_cm(geometry(POINT),float,int)
  IS 'Encodes geometry to CM Scientific OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_scientific_co(
  p_geom        geometry(POINT),
  p_uncertainty float  DEFAULT -1,
  p_grid_size   int    DEFAULT 0
) RETURNS jsonb AS $f$
    SELECT osmc.osmcode_encode_scientific(x,y,16,
      CASE
      WHEN p_uncertainty > -1 AND u >  2 THEN u-2
      WHEN p_uncertainty > -1 AND u <= 2 THEN 0
      ELSE 40
      END,
      9377,
      CASE
        WHEN u > 39 THEN 0
        WHEN p_grid_size > 0 AND u = 39 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 3 ELSE 2 END))
        WHEN p_grid_size > 0 AND u = 38 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 5 ELSE 4 END))
        WHEN p_grid_size > 0 AND u = 37 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 9 ELSE 8 END))
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits4(cbits),170,FALSE)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty), ST_X(p_geom), ST_Y(p_geom)) t(u,x,y)
    WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 2 AND x BETWEEN bbox[1] AND bbox[3] AND y BETWEEN bbox[2] AND bbox[4]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_scientific_co(geometry(POINT),float,int)
  IS 'Encodes geometry to CO Scientific OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_scientific_uy(
  p_geom        geometry(POINT),
  p_uncertainty float  DEFAULT -1,
  p_grid_size   int    DEFAULT 0
) RETURNS jsonb AS $f$
    SELECT osmc.osmcode_encode_scientific(x,y,18,
      CASE
      WHEN p_uncertainty > -1 AND u >  6 THEN u-6
      WHEN p_uncertainty > -1 AND u <= 6 THEN 0
      ELSE 40
      END,
      32721,
      CASE
        WHEN u = 40 THEN 0
        WHEN p_grid_size > 0 AND u = 39 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 3 ELSE 2 END))
        WHEN p_grid_size > 0 AND u = 38 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 5 ELSE 4 END))
        WHEN p_grid_size > 0 AND u = 37 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 9 ELSE 8 END))
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits8(cbits),858,FALSE)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty), ST_X(p_geom), ST_Y(p_geom)) t(u,x,y)
    WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 4 AND x BETWEEN bbox[1] AND bbox[3] AND y BETWEEN bbox[2] AND bbox[4]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_scientific_uy(geometry(POINT),float,int)
  IS 'Encodes geometry to UY Scientific OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_scientific_ec(
  p_geom        geometry(POINT),
  p_uncertainty float  DEFAULT -1,
  p_grid_size   int    DEFAULT 0
) RETURNS jsonb AS $f$
    SELECT osmc.osmcode_encode_scientific(x,y,16,
      CASE
      WHEN p_uncertainty > -1 AND u >  5 THEN u-5
      WHEN p_uncertainty > -1 AND u <= 5 THEN 0
      ELSE 40
      END,
      32717,
      CASE
        WHEN u = 40 THEN 0
        WHEN p_grid_size > 0 AND u = 39 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 3 ELSE 2 END))
        WHEN p_grid_size > 0 AND u = 38 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 5 ELSE 4 END))
        WHEN p_grid_size > 0 AND u = 37 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 9 ELSE 8 END))
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits8(cbits),218,FALSE)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty), ST_X(p_geom), ST_Y(p_geom)) t(u,x,y)
    WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 5 AND x BETWEEN bbox[1] AND bbox[3] AND y BETWEEN bbox[2] AND bbox[4]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_scientific_ec(geometry(POINT),float,int)
  IS 'Encodes geometry to EC Scientific OSMcode.'
;

-- encode logistics:

CREATE or replace FUNCTION osmc.encode_short_code(
  p_code           text,
  p_codebits       varbit,
  p_isolabel_ext   text,
  p_geom           geometry(POINT)
) RETURNS TABLE(isolabel_ext text, jurisd_local_id int, short_code text) AS $f$
    SELECT t.isolabel_ext, s.jurisd_local_id, t.short_code
    FROM
    (
        SELECT isolabel_ext, ( cindex || (CASE WHEN length(p_codebits) = length(cbits) THEN '' ELSE substring(p_code FROM length(kx_prefix)+1) END) ) AS short_code
        FROM osmc.coverage r
        WHERE is_country IS FALSE AND (cbits)::bit(12) = p_codebits::bit(12)
        AND CASE WHEN p_isolabel_ext IS NULL THEN TRUE ELSE isolabel_ext = p_isolabel_ext END
        AND CASE WHEN is_contained IS FALSE THEN ST_Contains(geom,p_geom) ELSE TRUE END
        AND cbits # substring(p_codebits FROM 1 FOR length(cbits)) = substring(0::bit(40) FROM 1 FOR length(cbits))
        ORDER BY length(kx_prefix) DESC
        LIMIT 1
    ) t
    LEFT JOIN optim.jurisdiction s
    ON s.isolabel_ext = t.isolabel_ext
    ;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_short_code(text,varbit,text,geometry)
  IS ''
;

CREATE or replace FUNCTION osmc.encode_postal(
  p_geom       geometry(POINT),
  p_x          float,
  p_y          float,
  p_bit_length int     DEFAULT 40,
  p_srid       int     DEFAULT 9377,
  p_grid_size  int     DEFAULT 32,
  p_bbox       int[]   DEFAULT array[0,0,0,0],
  p_l0code     varbit  DEFAULT b'0',
  p_jurisd_id  int     DEFAULT 170,
  p_lonlat     boolean DEFAULT false, -- false: latLon, true: lonLat
  p_type       int     DEFAULT 1, -- 1: isolabel_ext~short_code, 2: ISO-jurisd_local_id~short_code
  p_isolabel_ext text  DEFAULT NULL
) RETURNS jsonb AS $f$
    SELECT jsonb_build_object(
      'type', 'FeatureCollection',
      'features',
          jsonb_agg(ST_AsGeoJSONb(ST_Transform_resilient(c.geom,4326,0.005),8,0,null,
              jsonb_strip_nulls(jsonb_build_object(
                  'code', upper(natcod.vbit_to_strstd( osmc.cbits_16h_to_b32nvu(c.codebits,p_jurisd_id),'32nvu')),
                  'short_code', CASE p_type WHEN 2 THEN split_part(isolabel_ext,'-',1) || '-' || jurisd_local_id ELSE isolabel_ext END || '~' || short_code,
                  'area', ST_Area(c.geom),
                  'side', SQRT(ST_Area(c.geom)),
                  'base', '32nvu',
                  'jurisd_local_id', jurisd_local_id,
                  'jurisd_base_id', p_jurisd_id,
                  'isolabel_ext', p_isolabel_ext,
                  'isolabel_ext_abbrev', (SELECT abbrev FROM mvwjurisdiction_synonym_default_abbrev x WHERE x.isolabel_ext = p_isolabel_ext),
                  'scientic_code', CASE WHEN split_part(p_isolabel_ext,'-',1) IN ('BR','UY') THEN osmc.encode_16h1c(c.code,p_jurisd_id) ELSE c.code END
                  ))
          )::jsonb)
      )
    FROM
    (
      SELECT bit_string, natcod.vbit_to_baseh((CASE WHEN p_bit_length = 0 THEN p_l0code ELSE p_l0code||bit_string END),16,true) AS code,
      (CASE WHEN p_bit_length = 0 THEN p_l0code ELSE p_l0code||bit_string END) AS codebits, geom
      FROM  osmc.encode(p_x,p_y,p_bit_length,p_srid,p_bbox,p_l0code,p_lonlat)
    ) c
    -- responsável pelo código logístico
    LEFT JOIN LATERAL ( SELECT * FROM osmc.encode_short_code(upper(natcod.vbit_to_strstd( osmc.cbits_16h_to_b32nvu(c.codebits,p_jurisd_id),'32nvu')),p_jurisd_id::bit(8)||codebits,p_isolabel_ext,p_geom) ) t ON TRUE

    WHERE
    CASE WHEN p_jurisd_id = 4 THEN c.code NOT IN ('0eg','10g','12g','00r','12r','0eh','05q','11q') ELSE TRUE  END
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal(geometry(POINT),float,float,int,int,int,int[],varbit,int,boolean,int,text)
  IS 'Encodes geometry to Logistic AFAcode.'
;

CREATE or replace FUNCTION osmc.encode_postal_br(
  p_geom         geometry(POINT),
  p_uncertainty  float  DEFAULT -1,
  p_grid_size    int    DEFAULT 0,
  p_isolabel_ext text   DEFAULT NULL
) RETURNS jsonb AS $f$
    SELECT osmc.encode_postal(p_geom,x,y,
      CASE
      WHEN p_uncertainty > -1 THEN (u/5)*5
      ELSE 35
      END,
      952019,
      CASE
        WHEN u = 40 THEN 0
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits8(cbits),76,FALSE,1,p_isolabel_ext)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty), ST_X(p_geom), ST_Y(p_geom)) t(u,x,y)
    WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 1 AND x BETWEEN bbox[1] AND bbox[3] AND y BETWEEN bbox[2] AND bbox[4]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal_br(geometry(POINT),float,int,text)
  IS 'Encodes geometry to BR Logistic AFAcode.'
;

CREATE or replace FUNCTION osmc.encode_postal_cm(
  p_geom         geometry(POINT),
  p_uncertainty  float  DEFAULT -1,
  p_grid_size    int    DEFAULT 0,
  p_isolabel_ext text   DEFAULT NULL
) RETURNS jsonb AS $f$
    SELECT osmc.encode_postal(p_geom,x,y,
      CASE
      WHEN p_uncertainty > -1 THEN ((u-4)/5)*5 +1
      ELSE 31 -- 30 shift 1, 5.7m, L16.5
      END,
      102022,
      CASE
        WHEN u = 40 THEN 0
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits4(cbits),120,FALSE,1,p_isolabel_ext)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty), ST_X(p_geom), ST_Y(p_geom)) t(u,x,y)
    WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 3 AND x BETWEEN bbox[1] AND bbox[3] AND y BETWEEN bbox[2] AND bbox[4]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal_cm(geometry(POINT),float,int,text)
  IS 'Encodes geometry to CM Logistic AFAcode.'
;

CREATE or replace FUNCTION osmc.encode_postal_co(
  p_geom         geometry(POINT),
  p_uncertainty  float  DEFAULT -1,
  p_grid_size    int    DEFAULT 0,
  p_isolabel_ext text   DEFAULT NULL
) RETURNS jsonb AS $f$
    SELECT osmc.encode_postal(p_geom,x,y,
      CASE
      WHEN p_uncertainty > -1 THEN ((u-2)/5)*5 +3
      ELSE 33 -- 30 shift 3, 5.7m, L16.5
      END,
      9377,
      CASE
        WHEN u = 40 THEN 0
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits4(cbits),170,FALSE,2,p_isolabel_ext)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty), ST_X(p_geom), ST_Y(p_geom)) t(u,x,y)
    WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 2 AND x BETWEEN bbox[1] AND bbox[3] AND y BETWEEN bbox[2] AND bbox[4]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal_co(geometry(POINT),float,int,text)
  IS 'Encodes geometry to CO Logistic AFAcode.'
;

CREATE or replace FUNCTION osmc.encode_postal_uy(
  p_geom         geometry(POINT),
  p_uncertainty  float  DEFAULT -1,
  p_grid_size    int    DEFAULT 0,
  p_isolabel_ext text   DEFAULT NULL
) RETURNS jsonb AS $f$
    SELECT osmc.encode_postal(p_geom,x,y,
      CASE
      WHEN p_uncertainty > -1 AND u > 6  THEN ((u-6)/5)*5
      WHEN p_uncertainty > -1 AND u <= 6 THEN 0
      ELSE 35
      END,
      32721,
      CASE
        WHEN u > 31 THEN 0
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits8(cbits),858,FALSE,1,p_isolabel_ext)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty), ST_X(p_geom), ST_Y(p_geom)) t(u,x,y)
    WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 4 AND x BETWEEN bbox[1] AND bbox[3] AND y BETWEEN bbox[2] AND bbox[4]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal_uy(geometry(POINT),float,int,text)
  IS 'Encodes geometry to UY Logistic AFAcode.'
;

CREATE or replace FUNCTION osmc.encode_postal_ec(
  p_geom         geometry(POINT),
  p_uncertainty  float  DEFAULT -1,
  p_grid_size    int    DEFAULT 0,
  p_isolabel_ext text   DEFAULT NULL
) RETURNS jsonb AS $f$
    SELECT osmc.encode_postal(p_geom,x,y,
      CASE
      WHEN p_uncertainty > -1 AND u > 5  THEN ((u-5)/5)*5
      WHEN p_uncertainty > -1 AND u <= 5 THEN 0
      ELSE 35
      END,
      32717,
      CASE
        WHEN u > 35 THEN 0
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits8(cbits),218,TRUE,1,p_isolabel_ext)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty), ST_X(p_geom), ST_Y(p_geom)) t(u,x,y)
    WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = 5 AND x BETWEEN bbox[1] AND bbox[3] AND y BETWEEN bbox[2] AND bbox[4]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal_ec(geometry(POINT),float,int,text)
  IS 'Encodes geometry to EC Logistic AFAcode.'
;

------------------
-- Neighbors:

CREATE or replace FUNCTION osmc.neighbors_raw(
  p_x varbit -- without L0 bits
) RETURNS varbit[] AS $f$
    SELECT ARRAY[ (s | xEb), (s | u)   , (u | xEa) , (r | u)      , (r | xEb) , (r | t)     , (t | xEa) , (s | t)    ] AS neighbors
        --      [ North    , North East, East      , South East   , South     , South West  , West      , North West ]
    FROM
    (
        SELECT
            xEb,xEa,
            substring((xEa::bigint - 1)::bit(64) FROM 65 - length_bits) & a AS r,
            substring((xOb::bigint + 1)::bit(64) FROM 65 - length_bits) & a AS s,
            substring((xEb::bigint - 1)::bit(64) FROM 65 - length_bits) & b AS t,
            substring((xOa::bigint + 1)::bit(64) FROM 65 - length_bits) & b AS u
        FROM
        (
            SELECT
                a, b,
                length(p_x) AS length_bits,
                p_x & a AS xEa,
                p_x & b AS xEb,
                p_x | a AS xOa,
                p_x | b AS xOb
            FROM
            (
                SELECT
                    substring(b'1010101010101010101010101010101010101010101010101010101010101010' FROM 1 FOR length(p_x)) AS a,
                    substring(b'0101010101010101010101010101010101010101010101010101010101010101' FROM 1 FOR length(p_x)) AS b
            ) r
        ) s
    ) t
    ;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.neighbors_raw(varbit)
  IS 'Returns the neighbors of a cell in varbit array (without L0 bits), in order: North, North East, East, South East, South, South West, West, North West.'
;
-- 6aa82d8f1ec: 000001101010101010000010110110001111000111101100
-- EXPLAIN ANALYZE SELECT osmc.neighbors_raw(b'000001101010101010000010110110001111000111101100');

CREATE or replace FUNCTION osmc.cell_relate(
  p_x varbit -- without L0 bits
) RETURNS varbit AS $f$
        SELECT ( p_x & a = z)::int::bit || ( p_x & b = z)::int::bit || (np_x & a = z)::int::bit || (np_x & b = z)::int::bit AS mask
                 -- * * * *
                 -- | | | - East:  set if 1      in all  odd positions of p_x
                 -- | | --- North: set if 1      in all even positions of p_x
                 -- | ----- West:  set if 1 only in      odd positions of p_x
                 -- ------- South: set if 1 only in     even positions of p_x
        FROM
        (
            SELECT ~p_x AS np_x,
                substring(b'0000000000000000000000000000000000000000000000000000000000000000' FROM 1 FOR length(p_x)) AS z,
                substring(b'1010101010101010101010101010101010101010101010101010101010101010' FROM 1 FOR length(p_x)) AS a,
                substring(b'0101010101010101010101010101010101010101010101010101010101010101' FROM 1 FOR length(p_x)) AS b
        ) s
    ;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.cell_relate(varbit)
  IS 'Returns bit(4) with the relative position of the cell in L0.'
;
-- EXPLAIN ANALYZE SELECT osmc.cell_relate(osmc.vbit_withoutL0(natcod.baseh_to_vbit(osmc.decode_16h1c('6aaar',1)),1));

CREATE or replace FUNCTION osmc.neighborsl0(
  p_x    varbit, -- only L0 bits
  p_iso  int DEFAULT NULL
) RETURNS varbit[] AS $f$
    SELECT
        CASE                                            -- [North, North East, East, South East, South, South West, West, North West]
            WHEN p_iso = 1 AND p_x = b'00000000' THEN ARRAY[NULL,NULL,b'00000001',b'00000101',b'00000100',NULL,NULL,NULL]
            WHEN p_iso = 1 AND p_x = b'00000001' THEN ARRAY[NULL,NULL,b'00000010',b'00000110',b'00000101',b'00000100',b'00000000',NULL]
            WHEN p_iso = 1 AND p_x = b'00000010' THEN ARRAY[NULL,NULL,b'00000011',b'00000111',b'00000110',b'00000101',b'00000001',NULL]
            WHEN p_iso = 1 AND p_x = b'00000011' THEN ARRAY[NULL,NULL,b'00010000',b'00001000',b'00000111',b'00000110',b'00000010',NULL]
            WHEN p_iso = 1 AND p_x = b'00000100' THEN ARRAY[b'00000000',b'00000001',b'00000101',b'00001001',NULL,NULL,NULL,NULL]
            WHEN p_iso = 1 AND p_x = b'00000101' THEN ARRAY[b'00000001',b'00000010',b'00000110',b'00001010',b'00001001',NULL,b'00000100',b'00000000']
            WHEN p_iso = 1 AND p_x = b'00000110' THEN ARRAY[b'00000010',b'00000011',b'00000111',b'00001011',b'00001010',b'00001001',b'00000101',b'00000001']
            WHEN p_iso = 1 AND p_x = b'00000111' THEN ARRAY[b'00000011',b'00010000',b'00001000',b'00010001',b'00001011',b'00001010',b'00000110',b'00000010']
            WHEN p_iso = 1 AND p_x = b'00001000' THEN ARRAY[b'00010000',NULL,NULL,NULL,b'00010001',b'00001011',b'00000111',b'00000011']
            WHEN p_iso = 1 AND p_x = b'00001001' THEN ARRAY[b'00000101',b'00000110',b'00001010',b'00001101',b'00001100',NULL,NULL,b'00000100']
            WHEN p_iso = 1 AND p_x = b'00001010' THEN ARRAY[b'00000110',b'00000111',b'00001011',b'00001110',b'00001101',b'00001100',b'00001001',b'00000101']
            WHEN p_iso = 1 AND p_x = b'00001011' THEN ARRAY[b'00000111',b'00001000',b'00010001',NULL,b'00001110',b'00001101',b'00001010',b'00000110']
            WHEN p_iso = 1 AND p_x = b'00001100' THEN ARRAY[b'00001001',b'00001010',b'00001101',b'00001111',NULL,NULL,NULL,NULL]
            WHEN p_iso = 1 AND p_x = b'00001101' THEN ARRAY[b'00001010',b'00001011',b'00001110',NULL,b'00001111',NULL,b'00001100',b'00001001']
            WHEN p_iso = 1 AND p_x = b'00001110' THEN ARRAY[b'00001011',b'00010001',NULL,NULL,NULL,b'00001111',b'00001101',b'00001010']
            WHEN p_iso = 1 AND p_x = b'00001111' THEN ARRAY[b'00001101',b'00001110',NULL,NULL,NULL,NULL,NULL,b'00001100']
            WHEN p_iso = 1 AND p_x = b'00010000' THEN ARRAY[NULL,NULL,NULL,NULL,b'00001000',b'00000111',b'00000011',NULL]
            WHEN p_iso = 1 AND p_x = b'00010001' THEN ARRAY[b'00001000',NULL,NULL,NULL,NULL,b'00001110',b'00001011',b'00000111']
            WHEN p_iso = 2 AND p_x = b'0000'     THEN ARRAY[NULL,NULL,b'0010',b'0111',b'0101',NULL,NULL,NULL]
            WHEN p_iso = 2 AND p_x = b'0001'     THEN ARRAY[b'0100',b'0110',b'0011',NULL,NULL,NULL,NULL,NULL]
            WHEN p_iso = 2 AND p_x = b'0010'     THEN ARRAY[NULL,NULL,b'1111',b'1101',b'0111',b'0101',b'0000',NULL]
            WHEN p_iso = 2 AND p_x = b'0011'     THEN ARRAY[b'0110',b'1100',b'1001',b'1000',NULL,NULL,b'0001',b'0100']
            WHEN p_iso = 2 AND p_x = b'0100'     THEN ARRAY[b'0101',b'0111',b'0110',b'0011',b'0001',NULL,NULL,NULL]
            WHEN p_iso = 2 AND p_x = b'0101'     THEN ARRAY[b'0000',b'0010',b'0111',b'0110',b'0100',NULL,NULL,NULL]
            WHEN p_iso = 2 AND p_x = b'0110'     THEN ARRAY[b'0111',b'1101',b'1100',b'1001',b'0011',b'0001',b'0100',b'0101']
            WHEN p_iso = 2 AND p_x = b'0111'     THEN ARRAY[b'0010',b'1111',b'1101',b'1100',b'0110',b'0100',b'0101',b'0000']
            WHEN p_iso = 2 AND p_x = b'1000'     THEN ARRAY[b'1001',b'1011',b'1010',NULL,NULL,NULL,NULL,b'0011']
            WHEN p_iso = 2 AND p_x = b'1001'     THEN ARRAY[b'1100',b'1110',b'1011',b'1010',b'1000',NULL,b'0011',b'0110']
            WHEN p_iso = 2 AND p_x = b'1010'     THEN ARRAY[b'1011',NULL,NULL,NULL,NULL,NULL,b'1000',b'1001']
            WHEN p_iso = 2 AND p_x = b'1011'     THEN ARRAY[b'1110',NULL,NULL,NULL,b'1010',b'1000',b'1001',b'1100']
            WHEN p_iso = 2 AND p_x = b'1100'     THEN ARRAY[b'1101',NULL,b'1110',b'1011',b'1001',b'0011',b'0110',b'0111']
            WHEN p_iso = 2 AND p_x = b'1101'     THEN ARRAY[b'1111',NULL,NULL,b'1110',b'1100',b'0110',b'0111',b'0010']
            WHEN p_iso = 2 AND p_x = b'1110'     THEN ARRAY[NULL,NULL,NULL,NULL,b'1011',b'1001',b'1100',b'1101']
            WHEN p_iso = 2 AND p_x = b'1111'     THEN ARRAY[NULL,NULL,NULL,NULL,b'1101',b'0111',b'0010',NULL]
        END
    ;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.neighborsl0(varbit,int)
  IS 'Returns the neighbors of a cell L0 in varbit array, in order: North, North East, East, South East, South, South West, West, North West.'
;

CREATE or replace VIEW osmc.vw01neighborsl0 AS
  SELECT isolabel_ext, l0bits, array_agg(nl0bits)
  -- SELECT 'WHEN p_iso = ''' || isolabel_ext || ''' AND p_x = b''' || l0bits::text || ''' THEN ARRAY[' || string_agg(CASE WHEN nl0bits IS NULL THEN 'NULL' ELSE 'b''' || nl0bits::text || ''''  END,',') || ']'
  -- SELECT isolabel_ext, l0bits, MAX(code) AS L0, array_agg(nl0bits), array_agg(ncode)
  FROM
  (
      SELECT
          c.isolabel_ext,
          osmc.extract_L0bits(c.cbits) AS l0bits,
          CASE
          WHEN c.int_country_id IN (1,4) THEN osmc.encode_16h1c(natcod.vbit_to_baseh(osmc.extract_L0bits(c.cbits),16),c.int_country_id)
          ELSE                                                  natcod.vbit_to_baseh(osmc.extract_L0bits(c.cbits),16)
          END AS code,
          c.bbox AS bbox
          ,
          osmc.extract_L0bits(d.cbits) AS nl0bits,
          CASE
          WHEN c.int_country_id IN (1,4) THEN osmc.encode_16h1c(natcod.vbit_to_baseh(osmc.extract_L0bits(d.cbits),16),c.int_country_id)
          ELSE                                                  natcod.vbit_to_baseh(osmc.extract_L0bits(d.cbits),16)
          END AS ncode,
          d.bbox AS nbbox
      FROM
      (
          SELECT *,
              CASE npos
                  WHEN 1 THEN ARRAY[  bbox[1]        ,  bbox[4]        ,  bbox[3]        ,2*bbox[4]-bbox[2]] -- N
                  WHEN 2 THEN ARRAY[  bbox[3]        ,  bbox[4]        ,2*bbox[3]-bbox[1],2*bbox[4]-bbox[2]] -- NE
                  WHEN 3 THEN ARRAY[  bbox[3]        ,  bbox[2]        ,2*bbox[3]-bbox[1],  bbox[4]        ] -- E
                  WHEN 4 THEN ARRAY[  bbox[3]        ,2*bbox[2]-bbox[4],2*bbox[3]-bbox[1],  bbox[2]        ] -- SE
                  WHEN 5 THEN ARRAY[  bbox[1]        ,2*bbox[2]-bbox[4],  bbox[3]        ,  bbox[2]        ] -- S
                  WHEN 6 THEN ARRAY[2*bbox[1]-bbox[3],2*bbox[2]-bbox[4],  bbox[1]        ,  bbox[2]        ] -- SW
                  WHEN 7 THEN ARRAY[2*bbox[1]-bbox[3],  bbox[2]        ,  bbox[1]        ,  bbox[4]        ] -- W
                  WHEN 8 THEN ARRAY[2*bbox[1]-bbox[3],  bbox[4]        ,  bbox[1]        ,2*bbox[4]-bbox[2]] -- NW
              END AS nbbox
          FROM
          (
              SELECT cbits, bbox, isolabel_ext, unnest(ARRAY[1,2,3,4,5,6,7,8]) AS npos, osmc.extract_jurisdbits(cbits) AS int_country_id
              FROM osmc.coverage c
              WHERE c.is_country IS TRUE
          ) r
      ) c
      LEFT JOIN
      (
          SELECT cbits, bbox
          FROM osmc.coverage c
          WHERE c.is_country IS TRUE
      ) d
      ON c.nbbox = d.bbox
      ORDER BY 1, 2, 3, c.npos
  ) v
  GROUP BY isolabel_ext, l0bits
  ORDER BY isolabel_ext, l0bits
;

CREATE or replace FUNCTION osmc.neighbors(
  p_x    varbit, -- cell bits
  p_l0   varbit, -- L0 bits
  p_iso  int
) RETURNS varbit[] AS $f$
    SELECT array_agg(n_L0 || n_cell) AS neighbors
    FROM unnest
          (
              osmc.neighbors_raw(p_x),
              (
                  SELECT
                      CASE osmc.cell_relate(p_x)
                          WHEN b'0000' THEN ARRAY[p_l0,p_l0,p_l0,p_l0,p_l0,p_l0,p_l0,p_l0]
                          WHEN b'0010' THEN ARRAY[nL0[1],nL0[1],p_l0,p_l0,p_l0,p_l0,p_l0,nL0[1]]
                          WHEN b'0011' THEN ARRAY[nL0[1],nL0[2],nL0[3],nL0[3],p_l0,p_l0,p_l0,nL0[1]]
                          WHEN b'0001' THEN ARRAY[p_l0,nL0[3],nL0[3],nL0[3],p_l0,p_l0,p_l0,p_l0]
                          WHEN b'1001' THEN ARRAY[p_l0,nL0[3],nL0[3],nL0[4],nL0[5],nL0[5],p_l0,p_l0]
                          WHEN b'1000' THEN ARRAY[p_l0,p_l0,p_l0,nL0[5],nL0[5],nL0[5],p_l0,p_l0]
                          WHEN b'1100' THEN ARRAY[p_l0,p_l0,p_l0,nL0[5],nL0[5],nL0[6],nL0[7],nL0[7]]
                          WHEN b'0100' THEN ARRAY[p_l0,p_l0,p_l0,p_l0,p_l0,nL0[7],nL0[7],nL0[7]]
                          WHEN b'0110' THEN ARRAY[nL0[1],nL0[1],p_l0,p_l0,p_l0,nL0[7],nL0[7],nL0[8]]
                      END
                  FROM osmc.neighborsl0(p_L0,p_iso) t(nL0)
              )
          ) r(n_cell,n_L0)
    ;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.neighbors(varbit,varbit,int)
  IS 'Returns the neighbors of a cell in varbit array (with L0 bits), in order: North, North East, East, South East, South, South West, West, North West.'
;

CREATE or replace FUNCTION osmc.neighbors(
  p_x    varbit, -- with L0 bits
  p_iso  int,
  p_base int DEFAULT 16
) RETURNS varbit[] AS $wrap$
    SELECT osmc.neighbors(osmc.vbit_withoutL0(p_x,p_iso),osmc.extract_L0bits(0::bit(8)||p_x),p_iso);
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.neighbors(varbit,int,int)
  IS 'Returns the neighbors of a cell in varbit array (with L0 bits), in order: North, North East, East, South East, South, South West, West, North West.'
;
-- EXPLAIN ANALYZE SELECT osmc.neighbors(natcod.baseh_to_vbit(osmc.decode_16h1c('6aaar','BR')),'BR');

CREATE or replace FUNCTION osmc.neighbors_test(
  p_x    text[],
  p_iso  int,
  p_jurisd_id int,
  p_base int DEFAULT 16
) RETURNS TABLE (prefix text, neighbors text[]) AS $f$
    SELECT prefix, array_agg(CASE WHEN p_base = 18 THEN osmc.encode_16h1c(natcod.vbit_to_baseh(ng,16),p_jurisd_id) ELSE natcod.vbit_to_baseh(ng,16) END)
    FROM
    (
        SELECT prefix, unnest(osmc.neighbors(natcod.baseh_to_vbit(CASE WHEN p_base = 18 THEN osmc.decode_16h1c(prefix,p_iso) ELSE prefix END,16),p_iso,16)) AS ng
        FROM unnest(p_x) t(prefix)
    ) s
    GROUP BY prefix
    ;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.neighbors_test(text[],int,int,int)
  IS ''
;
-- SELECT * FROM osmc.neighbors_test(ARRAY['6aaar', '6000r', '6fabr', '6fffv', '67ffv', '6555m', '6000h', '6040h', '62aah', '6911m', '6fafr', '8a24v', '0aaar'],'BR',76,18);

CREATE or replace FUNCTION osmc.neighbors_test(
  p_x    text,
  p_iso  int,
  p_jurisd_id int,
  p_base int DEFAULT 16
) RETURNS TABLE (prefix text, neighbors text[]) AS $f$
    SELECT *
    FROM osmc.neighbors_test(ARRAY[p_x],p_iso,p_jurisd_id,p_base)
    ;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.neighbors_test(text[],int,int,int)
  IS ''
;
-- SELECT neighbors FROM osmc.neighbors_test('6aaar','BR',76,18);


------------------
-- hBig <-> AFAcodes scientific:

CREATE or replace FUNCTION osmc.hBig_to_afa_sci(
  p_b bigint
) RETURNS text AS $f$
SELECT
  CASE x::bit(8)
    WHEN b'00000001' THEN 'BR+' || osmc.encode_16h1c(natcod.vbit_to_baseh(substring(x from 9),16,true),76)
    -- ELSE
  END
FROM natcod.hBig_to_vBit(p_b) x
;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.hBig_to_afa_sci(bigint)
  IS ''
;
-- SELECT osmc.hBig_to_afa_sci(37996971798872115); -- BR+dfc16cd39s

CREATE or replace FUNCTION osmc.varbit_to_afa_sci(
  p_code varbit,
  p_jurisd_id  int
) RETURNS text AS $f$
SELECT
  CASE
    WHEN p_jurisd_id IN (76,868) THEN osmc.encode_16h1c(natcod.vbit_to_baseh(p_code,16,true),76)
    ELSE                                                natcod.vbit_to_baseh(p_code,16,true)
  END
;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.varbit_to_afa_sci(varbit,int)
  IS ''
;
-- SELECT osmc.varbit_to_afa_sci(b'0000110111111100000101101100110100111001100',76); -- BR+dfc16cd39s


CREATE or replace FUNCTION osmc.afa_sci_to_hBig(
  p_code text,
  p_separator text DEFAULT '\+'
) RETURNS bigint AS $f$
SELECT
  CASE u[1]
    WHEN 'BR' THEN natcod.vBit_to_hBig(1::bit(8)||(natcod.baseh_to_vbit(osmc.decode_16h1c(u[2],1),16)))
    -- ELSE
  END
FROM regexp_split_to_array(p_code,p_separator) u
;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.afa_sci_to_hBig(text,text)
  IS ''
;
-- SELECT osmc.afa_sci_to_hBig('BR+dfc16cd39s'); -- 37996971798872115
-- SELECT natcod.hBig_to_vBit(37996971798872115); -- 000000010000110111111100000101101100110100111001100
-- SELECT osmc.afa_sci_to_hBig(b'0000110111111100000101101100110100111001100','BR'); -- 37996971798872115


-- hBig <-> AFAcodes logistics:

CREATE or replace FUNCTION osmc.hBig_to_afa_log(
  p_b bigint
) RETURNS text AS $f$
SELECT natcod.b32nvu_to_vbit(natcod.vbit_to_strstd( osmc.cbits_16h_to_b32nvu(substring(x from 9),76),'32nvu')) AS code
FROM natcod.hBig_to_vBit(p_b) x
;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.hBig_to_afa_log(bigint)
  IS ''
;
-- SELECT osmc.hBig_to_afa_log(37996971798872115); -- BR+dfc16cd39s


CREATE or replace FUNCTION osmc.afa_log_to_hBig(
   p_code text,
   p_separator text DEFAULT '\~'
) RETURNS bigint AS $f$
  SELECT
  (
    SELECT
      natcod.vBit_to_hBig(((CASE split_part(co.isolabel_ext,'-',1)
        WHEN 'BR' THEN b'00000001' -- extrair do cbits
        -- WHEN 'CO' THEN b'00000010'
        -- WHEN 'CM' THEN
        -- WHEN 'EC' THEN
        -- WHEN 'UY' THEN
      END) || osmc.extract_cellbits(cbits) || natcod.b32nvu_to_vbit(upper(substring(u[2],2)))))
    FROM osmc.coverage co
    WHERE is_country IS FALSE AND co.isolabel_ext = (str_geocodeiso_decode(u[1]))[1]
      AND cindex = substring(upper(u[2]),1,1)
  )
  FROM regexp_split_to_array(p_code,p_separator) u
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.afa_log_to_hBig(text,text)
  IS 'Decode Postal OSMcode.'
;
-- EXPLAIN ANALYZE SELECT osmc.afa_log_to_hBig('BR-SP-SaoPaulo~MDUGD'); -- 37996971798872115

-- SELECT osmc.afa_log_to_hBig('BR-SP-SaoPaulo~MWMP'); -- 37996973917863982
-- SELECT osmc.afa_sci_to_hBig('BR+dfc17c9dm'); -- 37996973917863982
-- SELECT osmc.hBig_to_afa_sci(37996973917863982); -- BR+dfc17c9dm
-- SELECT natcod.hBig_to_vBit(37996973917863982); -- 00000001 00001101111111000001011111001001110101
-- SELECT osmc.afa_sci_to_hBig(b'00001101111111000001011111001001110101','BR'); --37996973917863982
