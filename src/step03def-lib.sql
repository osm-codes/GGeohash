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

CREATE or replace FUNCTION osmc.str_geocodeiso_decode(iso text)
RETURNS text[] as $f$
  SELECT isolabel_ext || array[split_part(isolabel_ext,'-',1)]
  FROM mvwjurisdiction_synonym
  WHERE synonym = lower((
    SELECT
      CASE
        WHEN cardinality(u)=2 AND u[2] ~ '^\d+?$'
        THEN u[1]::text || '-' || ((u[2])::integer)::text
        ELSE iso
      END
    FROM (SELECT regexp_split_to_array(iso,'(-)')::text[] AS u ) r
  ))
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.str_geocodeiso_decode(text)
  IS 'Decode abbrev isolabel_ext.'
;
--SELECT osmc.str_geocodeiso_decode('CO-Itagui');
--SELECT osmc.str_geocodeiso_decode('CO-05282');

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
    ELSE natcod.vbit_to_baseh(p_l0code || p_code || x,p_base)
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
CREATE INDEX osm_coverage_geom_idx1         ON osmc.coverage USING gist (geom);
CREATE INDEX osm_coverage_geom4326_idx1     ON osmc.coverage USING gist (geom_srid4326);
CREATE INDEX osm_coverage_isolabel_ext_idx1 ON osmc.coverage USING btree (isolabel_ext);
CREATE INDEX osm_coverage_cbits10true_idx        ON osmc.coverage ((cbits::bit(10))) WHERE is_country IS TRUE;
CREATE INDEX osm_coverage_isolabel_ext_true_idx  ON osmc.coverage (isolabel_ext) WHERE is_country IS TRUE;
CREATE INDEX osm_coverage_isolabel_ext_false_idx ON osmc.coverage (isolabel_ext) WHERE is_country IS FALSE;
CREATE INDEX osm_coverage_cbits15false_idx       ON osmc.coverage ((cbits::bit(14)),isolabel_ext) WHERE is_country IS FALSE;

COMMENT ON COLUMN osmc.coverage.cbits          IS 'Coverage cell identifier.';
COMMENT ON COLUMN osmc.coverage.isolabel_ext   IS 'ISO 3166-1 alpha-2 code and name (camel case); e.g. BR-SP-SaoPaulo.';
COMMENT ON COLUMN osmc.coverage.cindex         IS 'Coverage cell prefix in 32nvu.  Used only case is_country=false.';
COMMENT ON COLUMN osmc.coverage.bbox           IS 'Coverage cell bbox.';
COMMENT ON COLUMN osmc.coverage.status         IS 'Coverage status. Convention: 0: generated, 1: revised, 2: homologated.';
COMMENT ON COLUMN osmc.coverage.is_country     IS 'True if it is a cell of national coverage..';
COMMENT ON COLUMN osmc.coverage.is_contained   IS 'True if it is a cell contained in the jurisdiction..';
COMMENT ON COLUMN osmc.coverage.is_overlay     IS 'True if it is an overlay cell.';
COMMENT ON COLUMN osmc.coverage.kx_prefix      IS 'Coverage cell prefix in 32nvu.';
COMMENT ON COLUMN osmc.coverage.geom           IS 'Coverage cell geometry on default srid.';
COMMENT ON COLUMN osmc.coverage.geom_srid4326  IS 'Coverage cell geometry on 4326 srid. Used only case is_country=true.';

COMMENT ON TABLE osmc.coverage IS 'Jurisdictional coverage.';

------------------
-- encode/decode 16h1c:

CREATE or replace FUNCTION osmc.encode_16h1c(
  p_code           text,
  p_jurisd_base_id int
) RETURNS text AS $wrap$
  SELECT
    CASE
      -- tr g->F, q->F
      WHEN p_jurisd_base_id = 76 AND length(p_code) > 2
      THEN
      (
        ('{"00": "0", "01": "1", "02": "2", "03": "3", "04": "4", "05": "5", "06": "6", "07": "7",
           "08": "8", "09": "9", "0a": "a", "0b": "b", "0c": "c", "0d": "d", "0e": "e", "0f": "f",
           "10": "f", "11": "f", "12": "h", "13": "m", "14": "r", "15": "v", "16": "j", "17": "k",
           "18": "n", "19": "p", "1a": "s", "1b": "t", "1c": "z", "1d": "y"}'::jsonb)->>(substring(p_code,1,2))
      )
      -- tr g->E, q->5, h->0
      WHEN p_jurisd_base_id = 858 AND length(p_code) > 2 AND substring(p_code,1,3) IN ('100','101','102','10h','10j','10k', '12a','12b','12t', '11v','11z','11y','11c','11d','11e','11f')
      THEN
      (
        ('{"10": "e", "11": "5", "12": "0"}'::jsonb)->>(substring(p_code,1,2))
      )
      WHEN p_jurisd_base_id = 858 AND length(p_code) > 2 AND substring(p_code,1,3) NOT IN (
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
COMMENT ON FUNCTION osmc.encode_16h1c(text,int)
  IS 'Encodes ghosts in BR and UY.'
;

CREATE or replace FUNCTION osmc.decode_16h1c(
  p_code text,
  p_iso  text
) RETURNS text AS $wrap$
  SELECT
    CASE
      -- fr,ft,fs,fa,fb,f8,f9: tr f -> 0f
      WHEN p_iso = 'BR' AND substring(p_code,1,2) IN ('fr','ft','fs','fa','fb','f8','f9') THEN ('0f')
      -- fn,f4,f5: tr f -> q
      WHEN p_iso = 'BR' AND substring(p_code,1,2) IN ('fn','f4','f5')                     THEN ('11')
      -- fp,f6,f7: tr f -> g
      WHEN p_iso = 'BR' AND substring(p_code,1,2) IN ('fp','f6','f7')                     THEN ('10')

      -- e0,e1,e2,eh,ej,ek: tr f -> g
      WHEN p_iso = 'UY' AND substring(p_code,1,2) IN ('e0','e1','e2','eh','ej','ek')      THEN ('10')
      -- 0a,0b,0t: tr 0 -> h
      WHEN p_iso = 'UY' AND substring(p_code,1,2) IN ('0a','0b','0t')                     THEN ('12')
      -- 5v,5z,5y,5c,5d,5e,5f: tr 5 -> q
      WHEN p_iso = 'UY' AND substring(p_code,1,2) IN ('5v','5z','5y','5c','5d','5e','5f') THEN ('11')
      ELSE
      (
        ('{"0": "00", "1": "01", "2": "02", "3": "03", "4": "04", "5": "05", "6": "06", "7": "07",
        "8": "08", "9": "09", "a": "0a", "b": "0b", "c": "0c", "d": "0d", "e": "0e", "f": "0f",
        "g": "10", "q": "11", "h": "12", "m": "13", "r": "14", "v": "15", "j": "16", "k": "17",
        "n": "18", "p": "19", "s": "1a", "t": "1b", "z": "1c", "y": "1d"}'::jsonb)->>(substring(p_code,1,1))
      )
    END || substring(p_code,2)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.decode_16h1c(text,text)
  IS 'Decode ghosts in BR and UY.'
;
-- SELECT osmc.decode_16h1c('fr','BR');

CREATE or replace FUNCTION osmc.extract_L0bits(
  p_x   varbit,
  p_iso text
) RETURNS varbit AS $wrap$
  SELECT
    CASE
    WHEN p_iso IN ('BR','UY','EC') THEN (p_x<<10)::bit(8) -- Retorna 8 bits
    WHEN p_iso IN ('CO')           THEN (p_x<<10)::bit(4) -- Retorna 4 bits
    END
    ;
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.extract_L0bits(varbit,text)
  IS 'Return bits L0 from id cell.'
;

CREATE or replace FUNCTION osmc.extract_L0bits32(
  p_x   varbit,
  p_iso text
) RETURNS varbit AS $wrap$
  SELECT
    CASE
    WHEN p_iso IN ('BR','UY','EC') THEN (p_x<<13)::bit(5) -- Descarta 3 bits MSb
    WHEN p_iso IN ('CO')           THEN ((p_x<<10)::bit(5))>>1 -- Acrescenta '0' como MSb
    END
    ;
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.extract_L0bits32(varbit,text)
  IS 'Return bits L0 from id cell.'
;

CREATE or replace FUNCTION osmc.extract_cellbits(
  p_x  varbit
) RETURNS varbit AS $f$
  SELECT substring(p_x from 11);
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.extract_cellbits(varbit)
  IS 'Return cell bits. Discard jurisdiction bits.'
;

CREATE or replace FUNCTION osmc.vbit_from_b32nvu_to_vbit_16h(
  p_x  varbit,
  p_iso int
) RETURNS varbit AS $wrap$
  SELECT
    CASE
    WHEN p_iso IN (76,868,218) THEN b'000' || p_x         -- 5bits MSb viram 8
    WHEN p_iso IN (170)        THEN substring(p_x,2,4) || substring(p_x from 8) -- 5bits MSb viram 4. eg.: abcdefghijk -> bcdehijk
    END
    ;
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.vbit_from_b32nvu_to_vbit_16h(varbit,int)
  IS 'Convert 5-bit L0 to 4-bit L0.'
;

CREATE or replace FUNCTION osmc.vbit_from_16h_to_vbit_b32nvu(
  p_x  varbit,
  p_iso int
) RETURNS varbit AS $wrap$
  SELECT
    CASE
    WHEN p_iso IN (76,868,218) THEN substring(p_x from 4) -- 8bits MSb viram 5
    WHEN p_iso IN (170)        THEN b'0' || substring(p_x,1,4) || b'00' || substring(p_x from 5) -- 4bits MSb viram 5. eg.: xxxxxxxx -> 0xxxx00xxxx
    END
    ;
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.vbit_from_16h_to_vbit_b32nvu(varbit,int)
  IS 'Convert 4-bit L0 to 5-bit L0.'
;

CREATE or replace FUNCTION osmc.vbit_withoutL0(
  p_x  varbit,
  p_iso text,
  p_base int DEFAULT 16
) RETURNS varbit AS $wrap$
  SELECT
    CASE
    WHEN p_iso IN ('BR','UY','EC') AND p_base <> 32 THEN substring(p_x from 9) -- Remove 8 bits MSb
    WHEN p_iso IN ('CO')           AND p_base <> 32 THEN substring(p_x from 5) -- Remove 4 bits MSb
    WHEN p_base = 32                                THEN substring(p_x from 6) -- Remove 5 bits MSb
    END
    ;
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.vbit_withoutL0(varbit,text,int)
  IS 'Remove 4-bit or 8-bit L0.'
;

-- specific encode for b32nvu and 8 digits:

CREATE or replace FUNCTION osmc.encode_point_brazil(
  p_geom       geometry(POINT),
  p_bit_length int DEFAULT 40
) RETURNS text AS $wrap$
  SELECT
    (
      SELECT (natcod.vbit_to_strstd(osmc.extract_L0bits32(cbits,'BR') || ggeohash.encode3(ST_X(x),ST_Y(x),bbox,p_bit_length,false),'32nvu'))
      FROM osmc.coverage
      WHERE is_country IS TRUE AND cbits::bit(10) = 76::bit(10) AND ST_Contains(geom,x)
    )
  FROM (SELECT ST_Transform(p_geom,952019)) t(x)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_point_brazil(geometry(POINT))
  IS 'Encode Point for Brazil: base 32nvu, 8 digits'
;

CREATE or replace FUNCTION osmc.encode_point_colombia(
  p_geom       geometry(POINT),
  p_bit_length int DEFAULT 40
) RETURNS text AS $wrap$
  SELECT
    (
      SELECT (natcod.vbit_to_strstd(osmc.extract_L0bits32(cbits,'CO') || ggeohash.encode3(ST_X(x),ST_Y(x),bbox,p_bit_length,false),'32nvu'))
      FROM osmc.coverage
      WHERE is_country IS TRUE AND cbits::bit(10) = 170::bit(10) AND ST_Contains(geom,x)
    )
  FROM (SELECT ST_Transform(p_geom,9377)) t(x)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_point_colombia(geometry(POINT))
  IS 'Encode Point for Colombia: base 32nvu, 8 digits'
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

CREATE or replace FUNCTION osmc.osmcode_encode_scientific(
  p_geom       geometry(POINT),
  p_base       int     DEFAULT 16,
  p_bit_length int     DEFAULT 40,
  p_srid       int     DEFAULT 9377,
  p_grid_size  int     DEFAULT 2,
  p_bbox       int[]   DEFAULT array[0,0,0,0],
  p_l0code     varbit  DEFAULT b'0',
  p_jurisd_base_id int DEFAULT 170,
  p_lonlat     boolean DEFAULT false  -- false: latLon, true: lonLat
) RETURNS jsonb AS $f$
    SELECT jsonb_build_object(
      'type', 'FeatureCollection',
      'features',
        (
          (ST_AsGeoJSONb(ST_Transform_resilient(geom_cell,4326,0.005),8,0,null,
              jsonb_strip_nulls(jsonb_build_object(
                  'code', CASE WHEN p_base = 18 THEN osmc.encode_16h1c(code,p_jurisd_base_id) ELSE code END,
                  'area', ST_Area(geom_cell),
                  'side', SQRT(ST_Area(geom_cell)),
                  'base', base,
                  'jurisd_base_id',p_jurisd_base_id
                  ))
          )::jsonb) || m.subcells
        )
      )
    FROM
    (
      SELECT bit_string,
      ggeohash.draw_cell_bybox((CASE WHEN p_bit_length = 0 THEN p_bbox ELSE ggeohash.decode_box2(bit_string,p_bbox,p_lonlat) END),false,p_srid) AS geom_cell,
      osmc.string_base(p_base) AS base,
      natcod.vbit_to_baseh(CASE WHEN p_bit_length = 0 THEN p_l0code ELSE p_l0code||bit_string END,16) AS code
      FROM ggeohash.encode3(ST_X(p_geom),ST_Y(p_geom),p_bbox,p_bit_length,p_lonlat) r(bit_string)
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
                        'code_subcell', (CASE WHEN length(code2) = length(ghs2) THEN substring(ghs2 FROM length(code2)) ELSE substring(ghs2 FROM length(code2)+1) END) ,
                        'prefix', code2,
                        'area', geom_area,
                        'side', SQRT(geom_area),
                        'base', base
                        ))
                    )::jsonb)
              FROM
             (
              SELECT geom, ghs,
                  CASE WHEN p_base = 18 THEN osmc.encode_16h1c(ghs,p_jurisd_base_id)  ELSE ghs  END AS ghs2,
                  CASE WHEN p_base = 18 THEN osmc.encode_16h1c(code,p_jurisd_base_id) ELSE code END AS code2,
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

    WHERE
    CASE WHEN p_jurisd_base_id = 858 THEN code NOT IN ('0eg','10g','12g','00r','12r','0eh','05q','11q') ELSE TRUE  END
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.osmcode_encode_scientific(geometry(POINT),int,int,int,int,int[],varbit,int,boolean)
  IS 'Encodes geometry to OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_scientific_br(
  p_geom        geometry(POINT),
  p_uncertainty float  DEFAULT -1,
  p_grid_size   int    DEFAULT 0
) RETURNS jsonb AS $f$
    SELECT osmc.osmcode_encode_scientific(p_geom,18,
      CASE
      WHEN p_uncertainty > -1 THEN x
      ELSE 40
      END,
      952019,
      CASE
        WHEN x = 40 THEN 0
        WHEN p_grid_size > 0 AND x = 39 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 3 ELSE 2 END))
        WHEN p_grid_size > 0 AND x = 38 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 5 ELSE 4 END))
        WHEN p_grid_size > 0 AND x = 37 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 9 ELSE 8 END))
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits(cbits,'BR'),76,FALSE)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty)) t(x)
    WHERE is_country IS TRUE AND cbits::bit(10) = 76::bit(10) AND ST_Contains(geom,p_geom)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_scientific_br(geometry(POINT),float,int)
  IS 'Encodes geometry to BR Scientific OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_scientific_co(
  p_geom        geometry(POINT),
  p_uncertainty float  DEFAULT -1,
  p_grid_size   int    DEFAULT 0
) RETURNS jsonb AS $f$
    SELECT osmc.osmcode_encode_scientific(p_geom,16,
      CASE
      WHEN p_uncertainty > -1 AND x >  2 THEN x-2
      WHEN p_uncertainty > -1 AND x <= 2 THEN 0
      ELSE 40
      END,
      9377,
      CASE
        WHEN x > 39 THEN 0
        WHEN p_grid_size > 0 AND x = 39 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 3 ELSE 2 END))
        WHEN p_grid_size > 0 AND x = 38 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 5 ELSE 4 END))
        WHEN p_grid_size > 0 AND x = 37 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 9 ELSE 8 END))
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits(cbits,'CO'),170,FALSE)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty)) t(x)
    WHERE is_country IS TRUE AND cbits::bit(10) = 170::bit(10) AND ST_Contains(geom,p_geom)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_scientific_co(geometry(POINT),float,int)
  IS 'Encodes geometry to CO Scientific OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_scientific_uy(
  p_geom        geometry(POINT),
  p_uncertainty float  DEFAULT -1,
  p_grid_size   int    DEFAULT 0
) RETURNS jsonb AS $f$
    SELECT osmc.osmcode_encode_scientific(p_geom,18,
      CASE
      WHEN p_uncertainty > -1 AND x >  6 THEN x-6
      WHEN p_uncertainty > -1 AND x <= 6 THEN 0
      ELSE 40
      END,
      32721,
      CASE
        WHEN x = 40 THEN 0
        WHEN p_grid_size > 0 AND x = 39 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 3 ELSE 2 END))
        WHEN p_grid_size > 0 AND x = 38 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 5 ELSE 4 END))
        WHEN p_grid_size > 0 AND x = 37 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 9 ELSE 8 END))
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits(cbits,'UY'),858,FALSE)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty)) t(x)
    WHERE is_country IS TRUE AND cbits::bit(10) = 858::bit(10) AND ST_Contains(geom,p_geom)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_scientific_uy(geometry(POINT),float,int)
  IS 'Encodes geometry to UY Scientific OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_scientific_ec(
  p_geom        geometry(POINT),
  p_uncertainty float  DEFAULT -1,
  p_grid_size   int    DEFAULT 0
) RETURNS jsonb AS $f$
    SELECT osmc.osmcode_encode_scientific(p_geom,16,
      CASE
      WHEN p_uncertainty > -1 AND x >  5 THEN x-5
      WHEN p_uncertainty > -1 AND x <= 5 THEN 0
      ELSE 40
      END,
      32717,
      CASE
        WHEN x = 40 THEN 0
        WHEN p_grid_size > 0 AND x = 39 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 3 ELSE 2 END))
        WHEN p_grid_size > 0 AND x = 38 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 5 ELSE 4 END))
        WHEN p_grid_size > 0 AND x = 37 THEN least(p_grid_size,(CASE WHEN p_grid_size % 2 = 1 THEN 9 ELSE 8 END))
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits(cbits,'EC'),218,FALSE)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty)) t(x)
    WHERE is_country IS TRUE AND cbits::bit(10) = 218::bit(10) AND ST_Contains(geom,p_geom)
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
        WHERE is_country IS FALSE AND (cbits)::bit(14) = p_codebits::bit(14)
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
  p_bit_length int     DEFAULT 40,
  p_srid       int     DEFAULT 9377,
  p_grid_size  int     DEFAULT 32,
  p_bbox       int[]   DEFAULT array[0,0,0,0],
  p_l0code     varbit  DEFAULT b'0',
  p_jurisd_base_id int DEFAULT 170,
  p_lonlat     boolean DEFAULT false, -- false: latLon, true: lonLat
  p_type       int     DEFAULT 1, -- 1: isolabel_ext~short_code, 2: ISO-jurisd_local_id~short_code
  p_isolabel_ext text  DEFAULT NULL

) RETURNS jsonb AS $f$
    SELECT jsonb_build_object(
      'type', 'FeatureCollection',
      'features',
          jsonb_agg(ST_AsGeoJSONb(ST_Transform_resilient(geom_cell,4326,0.005),8,0,null,
              jsonb_strip_nulls(jsonb_build_object(
                  'code', code,
                  'short_code', CASE p_type WHEN 2 THEN split_part(isolabel_ext,'-',1) || '-' || jurisd_local_id ELSE isolabel_ext END || '~' || short_code,
                  'area', ST_Area(geom_cell),
                  'side', SQRT(ST_Area(geom_cell)),
                  'base', '32nvu',
                  'jurisd_local_id', jurisd_local_id,
                  'jurisd_base_id', p_jurisd_base_id,
                  'isolabel_ext', p_isolabel_ext,
                  'scientic_code', CASE
                                    WHEN p_jurisd_base_id IN (76,868)
                                    THEN osmc.encode_16h1c(natcod.vbit_to_baseh(codebits,16),p_jurisd_base_id)
                                    ELSE                   natcod.vbit_to_baseh(codebits,16)
                                    END
                  ))
          )::jsonb)
      )
    FROM
    (
      SELECT bit_string,
      ggeohash.draw_cell_bybox(ggeohash.decode_box2(bit_string,p_bbox,p_lonlat),false,p_srid) AS geom_cell,
      upper(natcod.vbit_to_strstd( osmc.vbit_from_16h_to_vbit_b32nvu((p_l0code || bit_string),p_jurisd_base_id),'32nvu')) AS code,
      p_l0code||bit_string AS codebits
      FROM ggeohash.encode3(ST_X(p_geom),ST_Y(p_geom),p_bbox,p_bit_length,p_lonlat) r(bit_string)
    ) c
    -- responsável pelo código logístico
    LEFT JOIN LATERAL ( SELECT * FROM osmc.encode_short_code(c.code,p_jurisd_base_id::bit(10)||codebits,p_isolabel_ext,p_geom) ) t ON TRUE

    WHERE
    CASE WHEN p_jurisd_base_id = 858 THEN code NOT IN ('0eg','10g','12g','00r','12r','0eh','05q','11q') ELSE TRUE  END
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal(geometry(POINT),int,int,int,int[],varbit,int,boolean,int,text)
  IS 'Encodes geometry to Postal OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_postal_br(
  p_geom         geometry(POINT),
  p_uncertainty  float  DEFAULT -1,
  p_grid_size    int    DEFAULT 0,
  p_isolabel_ext text   DEFAULT NULL
) RETURNS jsonb AS $f$
    SELECT osmc.encode_postal(p_geom,
      CASE
      WHEN p_uncertainty > -1 THEN (x/5)*5
      ELSE 35
      END,
      952019,
      CASE
        WHEN x = 40 THEN 0
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits(cbits,'BR'),76,FALSE,1,p_isolabel_ext)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty)) t(x)
    WHERE is_country IS TRUE AND cbits::bit(10) = 76::bit(10) AND ST_Contains(geom,p_geom)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal_br(geometry(POINT),float,int,text)
  IS 'Encodes geometry to BR Postal OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_postal_co(
  p_geom         geometry(POINT),
  p_uncertainty  float  DEFAULT -1,
  p_grid_size    int    DEFAULT 0,
  p_isolabel_ext text   DEFAULT NULL
) RETURNS jsonb AS $f$
    SELECT osmc.encode_postal(p_geom,
      CASE
      WHEN p_uncertainty > -1 THEN ((x-2)/5)*5 +3
      ELSE 35
      END,
      9377,
      CASE
        WHEN x = 40 THEN 0
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits(cbits,'CO'),170,FALSE,2,p_isolabel_ext)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty)) t(x)
    WHERE is_country IS TRUE AND cbits::bit(10) = 170::bit(10) AND ST_Contains(geom,p_geom)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal_co(geometry(POINT),float,int,text)
  IS 'Encodes geometry to CO Postal OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_postal_uy(
  p_geom         geometry(POINT),
  p_uncertainty  float  DEFAULT -1,
  p_grid_size    int    DEFAULT 0,
  p_isolabel_ext text   DEFAULT NULL
) RETURNS jsonb AS $f$
    SELECT osmc.encode_postal(p_geom,
      CASE
      WHEN p_uncertainty > -1 AND x > 6  THEN ((x-6)/5)*5
      WHEN p_uncertainty > -1 AND x <= 6 THEN 0
      ELSE 35
      END,
      32721,
      CASE
        WHEN x > 31 THEN 0
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits(cbits,'UY'),858,FALSE,1,p_isolabel_ext)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty)) t(x)
    WHERE is_country IS TRUE AND cbits::bit(10) = 858::bit(10) AND ST_Contains(geom,p_geom)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal_uy(geometry(POINT),float,int,text)
  IS 'Encodes geometry to UY Postal OSMcode.'
;

CREATE or replace FUNCTION osmc.encode_postal_ec(
  p_geom         geometry(POINT),
  p_uncertainty  float  DEFAULT -1,
  p_grid_size    int    DEFAULT 0,
  p_isolabel_ext text   DEFAULT NULL
) RETURNS jsonb AS $f$
    SELECT osmc.encode_postal(p_geom,
      CASE
      WHEN p_uncertainty > -1 AND x > 5  THEN ((x-5)/5)*5
      WHEN p_uncertainty > -1 AND x <= 5 THEN 0
      ELSE 35
      END,
      32717,
      CASE
        WHEN x > 35 THEN 0
        ELSE p_grid_size
      END
      ,bbox,osmc.extract_L0bits(cbits,'EC'),218,TRUE,1,p_isolabel_ext)
    FROM osmc.coverage u, (SELECT osmc.uncertain_base16h(p_uncertainty)) t(x)
    WHERE is_country IS TRUE AND cbits::bit(10) = 218::bit(10) AND ST_Contains(geom,p_geom)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal_ec(geometry(POINT),float,int,text)
  IS 'Encodes geometry to EC Postal OSMcode.'
;
