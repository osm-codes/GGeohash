--
--  Grade Estatistica/Postal
--

CREATE EXTENSION IF NOT EXISTS postgis;
CREATE SCHEMA    IF NOT EXISTS api;

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
  x0 float, -- referencia de inicio do eixo x [x0,y0]
  y0 float, -- referencia de inicio do eixo y [x0,y0]
  s  float  -- lado da célula
) RETURNS float[] AS $f$
  SELECT array[ x0+i::float*s, y0+j::float*s, x0+i::float*s+s, y0+j::float*s+s ]
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.ij_to_bbox(int,int,float,float,float)
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
/*
CREATE or replace FUNCTION osmc.xy_to_bbox(
  x  int,
  y  int,
  x0 int, -- referencia de inicio do eixo x [x0,y0]
  y0 int, -- referencia de inicio do eixo y [x0,y0]
  s  int  -- lado da célula
  ) RETURNS int[] AS $f$
  SELECT osmc.ij_to_bbox(osmc.xy_to_ij(x,y,x0,y0,s))
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.xy_to_bbox(int,int,int,int,int)
 IS 'Retorna bbox da célula que contém x,y.'
;*/
-- SELECT osmc.xy_to_bbox(4704288,1559788,4180000,1035500,262144);

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
  IS 'Uncertain base16h, base32 and base16'
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
  p_bbox      float[] DEFAULT array[0.,0.,0.,0.],
  p_lonlat    boolean DEFAULT false  -- false: latLon, true: lonLat
) RETURNS TABLE(ghs text, geom geometry) AS $f$
  SELECT vbit_to_baseh(p_l0code || p_code || x,p_base,0), 
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
  id            bigint NOT NULL,
  isolabel_ext  text,     -- used only in de-para, replace with 14bit in id
  prefix        text,     -- used only in de-para, cache
  bbox          float[],  -- used      in l0cover and de-para
  status        SMALLINT DEFAULT 0 CHECK (status IN (0,1,2)), -- 0: generated, 1: revised, 2: homologated
  is_overlay    boolean  DEFAULT FALSE,
  geom          geometry, -- used      in l0cover and de-para
  geom_srid4326 geometry  -- used only in l0cover
);
CREATE INDEX osm_coverage_geom_idx1         ON osmc.coverage USING gist (geom);
CREATE INDEX osm_coverage_geom4326_idx1     ON osmc.coverage USING gist (geom_srid4326);
CREATE INDEX osm_coverage_isolabel_ext_idx1 ON osmc.coverage USING btree (isolabel_ext);

COMMENT ON COLUMN osmc.coverage.id            IS 'Coverage cell identifier.';
COMMENT ON COLUMN osmc.coverage.isolabel_ext  IS 'ISO 3166-1 alpha-2 code and name (camel case); e.g. BR-SP-SaoPaulo.';
COMMENT ON COLUMN osmc.coverage.prefix        IS 'Coverage cell prefix.';
COMMENT ON COLUMN osmc.coverage.bbox          IS 'Coverage cell bbox.';
COMMENT ON COLUMN osmc.coverage.status        IS 'Coverage status. Convention: 0: generated, 1: revised, 2: homologated.';
COMMENT ON COLUMN osmc.coverage.is_overlay    IS 'True if it is an overlay cell.';
COMMENT ON COLUMN osmc.coverage.geom          IS 'Coverage cell geometry on default srid.';
COMMENT ON COLUMN osmc.coverage.geom_srid4326 IS 'Coverage cell geometry on 4326 srid.';

COMMENT ON TABLE osmc.coverage IS 'Jurisdictional coverage.';

------------------
-- encode:

CREATE or replace FUNCTION osmc.encode_16h1c(
  p_code           text,
  p_jurisd_base_id int
) RETURNS text AS $wrap$
  SELECT
    CASE
      -- tr g->F, h->F
      WHEN p_jurisd_base_id = 76 AND length(p_code) > 2
      THEN
      (
        ('{"00": "0", "01": "1", "02": "2", "03": "3", "04": "4", "05": "5", "06": "6", "07": "7",
          "08": "8", "09": "9", "0A": "A", "0B": "B", "0C": "C", "0D": "D", "0E": "E", "0F": "F",
          "10": "F", "11": "F", "12": "j", "13": "k", "14": "l", "15": "m", "16": "n", "17": "p",
          "18": "q", "19": "r", "1A": "s", "1B": "t", "1C": "v", "1D": "z"}'::jsonb)->>(substring(p_code,1,2))
      )
      -- tr g->E, h->5, j->0
      WHEN p_jurisd_base_id = 858 AND length(p_code) > 2 AND substring(p_code,1,3) IN ('100','101','102','10J','10N','10P', '12A','12B','12T', '11M','11V','11Z','11C','11D','11E','11F')
      THEN
      (
        ('{"10": "E", "11": "5", "12": "0"}'::jsonb)->>(substring(p_code,1,2))
      )
      WHEN p_jurisd_base_id = 858 AND length(p_code) > 2 AND substring(p_code,1,3) NOT IN (
      '0E0','0E1','0E2','0EN','0EJ','0EP',
      '00A','00B','00T',
      '05M','05V','05Z','05C','05D','05E','05F'

      '100','101','102','10J','10N','10P',
      '12A','12B','12T',
      '11M','11V','11Z','11C','11D','11E','11F'
      )
      THEN
      (
        ('{"00": "0", "01": "1", "02": "2", "03": "3", "04": "4", "05": "5", "06": "6", "07": "7",
          "08": "8", "09": "9", "0A": "A", "0B": "B", "0C": "C", "0D": "D", "0E": "E", "0F": "F"}'::jsonb)->>(substring(p_code,1,2))
      )
      WHEN length(p_code) = 2
      THEN
      (
        ('{"00": "0", "01": "1", "02": "2", "03": "3", "04": "4", "05": "5", "06": "6", "07": "7",
          "08": "8", "09": "9", "0A": "A", "0B": "B", "0C": "C", "0D": "D", "0E": "E", "0F": "F",
          "10": "g", "11": "h", "12": "j", "13": "k", "14": "l", "15": "m", "16": "n", "17": "p",
          "18": "q", "19": "r", "1A": "s", "1B": "t", "1C": "v", "1D": "z"}'::jsonb)->>(substring(p_code,1,2))
      )
    END || upper(substring(p_code,3))
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_16h1c(text,int)
  IS 'Encodes ghosts in BR and UY.'
;

-- -- :

CREATE or replace FUNCTION osmc.encode_point_brazil(
  p_geom  geometry(POINT)
) RETURNS text AS $wrap$
  SELECT (vbit_to_baseh((id::bit(64)<<30)::bit(5) || ggeohash.encode3(ST_X(cc),ST_Y(cc),bbox,40,false),32,0))
  FROM osmc.coverage, LATERAL (SELECT ST_Transform(p_geom,952019)) v(cc)
  WHERE ( (id::bit(64)    )::bit(10) ) = b'0001001100' -- 76, cover Brasil
    AND ( (id::bit(64)<<24)::bit(2)  ) = b'00' -- only country cover
    AND ST_Contains(geom_srid4326,p_geom)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_point_brazil(geometry(POINT))
  IS 'Encode Point for Brazil. base32, 8 digits'
;

CREATE or replace FUNCTION osmc.encode_point_colombia(
  p_geom  geometry(POINT)
) RETURNS text AS $wrap$
  SELECT (vbit_to_baseh((id::bit(64)<<30)::bit(5) || ggeohash.encode3(ST_X(cc),ST_Y(cc),bbox,40,false),32,0))
  FROM osmc.coverage, LATERAL (SELECT ST_Transform(p_geom,9377)) v(cc)
  WHERE ( (id::bit(64)    )::bit(10) ) = b'0010101010' -- 170, cover Colombia
    AND ( (id::bit(64)<<24)::bit(2)  ) = b'00' -- only country cover
    AND ST_Contains(geom_srid4326,p_geom)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_point_colombia(geometry(POINT))
  IS 'Encode Point for Colombia. base32, 8 digits'
;


CREATE or replace FUNCTION osmc.encode(
  p_geom       geometry(POINT),
  p_base       int     DEFAULT 32,
  p_bit_length int     DEFAULT 40,
  p_srid       int     DEFAULT 9377,
  p_grid_size  int     DEFAULT 32,
  p_bbox       float[] DEFAULT array[0.,0.,0.,0.],
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
                  'short_code', short_code,
                  'jurisd_local_id', jurisd_local_id,
                  'jurisd_base_id', p_jurisd_base_id,
                  'scientic_code', CASE
                                    WHEN p_base = 32 AND p_jurisd_base_id     IN (76,868) THEN osmc.encode_16h1c(vbit_to_baseh('000' ||CASE WHEN p_bit_length = 0 THEN p_l0code ELSE codebits END,16,0),p_jurisd_base_id)
                                    WHEN p_base = 32 AND p_jurisd_base_id NOT IN (76,868) THEN vbit_to_baseh('000' ||CASE WHEN p_bit_length = 0 THEN p_l0code ELSE codebits END,16,0)
                                    ELSE NULL END
                  ))
          )::jsonb) || m.subcells
        )
      )
    FROM
    (
      SELECT bit_string,
      ggeohash.draw_cell_bybox((CASE WHEN p_bit_length = 0 THEN p_bbox ELSE ggeohash.decode_box2(bit_string,p_bbox,p_lonlat) END),false,p_srid) AS geom_cell,
      CASE WHEN p_base = 16 THEN 'base16h'
           WHEN p_base = 17 THEN 'base16'
           WHEN p_base = 18 THEN 'base16h1c'
           ELSE                  'base32'
      END AS base,
      upper(vbit_to_baseh(CASE WHEN p_bit_length = 0 THEN p_l0code ELSE p_l0code||bit_string END,CASE WHEN p_base IN (16,17,18) THEN 16 ELSE 32 END,0)) AS code,
      p_l0code || bit_string AS codebits
      FROM ggeohash.encode3(ST_X(p_geom),ST_Y(p_geom),p_bbox,p_bit_length,p_lonlat) r(bit_string)
    ) c
    -- responsável por subcélulas
    LEFT JOIN LATERAL
    (
      SELECT
        CASE
        WHEN p_grid_size > 0 AND SQRT(ST_Area(c.geom_cell)) > 1
        THEN
          (
            SELECT jsonb_agg(
                ST_AsGeoJSONb(ST_Transform_resilient((CASE WHEN p_grid_size % 2 = 1 THEN ST_Centroid(geom) ELSE geom END),4326,0.005),8,0,null,
                    jsonb_strip_nulls(jsonb_build_object(
                        'code', upper(ghs2),
                        'code_subcell', substr(ghs2,length(code2)+1,length(ghs2)),
                        'prefix', code2,
                        'area', ST_Area(geom),
                        'side', SQRT(ST_Area(geom)),
                        'base', base,
                        'short_code', tt.short_code,
                        'jurisd_base_id', p_jurisd_base_id,
                        'jurisd_local_id', ss.jurisd_local_id
                        ))
                    )::jsonb)
              FROM
             (
              SELECT geom, ghs,
                  CASE WHEN p_base = 18 THEN osmc.encode_16h1c(ghs,p_jurisd_base_id) ELSE ghs END AS ghs2,
                  CASE WHEN p_base = 18 THEN osmc.encode_16h1c(code,p_jurisd_base_id) ELSE code END AS code2
                FROM osmc.ggeohash_GeomsFromVarbit(
                      c.bit_string,p_l0code,false,p_srid,CASE WHEN p_base IN (16,17,18) THEN 16 ELSE 32 END,
                      CASE
                        WHEN p_grid_size % 2 = 1 THEN p_grid_size - 1
                        ELSE p_grid_size
                      END,
                      p_bbox,
                      p_lonlat
                      )
             ) xx
              -- responsável pelo código curto na grade postal das subcélulas
              LEFT JOIN LATERAL
              (
                SELECT isolabel_ext, (isolabel_ext || '~' ||
                  CASE
                  WHEN p_base IN (16,17,18)
                  THEN vbit_to_baseh(((id::bit(64)<<27)::bit(8))>>3,16)
                  ELSE vbit_to_baseh( (id::bit(64)<<27)::bit(5)    ,32)
                  END
                || (CASE WHEN length(xx.ghs) = length(prefix32) THEN '' ELSE substr(xx.ghs,length(prefix32),length(xx.ghs)) END) ) AS short_code
                FROM osmc.coverage rr, LATERAL ( SELECT vbit_to_baseh( substring(baseh_to_vbit(prefix,16) from 4),32)) n(prefix32)
                WHERE
                    (id::bit(64))::bit(10) = p_jurisd_base_id::bit(10)
                AND ( (id::bit(64)<<24)::bit(2) ) <> 0::bit(2)
                AND CASE WHEN (id::bit(64)<<26)::bit(1) <> b'0' THEN ST_Contains(rr.geom,p_geom) ELSE TRUE  END
                -- (   ( (id::bit(64)<<32)::bit(20) #  codebits::bit(20)           ) = 0::bit(20)
                --  OR ( (id::bit(64)<<32)::bit(20) # (codebits::bit(15))::bit(20) ) = 0::bit(20)
                --  OR ( (id::bit(64)<<32)::bit(20) # (codebits::bit(10))::bit(20) ) = 0::bit(20)
                --  OR ( (id::bit(64)<<32)::bit(20) # (codebits::bit(5) )::bit(20) ) = 0::bit(20)
                -- )
                AND
                (  prefix32 = substr(xx.ghs,1,5)
                OR prefix32 = substr(xx.ghs,1,4)
                OR prefix32 = substr(xx.ghs,1,3)
                OR prefix32 = substr(xx.ghs,1,2)
                OR prefix32 = substr(xx.ghs,1,1)
                )

                ORDER BY length(prefix) DESC
              ) tt
              ON TRUE
              -- infos de jurisdiction
              LEFT JOIN LATERAL
              (
                SELECT jurisd_local_id
                FROM optim.jurisdiction
                WHERE isolabel_ext = tt.isolabel_ext
              ) ss
              ON TRUE
          )
        ELSE '[]'::jsonb
        END AS subcells
    ) m
    ON TRUE
    -- responsável pelo código curto na grade postal
    LEFT JOIN LATERAL
    (
      SELECT isolabel_ext, (isolabel_ext || '~' ||
        CASE
        WHEN p_base IN (16,17,18)
        THEN vbit_to_baseh(((id::bit(64)<<27)::bit(8))>>3,16)
        ELSE vbit_to_baseh( (id::bit(64)<<27)::bit(5)    ,32)
        END
      || (CASE WHEN length(c.code) = length(prefix32) THEN '' ELSE substr(c.code,length(prefix32)+1,length(c.code)) END) ) AS short_code
      FROM osmc.coverage r, LATERAL ( SELECT vbit_to_baseh( substring(baseh_to_vbit(prefix,16) from 4),32)) n(prefix32)
      WHERE
           (id::bit(64))::bit(10) = p_jurisd_base_id::bit(10)
      AND ( (id::bit(64)<<24)::bit(2) ) <> 0::bit(2)
      AND CASE WHEN (id::bit(64)<<26)::bit(1) <> b'0' THEN ST_Contains(r.geom,p_geom) ELSE TRUE  END
      -- (   ( (id::bit(64)<<32)::bit(20) #  codebits::bit(20)           ) = 0::bit(20)
      --  OR ( (id::bit(64)<<32)::bit(20) # (codebits::bit(15))::bit(20) ) = 0::bit(20)
      --  OR ( (id::bit(64)<<32)::bit(20) # (codebits::bit(10))::bit(20) ) = 0::bit(20)
      --  OR ( (id::bit(64)<<32)::bit(20) # (codebits::bit(5) )::bit(20) ) = 0::bit(20)
      -- )
      AND
      (   prefix32 = substr(c.code,1,5)
       OR prefix32 = substr(c.code,1,4)
       OR prefix32 = substr(c.code,1,3)
       OR prefix32 = substr(c.code,1,2)
       OR prefix32 = substr(c.code,1,1)
      )

      ORDER BY length(prefix) DESC
    ) t
    ON TRUE
    -- infos de jurisdiction
    LEFT JOIN LATERAL
    (
      SELECT jurisd_local_id
      FROM optim.jurisdiction
      WHERE isolabel_ext = t.isolabel_ext
    ) s
    ON TRUE

    WHERE
    CASE WHEN p_jurisd_base_id = 858 THEN code NOT IN (
    '0EG','10G','12G','00L','12L','0EJ','05H','11H'
    ) ELSE TRUE  END
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode(geometry(POINT),int,int,int,int,float[],varbit,int,boolean)
  IS 'Encodes geometry to OSMcode.'
;

CREATE or replace FUNCTION api.osmcode_encode(
  uri    text,
  p_base int DEFAULT 32,
  grid   int DEFAULT 0
) RETURNS jsonb AS $wrap$
  SELECT osmc.encode(
    ST_Transform(v.geom,u.srid),
    p_base,
    CASE
    WHEN latLon[4] IS NOT NULL
    THEN
    (
      SELECT
      CASE
        WHEN jurisd_base_id = 170 AND p_base = 32       AND x > 4 THEN ((x-4)/5)*5
        WHEN jurisd_base_id = 170 AND p_base = 16       AND x > 4 THEN   x-4
        WHEN jurisd_base_id = 858 AND p_base = 32       AND x > 6 THEN ((x-6)/5)*5
        WHEN jurisd_base_id = 858 AND p_base = 17       AND x > 6 THEN ((x-6)/4)*4
        WHEN jurisd_base_id = 858 AND p_base IN (16,18) AND x > 6 THEN   x-6
        WHEN jurisd_base_id = 218 AND p_base = 32       AND x > 5 THEN ((x-5)/5)*5
        WHEN jurisd_base_id = 218 AND p_base = 16       AND x > 5 THEN   x-5
        WHEN jurisd_base_id = 76  AND p_base = 32                 THEN  (x/5)*5
        WHEN jurisd_base_id = 76  AND p_base IN (16,18)           THEN   x
        ELSE 0
      END
      FROM osmc.uncertain_base16h(latLon[4]::int) t(x)
      )
    ELSE 35
    END,
    u.srid,
    grid,
    u.bbox,
    u.l0code,
    u.jurisd_base_id,
    CASE WHEN u.jurisd_base_id = 218 THEN TRUE ELSE FALSE END
  )
  FROM ( SELECT str_geouri_decode(uri) ) t(latLon),
  LATERAL ( SELECT ST_SetSRID(ST_MakePoint(latLon[2],latLon[1]),4326) ) v(geom),
  LATERAL
  (
    SELECT ((id::bit(64))::bit(10))::int AS jurisd_base_id, bbox, ST_SRID(geom) AS srid,
        CASE
        WHEN p_base IN (16,17,18) THEN (id::bit(64)<<27)::bit(8) -- 2 dígito  base16h
        ELSE                           (id::bit(64)<<30)::bit(5) -- 1 dígito  base32
        END AS l0code
    FROM osmc.coverage
    WHERE ( (id::bit(64)<<24)::bit(2) ) = 0::bit(2) -- cobertura nacional apenas
        AND ST_Contains(geom_srid4326,v.geom)
  ) u
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION api.osmcode_encode(text,int,int)
  IS 'Encodes Geo URI to OSMcode. Wrap for osmcode_encode(geometry)'
;
-- EXPLAIN ANALYZE SELECT api.osmcode_encode('geo:3.461,-76.577');
-- EXPLAIN ANALYZE SELECT api.osmcode_encode('geo:-15.5,-47.8');


CREATE or replace FUNCTION osmc.osmcode_encode_scientific(
  p_geom       geometry(POINT),
  p_base       int     DEFAULT 16,
  p_bit_length int     DEFAULT 40,
  p_srid       int     DEFAULT 9377,
  p_grid_size  int     DEFAULT 2,
  p_bbox       float[] DEFAULT array[0.,0.,0.,0.],
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
                  'base', base
                  ))
          )::jsonb) || m.subcells
        )
      )
    FROM
    (
      SELECT bit_string,
      ggeohash.draw_cell_bybox((CASE WHEN p_bit_length = 0 THEN p_bbox ELSE ggeohash.decode_box2(bit_string,p_bbox,p_lonlat) END),false,p_srid) AS geom_cell,
      CASE WHEN p_base = 17 THEN 'base16'
           WHEN p_base = 18 THEN 'base16h1c'
           ELSE                  'base16h'
      END AS base,
      upper(vbit_to_baseh(CASE WHEN p_bit_length = 0 THEN p_l0code ELSE p_l0code||bit_string END,16,0)) AS code,
      p_l0code || bit_string AS codebits
      FROM ggeohash.encode3(ST_X(p_geom),ST_Y(p_geom),p_bbox,p_bit_length,p_lonlat) r(bit_string)
    ) c
    -- responsável por subcélulas
    LEFT JOIN LATERAL
    (
      SELECT
        CASE
        WHEN p_grid_size > 0 AND SQRT(ST_Area(c.geom_cell)) > 1
        THEN
          (
            SELECT jsonb_agg(
                ST_AsGeoJSONb(ST_Transform_resilient((CASE WHEN p_grid_size % 2 = 1 THEN ST_Centroid(geom) ELSE geom END),4326,0.005),8,0,null,
                    jsonb_strip_nulls(jsonb_build_object(
                        'code', upper(ghs2),
                        'code_subcell', substr(ghs2,length(code2)+1,length(ghs2)),
                        'prefix', code2,
                        'area', ST_Area(geom),
                        'side', SQRT(ST_Area(geom)),
                        'base', base
                        ))
                    )::jsonb)
              FROM
             (
              SELECT geom, ghs,
                  CASE WHEN p_base = 18 THEN osmc.encode_16h1c(ghs,p_jurisd_base_id) ELSE ghs END AS ghs2,
                  CASE WHEN p_base = 18 THEN osmc.encode_16h1c(code,p_jurisd_base_id) ELSE code END AS code2
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
    CASE WHEN p_jurisd_base_id = 858 THEN code NOT IN (
    '0EG','10G','12G','00L','12L','0EJ','05H','11H'
    ) ELSE TRUE  END
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.osmcode_encode_scientific(geometry(POINT),int,int,int,int,float[],varbit,int,boolean)
  IS 'Encodes geometry to OSMcode.'
;

CREATE or replace FUNCTION api.osmcode_encode_scientific(
  uri    text,
  p_base int DEFAULT 16,
  grid   int DEFAULT 0,
  p_isolabel_ext text DEFAULT NULL
) RETURNS jsonb AS $wrap$
  SELECT
    -- CASE split_part(p_isolabel_ext,'-',1)
    CASE u.isolabel_ext
    WHEN 'BR' THEN

      osmc.osmcode_encode_scientific(ST_Transform(u.geom,952019),p_base,
      CASE
      WHEN latLon[4] IS NOT NULL
      THEN
      (
        SELECT
        CASE
          WHEN p_base IN (16,18)           THEN   x
          ELSE 0
        END
        FROM osmc.uncertain_base16h(latLon[4]::int) t(x)
        )
      ELSE 35
      END,
      952019,grid,u.bbox,u.l0code,76,FALSE)

    WHEN 'CO' THEN

      osmc.osmcode_encode_scientific(ST_Transform(u.geom,9377),p_base,
      CASE
      WHEN latLon[4] IS NOT NULL
      THEN
      (
        SELECT
        CASE
          WHEN p_base = 16       AND x > 4 THEN   x-4
          ELSE 0
        END
        FROM osmc.uncertain_base16h(latLon[4]::int) t(x)
        )
      ELSE 35
      END,
      9377,grid,u.bbox,u.l0code,170,FALSE)

    WHEN 'UY' THEN

      osmc.osmcode_encode_scientific(ST_Transform(u.geom,32721),p_base,
      CASE
      WHEN latLon[4] IS NOT NULL
      THEN
      (
        SELECT
        CASE
          WHEN p_base = 17       AND x > 6 THEN ((x-6)/4)*4
          WHEN p_base IN (16,18) AND x > 6 THEN   x-6
          ELSE 0
        END
        FROM osmc.uncertain_base16h(latLon[4]::int) t(x)
        )
      ELSE 35
      END,
      32721,grid,u.bbox,u.l0code,858,FALSE)

    WHEN 'EC' THEN

      osmc.osmcode_encode_scientific(ST_Transform(u.geom,32717),p_base,
      CASE
      WHEN latLon[4] IS NOT NULL
      THEN
      (
        SELECT
        CASE
          WHEN p_base = 16       AND x > 5 THEN   x-5
          ELSE 0
        END
        FROM osmc.uncertain_base16h(latLon[4]::int) t(x)
        )
      ELSE 35
      END,
      32717,grid,u.bbox,u.l0code,218,TRUE)

    END
  FROM ( SELECT str_geouri_decode(uri) ) t(latLon),
  LATERAL
  (
    SELECT ST_SetSRID(ST_MakePoint(latLon[2],latLon[1]),4326) AS geom, bbox,
        (id::bit(64)<<27)::bit(8) AS l0code, -- 2 dígito  base16h
        isolabel_ext
    FROM osmc.coverage
    WHERE
        ( (id::bit(64)<<24)::bit(2) ) = 0::bit(2) -- cobertura nacional apenas
        -- AND ( CASE WHEN p_isolabel_ext IS NOT NULL THEN isolabel_ext = p_isolabel_ext END )
        AND ST_Contains(geom_srid4326,ST_SetSRID(ST_MakePoint(latLon[2],latLon[1]),4326))
  ) u
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION api.osmcode_encode_scientific(text,int,int,text)
  IS 'Encodes Geo URI to OSMcode. Wrap for osmcode_encode_context(geometry)'
;
-- EXPLAIN ANALYZE SELECT api.osmcode_encode_scientific('geo:-15.5,-47.8');


CREATE or replace FUNCTION osmc.encode_postal(
  p_geom       geometry(POINT),
  p_bit_length int     DEFAULT 40,
  p_srid       int     DEFAULT 9377,
  p_grid_size  int     DEFAULT 32,
  p_bbox       float[] DEFAULT array[0.,0.,0.,0.],
  p_l0code     varbit  DEFAULT b'0',
  p_jurisd_base_id int DEFAULT 170,
  p_lonlat     boolean DEFAULT false,  -- false: latLon, true: lonLat
  p_isolabel_ext text  DEFAULT NULL
) RETURNS jsonb AS $f$
    SELECT jsonb_build_object(
      'type', 'FeatureCollection',
      'features',
        (
          (ST_AsGeoJSONb(ST_Transform_resilient(geom_cell,4326,0.005),8,0,null,
              jsonb_strip_nulls(jsonb_build_object(
                  'code', code,
                  'short_code', short_code,
                  'area', ST_Area(geom_cell),
                  'side', SQRT(ST_Area(geom_cell)),
                  'base', base,
                  'jurisd_local_id', jurisd_local_id,
                  'jurisd_base_id', p_jurisd_base_id,
                  'scientic_code', CASE
                                    WHEN p_jurisd_base_id     IN (76,868) THEN osmc.encode_16h1c(vbit_to_baseh('000' ||CASE WHEN p_bit_length = 0 THEN p_l0code ELSE codebits END,16,0),p_jurisd_base_id)
                                    WHEN p_jurisd_base_id NOT IN (76,868) THEN vbit_to_baseh('000' ||CASE WHEN p_bit_length = 0 THEN p_l0code ELSE codebits END,16,0)
                                    ELSE NULL END
                  ))
          )::jsonb) || m.subcells
        )
      )
    FROM
    (
      SELECT bit_string,
      ggeohash.draw_cell_bybox((CASE WHEN p_bit_length = 0 THEN p_bbox ELSE ggeohash.decode_box2(bit_string,p_bbox,p_lonlat) END),false,p_srid) AS geom_cell,
      'base32' AS base,
      upper(vbit_to_baseh(CASE WHEN p_bit_length = 0 THEN p_l0code ELSE p_l0code||bit_string END,32,0)) AS code,
      p_l0code || bit_string AS codebits
      FROM ggeohash.encode3(ST_X(p_geom),ST_Y(p_geom),p_bbox,p_bit_length,p_lonlat) r(bit_string)
    ) c
    -- responsável por subcélulas
    LEFT JOIN LATERAL
    (
      SELECT
        CASE
        WHEN p_grid_size > 0 AND SQRT(ST_Area(c.geom_cell)) > 1
        THEN
          (
            SELECT jsonb_agg(
                ST_AsGeoJSONb(ST_Transform_resilient((CASE WHEN p_grid_size % 2 = 1 THEN ST_Centroid(geom) ELSE geom END),4326,0.005),8,0,null,
                    jsonb_strip_nulls(jsonb_build_object(
                        'code', upper(ghs2),
                        'short_code', tt.short_code,
                        'code_subcell', substr(ghs2,length(code2)+1,length(ghs2)),
                        'prefix', code2,
                        'area', ST_Area(geom),
                        'side', SQRT(ST_Area(geom)),
                        'base', base,
                        'jurisd_base_id', p_jurisd_base_id,
                        'jurisd_local_id', ss.jurisd_local_id
                        ))
                    )::jsonb)
              FROM
             (
              SELECT geom, ghs, ghs AS ghs2, code AS code2
                FROM osmc.ggeohash_GeomsFromVarbit(
                      c.bit_string,p_l0code,false,p_srid,32,
                      CASE
                        WHEN p_grid_size % 2 = 1 THEN p_grid_size - 1
                        ELSE p_grid_size
                      END,
                      p_bbox,
                      p_lonlat
                      )
             ) xx
              -- responsável pelo código curto na grade postal das subcélulas
              LEFT JOIN LATERAL
              (
                SELECT isolabel_ext, (isolabel_ext || '~' || vbit_to_baseh( (id::bit(64)<<27)::bit(5)    ,32)
                || (CASE WHEN length(xx.ghs) = length(prefix32) THEN '' ELSE substr(xx.ghs,length(prefix32),length(xx.ghs)) END) ) AS short_code
                FROM osmc.coverage rr, LATERAL ( SELECT vbit_to_baseh( substring(baseh_to_vbit(prefix,16) from 4),32)) n(prefix32)
                WHERE
                    (id::bit(64))::bit(10) = p_jurisd_base_id::bit(10)
                AND rr.isolabel_ext = p_isolabel_ext
                AND ( (id::bit(64)<<24)::bit(2) ) <> 0::bit(2)
                AND CASE WHEN (id::bit(64)<<26)::bit(1) <> b'0' THEN ST_Contains(rr.geom,p_geom) ELSE TRUE  END
                AND
                (    prefix32 = substr(xx.ghs,1,5)
                  OR prefix32 = substr(xx.ghs,1,4)
                  OR prefix32 = substr(xx.ghs,1,3)
                  OR prefix32 = substr(xx.ghs,1,2)
                  OR prefix32 = substr(xx.ghs,1,1)
                )

                ORDER BY length(prefix) DESC
              ) tt
              ON TRUE
              -- infos de jurisdiction
              LEFT JOIN LATERAL
              (
                SELECT jurisd_local_id
                FROM optim.jurisdiction
                WHERE isolabel_ext = tt.isolabel_ext
              ) ss
              ON TRUE
          )
        ELSE '[]'::jsonb
        END AS subcells
    ) m
    ON TRUE
    -- responsável pelo código curto na grade postal
    LEFT JOIN LATERAL
    (
      SELECT isolabel_ext, (isolabel_ext || '~' || vbit_to_baseh( (id::bit(64)<<27)::bit(5)    ,32)
      || (CASE WHEN length(c.code) = length(prefix32) THEN '' ELSE substr(c.code,length(prefix32)+1,length(c.code)) END) ) AS short_code
      FROM osmc.coverage r, LATERAL ( SELECT vbit_to_baseh( substring(baseh_to_vbit(prefix,16) from 4),32)) n(prefix32)
      WHERE
           (id::bit(64))::bit(10) = p_jurisd_base_id::bit(10)
      AND r.isolabel_ext = p_isolabel_ext
      AND ( (id::bit(64)<<24)::bit(2) ) <> 0::bit(2)
      AND CASE WHEN (id::bit(64)<<26)::bit(1) <> b'0' THEN ST_Contains(r.geom,p_geom) ELSE TRUE  END
      AND
      (   prefix32 = substr(c.code,1,5)
       OR prefix32 = substr(c.code,1,4)
       OR prefix32 = substr(c.code,1,3)
       OR prefix32 = substr(c.code,1,2)
       OR prefix32 = substr(c.code,1,1)
      )

      ORDER BY length(prefix) DESC
    ) t
    ON TRUE
    -- infos de jurisdiction
    LEFT JOIN LATERAL
    (
      SELECT jurisd_local_id
      FROM optim.jurisdiction
      WHERE isolabel_ext = t.isolabel_ext
    ) s
    ON TRUE

    WHERE
    CASE WHEN p_jurisd_base_id = 858 THEN code NOT IN (
    '0EG','10G','12G','00L','12L','0EJ','05H','11H'
    ) ELSE TRUE  END
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal(geometry(POINT),int,int,int,float[],varbit,int,boolean,text)
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
      WHEN p_uncertainty > -1 THEN ((osmc.uncertain_base16h(p_uncertainty))/5)*5
      ELSE 35
      END,
      952019,p_grid_size,u.bbox,u.l0code,76,FALSE,p_isolabel_ext)
  FROM
  (
    SELECT bbox, (id::bit(64)<<30)::bit(5) AS l0code -- 1 dígito base32
    FROM osmc.coverage
    WHERE isolabel_ext = 'BR' AND ST_Contains(geom,p_geom)
  ) u
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
      WHEN p_uncertainty > -1
      THEN
      (
        SELECT
        CASE
          WHEN x > 4 THEN ((x-4)/5)*5
          ELSE 0
        END
        FROM osmc.uncertain_base16h(p_uncertainty) t(x)
      )
      ELSE 35
      END,
      9377,p_grid_size,u.bbox,u.l0code,170,FALSE,p_isolabel_ext)
  FROM
  (
    SELECT bbox, (id::bit(64)<<30)::bit(5) AS l0code -- 1 dígito base32
    FROM osmc.coverage
    WHERE isolabel_ext = 'CO' AND ST_Contains(geom,p_geom)
  ) u
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
      WHEN p_uncertainty > -1
      THEN
      (
        SELECT
        CASE
          WHEN x > 6 THEN ((x-6)/5)*5
          ELSE 0
        END
        FROM osmc.uncertain_base16h(p_uncertainty) t(x)
      )
      ELSE 35
      END,
      32721,p_grid_size,u.bbox,u.l0code,858,FALSE,p_isolabel_ext)
  FROM
  (
    SELECT bbox, (id::bit(64)<<30)::bit(5) AS l0code -- 1 dígito base32
    FROM osmc.coverage
    WHERE isolabel_ext = 'UY' AND ST_Contains(geom,p_geom)
  ) u
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
      WHEN p_uncertainty > -1
      THEN
      (
        SELECT
        CASE
          WHEN x > 5 THEN ((x-5)/5)*5
          ELSE 0
        END
        FROM osmc.uncertain_base16h(p_uncertainty) t(x)
      )
      ELSE 35
      END,
      32717,p_grid_size,u.bbox,u.l0code,218,TRUE,p_isolabel_ext)
  FROM
  (
    SELECT bbox, (id::bit(64)<<30)::bit(5) AS l0code -- 1 dígito base32
    FROM osmc.coverage
    WHERE isolabel_ext = 'EC' AND ST_Contains(geom,p_geom)
  ) u
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.encode_postal_ec(geometry(POINT),float,int,text)
  IS 'Encodes geometry to EC Postal OSMcode.'
;

CREATE or replace FUNCTION api.osmcode_encode_postal(
  uri    text,
  grid   int DEFAULT 0,
  p_isolabel_ext text DEFAULT NULL
) RETURNS jsonb AS $wrap$
  SELECT
    CASE split_part(p_isolabel_ext,'-',1)
    WHEN 'BR' THEN osmc.encode_postal_br(ST_Transform(ST_SetSRID(ST_MakePoint(u[2],u[1]),4326),952019),u[4],grid,p_isolabel_ext)
    WHEN 'CO' THEN osmc.encode_postal_co(ST_Transform(ST_SetSRID(ST_MakePoint(u[2],u[1]),4326),9377)  ,u[4],grid,p_isolabel_ext)
    WHEN 'UY' THEN osmc.encode_postal_uy(ST_Transform(ST_SetSRID(ST_MakePoint(u[2],u[1]),4326),32721) ,u[4],grid,p_isolabel_ext)
    WHEN 'EC' THEN osmc.encode_postal_ec(ST_Transform(ST_SetSRID(ST_MakePoint(u[2],u[1]),4326),32717) ,u[4],grid,p_isolabel_ext)
    END
  FROM (SELECT str_geouri_decode(uri) ) t(u)

$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION api.osmcode_encode_postal(text,int,text)
  IS 'Encodes Geo URI to Postal OSMcode. Wrap for osmcode_encode_postal.'
;
-- EXPLAIN ANALYZE SELECT api.osmcode_encode_postal('geo:-15.5,-47.8',0,'BR-GO-Planaltina');
-- EXPLAIN ANALYZE SELECT api.osmcode_encode('geo:-15.5,-47.8',32,0);

------------------
-- osmcode decode:

CREATE or replace FUNCTION api.osmcode_decode_scientific_absolute(
   p_code       text, -- e.g.: '645' in 16h1c
   p_iso        text, -- e.g.: 'BR'
   p_base       int  DEFAULT 16
) RETURNS jsonb AS $f$
  SELECT jsonb_build_object(
      'type', 'FeatureCollection',
      'features',
          (
            SELECT jsonb_agg(
                ST_AsGeoJSONb(ST_Transform_resilient(v.geom,4326,0.005),8,0,null,
                    jsonb_strip_nulls(jsonb_build_object(
                        'code', CASE WHEN p_base = 18 THEN c.code16h1c ELSE c.code16h END,
                        'area', ST_Area(v.geom),
                        'side', SQRT(ST_Area(v.geom)),
                        'truncated_code',truncated_code,
                        'base', CASE WHEN p_base = 17 THEN 'base16'
                                     WHEN p_base = 18 THEN 'base16h1c'
                                     ELSE                  'base16h'
                                END
                        ))
                    )::jsonb) AS gj
            FROM
            (
              SELECT DISTINCT upper(p_iso) AS upper_p_iso,

              CASE
                WHEN length(code16h) > 12 AND upper(p_iso) IN ('BR')           THEN substring(code16h,1,12)
                WHEN length(code16h) > 11 AND upper(p_iso) IN ('EC','CO','UY') THEN substring(code16h,1,11)
                ELSE code16h
              END AS code16h,
              CASE
                WHEN length(code16h) > 12 AND upper(p_iso) IN ('BR')           THEN TRUE
                WHEN length(code16h) > 11 AND upper(p_iso) IN ('EC','CO','UY') THEN TRUE
                ELSE NULL
              END AS truncated_code,
              CASE
                WHEN length(code16h) > 12 AND upper(p_iso) IN ('BR')           THEN baseh_to_vbit(substring(code16h,1,12),16)
                WHEN length(code16h) > 11 AND upper(p_iso) IN ('EC','CO','UY') THEN baseh_to_vbit(substring(code16h,1,11),16)
                ELSE baseh_to_vbit(code16h,16)
              END AS codebits,
              CASE
                WHEN length(code16h) > 11 AND upper(p_iso) IN ('BR') THEN substring(code16h1c,1,11)
                WHEN length(code16h) > 10 AND upper(p_iso) IN ('UY') THEN substring(code16h1c,1,10)
                ELSE code16h1c
              END AS code16h1c

              FROM
              (
                SELECT code AS code16h1c,
                CASE
                  WHEN p_base = 18
                  THEN
                    CASE
                      -- FL,FT,FS,FA,FB,F8,F9: tr F -> 0F
                      WHEN upper(p_iso) = 'BR' AND substring(code,1,2) IN ('FL','FT','FS','FA','FB','F8','F9') THEN ('0F')
                      -- FQ,F4,F5: tr F -> h
                      WHEN upper(p_iso) = 'BR' AND substring(code,1,2) IN ('FQ','F4','F5')                     THEN ('11')
                      -- FR,F6,F7: tr F -> g
                      WHEN upper(p_iso) = 'BR' AND substring(code,1,2) IN ('FR','F6','F7')                     THEN ('10')

                      -- E0,E1,E2: tr F -> g
                      WHEN upper(p_iso) = 'UY' AND substring(code,1,2) IN ('E0','E1','E2','EJ','EN','EP')      THEN ('10')
                      -- EE,ED,EF: tr 0 -> j
                      WHEN upper(p_iso) = 'UY' AND substring(code,1,2) IN ('0A','0B','0T')                     THEN ('12')
                      -- ,,: tr 5 -> h
                      WHEN upper(p_iso) = 'UY' AND substring(code,1,2) IN ('5M','5V','5Z','5C','5D','5E','5F') THEN ('11')
                      ELSE
                      (
                        ('{"0": "00", "1": "01", "2": "02", "3": "03", "4": "04", "5": "05", "6": "06", "7": "07",
                          "8": "08", "9": "09", "A": "0A", "B": "0B", "C": "0C", "D": "0D", "E": "0E", "F": "0F",
                          "g": "10", "h": "11", "j": "12", "k": "13", "l": "14", "m": "15", "n": "16", "p": "17",
                          "q": "18", "r": "19", "s": "1A", "t": "1B", "v": "1C", "z": "1D"}'::jsonb)->>(substring(code,1,1))
                      )
                    END || substring(code,2)
                  ELSE code
                END AS code16h
                    FROM regexp_split_to_table(upper(p_code),',') code
              ) u
            ) c,
            LATERAL
            (
              SELECT ggeohash.draw_cell_bybox(
                        ggeohash.decode_box2(
                         substring(codebits from 9) -- 8 bits base16h
                         ,bbox, CASE WHEN c.upper_p_iso='EC' THEN TRUE ELSE FALSE END)
                    ,false,ST_SRID(geom)
                    ) AS geom
              FROM osmc.coverage
              WHERE isolabel_ext = c.upper_p_iso -- cobertura nacional apenas
                AND ( ( (id::bit(64)<<27)::bit(8) # codebits::bit(8) ) = 0::bit(8) ) -- 2 dígitos base16h, prefixo conforme base
            ) v

            WHERE
            CASE WHEN upper_p_iso = 'UY' THEN c.code16h NOT IN ('0EG','10G','12G','00L','12L','0EJ','05H','11H') ELSE TRUE END
          )
      )
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION api.osmcode_decode_scientific_absolute(text,text,int)
  IS 'Decode absolute scientific OSMcode.'
;
-- EXPLAIN ANALYZE SELECT api.osmcode_decode_scientific_absolute('D1A','BR',18);

CREATE or replace FUNCTION api.osmcode_decode_scientific_absolute(
   p_code      text,
   p_base      int  DEFAULT 16,
   p_separator text DEFAULT '\+'
) RETURNS jsonb AS $f$
  SELECT api.osmcode_decode_scientific_absolute(u[2],u[1],p_base)
  FROM regexp_split_to_array(p_code,p_separator) u
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION api.osmcode_decode_scientific_absolute(text,int)
  IS 'Decode Scientific OSMcode. Wrap for osmcode_decode_scientific_absolute.'
;
-- EXPLAIN ANALYZE SELECT api.osmcode_decode_scientific_absolute('BR+D1A',18);

CREATE or replace FUNCTION api.osmcode_decode_postal_absolute(
   p_code       text, -- e.g.: '645' in 16h1c
   p_iso        text  -- e.g.: 'BR'
) RETURNS jsonb AS $f$
  SELECT jsonb_build_object(
      'type', 'FeatureCollection',
      'features',
          (
            SELECT jsonb_agg(
                ST_AsGeoJSONb(ST_Transform_resilient(v.geom,4326,0.005),8,0,null,
                    jsonb_strip_nulls(jsonb_build_object(
                        'code', code,
                        'area', ST_Area(v.geom),
                        'side', SQRT(ST_Area(v.geom)),
                        'base', 'base32',
                        'jurisd_local_id', jurisd_local_id,
                        'scientic_code', scientic_code,
                        'short_code', short_code
                        ))
                    )::jsonb) AS gj
            FROM
            (
              SELECT DISTINCT upper(p_iso) AS upper_p_iso, code, baseh_to_vbit(code,32) AS codebits
              FROM regexp_split_to_table(upper(p_code),',') code
            ) c,
            LATERAL
            (
              SELECT ggeohash.draw_cell_bybox(
                        ggeohash.decode_box2(
                          substring(codebits from 6) -- 5 bits base16h
                        ,bbox, CASE WHEN c.upper_p_iso='EC' THEN TRUE ELSE FALSE END)
                    ,false,ST_SRID(geom)
                    ) AS geom
              FROM osmc.coverage
              WHERE isolabel_ext = c.upper_p_iso -- cobertura nacional apenas
                AND ( ( (id::bit(64)<<30)::bit(5) # codebits::bit(5) ) = 0::bit(5) ) -- 1 dígito  base32, prefixo conforme base
            ) v
            -- responsável por infos do código postal
            LEFT JOIN LATERAL
            (
                SELECT  jurisd_local_id,
                        CASE
                          WHEN upper_p_iso     IN ('BR','UY') THEN osmc.encode_16h1c(vbit_to_baseh('000'||codebits,16,0),jurisd_base_id)
                          WHEN upper_p_iso NOT IN ('BR','UY') THEN vbit_to_baseh('000'||codebits,16,0)
                          ELSE NULL
                        END AS scientic_code,
                        (r.isolabel_ext || '~' || vbit_to_baseh( (id::bit(64)<<27)::bit(5)    ,32)
                || (CASE WHEN length(c.code) = length(prefix32) THEN '' ELSE substr(c.code,length(prefix32)+1,length(c.code)) END) ) AS short_code
                FROM osmc.coverage r
                LEFT JOIN optim.jurisdiction ju
                ON ju.isolabel_ext = r.isolabel_ext
                LEFT JOIN LATERAL (SELECT vbit_to_baseh( substring(baseh_to_vbit(prefix,16) from 4),32)) n(prefix32) ON TRUE
                WHERE
                -- somente coberturas do pais
                    ( (id::bit(64))::bit(10) = jurisd_base_id::bit(10) )
                AND ( (id::bit(64)<<24)::bit(2) ) <> 0::bit(2) -- cobertura municipal
                AND CASE WHEN (id::bit(64)<<26)::bit(1) <> b'0' THEN ST_Contains(r.geom,ST_Centroid(v.geom)) ELSE TRUE END
                AND
                (  prefix32 = substr(c.code,1,5)
                OR prefix32 = substr(c.code,1,4)
                OR prefix32 = substr(c.code,1,3)
                OR prefix32 = substr(c.code,1,2)
                OR prefix32 = substr(c.code,1,1)
                )
                ORDER BY length(prefix) DESC
                LIMIT 1
            ) t
            ON TRUE

            WHERE
            CASE WHEN upper_p_iso = 'UY' THEN c.code NOT IN ('0EG','10G','12G','00L','12L','0EJ','05H','11H') ELSE TRUE END
          )
      )
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION api.osmcode_decode_postal_absolute(text,text)
  IS 'Decode absolute postal OSMcode.'
;
-- EXPLAIN ANALYZE SELECT api.osmcode_decode_postal_absolute('9025NTJ','CO');

CREATE or replace FUNCTION api.osmcode_decode_postal_absolute(
   p_code text
) RETURNS jsonb AS $f$
  SELECT api.osmcode_decode_postal_absolute(u[2],u[1])
  FROM regexp_split_to_array(p_code,'~') u
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION api.osmcode_decode_postal_absolute(text)
  IS 'Decode Postal OSMcode. Wrap for osmcode_decode_postal_absolute.'
;
-- EXPLAIN ANALYZE SELECT api.osmcode_decode_postal_absolute('CO~9025NTJ');

CREATE or replace FUNCTION api.osmcode_decode_postal(
   p_code text,
   p_iso  text
) RETURNS jsonb AS $f$
  SELECT jsonb_build_object(
      'type', 'FeatureCollection',
      'features',
          (
            SELECT jsonb_agg(
                ST_AsGeoJSONb(ST_Transform_resilient(v.geom,4326,0.005),8,0,null,
                    jsonb_strip_nulls(jsonb_build_object(
                        'code', code,
                        'short_code', short_code,
                        'area', ST_Area(v.geom),
                        'side', SQRT(ST_Area(v.geom)),
                        'base', 'base32',
                        'jurisd_local_id', jurisd_local_id,
                        'truncated_code',truncated_code,
                        'scientic_code', CASE
                                          WHEN country_iso IN ('BR','UY') THEN osmc.encode_16h1c(vbit_to_baseh('000'||codebits,16,0),jurisd_base_id)
                                          ELSE                                                   vbit_to_baseh('000'||codebits,16,0)
                                         END
                        ))
                    )::jsonb) AS gj
            FROM
            (
              SELECT jurisd_local_id, jurisd_base_id, isolabel_ext, country_iso,
              CASE
                WHEN length(code) > 9 AND country_iso IN ('BR')      THEN substring(code,1,9)
                WHEN length(code) > 8 AND country_iso IN ('EC','CO') THEN substring(code,1,8)
                WHEN length(code) > 7 AND country_iso IN ('UY')      THEN substring(code,1,7)
                ELSE code
              END AS code,
              CASE
                WHEN length(code) > 9 AND country_iso IN ('BR')      THEN TRUE
                WHEN length(code) > 8 AND country_iso IN ('EC','CO') THEN TRUE
                WHEN length(code) > 7 AND country_iso IN ('UY')      THEN TRUE
                ELSE NULL
              END AS truncated_code,
              CASE
                WHEN length(code) > 9 AND country_iso IN ('BR')      THEN baseh_to_vbit(substring(code,1,9),32)
                WHEN length(code) > 8 AND country_iso IN ('EC','CO') THEN baseh_to_vbit(substring(code,1,8),32)
                WHEN length(code) > 7 AND country_iso IN ('UY')      THEN baseh_to_vbit(substring(code,1,7),32)
                ELSE baseh_to_vbit(code,32)
              END AS codebits,
              isolabel_ext || '~' ||
              CASE
                WHEN length(code) > 9 AND country_iso IN ('BR')      THEN substring(upper(p_code),1,length(p_code)-length(code)+9)
                WHEN length(code) > 8 AND country_iso IN ('EC','CO') THEN substring(upper(p_code),1,length(p_code)-length(code)+8)
                WHEN length(code) > 7 AND country_iso IN ('UY')      THEN substring(upper(p_code),1,length(p_code)-length(code)+7)
                ELSE upper(p_code)
              END AS short_code
              FROM
              (
                  SELECT jurisd_local_id, jurisd_base_id, co.isolabel_ext, split_part(co.isolabel_ext,'-',1) AS country_iso, vbit_to_baseh(substring(baseh_to_vbit(prefix,16) from 4),32) || upper(substring(p_code,2)) AS code
                  FROM osmc.coverage co
                  LEFT JOIN optim.jurisdiction ju
                  ON co.isolabel_ext = ju.isolabel_ext
                  WHERE co.isolabel_ext = (osmc.str_geocodeiso_decode(p_iso))[1] -- possível usar os 14bits do id na busca
                   AND ( (id::bit(64)<<27)::bit(5) # baseh_to_vbit(substring(upper(p_code),1,1),32) ) = 0::bit(5)
              ) u
            ) c,
            LATERAL
            (
              SELECT ggeohash.draw_cell_bybox(ggeohash.decode_box2(substring(codebits from 6),bbox, CASE WHEN country_iso = 'EC' THEN TRUE ELSE FALSE END)
                    ,false,ST_SRID(geom)) AS geom
              FROM osmc.coverage
              WHERE isolabel_ext = c.country_iso
                -- prefixo conforme base
                AND ( ( (id::bit(64)<<30)::bit(5) # codebits::bit(5) ) = 0::bit(5) ) -- 1 dígito  base32
            ) v

            WHERE
            CASE WHEN country_iso = 'UY' THEN c.code NOT IN ('0EG','10G','12G','00L','12L','0EJ','05H','11H') ELSE TRUE END
          )
      )
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION api.osmcode_decode_postal(text,text)
  IS 'Decode Postal OSMcode.'
;
-- EXPLAIN ANALYZE SELECT api.osmcode_decode_postal('8HB','CO-Itagui');
-- EXPLAIN ANALYZE SELECT api.osmcode_decode('9JBBHB','CO',32);

CREATE or replace FUNCTION api.osmcode_decode_postal(
   p_code text
) RETURNS jsonb AS $f$
  SELECT api.osmcode_decode_postal(u[2],u[1])
  FROM regexp_split_to_array(p_code,'~') u
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION api.osmcode_decode_postal(text)
  IS 'Decode Postal OSMcode. Wrap for osmcode_decode_postal.'
;
-- EXPLAIN ANALYZE SELECT api.osmcode_decode_postal('CO-Itagui~8HB');

------------------
-- jurisdiction coverage:

CREATE or replace FUNCTION api.jurisdiction_coverage(
   p_iso  text,
   p_base int     DEFAULT 32
) RETURNS jsonb AS $f$
  SELECT jsonb_build_object(
    'type', 'FeatureCollection',
    'features',
      (
        SELECT coalesce(jsonb_agg(
          ST_AsGeoJSONb(ST_Transform_resilient(geom,4326,0.005),8,0,null,
              jsonb_strip_nulls(jsonb_build_object(
                  'code',
                      CASE
                        WHEN p_base = 18
                        THEN
                        (
                          ('{"00": "0", "01": "1", "02": "2", "03": "3", "04": "4", "05": "5", "06": "6", "07": "7",
                            "08": "8", "09": "9", "0A": "A", "0B": "B", "0C": "C", "0D": "D", "0E": "E", "0F": "F",
                            "10": "g", "11": "h", "12": "j", "13": "k", "14": "l", "15": "m", "16": "n", "17": "p",
                            "18": "q", "19": "r", "1A": "s", "1B": "t", "1C": "v", "1D": "z"}'::jsonb)->>(substring(code,1,2))
                            || upper(substring(code,3))
                        )
                        ELSE code
                      END
                  ,
                  'area', s.area,
                  'side', SQRT(s.area),
                  'base', CASE WHEN p_base = 16 THEN 'base16h'
                               WHEN p_base = 17 THEN 'base16'
                               WHEN p_base = 18 THEN 'base16h1c'
                               ELSE                  'base32'
                          END,
                  'index', index
                  ))
              )::jsonb),'[]'::jsonb)
        FROM
        (
          (
            SELECT geom, bbox,
            vbit_to_baseh(
                CASE
                WHEN p_base IN (16,17,18) THEN (id::bit(64)<<27)::bit(8) -- 2 dígito  base16h
                ELSE                           (id::bit(64)<<30)::bit(5) -- 1 dígito  base32
                END,
                CASE WHEN p_base IN (16,17,18) THEN 16 ELSE 32 END) AS code,
                null AS index
              FROM osmc.coverage
              WHERE ( (id::bit(64))::bit(10) = ((('{"CO":170, "BR":76, "UY":858, "EC":218}'::jsonb)->(upper(p_iso)))::int)::bit(10) )
                  -- cobertura nacional apenas
                  AND (id::bit(64)<<24)::bit(2) = 0::bit(2)
          )
          UNION ALL
          (
            SELECT geom, bbox, prefix AS code, --**** CONVERTER
                    CASE
                    WHEN p_base IN (16,17,18)
                    THEN vbit_to_baseh(((id::bit(64)<<27)::bit(8))>>3,16)
                    ELSE vbit_to_baseh( (id::bit(64)<<27)::bit(5),32)
                    END AS index
              FROM osmc.coverage
              WHERE isolabel_ext = (osmc.str_geocodeiso_decode(p_iso))[1]
          )
        ) t
        -- area geom
        LEFT JOIN LATERAL
        (
          SELECT ST_Area(ggeohash.draw_cell_bybox(t.bbox,false,ST_SRID(t.geom))) AS area
        ) s
        ON TRUE
      )
    )
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION api.jurisdiction_coverage(text,int)
  IS 'Return l0cover.'
;
-- EXPLAIN ANALYZE SELECT api.jurisdiction_coverage('BR-SP-SaoCaetanoSul');
