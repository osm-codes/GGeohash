/**
 * (reference implementation, for asserts and PoCs, no performance)
 * PostgreSQL's Public schema, common Library (pubLib)
 * Original at http://git.AddressForAll.org/pg_pubLib-v1
 *
 * HCode is a left-to-right hierarchical code. See http://addressforall.org/_foundations/art1.pdf
 * A typical class of HCodes are the Geocode systems of regular hierarchical grids, as defined in
 *   https://en.wikipedia.org/w/index.php?title=Geocode&oldid=1052536888#Hierarchical_grids
 * Generalized Geohash is a typical example of valid HCode for this library.
 *
 * Module: HCode/EncodeDecode.
 * DependsOn: pubLib03-json
 * Prefix: hcode
 * license: CC0
 */

CREATE extension IF NOT EXISTS postgis;
CREATE SCHEMA IF NOT EXISTS ggeohash; -- Geographic application of hcodes and Morton Cruve

----------------
------ Criar publib04 vbit!  falta baseh_to_vbit


-- use publib: FUNCTION varbit_to_int( b varbit, blen int DEFAULT NULL)


-- use publib: FUNCTION vbit_to_baseh(
-- use publib: FUNCTION baseh_to_vbit(

-- -- -- -- -- --
-- Main functions

CREATE or replace FUNCTION ggeohash.encode(  -- to string, old ggeohash.encode
   x float,
   y float,
   code_size int default NULL, -- default 9 for base32 and 23 for base4
   code_digit_bits int default 5,   -- 5 for base32, 4 for base16 or 2 for base4
   code_digits_alphabet text default '0123456789BCDFGHJKLMNPQRSTUVWXYZ',
   -- see base32nvU at http://addressforall.org/_foundations/art1.pdf
   min_x float default -90.,
   min_y float default -180.,
   max_x float default 90.,
   max_y float default 180.
) RETURNS text as $f$
DECLARE
 chars      text[]  := array[]::text[];
 bits       int     := 0;
 bitsTotal  int     := 0;
 hash_value int     := 0;
 safe_loop  int     := 0;
 mid        float;
 digit      char;
BEGIN
 IF code_size IS NULL OR code_size=0 THEN
    code_size := (array[38,23,18,12,9])[code_digit_bits];
 END IF;
 WHILE safe_loop<200 AND cardinality(chars) < code_size LOOP
   IF bitsTotal % 2 = 0 THEN
     mid := (max_y + min_y) / 2.0;
     IF y > mid THEN
       hash_value := (hash_value << 1) + 1;
       min_y := mid;
     ELSE
       hash_value := (hash_value << 1) + 0;
       max_y := mid;
     END IF;
   ELSE -- \bitsTotal
     mid := (max_x + min_x) / 2.0;
     IF x > mid THEN
       hash_value := (hash_value << 1) + 1;
       min_x := mid;
     ELSE
       hash_value := (hash_value << 1) + 0;
       max_x := mid;
     END IF;
   END IF; -- \bitsTotal
   safe_loop := safe_loop + 1; -- new
   -- RAISE NOTICE '-- %. mid=% (% to % x=%) %bits.',safe_loop,mid,min_y,max_y,y,bits;
   bits := bits + 1;
   bitsTotal := bitsTotal +1;
   IF bits = code_digit_bits THEN
     digit := substr(code_digits_alphabet, hash_value+1, 1);
     chars := array_append(chars, digit);
     bits := 0;
     hash_value := 0;
   END IF;
 END LOOP; -- \chars
 RETURN  array_to_string(chars,''); -- code
END
$f$ LANGUAGE PLpgSQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.encode(float, float, integer, integer, text, float, float, float, float)
  IS 'Encondes LatLon WGS84 as Generalized Geohash. Algorithm adapted from https://github.com/ppKrauss/node-geohash/blob/master/main.js'
;

CREATE or replace FUNCTION ggeohash.encode( -- to string
   x float,
   y float,
   code_size int,
   code_digit_bits int,
   code_digits_alphabet text,
   bbox float[]
) RETURNS text as $f$
   SELECT ggeohash.encode($1, $2, $3, $4, $5, bbox[1], bbox[2], bbox[3], bbox[4])
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.encode(float, float, integer, integer, text, float[])
  IS 'Wrap for ggeohash.encode(float, float, integer, integer, text, float, float, float, float).'
;

CREATE or replace FUNCTION ggeohash.encode2( -- to JSON
   x float,
   y float,
   code_size int default NULL, -- default 9 for base32 and 23 for base4
   code_digit_bits int default 5,   -- 5 for base32, 4 for base16 or 2 for base4
   code_digits_alphabet text default '0123456789BCDFGHJKLMNPQRSTUVWXYZ',
   -- see base32nvU at http://addressforall.org/_foundations/art1.pdf
   min_x float default -90.,
   min_y float default -180.,
   max_x float default 90.,
   max_y float default 180.
) RETURNS jsonb as $f$
DECLARE
 chars      text[]  := array[]::text[];
 bits       int     := 0;
 bitsTotal  int     := 0;
 hash_value int     := 0;
 safe_loop  int     := 0;
 mid        float;
 digit      char;
BEGIN
 IF code_size IS NULL OR code_size=0 THEN
    code_size := (array[38,23,18,12,9])[code_digit_bits];
 END IF;
 WHILE safe_loop<200 AND cardinality(chars) < code_size LOOP
   IF bitsTotal % 2 = 0 THEN
     mid := (max_y + min_y) / 2.0;
     IF y > mid THEN
       hash_value := (hash_value << 1) + 1;
       min_y := mid;
     ELSE
       hash_value := (hash_value << 1) + 0;
       max_y := mid;
     END IF;
   ELSE -- \bitsTotal
     mid := (max_x + min_x) / 2.0;
     IF x > mid THEN
       hash_value := (hash_value << 1) + 1;
       min_x := mid;
     ELSE
       hash_value := (hash_value << 1) + 0;
       max_x := mid;
     END IF;
   END IF; -- \bitsTotal
   safe_loop := safe_loop + 1; -- new
   -- RAISE NOTICE '-- %. mid=% (% to % x=%) %bits.',safe_loop,mid,min_y,max_y,y,bits;
   bits := bits + 1;
   bitsTotal := bitsTotal +1;
   IF bits = code_digit_bits THEN
     digit := substr(code_digits_alphabet, hash_value+1, 1);
     chars := array_append(chars, digit);
     bits := 0;
     hash_value := 0;
   END IF;
 END LOOP; -- \chars
 RETURN  jsonb_build_object('code',array_to_string(chars,''), 'box',array[min_x, min_y, max_x, max_y]);
END
$f$ LANGUAGE PLpgSQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.encode2(float, float, integer, integer, text, float, float, float, float)
  IS 'Encondes LatLon WGS84 as Generalized Geohash. Algorithm adapted from https://github.com/ppKrauss/node-geohash/blob/master/main.js'
;

CREATE or replace FUNCTION ggeohash.encode2(  -- to JSON
   x float,
   y float,
   code_size int,
   code_digit_bits int,
   code_digits_alphabet text,
   bbox float[]
) RETURNS jsonb as $f$
   SELECT ggeohash.encode2($1, $2, $3, $4, $5, bbox[1], bbox[2], bbox[3], bbox[4])
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.encode2(float, float, integer, integer, text, float[])
  IS 'Wrap for ggeohash.encode2(float, float, integer, integer, text, float, float, float, float).'
;

CREATE or replace FUNCTION ggeohash.encode3(
   x float,
   y float,
   min_x float default -90.,
   min_y float default -180.,
   max_x float default 90.,
   max_y float default 180.,
   bit_length int default 40,
   lonlat boolean default false -- false: latLon, true: lonLat
) RETURNS varbit as $f$
DECLARE
 bit_string varbit := b'';
 i   int := 0;
 j   int := 0;
 mid float;
BEGIN
    IF lonlat THEN
      j := 1;
    END IF;
 FOR i in 0..(bit_length-1) LOOP
   IF i % 2 = j THEN
     mid := (max_y + min_y)::float / 2.0;
     IF y > mid THEN
       bit_string := bit_string || B'1';
       min_y := mid;
     ELSE
       bit_string := bit_string || B'0';
       max_y := mid;
     END IF;
   ELSE
     mid := (max_x + min_x)::float / 2.0;
     IF x > mid THEN
       bit_string := bit_string || B'1';
       min_x := mid;
     ELSE
       bit_string := bit_string || B'0';
       max_x := mid;
     END IF;
   END IF;
 END LOOP;
 RETURN bit_string;
END
$f$ LANGUAGE PLpgSQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.encode3(float, float, float, float, float, float, integer, boolean)
  IS 'Encondes LatLon WGS84 as Generalized Geohash.'
;
-- SELECT ggeohash.encode3(4642144.0,1759788.0,4442144,1559788,4704288,1821932,10);

CREATE or replace FUNCTION ggeohash.encode3(
   x float,
   y float,
   bbox float[],
   bit_length int,
   lonlat boolean default false -- false: latLon, true: lonLat
) RETURNS varbit as $f$
   SELECT ggeohash.encode3(x,y,bbox[1],bbox[2],bbox[3],bbox[4],bit_length,lonlat)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.encode3(float, float, float[], integer, boolean)
  IS 'Wrap for ggeohash.encode3(float, float, float, float, float, float, integer, boolean).'
;
-- -- --

CREATE or replace FUNCTION ggeohash.decode_box( -- to box array
   code text,
   code_digit_bits int default 5,  -- 5 for base32, 4 for base16 or 2 for base4
   -- SELECT jsonb_object_agg(x,i-1) from regexp_split_to_table(code_digits_alphabet,'') WITH ORDINALITY AS t(x,i);
   code_digits_lookup jsonb  default '{"0":0, "1":1, "2":2, "3":3, "4":4, "5":5, "6":6, "7":7, "8":8, "9":9, "b":10, "c":11, "d":12, "e":13, "f":14, "g":15, "h":16, "j":17, "k":18, "m":19, "n":20, "p":21, "q":22, "r":23, "s":24, "t":25, "u":26, "v":27, "w":28, "x":29, "y":30, "z":31}'::jsonb,
   min_x float default -90.,
   min_y float default -180.,
   max_x float default 90.,
   max_y float default 180.
) RETURNS float[] as $f$
DECLARE
  isX  boolean := true;
  hashValue int := 0;
  mid    float;
  bits   int;
  bit    int;
  i int;
  digit text;
BEGIN
   code = lower(code);
   FOR i IN 1..length(code) LOOP
      digit = substr(code,i,1);
      hashValue := (code_digits_lookup->digit)::int;
      FOR bits IN REVERSE
      (
        (
        CASE -- gambiarra para decode de base16h, ver ggeohash.decode_box2 com input varbit
          WHEN code_digit_bits = 4 AND digit IN ('g','h')                         THEN 1
          WHEN code_digit_bits = 4 AND digit IN ('j','k','l','m')                 THEN 2
          WHEN code_digit_bits = 4 AND digit IN ('n','p','q','r','s','t','v','z') THEN 3
          ELSE code_digit_bits
          END
        )-1
      )..0 LOOP
        bit = (hashValue >> bits) & 1; -- can be boolean
        IF isX THEN
          mid = (max_y + min_y) / 2;
          IF bit = 1 THEN
            min_y := mid;
          ELSE
            max_y := mid;
          END IF; -- \bit
        ELSE
          mid = (max_x + min_x) / 2;
          IF bit =1 THEN
            min_x = mid;
          ELSE
            max_x = mid;
          END IF; --\bit
        END IF; -- \isX
        isX := NOT(isX);
      END LOOP; -- \bits
   END LOOP; -- \i
   RETURN array[min_x, min_y, max_x, max_y];
END
$f$ LANGUAGE PLpgSQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.decode_box(text, integer, jsonb, float, float, float, float)
  IS 'Decodes string of a Generalized Geohash into a bounding Box that matches it. Returns a four-element array: [minlat, minlon, maxlat, maxlon]. Algorithm adapted from https://github.com/ppKrauss/node-geohash/blob/master/main.js'
;

CREATE or replace FUNCTION ggeohash.decode_box(
   code text,                 -- 1
   code_digit_bits int,       -- 2
   code_digits_lookup jsonb,  -- 3
   bbox float[]
) RETURNS float[] as $wrap$
  SELECT ggeohash.decode_box($1, $2, $3, bbox[1], bbox[2], bbox[3], bbox[4])
$wrap$ LANGUAGE sql IMMUTABLE;
COMMENT ON FUNCTION ggeohash.decode_box(text, integer, jsonb, float[])
  IS 'Wrap for ggeohash.decode_box(text, integer, jsonb, float, float, float, float).'
;

-- pode substituir ggeohash.decode_box
CREATE or replace FUNCTION ggeohash.decode_box2(
   code  varbit,
   min_x  float   default -90.,
   min_y  float   default -180.,
   max_x  float   default 90.,
   max_y  float   default 180.,
   lonlat boolean default false -- false: latLon, true: lonLat
) RETURNS float[] as $f$
DECLARE
  mid float;
  bit int;
  i   int;
  j   int := 0;
BEGIN
    IF lonlat THEN
      j := 1;
    END IF;
   FOR i IN 0..(bit_length(code)-1) LOOP
      bit = get_bit(code,i);
      IF i % 2 = j THEN
        mid = (max_y + min_y)::float / 2.0;
        IF bit = 1 THEN
          min_y := mid;
        ELSE
          max_y := mid;
        END IF;
      ELSE
        mid = (max_x + min_x)::float / 2.0;
        IF bit =1 THEN
          min_x = mid;
        ELSE
          max_x = mid;
        END IF;
      END IF;
   END LOOP;
   RETURN array[min_x, min_y, max_x, max_y];
END
$f$ LANGUAGE PLpgSQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.decode_box2(varbit, float, float, float, float, boolean)
  IS 'Decodes string of a Generalized Geohash into a bounding Box that matches it. Returns a four-element array: [minlat, minlon, maxlat, maxlon]. Algorithm adapted from https://github.com/ppKrauss/node-geohash/blob/master/main.js'
;

CREATE or replace FUNCTION ggeohash.decode_box2(
   code varbit,
   bbox float[],
   lonlat boolean default false
) RETURNS float[] as $wrap$
  SELECT ggeohash.decode_box2($1, bbox[1], bbox[2], bbox[3], bbox[4],lonlat)
$wrap$ LANGUAGE sql IMMUTABLE;
COMMENT ON FUNCTION ggeohash.decode_box2(varbit, float[],boolean)
  IS 'Wrap for ggeohash.decode_box2(varbit, float, float, float, float, boolean).'
;

------------------------------
------------------------------
------------------------------
--- Classic Geohash functions:

CREATE or replace FUNCTION ggeohash.classic_encode(
 latitude float,
 longitude float,
 code_size int default NULL
) RETURNS text as $f$
 SELECT ggeohash.encode(latitude,longitude,code_size,5,'0123456789bcdefghjkmnpqrstuvwxyz')
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.classic_encode(float,float,int)
  IS 'Encondes LatLon as classic Geohash of Niemeyer 2008.'
;

CREATE or replace FUNCTION ggeohash.classic_decode(
   code text,
   witherror boolean default false
) RETURNS float[] as $f$
  SELECT CASE WHEN witherror THEN latlon || array[bbox[3] - latlon[1], bbox[4] - latlon[2]] ELSE latlon END
  FROM (
    SELECT array[(bbox[1] + bbox[3]) / 2, (bbox[2] + bbox[4]) / 2] as latlon, bbox
    FROM (SELECT ggeohash.decode_box(code)) t1(bbox)
  ) t2
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.classic_decode(text,boolean)
  IS 'Decodes classic Geohash (of Niemeyer 2008) to latitude and longitude, optionally adding error to the array.'
;

-- -- -- -- -- -- -- -- -- --
-- Wrap and helper functions:

-- use pgLIB: FUNCTION str_geouri_decode(uri text) RETURNS float[] as $f$

CREATE or replace FUNCTION ggeohash.classic_encode(
  latLon text -- geoURI
) RETURNS text as $wrap$
  SELECT ggeohash.classic_encode(x[1],x[2],8)
  FROM (SELECT str_geouri_decode(LatLon)) t(x)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.classic_encode(text)
  IS 'Wrap for ggeohash.classic_encode() with text GeoURI input.'
;

CREATE or replace FUNCTION ggeohash.classic_encode(
  latLon float[],
  code_size int default NULL
) RETURNS text as $wrap$
  SELECT ggeohash.classic_encode(latLon[1],latLon[2],code_size)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.classic_encode(float[],int)
  IS 'Wrap for ggeohash.classic_encode() with array input.'
;

-------------------------------------
----- Using UV normalized coordinates

-- pending XY_to_UV(x,y,bbox) and UV_to_XY(u,v,bbox)

CREATE or replace FUNCTION ggeohash.uv_encode(
   u float,  -- 0.0 to 1.0, normalized X.
   v float,  -- 0.0 to 1.0, normalized Y.
   code_size int default NULL,
   code_digit_bits int default 5,   -- 5 for base32, 4 for base16 or 2 for base4
   code_digits_alphabet text default '0123456789BCDFGHJKLMNPQRSTUVWXYZ'
	-- see base32nvU at http://addressforall.org/_foundations/art1.pdf
) RETURNS text as $wrap$
   SELECT ggeohash.encode($1, $2, $3, $4, $5, 0.0, 0.0, 1.0, 1.0)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.uv_encode
  IS 'Wrap for ggeohash.encode() with normalized UV coordinates.'
;

CREATE or replace FUNCTION ggeohash.uv_decode_box(
   code text,
   code_digit_bits int default 5,  -- 5 for base32, 4 for base16 or 2 for base4
   code_digits_lookup jsonb  default '{"0":0, "1":1, "2":2, "3":3, "4":4, "5":5, "6":6, "7":7, "8":8, "9":9, "b":10, "c":11, "d":12, "f":13, "g":14, "h":15, "j":16, "k":17, "l":18, "m":19, "n":20, "p":21, "q":22, "r":23, "s":24, "t":25, "u":26, "v":27, "w":28, "x":29, "y":30, "z":31}'::jsonb
) RETURNS float[] as $wrap$
   SELECT ggeohash.decode_box($1, $2, $3, 0.0, 0.0, 1.0, 1.0)
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.uv_decode_box
  IS 'Wrap for ggeohash.decode_box(), returning normalized UV coordinates.'
;

-----

CREATE or replace FUNCTION ggeohash.draw_cell_bycenter(
  cx int,  -- Center X
  cy int,  -- Center Y
  r int,   -- halfside ou raio do circulo inscrito
  p_translate boolean DEFAULT false, -- true para converter em LatLong (WGS84 sem projeção)
  p_srid int DEFAULT 4326          -- WGS84
) RETURNS geometry AS $f$
SELECT CASE WHEN p_translate THEN ST_Transform(geom,4326) ELSE geom END
FROM (
  SELECT ST_GeomFromText( format(
    'POLYGON((%s %s,%s %s,%s %s,%s %s,%s %s))',
    cx-r,cy-r, cx-r,cy+r, cx+r,cy+r, cx+r,cy-r, cx-r,cy-r
  ), p_srid) AS geom
) t
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.draw_cell_bycenter(int,int,int,boolean,int)
  IS 'Draws a square-cell centered on the requested point, with requested radius (half side) and optional translation and SRID.'
;

CREATE or replace FUNCTION ggeohash.draw_cell_bybox(
  b float[],  -- bbox [min_x, min_y, max_x, max_y]
  p_translate boolean DEFAULT false, -- true para converter em LatLong (WGS84 sem projeção)
  p_srid int DEFAULT 4326            -- WGS84
) RETURNS geometry AS $f$
SELECT CASE WHEN p_translate THEN ST_Transform(geom,4326) ELSE geom END
FROM (
  SELECT ST_GeomFromText( format(
    'POLYGON((%s %s,%s %s,%s %s,%s %s,%s %s))',
    b[1],b[2], b[1],b[4], b[3],b[4], b[3],b[2], b[1],b[2]
    -- min_x,min_y, min_x,max_y, max_x,max_y, max_x,min_y, min_x,min_y
  ), p_srid) AS geom
) t
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION ggeohash.draw_cell_bybox(float[],boolean,int)
  IS 'Draws a square-cell from BBOX.'
;
