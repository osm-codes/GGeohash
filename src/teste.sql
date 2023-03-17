
DROP SCHEMA IF EXISTS newstd CASCADE;
CREATE SCHEMA newstd;

/**
 * Converts bit string to text, using base2h, base4h, base8h, base16h or base32.
 * Uses letters "G" and "H" to sym44bolize non strandard bit strings (0 for44 bases44)
 * Uses extended alphabet (with no letter I,O,U W or X) for base8h and base16h.
 * @see http://osm.codes/_foundations/art1.pdf
 * @version 1.0.1.
 */
CREATE or replace FUNCTION newstd.vbit_to_baseh(
  p_val varbit,  -- input
  p_base int DEFAULT 4, -- selecting base2h, base4h, base8h, base16h or base32
  p_size int DEFAULT 0
) RETURNS text AS $f$
DECLARE
    vlen int;
    pos0 int;
    ret text := '';
    blk varbit;
    blk_n int;
    bits_per_digit int;
    tr int[] := '{ {1,2,0,0,0}, {1,3,4,0,0}, {1,3,5,6,0}, {0,0,0,0,7} }'::int[]; --4h(bits,pos), 8h(bits,pos)
    tr_selected JSONb;
    trtypes JSONb := '{"2":[1,1], "4":[1,2], "8":[2,3], "16":[3,4], "32":[4,5]}'::JSONb; -- TrPos,bits
    trpos int;
    baseh "char"[] := array[
      '[0:31]={G,Q,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x}'::"char"[], --1. 4h,8h,16h 1bit
      '[0:31]={0,1,2,3,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x}'::"char"[], --2. 4h        2bit
      '[0:31]={H,M,R,V,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x}'::"char"[], --3. 8h,16h    2bit
      '[0:31]={0,1,2,3,4,5,6,7,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x}'::"char"[], --4. 8h        3bit
      '[0:31]={J,K,N,P,S,T,Z,Y,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x}'::"char"[], --5. 16h       3bit
      '[0:31]={0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x}'::"char"[], --6. 16h       4bit
      '[0:31]={0,1,2,3,4,5,6,7,8,9,B,C,D,F,G,H,J,K,L,M,N,P,Q,R,S,T,U,V,W,X,Y,Z}'::"char"[]  --7. 32        5bit
    ]; -- jumpping I,O and U,W,X letters!
       -- the standard alphabet is https://tools.ietf.org/html/rfc4648#section-6
BEGIN
  vlen := bit_length(p_val);
  tr_selected := trtypes->(p_base::text);
  IF p_val IS NULL OR tr_selected IS NULL OR vlen=0 THEN
    RETURN NULL; -- or  p_retnull;
  END IF;
  IF p_base=2 THEN
    RETURN $1::text; --- direct bit string as string
  END IF;
  bits_per_digit := (tr_selected->>1)::int;
  blk_n := vlen/bits_per_digit;  -- poderia controlar p_size por aqui
  pos0  := (tr_selected->>0)::int;
  trpos := tr[pos0][bits_per_digit];
  FOR counter IN 1..blk_n LOOP
      blk := substring(p_val FROM 1 FOR bits_per_digit);
      ret := ret || baseh[trpos][ varbit_to_int(blk,bits_per_digit) ];
      p_val := substring(p_val FROM bits_per_digit+1); -- same as p_val<<(bits_per_digit*blk_n)
  END LOOP;
  vlen := bit_length(p_val);
  IF p_val!=b'' THEN -- vlen % bits_per_digit>0
    trpos := tr[pos0][vlen];
    ret := ret || baseh[trpos][ varbit_to_int(p_val,vlen) ];
  END IF;
  IF p_size>0 THEN
    ret := substr(ret,1,p_size);
  END IF;
  RETURN ret;
END
$f$ LANGUAGE plpgsql IMMUTABLE;
COMMENT ON FUNCTION newstd.vbit_to_baseh(varbit,int,int)
 IS 'Encodes varbit (string of bits) into Base4h, Base8h, Base16h or Base32. See http://osm.codes/_foundations/art1.pdf'
;

CREATE or replace FUNCTION newstd.baseh_to_vbit(
  p_val text,  -- input
  p_base int DEFAULT 4 -- selecting base2h, base4h, base8h, base16h or base32.
) RETURNS varbit AS $f$
DECLARE
  tr_hdig jsonb := '{
    "G":[1,0],"Q":[1,1],
    "H":[2,0],"M":[2,1],"R":[2,2],"V":[2,3],
    "J":[3,0],"K":[3,1],"N":[3,2],"P":[3,3],
    "S":[3,4],"T":[3,5],"Z":[3,6],"Y":[3,7]
  }'::jsonb;
  tr_full jsonb := '{
    "0":0,"1":1,"2":2,"3":3,"4":4,"5":5,"6":6,"7":7,"8":8,
    "9":9,"a":10,"b":11,"c":12,"d":13,"e":14,"f":15
  }'::jsonb;
  tr_full32 jsonb := '{
    "0":0,"1":1,"2":2,"3":3,"4":4,"5":5,"6":6,"7":7,"8":8,
    "9":9,"B":10,"C":11,"D":12,"F":13,"G":14,"H":15,"J":16,
    "K":17,"L":18,"M":19,"N":20,"P":21,"Q":22,"R":23,"S":24,
    "T":25,"U":26,"V":27,"W":28,"X":29,"Y":30,"Z":31
    }'::jsonb;
  blk text[];
  bits varbit;
  n int;
  i char;
  ret varbit;
  BEGIN
  ret = '';
  blk := regexp_match(p_val,'^([0-9a-f]*)([GHJKMNPQRSTVYZ])?$','i');
  IF blk[1] >'' AND p_base <> 32 THEN
    FOREACH i IN ARRAY regexp_split_to_array(lower(blk[1]),'') LOOP
      ret := ret || CASE p_base
        WHEN 16 THEN (tr_full->>i)::int::bit(4)::varbit
        WHEN 8 THEN (tr_full->>i)::int::bit(3)::varbit
        WHEN 4 THEN (tr_full->>i)::int::bit(2)::varbit
        END;
    END LOOP;
  END IF;
  IF blk[2] >'' AND p_base <> 32 THEN
    n = (tr_hdig->blk[2]->>0)::int;
    ret := ret || CASE n
      WHEN 1 THEN (tr_hdig->blk[2]->>1)::int::bit(1)::varbit
      WHEN 2 THEN (tr_hdig->blk[2]->>1)::int::bit(2)::varbit
      WHEN 3 THEN (tr_hdig->blk[2]->>1)::int::bit(3)::varbit
      END;
  END IF;
  blk := regexp_match(p_val,'^([0123456789BCDFGHJKLMNPQRSTUVWXYZ]*)$');
  IF blk[1] >'' AND p_base = 32 THEN
    FOREACH i IN ARRAY regexp_split_to_array(blk[1],'') LOOP
      ret := ret || (tr_full32->>i)::int::bit(5)::varbit;
    END LOOP;
  END IF;

  RETURN ret;
  END
$f$ LANGUAGE PLpgSQL IMMUTABLE;
COMMENT ON FUNCTION newstd.baseh_to_vbit(text,int)
 IS 'Dencodes Base4h, Base8h, Base16h or Base32 into varbit (string of bits). See http://osm.codes/_foundations/art1.pdf'
;
-- select baseh_to_vbit('F3V',16);


CREATE or replace FUNCTION newstd.vbit_to_hbig(
    p_val  varbit
) RETURNS bigint as $f$  -- hb_encode
  SELECT overlay( b'0'::bit(64) PLACING (b'1' || p_val) FROM 64-length(p_val) )::bigint
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION newstd.vbit_to_hbig(varbit)
  IS 'Converts varbit into a Bigint representing hidden-bit (hb).'
;

CREATE or replace FUNCTION newstd.vbit_to_hbig(
    p_val  varbit,
    p_ison int -- E. g. 76
) RETURNS bigint as $f$  -- hb_encode
  SELECT newstd.vbit_to_hbig(p_ison::bit(10) || p_val)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION newstd.vbit_to_hbig(varbit,int)
  IS 'Converts (varbit,ison) into a Bigint representing hidden-bit (hb).'
;
-- SELECT newstd.vbit_to_hbig('1010',76);
-- SELECT x, x::bit(64) FROM newstd.vbit_to_hbig('1010',76) t(x);

CREATE or replace FUNCTION newstd.hbig_to_vbit(
    p_val bigint
) RETURNS varbit AS $f$  -- hb_decode
  SELECT substring( x FROM 11 + position(b'1' IN x) )
  FROM (SELECT p_val::bit(64)) t(x)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION newstd.hbig_to_vbit
  IS 'Converts hiherarchical Bigint (hbig), representing hidden-bit, into varbit.'
;
-- SELECT newstd.hbig_to_vbit(17610);

CREATE or replace FUNCTION newstd.hbig_to_arrvbit(
    p_val bigint
) RETURNS varbit[] AS $f$  -- hb_decode
  SELECT ARRAY[(x<<position(b'1' IN x))::bit(10),substring( x FROM 11 + position(b'1' IN x) )]
  FROM (SELECT p_val::bit(64)) t(x)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION newstd.hbig_to_vbit
  IS 'Converts hiherarchical Bigint (hbig), representing hidden-bit, into [ison,varbit].'
;
-- SELECT newstd.hbig_to_arrvbit(17610);

CREATE or replace FUNCTION newstd.baseh_to_bigint(
  p_val text,           -- input
  p_base int DEFAULT 4, -- selecting base2h, base4h, base8h, base16h or base32.
  p_ison int DEFAULT 76 -- E. g. 76
) RETURNS bigint AS $f$
  SELECT newstd.vbit_to_hbig(newstd.baseh_to_vbit(p_val,p_base),p_ison);
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION newstd.baseh_to_bigint
  IS 'Converts baseh into hiherarchical Bigint (hbig).'
;
-- SELECT newstd.baseh_to_bigint('471E26F54N',16,76)::bit(64);

CREATE or replace FUNCTION newstd.bigint_to_baseh(
  p_val bigint,           -- input
  p_base int DEFAULT 4, -- selecting base2h, base4h, base8h, base16h or base32.
  p_ison int DEFAULT 76 -- E. g. 76
) RETURNS text AS $f$
  SELECT newstd.vbit_to_baseh(newstd.hbig_to_vbit(p_val),p_base) ;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION newstd.bigint_to_baseh
  IS 'Converts hiherarchical Bigint (hbig) into baseh.'
;
-- SELECT newstd.bigint_to_baseh(604884119550626,16);


-- -- CONVERT:

CREATE FUNCTION newstd.generatep_hb_series(bit_len int) RETURNS setof bigint as $f$
  SELECT i::bigint | maxval as x
  FROM (SELECT (2^bit_len)::bigint) t(maxval),
       LATERAL generate_series(0,maxval-1) s(i)
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION newstd.generatep_hb_series
  IS 'Obtain a sequency of hidden-bit (fixed length), from zero to 2^bit_len-1.'
;

CREATE FUNCTION newstd.generate_hb_series(bit_len int) RETURNS setof bigint as $f$
-- See optimized at https://stackoverflow.com/q/75503880/287948
DECLARE
  s text;
BEGIN
  s := 'SELECT * FROM newstd.generatep_hb_series(1)';
  FOR i IN 2..bit_len LOOP
    s := s || ' UNION ALL  SELECT * FROM newstd.generatep_hb_series('|| i::text ||')';
  END LOOP;
  RETURN QUERY EXECUTE s;
END;
$f$ LANGUAGE PLpgSQL IMMUTABLE;
COMMENT ON FUNCTION newstd.generate_hb_series
  IS 'Obtain a sequency of generatep_hb_series, from 0 to 2^bit_len-1.'
;

CREATE FUNCTION newstd.generate_vbit_series(bit_len int) RETURNS setof varbit as $f$
  SELECT newstd.hbig_to_vbit(hb)
  FROM newstd.generate_hb_series($1) t(hb) ORDER BY 1
$f$ LANGUAGE SQL IMMUTABLE;
