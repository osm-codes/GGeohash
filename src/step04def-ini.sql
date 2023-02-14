------------------
-- Table coverage:

-- L0cover COLOMBIA
--DELETE FROM osmc.coverage  WHERE (id::bit(64)<<24)::bit(2) = 0::bit(2) AND (id::bit(64))::bit(10) = 170::bit(10);
INSERT INTO osmc.coverage(id,isolabel_ext,bbox,status,is_overlay,geom,geom_srid4326)
SELECT (jurisd_base_id::bit(10) || 0::bit(14) || '00' ||
        (CASE WHEN ST_ContainsProperly(geom_country,geom_cell) IS FALSE THEN '1' ELSE '0' END) ||
        rpad((baseh_to_vbit(prefix,16))::text, 34, '0000000000000000000000000000000000'))::bit(64)::bigint,
        'CO',bbox,1::SMALLINT,false,geom,ST_Transform(geom,4326)
FROM
(
  SELECT 170 AS jurisd_base_id,prefix,bbox,geom_country,
    ST_Intersection(ggeohash.draw_cell_bybox(bbox,false,9377),geom_country) AS geom,
    ggeohash.draw_cell_bybox(bbox,false,9377) AS geom_cell
  FROM unnest
      (
      '{00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,1B,1C,1D,1E,1F}'::text[],
      --'{0,1,2,3,4,5,6,7,8,9,B,C,D,F,G,H,J,K,L,M,N,P,Q,R,S,T,U,V,W,X,Y,Z}'::text[],
      array[0,45,37,38,39,31,32,33,25,26,27,28,29,18,19,20,21,22,23,12,13,14,15,16,17,8,9,10,3,4]
      ) t(prefix,quadrant),
      LATERAL (SELECT osmc.ij_to_bbox(quadrant%6,quadrant/6,4180000.0,1035500.0,262144.0)) u(bbox),
      LATERAL (SELECT ST_Transform(geom,9377) FROM optim.vw01full_jurisdiction_geom g WHERE g.isolabel_ext = 'CO' AND jurisd_base_id = 170) r(geom_country)
  WHERE quadrant IS NOT NULL AND quadrant > 0
) y
ORDER BY 1
;

-- L0cover BRASIL
--DELETE FROM osmc.coverage  WHERE (id::bit(64)<<24)::bit(2) = 0::bit(2) AND (id::bit(64))::bit(10) = 76::bit(10);
INSERT INTO osmc.coverage(id,isolabel_ext,bbox,status,is_overlay,geom,geom_srid4326)
SELECT (jurisd_base_id::bit(10) || 0::bit(14) || '00' ||
        (CASE WHEN ST_ContainsProperly(geom_country,geom_cell) IS FALSE THEN '1' ELSE '0' END) ||
        rpad((baseh_to_vbit(prefix,16))::text, 34, '0000000000000000000000000000000000'))::bit(64)::bigint,
        'BR',bbox,1::SMALLINT,false,geom,ST_Transform(geom,4326)
FROM
(
    SELECT 76 AS jurisd_base_id, prefix, bbox,geom_country,
      ST_Intersection(ggeohash.draw_cell_bybox(bbox,false,952019),geom_country) AS geom,
      ggeohash.draw_cell_bybox(bbox,false,952019) AS geom_cell
    FROM unnest
        (
        '{00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,1B,1C,1D,1E,1F}'::text[],
        array[20,21,22,23,15,16,17,18,19,11,12,13,6,7,8,2,24,14]
        ) t(prefix,quadrant),
        LATERAL (SELECT osmc.ij_to_bbox(quadrant%5,quadrant/5,2715000.0,6727000.0,1048576.0)) u(bbox),
        LATERAL (SELECT ST_Transform(geom,952019) FROM optim.vw01full_jurisdiction_geom g WHERE g.isolabel_ext = 'BR' AND jurisd_base_id = 76) r(geom_country)
    WHERE quadrant IS NOT NULL
) y
ORDER BY 1
;

-- L0cover URUGUAI
--DELETE FROM osmc.coverage  WHERE (id::bit(64)<<24)::bit(2) = 0::bit(2) AND (id::bit(64))::bit(10) = 858::bit(10);
INSERT INTO osmc.coverage(id,isolabel_ext,bbox,status,is_overlay,geom,geom_srid4326)
SELECT (jurisd_base_id::bit(10) || 0::bit(14) || '00' ||
        (CASE WHEN ST_ContainsProperly(geom_country,geom_cell) IS FALSE THEN '1' ELSE '0' END) ||
        rpad((baseh_to_vbit(prefix,16))::text, 34, '0000000000000000000000000000000000'))::bit(64)::bigint,
        'UY',bbox,1::SMALLINT,false,geom,ST_Transform(geom,4326)
FROM
(
  SELECT 858 AS jurisd_base_id,prefix,bbox,status,is_overlay,geom_country,
    ST_Intersection(ggeohash.draw_cell_bybox(bbox,false,32721),geom_country) AS geom,
    ggeohash.draw_cell_bybox(bbox,false,32721) AS geom_cell
  FROM unnest
      (
        '{00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,1B,1C,1D,1E,1F}'::text[],
        array[40,41,30,31,32,33,20,21,22,23,10,11,12,13,1,2,42,0,3]
      ) t(prefix,quadrant),
      LATERAL (SELECT osmc.ij_to_bbox(quadrant%10,quadrant/10,353000.0,6028000.0,131072.0)) u(bbox),
      LATERAL (SELECT ST_Transform(geom,32721) FROM optim.vw01full_jurisdiction_geom g WHERE g.isolabel_ext = 'UY' AND jurisd_base_id = 858) r(geom_country)
  WHERE quadrant IS NOT NULL
) z
;

-- L0cover ECUADOR
--DELETE FROM osmc.coverage  WHERE (id::bit(64)<<24)::bit(2) = 0::bit(2) AND (id::bit(64))::bit(10) = 218::bit(10);
INSERT INTO osmc.coverage(id,isolabel_ext,bbox,status,is_overlay,geom,geom_srid4326)
SELECT (jurisd_base_id::bit(10) || 0::bit(14) || '00' ||
        (CASE WHEN ST_ContainsProperly(geom_country,geom_cell) IS FALSE THEN '1' ELSE '0' END) ||
        rpad((baseh_to_vbit(prefix,16))::text, 34, '0000000000000000000000000000000000'))::bit(64)::bigint,
        'EC',bbox,1::SMALLINT,false,geom,ST_Transform(geom,4326)
FROM
(
  (
    SELECT 218 AS jurisd_base_id,prefix,bbox,geom_country,
      ST_Intersection(ggeohash.draw_cell_bybox(bbox,false,32717),geom_country) AS geom,
      ggeohash.draw_cell_bybox(bbox,false,32717) AS geom_cell
    FROM unnest
        (
        '{00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,1B,1C,1D,1E,1F}'::text[],
        array[60,50,51,55,56,40,41,45,46,47,30,31,35,36,37,25,26,27,15,16,5,6]
        ) t(prefix,quadrant),
        LATERAL (SELECT ARRAY[ -870000 + (quadrant%10)*262144, 9401000 + (quadrant/10)*(131072), -870000 + (quadrant%10)*262144+262144, 9401000 + (quadrant/10)*(131072)+131072 ]) u(bbox),
        LATERAL (SELECT ST_Transform(geom,32717) FROM optim.vw01full_jurisdiction_geom g WHERE g.isolabel_ext = 'EC' AND jurisd_base_id = 218) r(geom_country)
    WHERE quadrant IS NOT NULL
  )
) z
;

-- DE_PARA COVER
CREATE or replace FUNCTION osmc.update_coverage_isolevel3(
  p_isolabel_ext text,
  p_status       smallint, -- 0: generated, 1: revised, 2: homologated
  p_cover        text[],
  p_overlay      text[] DEFAULT array[]::text[]
) RETURNS text AS $f$

-- DELETE FROM osmc.coverage WHERE (id::bit(64)<<24)::bit(2) <> 0::bit(2);
  DELETE FROM osmc.coverage WHERE isolabel_ext = p_isolabel_ext;
  INSERT INTO osmc.coverage(id,bbox,isolabel_ext,prefix,status,is_overlay,geom)
  SELECT ((j_id_bit || l_id_bit || mun_princ || cover_parcial || order_prefix_5bits || prefix_bits_pad32)::bit(64))::bigint , bbox, isolabel_ext, prefix, p_status, is_overlay, geom
  FROM
  (
    SELECT j_id_bit, l_id_bit, '01' AS mun_princ,
    CASE
      WHEN ST_ContainsProperly(r.geom_transformed,ggeohash.draw_cell_bybox(bbox,false,p.srid)) IS FALSE
      THEN '1'
      ELSE '0'
    END AS cover_parcial,
    rpad(prefix_bits::text, 32, '00000000000000000000000000000000') AS prefix_bits_pad32,
    (order_prefix::int)::bit(5) AS order_prefix_5bits,
    q.isolabel_ext, prefix, is_overlay,
    ST_Intersection(r.geom_transformed,ggeohash.draw_cell_bybox(bbox,false,p.srid)) AS geom,
    bbox
    FROM
    (
      SELECT isolabel_ext, srid, jurisd_base_id, is_overlay, prefix,
            ROW_NUMBER() OVER (PARTITION BY isolabel_ext ORDER BY length(prefix), prefix ASC) AS order_prefix,
            baseh_to_vbit(prefix,16) AS prefix_bits
      FROM
      (
        SELECT p_isolabel_ext AS isolabel_ext, is_overlay,
               ((('{"CO":9377, "BR":952019, "UY":32721, "EC":32717}'::jsonb)->(split_part(p_isolabel_ext,'-',1)))::int) AS srid,
               ((('{"CO":170, "BR":76, "UY":858, "EC":218}'::jsonb)->(split_part(p_isolabel_ext,'-',1)))::int) AS jurisd_base_id,
               prefix
        FROM
        (
               SELECT false AS is_overlay, unnest(p_cover) AS prefix

               UNION

               SELECT true AS is_overlay, unnest(p_overlay) AS prefix
        ) a
      ) pp
    ) p
    -- new order isolevel=3, number -> 14 bits
    LEFT JOIN LATERAL
    (
      SELECT jurisd_base_id::bit(10) AS j_id_bit, gid::bit(14) AS l_id_bit, t.*
      FROM(
          SELECT ROW_NUMBER() OVER(ORDER BY jurisd_local_id ASC) AS gid, jurisd_base_id, jurisd_local_id, isolabel_ext
          FROM optim.jurisdiction
          WHERE jurisd_base_id=p.jurisd_base_id AND isolevel::int >2
          ORDER BY jurisd_local_id
      ) t
    ) q
    ON lower(p.isolabel_ext) = lower(q.isolabel_ext)
    -- geom jurisdiction
    LEFT JOIN LATERAL
    (
      SELECT isolabel_ext, jurisd_base_id, ST_Transform(geom,p.srid) AS geom_transformed, geom
      FROM optim.vw01full_jurisdiction_geom g
    ) r
    ON lower(r.isolabel_ext) = lower(q.isolabel_ext) AND r.jurisd_base_id = p.jurisd_base_id
    -- bbox prefix
    LEFT JOIN LATERAL
    (
      SELECT (CASE WHEN length(p.prefix)>1 THEN ggeohash.decode_box2(substring(p.prefix_bits from 9),bbox) ELSE bbox END) AS bbox
      FROM osmc.coverage
      WHERE   (  (id::bit(64)    )::bit(10) = ((('{"CO":170, "BR":76, "UY":858, "EC":218}'::jsonb)->(split_part(p.isolabel_ext,'-',1)))::int)::bit(10) )
          AND (  (id::bit(64)<<24)::bit(2) ) = 0::bit(2)
          AND ( ((id::bit(64)<<27)::bit(8) # prefix_bits::bit(8) ) = 0::bit(8)  )-- L0 2 dígitos base16h
    ) s
    ON TRUE

    ORDER BY q.isolabel_ext, order_prefix
  ) x

  RETURNING 'Ok.'
$f$ LANGUAGE SQL;
COMMENT ON FUNCTION osmc.update_coverage_isolevel3(text,smallint,text[],text[])
  IS 'Update coverage isolevel3 in base 16h.'
;
-- SELECT osmc.update_coverage_isolevel3('BR-PA-Altamira',0::smallint,'{021G,062H,063G,063H,068G,068H,069G,069H,06AG,06AH,06BG,06BH}'::text[],'{0211FP,0211FS,0211FT,0211FV,0211FZ,02135N,02135Q,0211K,0211L,0211M,0213K,0214L}'::text[]);
-- SELECT osmc.update_coverage_isolevel3('CO-RIS-Pereira',0::smallint,'{0FAH,085G,090G,0EFH}'::text[],'{08551Z,08553P,08553Q,08553R,08553V,08554T,08554Z,08555T,08555Z,08556N,08556P,08556Q,08556R,08556S,08556T,08556V,08556Z,08557N,08557P,08557Q,08557R,08557S,08557T,08557V,09002N,09002P}'::text[]);

CREATE or replace FUNCTION osmc.update_coverage_isolevel3_161c(
  p_isolabel_ext text,
  p_status       smallint, -- 0: generated, 1: revised, 2: homologated
  p_cover        text[],
  p_overlay      text[] DEFAULT array[]::text[]
) RETURNS text AS $f$
  SELECT osmc.update_coverage_isolevel3(p_isolabel_ext,p_status,
    ARRAY(
    SELECT
          CASE
            -- FL,FT,FS,FA,FB,F8,F9: tr F -> 0F
            WHEN split_part(p_isolabel_ext,'-',1) = 'BR' AND substring(prefix,1,2) IN ('FL','FT','FS','FA','FB','F8','F9')
            THEN ('0F')
            -- FQ,F4,F5: tr F -> h
            WHEN split_part(p_isolabel_ext,'-',1) = 'BR' AND substring(prefix,1,2) IN ('FQ','F4','F5')
            THEN ('11')
            -- FR,F6,F7: tr F -> g
            WHEN split_part(p_isolabel_ext,'-',1) = 'BR' AND substring(prefix,1,2) IN ('FR','F6','F7')
            THEN ('10')

            -- E0,E1,E2: tr F -> g
            WHEN split_part(p_isolabel_ext,'-',1) = 'UY' AND substring(prefix,1,2) IN ('E0','E1','E2','EJ','EN','EP')
            THEN ('10')
            -- EE,ED,EF: tr 0 -> j
            WHEN split_part(p_isolabel_ext,'-',1) = 'UY' AND substring(prefix,1,2) IN ('0A','0B','0T')
            THEN ('12')
            -- ,,: tr 5 -> h
            WHEN split_part(p_isolabel_ext,'-',1) = 'UY' AND substring(prefix,1,2) IN ('5M','5V','5Z','5C','5D','5E','5F')
            THEN ('11')
            ELSE
            (
              ('{"0": "00", "1": "01", "2": "02", "3": "03", "4": "04", "5": "05", "6": "06", "7": "07",
                "8": "08", "9": "09", "A": "0A", "B": "0B", "C": "0C", "D": "0D", "E": "0E", "F": "0F",
                "g": "10", "h": "11", "j": "12", "k": "13", "l": "14", "m": "15", "n": "16", "p": "17",
                "q": "18", "r": "19", "s": "1A", "t": "1B", "v": "1C", "z": "1D"}'::jsonb)->>(substring(prefix,1,1))
            )
          END || upper(substring(prefix,2))
    FROM unnest(p_cover) g(prefix)
    ),
    ARRAY(
    SELECT
          CASE
            -- FL,FT,FS,FA,FB,F8,F9: tr F -> 0F
            WHEN split_part(p_isolabel_ext,'-',1) = 'BR' AND substring(prefix,1,2) IN ('FL','FT','FS','FA','FB','F8','F9')
            THEN ('0F')
            -- FQ,F4,F5: tr F -> h
            WHEN split_part(p_isolabel_ext,'-',1) = 'BR' AND substring(prefix,1,2) IN ('FQ','F4','F5')
            THEN ('11')
            -- FR,F6,F7: tr F -> g
            WHEN split_part(p_isolabel_ext,'-',1) = 'BR' AND substring(prefix,1,2) IN ('FR','F6','F7')
            THEN ('10')

            -- E0,E1,E2: tr F -> g
            WHEN split_part(p_isolabel_ext,'-',1) = 'UY' AND substring(prefix,1,2) IN ('E0','E1','E2','EJ','EN','EP')
            THEN ('10')
            -- EE,ED,EF: tr 0 -> j
            WHEN split_part(p_isolabel_ext,'-',1) = 'UY' AND substring(prefix,1,2) IN ('0A','0B','0T')
            THEN ('12')
            -- ,,: tr 5 -> h
            WHEN split_part(p_isolabel_ext,'-',1) = 'UY' AND substring(prefix,1,2) IN ('5M','5V','5Z','5C','5D','5E','5F')
            THEN ('11')
            ELSE
            (
              ('{"0": "00", "1": "01", "2": "02", "3": "03", "4": "04", "5": "05", "6": "06", "7": "07",
                "8": "08", "9": "09", "A": "0A", "B": "0B", "C": "0C", "D": "0D", "E": "0E", "F": "0F",
                "g": "10", "h": "11", "j": "12", "k": "13", "l": "14", "m": "15", "n": "16", "p": "17",
                "q": "18", "r": "19", "s": "1A", "t": "1B", "v": "1C", "z": "1D"}'::jsonb)->>(substring(prefix,1,1))
            )
          END || upper(substring(prefix,2))
    FROM unnest(p_overlay) g(prefix)
    )
  );
$f$ LANGUAGE SQL;
COMMENT ON FUNCTION osmc.update_coverage_isolevel3_161c(text,smallint,text[],text[])
  IS 'Update coverage isolevel3 in base 16h1c.'
;
-- SELECT osmc.update_coverage_isolevel3_161c('BR-PA-Altamira',0::smallint,'{21G,62H,63G,63H,68G,68H,69G,69H,6AG,6AH,6BG,6BH}'::text[],'{211FP,211FS,211FT,211FV,211FZ,2135N,2135Q,211K,211L,211M,213K,214L}'::text[]);


CASE
WHEN p_isolabel_ext IN ('BR') THEN osmc.encode_16h1c(prefix,76)
WHEN p_isolabel_ext IN ('UY') THEN osmc.encode_16h1c(prefix,858)
ELSE prefix
END


CREATE or replace FUNCTION osmc.generate_cover_csv(
  p_isolabel_ext text,
  p_path text
) RETURNS text AS $f$
DECLARE
    q_copy text;
BEGIN
  q_copy := $$
    COPY (

    SELECT a.isolabel_ext, LEAST(a.status,b.status) AS status, a.cover, b.overlay
    FROM
    (
      SELECT isolabel_ext, MIN(status) AS status, string_agg(prefix,' ') AS cover
      FROM
      (
        SELECT isolabel_ext, status,

          CASE
          WHEN '%s' IN ('BR') THEN osmc.encode_16h1c(prefix,76)
          WHEN '%s' IN ('UY') THEN osmc.encode_16h1c(prefix,858)
          ELSE prefix
          END AS prefix

        FROM osmc.coverage
        WHERE ( (id::bit(64))::bit(10) ) = ((('{"CO":170, "BR":76, "UY":858, "EC":218}'::jsonb)->('%s'))::int)::bit(10) -- country cover
              AND ( (id::bit(64)<<24)::bit(2) ) <> 0::bit(2) -- isolevel3 cover
              AND is_overlay IS FALSE
        ORDER BY isolabel_ext, length(prefix), prefix
      ) r
      GROUP BY isolabel_ext
      ORDER BY 1
    ) a
    LEFT JOIN
    (
      SELECT isolabel_ext, MIN(status) AS status, string_agg(prefix,' ') AS overlay
      FROM
      (
        SELECT isolabel_ext, status,

          CASE
          WHEN '%s' IN ('BR') THEN osmc.encode_16h1c(prefix,76)
          WHEN '%s' IN ('UY') THEN osmc.encode_16h1c(prefix,858)
          ELSE prefix
          END AS prefix

        FROM osmc.coverage
        WHERE ( (id::bit(64))::bit(10) ) = ((('{"CO":170, "BR":76, "UY":858, "EC":218}'::jsonb)->('%s'))::int)::bit(10) -- 76, country cover
              AND ( (id::bit(64)<<24)::bit(2) ) <> 0::bit(2) -- isolevel3 cover
              AND is_overlay IS TRUE
        ORDER BY isolabel_ext, length(prefix), prefix
      ) r
      GROUP BY isolabel_ext
      ORDER BY 1
    ) b
    ON a.isolabel_ext = b.isolabel_ext

    ) TO '%s' CSV HEADER
  $$;

  EXECUTE format(q_copy,p_isolabel_ext,p_isolabel_ext,p_isolabel_ext,p_isolabel_ext,p_isolabel_ext,p_isolabel_ext,p_path);

  RETURN 'Ok.';
END
$f$ LANGUAGE PLpgSQL;
COMMENT ON FUNCTION osmc.generate_cover_csv(text,text)
  IS 'Generate csv with isolevel=3 coverage and overlay in separate array.'
;

-- SELECT osmc.generate_cover_csv('BR','/tmp/pg_io/coveragebr.csv');
-- SELECT osmc.generate_cover_csv('CO','/tmp/pg_io/coverageco.csv');
-- SELECT osmc.generate_cover_csv('UY','/tmp/pg_io/coverageuy.csv');

CREATE or replace FUNCTION osmc.check_coverage(
  p_isolabel_ext text,
  p_cover     text[] -- 16h
) RETURNS TABLE(isolabel_ext text, prefix text[], order_prefix int[], ContainsProperly boolean[], Intersects boolean[], UnionContainsProperly boolean) AS $f$

SELECT p_isolabel_ext, prefix, order_prefix, ContainsProperly, Intersects, ST_ContainsProperly(geomunion,z.geom_transformed) AS UnionContainsProperly
FROM
(
  SELECT array_agg(prefix) AS prefix, array_agg(order_prefix) AS order_prefix,
    array_agg(ST_ContainsProperly(r.geom_transformed,geom)) AS ContainsProperly,
    array_agg(ST_Intersects(r.geom_transformed,geom)) AS Intersects,
    MAX(srid) AS srid,
    ST_Union(x.geom) AS geomunion
  FROM
  (
    SELECT
    order_prefix,
    prefix,
    ggeohash.draw_cell_bybox(bbox,false,p.srid) AS geom,
    p.srid AS srid
    FROM
    (
      SELECT isolabel_ext, srid, jurisd_base_id, prefix,
            ROW_NUMBER() OVER (PARTITION BY isolabel_ext ORDER BY length(prefix), prefix ASC) AS order_prefix,
            baseh_to_vbit(prefix,16) AS prefix_bits
      FROM
      (
        SELECT p_isolabel_ext AS isolabel_ext,
               ((('{"CO":9377, "BR":952019, "UY":32721, "EC":32717}'::jsonb)->(split_part(p_isolabel_ext,'-',1)))::int) AS srid,
               ((('{"CO":170, "BR":76, "UY":858, "EC":218}'::jsonb)->(split_part(p_isolabel_ext,'-',1)))::int) AS jurisd_base_id,
               unnest(p_cover) AS prefix
      ) pp
    ) p,
    -- bbox prefix
    LATERAL
    (
      SELECT (CASE WHEN length(p.prefix)>1 THEN ggeohash.decode_box2(substring(p.prefix_bits from 9),bbox) ELSE bbox END) AS bbox
      FROM osmc.coverage
      WHERE   (  (id::bit(64)    )::bit(10) = ((('{"CO":170, "BR":76, "UY":858, "EC":218}'::jsonb)->(split_part(p.isolabel_ext,'-',1)))::int)::bit(10) )
          AND (  (id::bit(64)<<24)::bit(2) ) = 0::bit(2)
          AND ( ((id::bit(64)<<27)::bit(8) # prefix_bits::bit(8) ) = 0::bit(8)  )-- L0 2 dígitos base16h
    ) s
    ORDER BY p.isolabel_ext, order_prefix
  ) x,
  -- geom jurisdiction
  LATERAL
  (
    SELECT ST_Transform(g.geom,x.srid) AS geom_transformed
    FROM optim.vw01full_jurisdiction_geom g
    WHERE isolabel_ext = p_isolabel_ext
  ) r
) y,
-- geom jurisdiction
LATERAL
(
  SELECT ST_Transform(g.geom,y.srid) AS geom_transformed
  FROM optim.vw01full_jurisdiction_geom g
  WHERE isolabel_ext = p_isolabel_ext
) z
$f$ LANGUAGE SQL;
COMMENT ON FUNCTION osmc.check_coverage(text,text[])
  IS 'Update coverage isolevel3 in base 16h.'
;
-- SELECT osmc.check_coverage('BR-PA-Altamira','{021G,062H,063G,063H,068G,068H,069G,069H,06AG,06AH,06BG,06BH,0211FP,0211FS,0211FT,0211FV,0211FZ,02135N,02135Q,0211K,0211L,0211M,0213K,0214L}'::text[]);
-- SELECT osmc.check_coverage('BR-SP-SaoPaulo','{0DF6J,0DF6L,0DF6M,0DFCJ,0DFCK,0DF69T,0DF6AV,0DF6BN,0DF6BS,0DF6BT,0DF6BZ,0DFC0R,0DFC1N,0DFC1P,0DFC1Q,0DFC1R,0DFC1S,0DFC1T,0DFC1V,0DFC1Z,0DFC2Q,0DFC3N,0DFC3Q,0DFC4N,0DFC4P,0DFC4R,0DFC4S,0DFC4V,0DF69P,0DF6AZ,0DFC0Q}'::text[]);

------------------
-- generate coverage :

CREATE OR REPLACE FUNCTION osmc.buffer_geom(geom geometry, buffer_type integer )
RETURNS geometry AS $f$
    SELECT
        CASE
        WHEN buffer_type=0 THEN geom                  -- no buffer
        WHEN buffer_type=1 THEN ST_Buffer(geom,0.001) -- ~100m
        WHEN buffer_type=2 THEN ST_Buffer(geom,0.05)  -- ~5000m
        WHEN buffer_type=3 THEN ST_Buffer(geom,0.5)   -- ~50km
        WHEN buffer_type=4 THEN ST_Buffer(geom,5)     -- ~500km
        ELSE geom                                     -- no buffer
        END
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.buffer_geom(geometry,integer)
  IS 'Add standardized buffer to geometries.'
;

CREATE or replace FUNCTION osmc.generate_gridcodes(
  p_isolabel_ext text,
  p_fraction     float DEFAULT 0.005, -- fraction of ST_CharactDiam
  buffer_type    integer DEFAULT 0
) RETURNS TABLE(id int, ggeohash text, geom geometry) AS $f$
    SELECT row_number() OVER() AS id,
          CASE split_part(p_isolabel_ext,'-',1)
            WHEN 'BR' THEN osmc.encode_point_brazil(geom_centroid)
            WHEN 'CO' THEN osmc.encode_point_colombia(geom_centroid)
          END AS ggeohash,
          geom
    FROM
    (
        SELECT ST_Centroid(geom) AS geom_centroid, geom
        FROM
        (
            SELECT (ST_SquareGrid(ST_CharactDiam(osmc.buffer_geom(g.geom,buffer_type))*p_fraction, osmc.buffer_geom(g.geom,buffer_type))).*
            FROM optim.vw01full_jurisdiction_geom g
            WHERE g.isolabel_ext = p_isolabel_ext
        ) a
        WHERE ST_Contains((SELECT osmc.buffer_geom(geom,buffer_type) FROM optim.vw01full_jurisdiction_geom g WHERE g.isolabel_ext = p_isolabel_ext),ST_Centroid(geom))

        UNION

        SELECT (pt).geom, (pt).geom
        FROM ( SELECT ST_DumpPoints((SELECT geom FROM optim.vw01full_jurisdiction_geom g WHERE g.isolabel_ext = p_isolabel_ext)) ) t1(pt)

        UNION

        SELECT geom, geom
        FROM
        (
          SELECT
            (
                ST_Dump
                (
                    ST_GeneratePoints
                    (
                      (
                        SELECT ST_Difference(geom,ST_Buffer(geom,-0.0001)) AS geom
                        FROM optim.vw01full_jurisdiction_geom g
                        WHERE g.isolabel_ext = p_isolabel_ext
                      )
                      ,
                      (FLOOR(ST_Perimeter((SELECT geom
                        FROM optim.vw01full_jurisdiction_geom g
                        WHERE g.isolabel_ext = p_isolabel_ext),true)))::int
                    )
                )
            ).geom AS geom
        ) h

    ) b
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.generate_gridcodes(text,float,integer)
  IS 'Returns geohash table of grid centroids within the jurisdiction using the characteristic diameter.'
;
-- SELECT * FROM osmc.generate_gridcodes('BR-SP-SaoPaulo');

CREATE or replace FUNCTION osmc.generate_cover(
  p_isolabel_ext text,
  p_fraction     float DEFAULT 0.005, -- fraction of ST_CharactDiam
  buffer_type    integer DEFAULT 0
) RETURNS TABLE(number_cells int, cover text[]) AS $f$

    WITH list_ggeohash AS
    (
        SELECT *
        FROM osmc.generate_gridcodes(p_isolabel_ext,p_fraction,buffer_type)
    )
    SELECT *
    FROM
    (
        -- coverage with 7-digit cells
        SELECT cardinality(cover) AS number_cells, cover
        FROM
        (
            SELECT
                ARRAY(
                    SELECT substring(ggeohash,1,7) AS cell
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover
        ) t7

        UNION ALL

        -- coverage with 6-digit cells
        SELECT cardinality(cover) AS number_cells, cover
        FROM
        (
            SELECT
                ARRAY(
                    SELECT substring(ggeohash,1,6) AS cell
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover
        ) t6

        UNION ALL

        -- coverage with 5-digit cells
        SELECT cardinality(cover) AS number_cells, cover
        FROM
        (
            SELECT
                ARRAY(
                    SELECT substring(ggeohash,1,5) AS cell
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover
        ) t5

        UNION ALL

        -- coverage with 4-digit cells
        SELECT cardinality(cover) AS number_cells, cover
        FROM
        (
            SELECT
                ARRAY(
                    SELECT substring(ggeohash,1,4) AS cell
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover
        ) t4

        UNION ALL

        -- coverage with 3-digit cells
        SELECT cardinality(cover) AS number_cells, cover
        FROM
        (
            SELECT
                ARRAY(
                    SELECT substring(ggeohash,1,3) AS cell
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover
        ) t3

        UNION ALL

        -- coverage with 2-digit cells
        SELECT cardinality(cover) AS number_cells, cover
        FROM
        (
            SELECT
                ARRAY(
                    SELECT substring(ggeohash,1,2) AS cell
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover
        ) t2
    ) t
;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.generate_cover(text,float,integer)
  IS 'Simple generation of jurisdiction coverage possibilities. No overlay.'
;
-- SELECT * FROM osmc.generate_cover('BR-SP-SaoPaulo');

CREATE or replace FUNCTION osmc.select_cover(
  p_isolabel_ext text,
  p_fraction     float DEFAULT 0.005, -- fraction of ST_CharactDiam
  buffer_type    integer DEFAULT 0
) RETURNS TABLE(p_isolabel_ext text, srid int, jurisd_base_id int, number_cells int, cover text[], cover_scientific text[]) AS $f$
SELECT p_isolabel_ext::text, srid, jurisd_base_id, number_cells, cover, cover_scientific
FROM
(
    SELECT number_cells, cover, split_part(p_isolabel_ext,'-',1) AS iso
    FROM osmc.generate_cover(p_isolabel_ext,p_fraction,buffer_type)
    WHERE number_cells < 32 -- MAX 31 cells
    ORDER BY number_cells DESC
    LIMIT 1
) p,
-- generate array in scientific base. 16h
LATERAL (
    SELECT
        ARRAY(
            SELECT vbit_to_baseh('000'||baseh_to_vbit(code,32),16,0)
                -- CASE
                -- WHEN iso     IN ('BR') THEN osmc.encode_16h1c(vbit_to_baseh('000'||baseh_to_vbit(code,32),16,0),76)
                -- WHEN iso     IN ('UY') THEN osmc.encode_16h1c(vbit_to_baseh('000'||baseh_to_vbit(code,32),16,0),858)
                -- WHEN iso NOT IN ('BR','UY') THEN vbit_to_baseh('000'||baseh_to_vbit(code,32),16,0)
                -- ELSE NULL
                -- END
            FROM unnest(cover) t(code)
        ) AS cover_scientific,
        (('{"CO":9377, "BR":952019, "UY":32721, "EC":32717}'::jsonb)->(iso))::int AS srid,
        (('{"CO":170, "BR":76, "UY":858, "EC":218}'::jsonb)->(iso))::int AS jurisd_base_id
) q
;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.select_cover(text,float,integer)
  IS 'Returns first coverage with less than 32 cells.'
;
-- EXPLAIN ANALYSE SELECT * FROM osmc.select_cover('BR-SP-SaoPaulo');
-- SELECT * FROM osmc.select_cover('BR-SP-Campinas',0.5,0);


CREATE or replace FUNCTION osmc.cover_child_geometries(
   p_code         text, -- e.g.: '0977M,0977J,0977K,0975M,0975L' in 16h
   p_isolabel_ext text, -- e.g.: 'CO-BOY-Tunja'
   p_base         int  DEFAULT 16 -- 16h
) RETURNS  TABLE (code text, code_child text, geom geometry) AS $f$
    SELECT
            c.code16h AS code,
            vbit_to_baseh( '000' || baseh_to_vbit(ghs,32) ,16) AS code_child,
            geom
    FROM
    (
        SELECT DISTINCT

        CASE
        WHEN length(code16h) > 12 AND split_part(p_isolabel_ext,'-',1) IN ('BR')           THEN substring(code16h,1,12)
        WHEN length(code16h) > 11 AND split_part(p_isolabel_ext,'-',1) IN ('EC','CO','UY') THEN substring(code16h,1,11)
        ELSE code16h
        END AS code16h,

        CASE
        WHEN length(code16h) > 12 AND split_part(p_isolabel_ext,'-',1) IN ('BR')           THEN baseh_to_vbit(substring(code16h,1,12),16)
        WHEN length(code16h) > 11 AND split_part(p_isolabel_ext,'-',1) IN ('EC','CO','UY') THEN baseh_to_vbit(substring(code16h,1,11),16)
        ELSE baseh_to_vbit(code16h,16)
        END AS codebits

        FROM
        (
            SELECT code AS code16h1c,
                CASE
                    WHEN p_base = 18
                    THEN
                    CASE
                        -- FL,FT,FS,FA,FB,F8,F9: tr F -> 0F
                        WHEN split_part(p_isolabel_ext,'-',1) = 'BR' AND substring(code,1,2) IN ('FL','FT','FS','FA','FB','F8','F9') THEN ('0F')
                        -- FQ,F4,F5: tr F -> h
                        WHEN split_part(p_isolabel_ext,'-',1) = 'BR' AND substring(code,1,2) IN ('FQ','F4','F5')                     THEN ('11')
                        -- FR,F6,F7: tr F -> g
                        WHEN split_part(p_isolabel_ext,'-',1) = 'BR' AND substring(code,1,2) IN ('FR','F6','F7')                     THEN ('10')

                        -- E0,E1,E2: tr F -> g
                        WHEN split_part(p_isolabel_ext,'-',1) = 'UY' AND substring(code,1,2) IN ('E0','E1','E2','EJ','EN','EP')      THEN ('10')
                        -- EE,ED,EF: tr 0 -> j
                        WHEN split_part(p_isolabel_ext,'-',1) = 'UY' AND substring(code,1,2) IN ('0A','0B','0T')                     THEN ('12')
                        -- ,,: tr 5 -> h
                        WHEN split_part(p_isolabel_ext,'-',1) = 'UY' AND substring(code,1,2) IN ('5M','5V','5Z','5C','5D','5E','5F') THEN ('11')
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
        SELECT bbox, ST_SRID(geom) AS srid,
        CASE
        WHEN p_base IN (16,17,18) THEN (id::bit(64)<<27)::bit(8) -- 2 dígito  base16h
        ELSE                           (id::bit(64)<<30)::bit(5) -- 1 dígito  base32
        END AS l0code

        FROM osmc.coverage
        WHERE isolabel_ext = split_part(p_isolabel_ext,'-',1) -- cobertura nacional apenas
        AND ( ( (id::bit(64)<<27)::bit(8) # codebits::bit(8) ) = 0::bit(8) ) -- 2 dígitos base16h, prefixo conforme base
    ) v,
    LATERAL
    (
        SELECT ghs,

        ST_Intersection(geom,
            (
                SELECT ST_Transform(g.geom,v.srid) AS geom_transformed
                FROM optim.vw01full_jurisdiction_geom g
                WHERE isolabel_ext = p_isolabel_ext
            )
        ) AS geom

        FROM osmc.ggeohash_GeomsFromVarbit(substring(codebits from 9),(codebits<<3)::bit(5),false,srid,32,32,bbox,false)

        WHERE
            ST_Intersects(
                (
                    SELECT ST_Transform(g.geom,v.srid) AS geom_transformed
                    FROM optim.vw01full_jurisdiction_geom g
                    WHERE isolabel_ext = p_isolabel_ext
                )
            ,geom)
    ) u

    WHERE
    CASE WHEN split_part(p_isolabel_ext,'-',1) = 'UY' THEN c.code16h NOT IN ('0EG','10G','12G','00L','12L','0EJ','05H','11H') ELSE TRUE END

    ORDER BY 1,2
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.cover_child_geometries(text,text,int)
  IS 'Child geometries of the main cover.'
;

/*
EXPLAIN ANALYZE SELECT osmc.cover_child_geometries('0977M,0977J,0977K,0975M,0975L','CO-BOY-Tunja',16);

-- Exemplo para obter densidade dos pontos do DANE
-- Na dl03t_main
DROP TABLE tmp_orig.tunja_child;
CREATE TABLE tmp_orig.tunja_child AS
SELECT * FROM osmc.cover_child_geometries('0977M,0977J,0977K,0975M,0975L','CO-BOY-Tunja',16);

-- no terminal
pg_dump -t tmp_orig.tunja_child postgres://postgres@localhost/dl03t_main | psql postgres://postgres@localhost/ingest99

-- Na ingest99
CREATE TABLE tmp_orig.tunja_child_with_points AS
SELECT r.*, s.feature_id, ST_Transform(s.geom,9377) AS geom_point
FROM tmp_orig.tunja_child r
LEFT JOIN tmp_orig.pontos_pereira_tunja s
ON ST_Contains(r.geom,ST_Transform(s.geom,9377))
;

SELECT string_agg(code_child,',')
FROM
(
    SELECT code_child, code, qtd_points, ST_Area(geom) AS area, (qtd_points/ST_Area(geom))::float AS density
    FROM
    (
        SELECT code_child, MAX(code) AS code, count(*) AS qtd_points, MAX(geom) AS geom
        FROM tmp_orig.tunja_child_with_points
        GROUP BY code_child
    ) r
    ORDER BY 5 DESC
    LIMIT 25
) s
;
-- 09774P,09774S,09774T,09776N,09776Z,09776R,09776V,09774Z,09777T,09776Q,0977DN,09774N,09774V,09774R,09776P,0977CQ,0975ET,09777S,0975BV,09758T,0975BN,0975BZ,09759T,09759S,09770Q

-- Na dl03t_main
SELECT osmc.update_coverage_isolevel3('CO-BOY-Tunja',0::smallint,'{0977M,0977J,0977K,0975M,0975L}'::text[],'{09774P,09774S,09774T,09776N,09776Z,09776R,09776V,09774Z,09777T,09776Q,0977DN,09774N,09774V,09774R,09776P,0977CQ,0975ET,09777S,0975BV,09758T,0975BN,0975BZ,09759T,09759S,09770Q}'::text[]);

DROP TABLE tmp_orig.tunja_child_density;
CREATE TABLE tmp_orig.tunja_child_density AS
SELECT r.code_child, r.code, r.qtd_points, ST_Area(s.geom) AS area, (qtd_points/ST_Area(s.geom))::float AS density, s.geom
FROM
(
    SELECT code_child, MAX(code) AS code, count(*) AS qtd_points
    FROM tmp_orig.tunja_child_with_points
    GROUP BY code_child
) r
LEFT JOIN tmp_orig.tunja_child s
ON r.code_child = s.code_child
;

*/



/*
------------------

DROP TABLE osmc.tmp_coverage_city;
CREATE TABLE osmc.tmp_coverage_city (
  isolabel_ext text   NOT NULL,
  srid         int    NOT NULL,
  jurisd_base_id int NOT NULL,
  number_cells int NOT NULL,
  cover        text[] NOT NULL,
  cover_scientific text[] NOT NULL -- 16h
);

-- Gera coberturas para isolevel3 de osm_id isolevel2
CREATE OR replace PROCEDURE osmc.cover_loop(
    p_state_osm_id bigint -- osm_id da unidade da federação
)
LANGUAGE PLpgSQL
AS $$
DECLARE
    r RECORD;
BEGIN
    FOR r IN EXECUTE format('SELECT isolabel_ext FROM optim.jurisdiction WHERE parent_id = %s;' ,p_state_osm_id)
    LOOP
        RAISE NOTICE 'Gerando cobertura de: %', r.isolabel_ext;
        INSERT INTO osmc.tmp_coverage_city SELECT * FROM osmc.select_cover((r.isolabel_ext)::text);
        COMMIT;
        RAISE NOTICE 'Cobertura inserida';
    END LOOP;
END;
$$;

-- ADD type 1 coverage
CREATE OR replace PROCEDURE osmc.cover_loop2(
)
LANGUAGE PLpgSQL
AS $$
DECLARE
    r RECORD;
BEGIN
    FOR r IN (SELECT isolabel_ext, prefix FROM osmc.tmp_check_coverage WHERE UnionContainsProperly is true AND NOT (false = ANY(Intersects)))
    LOOP
        RAISE NOTICE 'Add cobertura de: %', r.isolabel_ext;
        PERFORM osmc.update_coverage_isolevel3(r.isolabel_ext,r.prefix);
        COMMIT;
        RAISE NOTICE 'Cobertura inserida';
    END LOOP;
END;
$$;

-- Refinar grid e gerar novas coberturas para coberturas type 3 e type 4
CREATE OR replace PROCEDURE osmc.cover_loop3(
  p_fraction float
)
LANGUAGE PLpgSQL
AS $$
DECLARE
    r RECORD;
BEGIN
    FOR r IN (SELECT isolabel_ext FROM osmc.tmp_check_coverage WHERE UnionContainsProperly is false)
    LOOP
        RAISE NOTICE 'Gerando cobertura de: %', r.isolabel_ext;
        INSERT INTO osmc.tmp_coverage_city SELECT * FROM osmc.select_cover((r.isolabel_ext)::text,p_fraction);
        COMMIT;
        RAISE NOTICE 'Cobertura inserida';
    END LOOP;
END;
$$;

-- rodar no tmux
psql postgres://postgres@localhost/dl03t_main -c "CALL osmc.cover_loop(296584);" &> log_cover_sc


DROP TABLE osmc.tmp_check_coverage;
CREATE TABLE osmc.tmp_check_coverage (
  isolabel_ext text   NOT NULL,
  prefix text[],
  order_prefix int[],
  ContainsProperly boolean[],
  Intersects boolean[],
  UnionContainsProperly boolean
);

INSERT INTO osmc.tmp_check_coverage
SELECT (osmc.check_coverage(isolabel_ext,array_remove(cover_scientific,'N'))).*
FROM osmc.tmp_coverage_city
;
-- 5569

-- Return coverage OK
-- cover type 1
SELECT count(*)
FROM osmc.tmp_check_coverage
WHERE UnionContainsProperly is true AND NOT (false = ANY(Intersects));
-- 5246

-- Return complete coverage with non-intercepting cells.
-- Solution: remove cells that do not intersect
-- cover type 2
SELECT count(*)
FROM osmc.tmp_check_coverage
WHERE UnionContainsProperly is true AND false = ANY(Intersects);
-- 20

-- Return partial coverage.
-- Possible solution: increase amount of points
-- cover type 3
SELECT count(*)
FROM osmc.tmp_check_coverage
WHERE (UnionContainsProperly is false) AND NOT (false = ANY(Intersects));
-- 296

-- Return partial coverage with non-intercepting cells.
-- Possible solution: increase amount of points and remove cells that do not intersect
-- cover type 4
SELECT count(*)
FROM osmc.tmp_check_coverage
WHERE UnionContainsProperly is false AND false = ANY(Intersects);
-- 7

-- deletar coberturas já existentes
DELETE FROM osmc.tmp_check_coverage
WHERE isolabel_ext IN (SELECT isolabel_ext FROM osmc.coverage);

-- adicionar coberturas type 1
-- usar procedure osmc.cover_loop2
SELECT osmc.update_coverage_isolevel3(isolabel_ext,prefix)
FROM osmc.tmp_check_coverage
WHERE UnionContainsProperly is true AND false != ANY(Intersects);

-- deletar coberturas já existentes
DELETE FROM osmc.tmp_check_coverage
WHERE isolabel_ext IN (SELECT isolabel_ext FROM osmc.coverage);

-- adicionar coberturas type 2
SELECT osmc.update_coverage_isolevel3(isolabel_ext,prefix)
FROM
(
  SELECT MAX(isolabel_ext) AS isolabel_ext, array_agg(prefix) AS prefix
  -- SELECT osmc.check_coverage(MAX(isolabel_ext), array_agg(prefix))
  FROM
  (
    SELECT isolabel_ext, unnest(prefix) AS prefix, unnest(Intersects) AS Intersects
    FROM osmc.tmp_check_coverage
    WHERE UnionContainsProperly is true AND false = ANY(Intersects)
  ) x
  WHERE Intersects IS TRUE
  GROUP BY isolabel_ext
) y
;

-- deletar coberturas já existentes
DELETE FROM osmc.tmp_check_coverage
WHERE isolabel_ext IN (SELECT isolabel_ext FROM osmc.coverage);

-- Refinar grid de coberturas type 3 e type 4
DELETE FROM osmc.tmp_coverage_city;
psql postgres://postgres@localhost/dl03t_main -c "CALL osmc.cover_loop3(0.001);" &> log_cover_sc

-- repetir processo e refinar as que ainda forem type 3 e 4
DELETE FROM osmc.tmp_coverage_city;
psql postgres://postgres@localhost/dl03t_main -c "CALL osmc.cover_loop3(0.0001);" &> log_cover_sc


-- para checar coberturas existentes:
-- corrigir manualmente com: SELECT osmc.update_coverage_isolevel3('BR-PA-PortoMoz','{x,y,z}'::text[]);
SELECT isolabel_ext, prefix, intersects, unioncontainsproperly
FROM
(
  SELECT (osmc.check_coverage(isolabel_ext,cover)).*
  FROM
  (
    SELECT MAX(isolabel_ext) AS isolabel_ext, array_agg(prefix) AS cover
    FROM osmc.coverage
    GROUP BY isolabel_ext
  )r
) s
WHERE unioncontainsproperly is false or false = ANY(intersects);


SELECT osmc.update_coverage_isolevel3('BR-PA-PortoMoz','{}'::text[]);
SELECT * FROM osmc.tmp_coverage_city WHERE isolabel_ext ='BR-AM-Manaus'
*/
