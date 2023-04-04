
CREATE or replace FUNCTION osmc.L0cover_upsert_br() RETURNS text AS $f$
  -- L0cover BRASIL
  -- DELETE FROM osmc.coverage  WHERE (id::bit(64)<<24)::bit(2) = 0::bit(2) AND (id::bit(64))::bit(10) = 76::bit(10);
  DELETE FROM osmc.coverage  WHERE isolabel_ext='BR';
  INSERT INTO osmc.coverage(cbits,isolabel_ext,bbox,status,is_country,is_contained,is_overlay,geom,geom_srid4326)
  SELECT jurisd_base_id::bit(10) || (natcod.baseh_to_vbit(prefix,16)),
         'BR',bbox,1::SMALLINT,TRUE,
         (CASE WHEN ST_ContainsProperly(geom_country,geom_cell) IS FALSE THEN TRUE ELSE FALSE END),
         FALSE,geom,ST_Transform(geom,4326)
  -- SELECT (jurisd_base_id::bit(10) || 0::bit(14) || '00' ||
  --         (CASE WHEN ST_ContainsProperly(geom_country,geom_cell) IS FALSE THEN '1' ELSE '0' END) ||
  --         rpad((natcod.baseh_to_vbit(prefix,16))::text, 34, '0000000000000000000000000000000000'))::bit(64)::bigint,
  --         'BR',bbox,1::SMALLINT,false,geom,ST_Transform(geom,4326)
  FROM
  (
      SELECT 76 AS jurisd_base_id, prefix, bbox,geom_country,
        ST_Intersection(ggeohash.draw_cell_bybox(bbox,false,952019),geom_country) AS geom,
        ggeohash.draw_cell_bybox(bbox,false,952019) AS geom_cell
      FROM unnest
          (
          '{00,01,02,03,04,05,06,07,08,09,0a,0b,0c,0d,0e,0f,10,11,12,13,14,15,16,17,18,19,1a,1b,1c,1d,1e,1f}'::text[],
          array[20,21,22,23,15,16,17,18,19,11,12,13,6,7,8,2,24,14]
          ) t(prefix,quadrant),
          LATERAL (SELECT osmc.ij_to_bbox(quadrant%5,quadrant/5,2715000,6727000,1048576)) u(bbox),
          LATERAL (SELECT ST_Transform(geom,952019) FROM optim.vw01full_jurisdiction_geom g WHERE g.isolabel_ext = 'BR' AND jurisd_base_id = 76) r(geom_country)
      WHERE quadrant IS NOT NULL
  ) y
  ORDER BY 1
  RETURNING 'Ok.'
  ;
$f$ LANGUAGE SQL;
COMMENT ON FUNCTION osmc.L0cover_upsert_br()
  IS 'Upsert L0cover from BR.'
;

CREATE or replace FUNCTION osmc.L0cover_upsert_co() RETURNS text AS $f$
  -- L0cover COLOMBIA
  -- DELETE FROM osmc.coverage  WHERE (id::bit(64)<<24)::bit(2) = 0::bit(2) AND (id::bit(64))::bit(10) = 170::bit(10);
  DELETE FROM osmc.coverage  WHERE isolabel_ext='CO';
  INSERT INTO osmc.coverage(cbits,isolabel_ext,bbox,status,is_country,is_contained,is_overlay,geom,geom_srid4326)
  SELECT jurisd_base_id::bit(10) || (natcod.baseh_to_vbit(prefix,16)),
         'CO',bbox,1::SMALLINT,TRUE,
         (CASE WHEN ST_ContainsProperly(geom_country,geom_cell) IS FALSE THEN TRUE ELSE FALSE END),
         FALSE,geom,ST_Transform(geom,4326)
  -- SELECT (jurisd_base_id::bit(10) || 0::bit(14) || '00' ||
  --         (CASE WHEN ST_ContainsProperly(geom_country,geom_cell) IS FALSE THEN '1' ELSE '0' END) ||
  --         rpad((natcod.baseh_to_vbit(prefix,16))::text, 34, '0000000000000000000000000000000000'))::bit(64)::bigint,
  --         'CO',bbox,1::SMALLINT,false,geom,ST_Transform(geom,4326)
  FROM
  (
    SELECT 170 AS jurisd_base_id,prefix,bbox,geom_country,
      ST_Intersection(ggeohash.draw_cell_bybox(bbox,false,9377),geom_country) AS geom,
      ggeohash.draw_cell_bybox(bbox,false,9377) AS geom_cell
    FROM unnest
        (
          '{8,a,1,3,9,b,4,6,c,e,5,7,d,0,2,f}'::text[],
          array[2,3,10,11,12,13,20,21,22,23,30,31,32,40,41,42]
        ) t(prefix,quadrant),
        LATERAL (SELECT osmc.ij_to_bbox(quadrant%10,quadrant/10,3678500,970000,524288)) u(bbox),
        LATERAL (
          SELECT ST_UNION(geom)
          FROM
          (
            SELECT ST_Transform(geom,9377) AS geom FROM optim.jurisdiction_eez           WHERE isolabel_ext IN ('CO','CO/JM')

            UNION

            SELECT ST_Transform(geom,9377) AS geom FROM optim.vw01full_jurisdiction_geom WHERE isolabel_ext = 'CO' AND jurisd_base_id = 170
          ) x
        ) r(geom_country)
    WHERE quadrant IS NOT NULL
  ) y
  ORDER BY 1

  RETURNING 'Ok.'
  ;
$f$ LANGUAGE SQL;
COMMENT ON FUNCTION osmc.L0cover_upsert_co()
  IS 'Upsert L0cover from CO.'
;

CREATE or replace FUNCTION osmc.L0cover_upsert_ec() RETURNS text AS $f$
  -- L0cover ECUADOR
  -- DELETE FROM osmc.coverage  WHERE (id::bit(64)<<24)::bit(2) = 0::bit(2) AND (id::bit(64))::bit(10) = 218::bit(10);
  DELETE FROM osmc.coverage  WHERE isolabel_ext='EC';
  INSERT INTO osmc.coverage(cbits,isolabel_ext,bbox,status,is_country,is_contained,is_overlay,geom,geom_srid4326)
  SELECT jurisd_base_id::bit(10) || (natcod.baseh_to_vbit(prefix,16)),
         'EC',bbox,1::SMALLINT,TRUE,
         (CASE WHEN ST_ContainsProperly(geom_country,geom_cell) IS FALSE THEN TRUE ELSE FALSE END),
         FALSE,geom,ST_Transform(geom,4326)
  -- SELECT (jurisd_base_id::bit(10) || 0::bit(14) || '00' ||
  --         (CASE WHEN ST_ContainsProperly(geom_country,geom_cell) IS FALSE THEN '1' ELSE '0' END) ||
  --         rpad((natcod.baseh_to_vbit(prefix,16))::text, 34, '0000000000000000000000000000000000'))::bit(64)::bigint,
  --         'EC',bbox,1::SMALLINT,false,geom,ST_Transform(geom,4326)
  FROM
  (
    (
      SELECT 218 AS jurisd_base_id,prefix,bbox,geom_country,
        ST_Intersection(ggeohash.draw_cell_bybox(bbox,false,32717),geom_country) AS geom,
        ggeohash.draw_cell_bybox(bbox,false,32717) AS geom_cell
      FROM unnest
          (
          '{00,01,02,03,04,05,06,07,08,09,0a,0b,0c,0d,0e,0f,10,11,12,13,14,15,16,17,18,19,1a,1b,1c,1d,1e,1f}'::text[],
          array[60,50,51,55,56,40,41,45,46,47,30,31,35,36,37,25,26,27,15,16,5,6]
          ) t(prefix,quadrant),
          LATERAL (SELECT ARRAY[ -870000 + (quadrant%10)*262144, 9401000 + (quadrant/10)*(131072), -870000 + (quadrant%10)*262144+262144, 9401000 + (quadrant/10)*(131072)+131072 ]) u(bbox),
          LATERAL (SELECT ST_Transform(geom,32717) FROM optim.vw01full_jurisdiction_geom g WHERE g.isolabel_ext = 'EC' AND jurisd_base_id = 218) r(geom_country)
      WHERE quadrant IS NOT NULL
    )
  ) z
  RETURNING 'Ok.'
  ;
$f$ LANGUAGE SQL;
COMMENT ON FUNCTION osmc.L0cover_upsert_ec()
  IS 'Upsert L0cover from .'
;

CREATE or replace FUNCTION osmc.L0cover_upsert_uy() RETURNS text AS $f$
  -- L0cover URUGUAI
  -- DELETE FROM osmc.coverage  WHERE (id::bit(64)<<24)::bit(2) = 0::bit(2) AND (id::bit(64))::bit(10) = 858::bit(10);
  DELETE FROM osmc.coverage  WHERE isolabel_ext='UY';
  INSERT INTO osmc.coverage(cbits,isolabel_ext,bbox,status,is_country,is_contained,is_overlay,geom,geom_srid4326)
  SELECT jurisd_base_id::bit(10) || (natcod.baseh_to_vbit(prefix,16)),
         'UY',bbox,1::SMALLINT,TRUE,
         (CASE WHEN ST_ContainsProperly(geom_country,geom_cell) IS FALSE THEN TRUE ELSE FALSE END),
         FALSE,geom,ST_Transform(geom,4326)
  -- SELECT (jurisd_base_id::bit(10) || 0::bit(14) || '00' ||
  --         (CASE WHEN ST_ContainsProperly(geom_country,geom_cell) IS FALSE THEN '1' ELSE '0' END) ||
  --         rpad((natcod.baseh_to_vbit(prefix,16))::text, 34, '0000000000000000000000000000000000'))::bit(64)::bigint,
  --         'UY',bbox,1::SMALLINT,false,geom,ST_Transform(geom,4326)
  FROM
  (
    SELECT 858 AS jurisd_base_id,prefix,bbox,geom_country,
      ST_Intersection(ggeohash.draw_cell_bybox(bbox,false,32721),geom_country) AS geom,
      ggeohash.draw_cell_bybox(bbox,false,32721) AS geom_cell
    FROM unnest
        (
          '{00,01,02,03,04,05,06,07,08,09,0a,0b,0c,0d,0e,0f,10,11,12,13,14,15,16,17,18,19,1a,1b,1c,1d,1e,1f}'::text[],
          array[40,41,30,31,32,33,20,21,22,23,10,11,12,13,1,2,42,0,3]
        ) t(prefix,quadrant),
        LATERAL (SELECT osmc.ij_to_bbox(quadrant%10,quadrant/10,353000,6028000,131072)) u(bbox),
        LATERAL (SELECT ST_Transform(geom,32721) FROM optim.vw01full_jurisdiction_geom g WHERE g.isolabel_ext = 'UY' AND jurisd_base_id = 858) r(geom_country)
    WHERE quadrant IS NOT NULL
  ) z
  RETURNING 'Ok.'
  ;
$f$ LANGUAGE SQL;
COMMENT ON FUNCTION osmc.L0cover_upsert_uy()
  IS 'Upsert L0cover from UY.'
;

CREATE or replace FUNCTION osmc.L0cover_upsert(
  p_iso text
) RETURNS text AS $wrap$
  SELECT
    CASE split_part(p_iso,'-',1)
    WHEN 'BR' THEN osmc.L0cover_upsert_br()
    WHEN 'CO' THEN osmc.L0cover_upsert_co()
    WHEN 'UY' THEN osmc.L0cover_upsert_uy()
    WHEN 'EC' THEN osmc.L0cover_upsert_ec()
    END
    ;
$wrap$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.L0cover_upsert(text)
  IS 'Upsert L0cover from ISO.'
;
-- SELECT osmc.L0cover_upsert('BR');

-- DE_PARA COVER
CREATE or replace FUNCTION osmc.update_coverage_isolevel3(
  p_isolabel_ext text,
  p_status       smallint, -- 0: generated, 1: revised, 2: homologated
  p_cover        text[],
  p_overlay      text[] DEFAULT array[]::text[]
) RETURNS text AS $f$
  DELETE FROM osmc.coverage WHERE isolabel_ext = p_isolabel_ext;
  INSERT INTO osmc.coverage(cbits,isolabel_ext,cindex,bbox,status,is_country,is_contained,is_overlay,kx_prefix,geom)
  SELECT jurisd_base_id::bit(10) || prefix_bits,isolabel_ext,cindex,bbox,2::smallint,FALSE,is_contained,is_overlay,kx_prefix,geom
  FROM
  (
    SELECT prefix_bits,
    CASE
      WHEN ST_ContainsProperly(c.geom_transformed,ggeohash.draw_cell_bybox(bbox,false,b.srid)) IS FALSE
      THEN TRUE
      ELSE FALSE
    END AS is_contained,
    c.isolabel_ext, prefix, is_overlay,
    natcod.vbit_to_strstd((order_prefix::int)::bit(5),'32nvu') AS cindex,
    ST_Intersection(c.geom_transformed,ggeohash.draw_cell_bybox(bbox,false,b.srid)) AS geom,
    natcod.vbit_to_strstd( osmc.vbit_from_16h_to_vbit_b32nvu(prefix_bits,jurisd_base_id),'32nvu') AS kx_prefix,
    bbox,jurisd_base_id
    FROM
    (
      SELECT is_overlay, prefix,
            ((('{"CO":9377, "BR":952019, "UY":32721, "EC":32717}'::jsonb)->(split_part(p_isolabel_ext,'-',1)))::int) AS srid,
            ROW_NUMBER() OVER (PARTITION BY isolabel_ext ORDER BY length(prefix), prefix ASC) AS order_prefix,
            natcod.baseh_to_vbit(prefix,16) AS prefix_bits,
            split_part(p_isolabel_ext,'-',1) AS isocountry
      FROM
      (
        SELECT FALSE AS is_overlay, p_isolabel_ext AS isolabel_ext, unnest(p_cover)   AS prefix
        UNION
        SELECT TRUE  AS is_overlay, p_isolabel_ext AS isolabel_ext, unnest(p_overlay) AS prefix
      ) a
    ) b
    -- geom jurisdiction
    LEFT JOIN LATERAL
    (
      SELECT isolabel_ext, jurisd_base_id, ST_Transform(geom,b.srid) AS geom_transformed, geom
      FROM optim.vw01full_jurisdiction_geom g
    ) c
    ON c.isolabel_ext = p_isolabel_ext

    -- bbox prefix
    LEFT JOIN LATERAL
    (
      SELECT (CASE WHEN length(b.prefix)>1 THEN ggeohash.decode_box2(osmc.vbit_withoutL0(b.prefix_bits,b.isocountry,16),bbox) ELSE bbox END) AS bbox
      FROM osmc.coverage
      WHERE isolabel_ext = b.isocountry AND is_country IS TRUE
          AND (
                CASE
                WHEN b.isocountry = 'CO' THEN ( ( osmc.extract_L0bits(cbits,'CO')   # prefix_bits::bit(4) ) = 0::bit(4) ) -- 1 dígitos base16h
                ELSE                    ( ( osmc.extract_L0bits(cbits,b.isocountry) # prefix_bits::bit(8) ) = 0::bit(8) ) -- 2 dígitos base16h
                END
          )
    ) s
    ON TRUE

    ORDER BY c.isolabel_ext, order_prefix
  ) x

  RETURNING 'Ok.'
$f$ LANGUAGE SQL;
COMMENT ON FUNCTION osmc.update_coverage_isolevel3(text,smallint,text[],text[])
  IS 'Update coverage isolevel3 in base 16h.'
;
-- SELECT osmc.update_coverage_isolevel3('BR-PA-Altamira',0::smallint,'{021G,062H,063G,063H,068G,068H,069G,069H,06AG,06AH,06BG,06BH}'::text[],'{0211FP,0211FS,0211FT,0211FV,0211FZ,02135N,02135Q,0211K,0211L,0211M,0213K,0214L}'::text[]);
-- SELECT osmc.update_coverage_isolevel3('CO-BOY-Tunja',0::smallint,'{c34p,c34z,c35k,c35s,c35t}'::text[],'{}'::text[]);



CREATE or replace FUNCTION osmc.update_coverage_isolevel3_161c(
  p_isolabel_ext text,
  p_status       smallint, -- 0: generated, 1: revised, 2: homologated
  p_cover        text[],
  p_overlay      text[] DEFAULT array[]::text[]
) RETURNS text AS $f$
  SELECT osmc.update_coverage_isolevel3(p_isolabel_ext,p_status,
    ARRAY(
    SELECT osmc.decode_16h1c(prefix,upper(split_part(p_isolabel_ext,'-',1)))
    FROM unnest(p_cover) g(prefix)
    ),
    ARRAY(
    SELECT osmc.decode_16h1c(prefix,upper(split_part(p_isolabel_ext,'-',1)))
    FROM unnest(p_overlay) g(prefix)
    )
  );
$f$ LANGUAGE SQL;
COMMENT ON FUNCTION osmc.update_coverage_isolevel3_161c(text,smallint,text[],text[])
  IS 'Update coverage isolevel3 in base 16h1c.'
;
-- SELECT osmc.update_coverage_isolevel3_161c('BR-PA-Altamira',0::smallint,'{21G,62H,63G,63H,68G,68H,69G,69H,6AG,6AH,6BG,6BH}'::text[],'{211FP,211FS,211FT,211FV,211FZ,2135N,2135Q,211K,211L,211M,213K,214L}'::text[]);

-- gerar string a partir dos bits e não do case prefix
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
            natcod.baseh_to_vbit(prefix,16) AS prefix_bits
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
      SELECT (CASE WHEN length(p.prefix)>1 THEN ggeohash.decode_box2(osmc.vbit_withoutL0(p.prefix_bits,(split_part(p.isolabel_ext,'-',1)),16),bbox) ELSE bbox END) AS bbox
      FROM osmc.coverage
      WHERE   (  (id::bit(64)    )::bit(10) = ((('{"CO":170, "BR":76, "UY":858, "EC":218}'::jsonb)->(split_part(p.isolabel_ext,'-',1)))::int)::bit(10) )
          AND (  (id::bit(64)<<24)::bit(2) ) = 0::bit(2)
          AND (
                CASE
                WHEN (split_part(p.isolabel_ext,'-',1)) = 'CO' THEN ( ( osmc.extract_L0bits(id,'CO')   # prefix_bits::bit(4) ) = 0::bit(4) ) -- 1 dígitos base16h
                ELSE                    ( ( osmc.extract_L0bits(id,(split_part(p.isolabel_ext,'-',1))) # prefix_bits::bit(8) ) = 0::bit(8) ) -- 2 dígitos base16h
                END
          )

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
-- SELECT osmc.check_coverage('CO-BOY-Tunja','{c347k,c347n,c347p,c347s,c347t,c347y,c347z,c34dn,c34dp,c34dy,c34dz,c352k,c352s,c352t,c352y,c352z,c358j,c358k,c358n,c358p,c358s,c358t,c358y,c358z,c359k,c359s,c359t,c35an,c35bj}'::text[]);
-- SELECT osmc.check_coverage('BR-SP-SaoPaulo','{0DF6J,0DF6L,0DF6M,0DFCJ,0DFCK,0DF69T,0DF6AV,0DF6BN,0DF6BS,0DF6BT,0DF6BZ,0DFC0R,0DFC1N,0DFC1P,0DFC1Q,0DFC1R,0DFC1S,0DFC1T,0DFC1V,0DFC1Z,0DFC2Q,0DFC3N,0DFC3Q,0DFC4N,0DFC4P,0DFC4R,0DFC4S,0DFC4V,0DF69P,0DF6AZ,0DFC0Q}'::text[]);

------------------
-- generate coverage :

CREATE or replace FUNCTION osmc.generate_gridcodes(
  p_isolabel_ext text,
  p_fraction     float DEFAULT 0.005, -- fraction of ST_CharactDiam
  buffer_type    integer DEFAULT 0
) RETURNS TABLE(id int, ggeohash text, geom geometry) AS $f$
    WITH jurisd_geom AS
    (
      SELECT geom FROM optim.vw01full_jurisdiction_geom WHERE isolabel_ext = p_isolabel_ext
    )
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
            SELECT (ST_SquareGrid(ST_CharactDiam(geom)*p_fraction,geom)).*
            FROM jurisd_geom
        ) a

        UNION

        SELECT (pt).geom, (pt).geom
        FROM
        (
          SELECT ST_DumpPoints(geom)
          FROM jurisd_geom
        ) t1(pt)

        UNION

        -- SELECT ST_Centroid(geom) AS geom_centroid, geom
        -- FROM
        -- (
        --     SELECT (ST_SquareGrid(0.00001,ST_Difference(geom,ST_Buffer(geom,-0.0001)))).*
        --     FROM jurisd_geom
        -- ) h

        SELECT geom, geom
        FROM
        (
          SELECT (ST_Dump(ST_GeneratePoints(ST_Difference(geom,ST_Buffer(geom,-0.00005)),LEAST((FLOOR(ST_Perimeter(geom,true)))::int,50000)))).geom AS geom
          FROM jurisd_geom
        ) h
    ) b
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.generate_gridcodes(text,float,integer)
  IS 'Returns geohash table of grid centroids within the jurisdiction using the characteristic diameter.'
;
-- SELECT * FROM osmc.generate_gridcodes('BR-SP-SaoPaulo');
-- CREATE VIEW abcd AS SELECT * FROM osmc.generate_gridcodes('CO-BOY-Tunja');


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
-- SELECT * FROM osmc.generate_cover('CO-BOY-Tunja');

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
) p,
-- generate array in scientific base. 16h
LATERAL (
    SELECT
        ARRAY(
            SELECT
                CASE
                WHEN iso     IN ('BR') THEN osmc.encode_16h1c(natcod.vbit_to_baseh(osmc.vbit_from_b32nvu_to_vbit_16h(natcod.b32nvu_to_vbit(code),76),16),76)
                WHEN iso     IN ('UY') THEN osmc.encode_16h1c(natcod.vbit_to_baseh(osmc.vbit_from_b32nvu_to_vbit_16h(natcod.b32nvu_to_vbit(code),858),16),858)
                WHEN iso     IN ('CO') THEN natcod.vbit_to_baseh(osmc.vbit_from_b32nvu_to_vbit_16h(natcod.b32nvu_to_vbit(code),170),16)
                WHEN iso     IN ('EC') THEN natcod.vbit_to_baseh(osmc.vbit_from_b32nvu_to_vbit_16h(natcod.b32nvu_to_vbit(code),218),16)
                ELSE NULL
                END
            FROM unnest(cover) t(code)
        ) AS cover_scientific,
        (('{"CO":9377, "BR":952019, "UY":32721, "EC":32717}'::jsonb)->(iso))::int AS srid,
        (('{"CO":170, "BR":76, "UY":858, "EC":218}'::jsonb)->(iso))::int AS jurisd_base_id
) q,
--
-- LATERAL (SELECT (osmc.check_coverage(p_isolabel_ext,array_remove(cover_scientific,'N')))) r
LATERAL (SELECT * FROM osmc.check_coverage(p_isolabel_ext,cover_scientific)) r
-- WHERE r.unioncontainsproperly IS TRUE
;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.select_cover(text,float,integer)
  IS 'Returns first coverage with less than 32 cells.'
;
-- EXPLAIN ANALYSE SELECT * FROM osmc.select_cover('BR-SP-SaoPaulo');
-- EXPLAIN ANALYSE SELECT * FROM osmc.select_cover('CO-BOY-Tunja');
-- SELECT * FROM osmc.select_cover('BR-SP-Campinas',0.5,0);

CREATE or replace FUNCTION osmc.cover_child_geometries(
   p_code         text, -- e.g.: '0977M,0977J,0977K,0975M,0975L' in 16h
   p_isolabel_ext text, -- e.g.: 'CO-BOY-Tunja'
   p_base         int  DEFAULT 16 -- 16h
) RETURNS  TABLE (code text, code_child text, geom geometry) AS $f$
    SELECT
            c.code16h AS code,
            natcod.vbit_to_baseh( '000' || natcod.b32nvu_to_vbit(ghs) ,16) AS code_child,
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
        WHEN length(code16h) > 12 AND split_part(p_isolabel_ext,'-',1) IN ('BR')           THEN natcod.baseh_to_vbit(substring(code16h,1,12),16)
        WHEN length(code16h) > 11 AND split_part(p_isolabel_ext,'-',1) IN ('EC','CO','UY') THEN natcod.baseh_to_vbit(substring(code16h,1,11),16)
        ELSE natcod.baseh_to_vbit(code16h,16)
        END AS codebits

        FROM
        (
            SELECT code AS code16h1c,
                CASE
                    WHEN p_base = 18 THEN osmc.decode_16h1c(code,upper(split_part(p_isolabel_ext,'-',1)))
                    ELSE code
                END AS code16h
            FROM regexp_split_to_table(upper(p_code),',') code
        ) u
    ) c,
    LATERAL
    (
        SELECT bbox, ST_SRID(geom) AS srid, osmc.extract_L0bits(id,isolabel_ext,p_base) AS l0code

        FROM osmc.coverage
        WHERE isolabel_ext = split_part(p_isolabel_ext,'-',1) -- cobertura nacional apenas
        AND
          CASE
          WHEN isolabel_ext = 'CO' THEN ( ( osmc.extract_L0bits(id,'CO')   # codebits::bit(4) ) = 0::bit(4) ) -- 1 dígitos base16h
          ELSE                    ( ( osmc.extract_L0bits(id,isolabel_ext) # codebits::bit(8) ) = 0::bit(8) ) -- 2 dígitos base16h
          END
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

        FROM osmc.ggeohash_GeomsFromVarbit(osmc.vbit_withoutL0(codebits,(split_part(p_isolabel_ext,'-',1)),16),(codebits<<3)::bit(5),false,srid,32,32,bbox,false)

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

DROP TABLE osmc.tmp_check_coverage;
CREATE TABLE osmc.tmp_check_coverage (
  isolabel_ext text   NOT NULL,
  prefix text[],
  order_prefix int[],
  ContainsProperly boolean[],
  Intersects boolean[],
  UnionContainsProperly boolean
);

-- Tabela para armazenar os isolabel_ext que terão cobertura gerada
DROP TABLE osmc.tmp_gerar;
CREATE TABLE osmc.tmp_gerar AS
SELECT isolabel_ext FROM optim.jurisdiction WHERE isolabel_ext LIKE 'BR-%-%' AND isolabel_ext NOT IN (SELECT isolabel_ext FROM osmc.coverage)
;

-- COBERTURAS para isolabel_ext em osmc.tmp_gerar
CREATE OR replace PROCEDURE osmc.cover_loop(
    p_fraction float DEFAULT 0.005 -- grid
)
LANGUAGE PLpgSQL
AS $$
DECLARE
    r RECORD;
BEGIN
    FOR r IN EXECUTE format('SELECT isolabel_ext FROM osmc.tmp_gerar ;','')
    LOOP
        RAISE NOTICE 'Gerando cobertura de: %', r.isolabel_ext;
        INSERT INTO osmc.tmp_coverage_city SELECT * FROM osmc.select_cover((r.isolabel_ext)::text,p_fraction);

        INSERT INTO osmc.tmp_check_coverage
        SELECT (osmc.check_coverage(isolabel_ext,array_remove(cover_scientific,'N'))).*
        FROM osmc.tmp_coverage_city
        WHERE isolabel_ext = '(r.isolabel_ext)::text';

        COMMIT;
        RAISE NOTICE 'Cobertura inserida';
    END LOOP;

    -- deletar coberturas já existentes
    -- DELETE FROM osmc.tmp_check_coverage
    -- WHERE isolabel_ext IN (SELECT isolabel_ext FROM osmc.coverage);
END;
$$;

-- ADD COVER TYPE 1 in osmc.coverage
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


-- tmux
psql postgres://postgres@localhost/dl03t_main -c "CALL osmc.cover_loop();" &> log

-- COVER TYPE 1
-- Return coverage OK
SELECT count(*)
FROM osmc.tmp_check_coverage
WHERE UnionContainsProperly is true AND NOT (false = ANY(Intersects));

-- COVER TYPE 2
-- Return complete coverage with non-intercepting cells.
-- Solution: remove cells that do not intersect
SELECT count(*)
FROM osmc.tmp_check_coverage
WHERE UnionContainsProperly is true AND false = ANY(Intersects);

-- COVER TYPE 3
-- Return partial coverage.
-- Possible solution: increase amount of points
SELECT count(*)
FROM osmc.tmp_check_coverage
WHERE (UnionContainsProperly is false) AND NOT (false = ANY(Intersects));

-- COVER TYPE 4
-- Return partial coverage with non-intercepting cells.
-- Possible solution: increase amount of points and remove cells that do not intersect
SELECT count(*)
FROM osmc.tmp_check_coverage
WHERE UnionContainsProperly is false AND false = ANY(Intersects);


-- ADD COVER TYPE 1, use osmc.cover_loop2
SELECT osmc.update_coverage_isolevel3(isolabel_ext,prefix)
FROM osmc.tmp_check_coverage
WHERE UnionContainsProperly is true AND NOT (false = ANY(Intersects));

-- ADD COVER TYPE 2
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
DELETE FROM osmc.tmp_gerar
WHERE isolabel_ext IN (SELECT isolabel_ext FROM osmc.coverage);

-- REFINAR COBERTURAS TYPE 3 E TYPE 4
-- repetir processo e refinar as que ainda forem type 3 e 4
DELETE FROM osmc.tmp_coverage_city;
psql postgres://postgres@localhost/dl03t_main -c "CALL osmc.cover_loop(0.001);" &> log_cover_sc


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
