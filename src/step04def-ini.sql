CREATE or replace FUNCTION osmc.L0_upsert(
  p_isolabel_ext   text,
  p_status         smallint,
  p_srid           int,
  p_x              int,
  p_y              int,
  p_side           int,
  p_grid           int[],
  p_grid_16        text[]
) RETURNS text AS $f$

  DELETE FROM osmc.coverage  WHERE isolabel_ext= p_isolabel_ext;

  INSERT INTO osmc.coverage(cbits,isolabel_ext,bbox,status,is_country,is_contained,is_overlay,geom,geom_srid4326)
  SELECT int_country_id::bit(8) || (natcod.baseh_to_vbit(prefix,16)),p_isolabel_ext,bbox,p_status,TRUE,
         ST_ContainsProperly(geom_country,geom_cell),
         FALSE,geom,ST_Transform(geom,4326)
  FROM
  (
      SELECT prefix, bbox,geom_country,int_country_id,
        ST_Intersection(ggeohash.draw_cell_bybox(bbox,false,p_srid),geom_country) AS geom,
        ggeohash.draw_cell_bybox(bbox,false,p_srid) AS geom_cell
      FROM unnest(p_grid_16,p_grid) t(prefix,quadrant),
      LATERAL
      (
        SELECT
          CASE
            WHEN p_isolabel_ext IN ('EC') THEN ARRAY[ p_x + (quadrant%10)*262144, p_y + (quadrant/10)*(131072), p_x + (quadrant%10)*262144+262144, p_y + (quadrant/10)*(131072)+131072 ]
            ELSE osmc.ij_to_bbox(quadrant%10,quadrant/10,p_x,p_y,p_side)
          END
      ) u(bbox),
      -- LATERAL (SELECT int_country_id, ST_Transform(geom,p_srid) FROM optim.vw01full_jurisdiction_geom g WHERE g.isolabel_ext = p_isolabel_ext AND isolevel = 1) r(int_country_id,geom_country)

      LATERAL (
        SELECT MAX(int_country_id), ST_UNION(geom)
        FROM
        (
          SELECT null AS int_country_id, ST_Transform(geom,p_srid) AS geom
          FROM optim.jurisdiction_eez
          WHERE p_isolabel_ext IN ('CO','CO/JM')

          UNION

          SELECT int_country_id, ST_Transform(geom,p_srid) AS geom
          FROM optim.vw01full_jurisdiction_geom
          WHERE isolabel_ext = p_isolabel_ext AND isolevel = 1
        ) x
        WHERE
            (
              CASE
                WHEN p_isolabel_ext NOT IN ('CO') THEN geom IS NOT NULL
                ELSE TRUE
              END
            )
      ) r(int_country_id,geom_country)

      WHERE quadrant IS NOT NULL
  ) y
  ORDER BY 1
  RETURNING 'Ok.'
  ;
$f$ LANGUAGE SQL;
COMMENT ON FUNCTION osmc.L0_upsert(text,smallint,int,int,int,int,int[],text[])
  IS 'Upsert L0cover from '
;

CREATE or replace FUNCTION osmc.L0cover_upsert(
  p_iso text
) RETURNS text AS $wrap$
  SELECT
    CASE split_part(p_iso,'-',1)
    WHEN 'BR' THEN osmc.L0_upsert('BR',1::smallint,952019,2715000,6727000,1048576,'{40,41,42,43,30,31,32,33,34,21,22,23,11,12,13,2,44,24}'::int[],'{00,01,02,03,04,05,06,07,08,09,0a,0b,0c,0d,0e,0f,10,11}'::text[])
    WHEN 'CM' THEN osmc.L0_upsert('CM',1::smallint,32632,408600,164150,262144,'{0,1,2,3,10,11,12,20,21,22,23,31,32,33,42}'::int[],'{b,c,d,e,8,9,a,4,5,6,7,1,2,3,0}'::text[])
    WHEN 'UY' THEN osmc.L0_upsert('UY',1::smallint,32721,353000,6028000,131072,'{40,41,30,31,32,33,20,21,22,23,10,11,12,13,1,2,42,0,3}'::int[],'{00,01,02,03,04,05,06,07,08,09,0a,0b,0c,0d,0e,0f,10,11,12}'::text[])
    WHEN 'CO' THEN osmc.L0_upsert('CO',1::smallint,9377,3678500,970000,524288,'{2,3,10,11,12,13,20,21,22,23,30,31,32,40,41,42}'::int[],'{8,a,1,3,9,b,4,6,c,e,5,7,d,0,2,f}'::text[])
    WHEN 'EC' THEN osmc.L0_upsert('EC',1::smallint,32717,-870000,9401000,262144,'{60,50,51,55,56,40,41,45,46,47,30,31,35,36,37,25,26,27,15,16,5,6}'::int[],'{00,01,02,03,04,05,06,07,08,09,0a,0b,0c,0d,0e,0f,10,11,12,13,14,15}'::text[])
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
  p_status       smallint, -- 0: generated, 1: revised, 2: homologated, 3: official
  p_cover        text[],
  p_overlay      text[] DEFAULT array[]::text[]
) RETURNS text AS $f$
  DELETE FROM osmc.coverage WHERE isolabel_ext = p_isolabel_ext;
  INSERT INTO osmc.coverage(cbits,isolabel_ext,cindex,bbox,status,is_country,is_contained,is_overlay,kx_prefix,geom)
  SELECT
        int_country_id::bit(8) || prefix_bits AS cbits,
        p_isolabel_ext AS isolabel_ext,
        natcod.vbit_to_strstd((ROW_NUMBER() OVER (ORDER BY is_overlay ASC, natcod.vBit_to_hBig(int_country_id::bit(8)||prefix_bits) ASC) - (CASE WHEN array_position(p_cover, NULL) = 1 THEN 0 ELSE 1 END))::bit(5),'32nvu') AS cindex,
        bbox AS bbox,
        p_status AS status,
        FALSE AS is_country,
        ST_ContainsProperly(geom_transformed,geom_cell) AS is_contained,
        is_overlay AS is_overlay,
        natcod.vbit_to_strstd( osmc.cbits_16h_to_b32nvu(prefix_bits,int_country_id),'32nvu') AS kx_prefix,
        ST_Intersection(geom_transformed,geom_cell) AS geom
  FROM
  (
    SELECT is_overlay, prefix, natcod.baseh_to_vbit(prefix,16) AS prefix_bits
    FROM
    (

      SELECT FALSE AS is_overlay,
        lower(CASE split_part(p_isolabel_ext,'-',1)
          WHEN 'BR' THEN osmc.decode_16h1c_br(prefix)
          WHEN 'UY' THEN osmc.decode_16h1c_uy(prefix)
          ELSE prefix
        END) AS prefix
      FROM unnest(p_cover) t(prefix)
      WHERE prefix IS NOT NULL

      UNION

      -- SELECT TRUE  AS is_overlay, unnest(p_overlay) AS prefix

      SELECT TRUE  AS is_overlay,
        lower(CASE split_part(p_isolabel_ext,'-',1)
          WHEN 'BR' THEN osmc.decode_16h1c_br(prefix_overlay)
          WHEN 'UY' THEN osmc.decode_16h1c_uy(prefix_overlay)
          ELSE prefix_overlay
        END) AS prefix
      FROM unnest(p_overlay) s(prefix_overlay)

    ) a
  ) p

  LEFT JOIN LATERAL
  (
    SELECT bbox, ggeohash.draw_cell_bybox(bbox,false,srid) AS geom_cell, geom_transformed, int_country_id
    FROM
    (
      SELECT
        ( CASE WHEN length(p.prefix) > 1 THEN ggeohash.decode_box2(osmc.vbit_withoutL0(p.prefix_bits,osmc.extract_jurisdbits(cbits)),bbox) ELSE bbox END ) AS bbox,
        ST_SRID(geom) AS srid
      FROM osmc.coverage
      WHERE isolabel_ext = split_part(p_isolabel_ext,'-',1) AND is_country IS TRUE
            AND
            (
              CASE
              WHEN split_part(p_isolabel_ext,'-',1) IN ('CO','CM') THEN ( ( osmc.extract_L0bits(cbits) # prefix_bits::bit(4) ) = 0::bit(4) ) -- 1 dígito  base16h
              ELSE                                                      ( ( osmc.extract_L0bits(cbits) # prefix_bits::bit(8) ) = 0::bit(8) ) -- 2 dígitos base16h
              END
            )
      ) r,
      LATERAL
      (
        SELECT int_country_id, ST_Transform(geom,r.srid) AS geom_transformed
        FROM optim.vw01full_jurisdiction_geom g
        WHERE isolabel_ext = p_isolabel_ext
      ) s
  ) q
  ON TRUE

  ORDER BY cindex

  RETURNING 'Ok.'
$f$ LANGUAGE SQL;
COMMENT ON FUNCTION osmc.update_coverage_isolevel3(text,smallint,text[],text[])
  IS 'Update coverage isolevel3 in base 16h.'
;
-- SELECT osmc.update_coverage_isolevel3('CO-BOY-Tunja',0::smallint,'{NULL,c347g,c347q,c34dg,c34dq,c352g,c352q,c358g,c358q,c359q,c35ag,c35bg}'::text[],'{c3581r,c3581v,c3583h,c3583m,c3583r,c3583v,c3589h,c3589m,c3589v,c358ch,c358cr}'::text[]);
-- SELECT osmc.update_coverage_isolevel3('BR-PA-Altamira',0::smallint,'{21G,62Q,63G,63Q,68G,68Q,69G,69Q,6aG,6aQ,6bG,6bQ}'::text[],'{211M,211R,211V,211fK,211fS,211fT,211fZ,211fY,213M,2135J,2135N,214R}'::text[]);

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
          WHEN '%s' IN ('BR','UY') THEN osmc.encode_16h1c(natcod.vbit_to_baseh(osmc.extract_cellbits(cbits),16,true),osmc.extract_jurisdbits(cbits))
          ELSE natcod.vbit_to_baseh(osmc.extract_cellbits(cbits),16,true)
          END AS prefix

        FROM osmc.coverage
        WHERE osmc.extract_jurisdbits(cbits) = ( SELECT int_country_id FROM optim.jurisdiction WHERE isolevel = 1 AND isolabel_ext = '%s' ) -- country cover
              AND is_country IS FALSE -- isolevel3 cover
              AND is_overlay IS FALSE
        ORDER BY isolabel_ext, natcod.vBit_to_hBig(cbits) ASC
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
          WHEN '%s' IN ('BR','UY') THEN osmc.encode_16h1c(natcod.vbit_to_baseh(osmc.extract_cellbits(cbits),16,true),osmc.extract_jurisdbits(cbits))
          ELSE natcod.vbit_to_baseh(osmc.extract_cellbits(cbits),16,true)
          END AS prefix

        FROM osmc.coverage
        WHERE osmc.extract_jurisdbits(cbits) = ( SELECT int_country_id FROM optim.jurisdiction WHERE isolevel = 1 AND isolabel_ext = '%s' ) -- country cover
              AND is_country IS FALSE -- isolevel3 cover
              AND is_overlay IS TRUE
        ORDER BY isolabel_ext, natcod.vBit_to_hBig(cbits) ASC
      ) r
      GROUP BY isolabel_ext
      ORDER BY 1
    ) b
    ON a.isolabel_ext = b.isolabel_ext

    ) TO '%s' CSV HEADER
  $$;

  EXECUTE format(q_copy,p_isolabel_ext,p_isolabel_ext,p_isolabel_ext,p_isolabel_ext,p_path);

  RETURN 'Ok.';
END
$f$ LANGUAGE PLpgSQL;
COMMENT ON FUNCTION osmc.generate_cover_csv(text,text)
  IS 'Generate csv with isolevel=3 coverage and overlay in separate array.'
;
/*
SELECT osmc.generate_cover_csv('BR','/tmp/pg_io/coveragebr.csv');
SELECT osmc.generate_cover_csv('CO','/tmp/pg_io/coverageco.csv');
SELECT osmc.generate_cover_csv('UY','/tmp/pg_io/coverageuy.csv');
SELECT osmc.generate_cover_csv('CM','/tmp/pg_io/coveragecm.csv');
*/

CREATE or replace FUNCTION osmc.check_coverage(
  p_isolabel_ext text,
  p_cover     text[] -- 16h
) RETURNS TABLE(isolabel_ext text, prefix text[], order_prefix int[], ContainsProperly boolean[], Intersects boolean[], UnionContainsProperly boolean, AreaIntersection float[]) AS $f$

SELECT p_isolabel_ext, prefix, order_prefix, ContainsProperly, Intersects, ST_ContainsProperly(geomunion,z.geom_transformed) AS UnionContainsProperly, AreaIntersection
FROM
(
  SELECT array_agg(prefix) AS prefix, array_agg(order_prefix) AS order_prefix,
    array_agg(ST_ContainsProperly(r.geom_transformed,geom)) AS ContainsProperly,
    array_agg(ST_Intersects(r.geom_transformed,geom)) AS Intersects,
    MAX(srid) AS srid,
    ST_Union(x.geom) AS geomunion,
    array_agg(ST_Area(ST_Intersection(r.geom_transformed,geom))) AS AreaIntersection
  FROM
  (
    SELECT
    order_prefix,
    prefix,
    ggeohash.draw_cell_bybox(bbox,false,p.srid) AS geom,
    p.srid AS srid
    FROM
    (
      SELECT isolabel_ext, srid, prefix,
            ROW_NUMBER() OVER (PARTITION BY isolabel_ext ORDER BY length(prefix), prefix ASC) AS order_prefix,
            natcod.baseh_to_vbit(prefix,16) AS prefix_bits
      FROM
      (
        SELECT p_isolabel_ext AS isolabel_ext,
               ((('{"CM":32632, "CO":9377, "BR":952019, "UY":32721, "EC":32717}'::jsonb)->(split_part(p_isolabel_ext,'-',1)))::int) AS srid,
               unnest(p_cover) AS prefix
      ) pp
    ) p,
    -- bbox prefix
    LATERAL
    (
      SELECT (CASE WHEN length(p.prefix)>1 THEN ggeohash.decode_box2(osmc.vbit_withoutL0(p.prefix_bits,osmc.extract_jurisdbits(cbits)),bbox) ELSE bbox END) AS bbox
      FROM osmc.coverage
      WHERE osmc.extract_jurisdbits(cbits) = ( SELECT int_country_id FROM optim.jurisdiction WHERE isolevel = 1 AND isolabel_ext = (split_part(p.isolabel_ext,'-',1)) )
          AND is_country IS TRUE
          AND (
                CASE
                WHEN (split_part(p.isolabel_ext,'-',1)) IN ('CO','CM')
                THEN ( ( osmc.extract_L0bits(cbits) # prefix_bits::bit(4) ) = 0::bit(4) ) -- 1 dígitos base16h
                ELSE ( ( osmc.extract_L0bits(cbits) # prefix_bits::bit(8) ) = 0::bit(8) ) -- 2 dígitos base16h
                END
          )
    ) s
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
-- SELECT osmc.check_coverage('BR-AL-OuroBranco','{077c4j,0776az,077c1z,0776bz,077c1s,077c0n,0776ay,0776by,077c4s,077c1n,0776bt,077c1k}'::text[]);

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
            WHEN 'CM' THEN osmc.encode_point_cm(geom_centroid)
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
          SELECT (ST_Dump(ST_GeneratePoints(ST_Difference(geom,ST_Buffer(geom,-0,00005)),LEAST((FLOOR(ST_Perimeter(geom,true)))::int,50000)))).geom AS geom
          FROM jurisd_geom
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
) RETURNS TABLE(number_cells int, length_cell int, cover text[], cover_scientific text[]) AS $f$

    WITH list_ggeohash AS
    (
        SELECT *, ( SELECT int_country_id FROM optim.jurisdiction WHERE isolevel = 1 AND isolabel_ext = (split_part(p_isolabel_ext,'-',1)) ) AS int_country_id
        FROM osmc.generate_gridcodes(p_isolabel_ext,p_fraction,buffer_type)
        WHERE ggeohash IS NOT NULL
    )
    SELECT *
    FROM
    (
        -- coverage with 5-digit cells
        SELECT cardinality(cover) AS number_cells, 5, cover, cover_scientific
        FROM
        (
            SELECT
                ARRAY(
                    SELECT substring(ggeohash,1,5) AS cell
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover,
                ARRAY(
                    SELECT natcod.vbit_to_baseh(osmc.cbits_b32nvu_to_16h(natcod.b32nvu_to_vbit(substring(ggeohash,1,5)),int_country_id),16)
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover_scientific --generate array in scientific base16h
        ) t5

        UNION ALL

        -- coverage with 4-digit cells
        SELECT cardinality(cover) AS number_cells, 4, cover, cover_scientific
        FROM
        (
            SELECT
                ARRAY(
                    SELECT substring(ggeohash,1,4) AS cell
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover,
                ARRAY(
                    SELECT natcod.vbit_to_baseh(osmc.cbits_b32nvu_to_16h(natcod.b32nvu_to_vbit(substring(ggeohash,1,4)),int_country_id),16)
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover_scientific --generate array in scientific base16h
        ) t4

        UNION ALL

        -- coverage with 3-digit cells
        SELECT cardinality(cover) AS number_cells, 3, cover, cover_scientific
        FROM
        (
            SELECT
                ARRAY(
                    SELECT substring(ggeohash,1,3) AS cell
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover,
                ARRAY(
                    SELECT natcod.vbit_to_baseh(osmc.cbits_b32nvu_to_16h(natcod.b32nvu_to_vbit(substring(ggeohash,1,3)),int_country_id),16)
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover_scientific --generate array in scientific base16h
        ) t3

        UNION ALL

        -- coverage with 2-digit cells
        SELECT cardinality(cover) AS number_cells, 2, cover, cover_scientific
        FROM
        (
            SELECT
                ARRAY(
                    SELECT substring(ggeohash,1,2) AS cell
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover,
                ARRAY(
                    SELECT natcod.vbit_to_baseh(osmc.cbits_b32nvu_to_16h(natcod.b32nvu_to_vbit(substring(ggeohash,1,2)),int_country_id),16)
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover_scientific --generate array in scientific base16h
        ) t2
        UNION ALL

        -- coverage with 1-digit cells
        SELECT cardinality(cover) AS number_cells, 1, cover, cover_scientific
        FROM
        (
            SELECT
                ARRAY(
                    SELECT substring(ggeohash,1,1) AS cell
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover,
                ARRAY(
                    SELECT natcod.vbit_to_baseh(osmc.cbits_b32nvu_to_16h(natcod.b32nvu_to_vbit(substring(ggeohash,1,1)),int_country_id),16)
                    FROM list_ggeohash
                    GROUP BY 1
                ) AS cover_scientific --generate array in scientific base16h
        ) t1
    ) t
;
$f$ LANGUAGE SQL IMMUTABLE;
COMMENT ON FUNCTION osmc.generate_cover(text,float,integer)
  IS 'Simple generation of jurisdiction coverage possibilities. No overlay.'
;
-- SELECT * FROM osmc.generate_cover('CO-AMA-ElEncanto');

CREATE or replace FUNCTION osmc.cover_child_geometries(
   p_code         text, -- e.g.: '0977M,0977J,0977K,0975M,0975L' in 16h
   p_isolabel_ext text, -- e.g.: 'CO-BOY-Tunja'
   p_base         int  DEFAULT 16 -- 16: 16h, 18: 16h1c
) RETURNS  TABLE (code text, code_child text, geom geometry) AS $f$
    SELECT
            c.code16h AS code,
            natcod.vbit_to_baseh( '000' || natcod.b32nvu_to_vbit(ghs) ,16) AS code_child,
            geom
    FROM
    (
        SELECT DISTINCT

        -- trunca code16h
        CASE
        WHEN length(code16h) > 12 AND split_part(p_isolabel_ext,'-',1) IN ('BR')           THEN substring(code16h,1,12)
        WHEN length(code16h) > 11 AND split_part(p_isolabel_ext,'-',1) IN ('EC','CO','UY') THEN substring(code16h,1,11)
        ELSE code16h
        END AS code16h,

        -- converte code16h->vbit
        CASE
        WHEN length(code16h) > 12 AND split_part(p_isolabel_ext,'-',1) IN ('BR')           THEN natcod.baseh_to_vbit(substring(code16h,1,12),16)
        WHEN length(code16h) > 11 AND split_part(p_isolabel_ext,'-',1) IN ('EC','CO','UY') THEN natcod.baseh_to_vbit(substring(code16h,1,11),16)
        ELSE natcod.baseh_to_vbit(code16h,16)
        END AS codebits

        FROM
        (
            SELECT code AS code16h1c,
                CASE
                    WHEN p_base = 18 THEN osmc.decode_16h1c(code, ( SELECT int_country_id FROM optim.jurisdiction WHERE isolevel = 1 AND isolabel_ext = (split_part(p_isolabel_ext,'-',1)) ) )
                    ELSE code
                END AS code16h
            FROM regexp_split_to_table(upper(p_code),',') code
        ) u
    ) c,
    LATERAL
    (
        SELECT bbox, ST_SRID(geom) AS srid, osmc.extract_L0bits(cbits) AS l0code, cbits

        FROM osmc.coverage
        WHERE isolabel_ext = split_part(p_isolabel_ext,'-',1) -- cobertura nacional apenas
        AND
          CASE
          WHEN isolabel_ext IN ('CO','CM')
          THEN ( ( osmc.extract_L0bits(cbits) # codebits::bit(4) ) = 0::bit(4) ) -- 1 dígitos base16h
          ELSE ( ( osmc.extract_L0bits(cbits) # codebits::bit(8) ) = 0::bit(8) ) -- 2 dígitos base16h
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

        FROM osmc.ggeohash_GeomsFromVarbit(osmc.vbit_withoutL0(codebits,osmc.extract_jurisdbits(cbits)),(codebits<<3)::bit(5),false,srid,32,32,bbox,false)

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

CREATE or replace FUNCTION osmc.cellContainsProperly(
  p_val text,  -- input
  p_isolabel_ext text,
  p_jurisd_id int,
  p_country_iso text,
  p_srid int
) RETURNS text AS $f$
DECLARE
v boolean;
BEGIN
 v:= (SELECT ST_ContainsProperly(
  (
    SELECT ggeohash.draw_cell_bybox(ggeohash.decode_box2(osmc.vbit_withoutL0(osmc.cbits_b32nvu_to_16h((natcod.b32nvu_to_vbit(p_val)),osmc.extract_jurisdbits(cbits)),osmc.extract_jurisdbits(cbits)),bbox, CASE WHEN p_country_iso = 'EC' THEN TRUE ELSE FALSE END),false,ST_SRID(geom)) AS geom
    FROM osmc.coverage
    WHERE is_country IS TRUE AND osmc.extract_jurisdbits(cbits) = p_jurisd_id AND ( ( osmc.cbits_16h_to_b32nvu(osmc.extract_L0bits(cbits),osmc.extract_jurisdbits(cbits)) # (natcod.b32nvu_to_vbit(p_val))::bit(5) ) = 0::bit(5) ) -- 1 dígito  base 32nvu
  )
  ,
  (
    SELECT ST_Transform(g.geom,p_srid)
    FROM optim.vw01full_jurisdiction_geom g
    WHERE isolabel_ext = p_isolabel_ext
  )
));
    -- RAISE NOTICE  'val % %', p_val,v;
  IF v is TRUE OR length(p_val) = 1 THEN
    RETURN p_val;
  ELSE
  RETURN osmc.cellContainsProperly(substring(p_val,1,length(p_val)-1),p_isolabel_ext,p_jurisd_id,p_country_iso,p_srid);
  END IF;
END
$f$ LANGUAGE PLpgSQL IMMUTABLE;
COMMENT ON FUNCTION osmc.cellContainsProperly
 IS 'Retorna a maior célula que contem a jurisdição.'
;
-- SELECT osmc.cellContainsProperly('7XSU0YTNP','BR-CE-Sobral',76,'BR',952019);

-- DROP VIEW osmc.tmpvwcellContainsProperly;
CREATE or replace VIEW osmc.tmpvwcellContainsProperly AS
  SELECT isolabel_ext, osmc.cellContainsProperly(ggeohash,isolabel_ext,int_country_id,country_iso,srid)  /*, natcod.b32nvu_to_vbit(ggeohash) AS codebits*/
  FROM
  (
    SELECT isolabel_ext, int_country_id, split_part(isolabel_ext,'-',1) AS country_iso, ((('{"CO":9377, "BR":952019, "UY":32721, "EC":32717}'::jsonb)->(split_part(isolabel_ext,'-',1)))::int) AS srid,
          CASE split_part(isolabel_ext,'-',1)
            WHEN 'BR' THEN osmc.encode_point_brazil(geom_centroid)
            WHEN 'CO' THEN osmc.encode_point_colombia(geom_centroid)
          END AS ggeohash
    FROM
    (
        SELECT isolabel_ext, int_country_id, ST_PointOnSurface(ST_Transform(g.geom,((('{"CO":9377, "BR":952019, "UY":32721, "EC":32717}'::jsonb)->(split_part(isolabel_ext,'-',1)))::int))) AS geom_centroid
        FROM optim.vw01full_jurisdiction_geom g
        WHERE jurisd_base_id IN (170,76) AND isolevel = 3
    ) r
  ) s
;

-- DROP VIEW osmc.tmpvw10;
CREATE or replace VIEW osmc.tmpvw10 AS
SELECT isolabel_ext, size_prefix
FROM
(
  SELECT isolabel_ext, size_prefix,
        row_number() OVER (PARTITION BY split_part(isolabel_ext,'-',1) ORDER BY size_prefix ASC ,isolabel_ext ASC)  AS id,
        row_number() OVER (PARTITION BY split_part(isolabel_ext,'-',1) ORDER BY size_prefix DESC,isolabel_ext DESC) AS id2
  FROM
  (
    SELECT isolabel_ext, MAX(length(kx_prefix)) AS size_prefix
    FROM osmc.coverage
    WHERE is_overlay IS FALSE AND isolabel_ext LIKE '%-%'
    GROUP BY isolabel_ext
    ORDER BY split_part(isolabel_ext,'-',1), 2, isolabel_ext
  ) r
) s
WHERE id < 11 OR id2 < 11
;
COMMENT ON VIEW osmc.tmpvw10 IS '10 maiores e menores coberturas de cada país.';

CREATE or replace VIEW osmc.tmpvwpoeira AS
SELECT *
FROM osmc.coverage
WHERE ST_Area(geom) < 100
;

-- DROP VIEW osmc.tmpvwcoverl0;
CREATE or replace VIEW osmc.tmpvwcoverl0 AS
  SELECT *, ggeohash.draw_cell_bybox(bbox,false,ST_SRID(geom)) AS geombbox,
      CASE
      WHEN isolabel_ext IN ('BR','UY') THEN osmc.encode_16h1c(natcod.vbit_to_baseh( osmc.extract_L0bits(cbits),16),(('{"CO":170, "BR":76, "UY":858, "EC":218}'::jsonb)->(isolabel_ext))::int)
      ELSE                                  natcod.vbit_to_baseh( osmc.extract_L0bits(cbits),16)
      END AS code
  FROM osmc.coverage
  WHERE is_country IS TRUE
;
COMMENT ON VIEW osmc.tmpvwcoverl0 IS '10 maiores e menores coberturas de cada país.';
