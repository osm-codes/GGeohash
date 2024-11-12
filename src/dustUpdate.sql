CREATE TABLE osmc.citycover_dust_raw (
  dust_b16h text NOT NULL, -- PK. from gid
  dust_city int NOT NULL,  -- PK. jurisd_local_id
  dust_city_label text,  -- redundant. isolabel_ext
  merge_score int,   -- redundant.
  receptor_b16h text NOT NULL, -- important
  receptor_city int NOT NULL, -- important
  UNIQUE (dust_b16h,dust_city)
);

COPY osmc.citycover_dust_raw FROM '/var/gits/_osmc/BR_new/data/citycover_dust.csv' CSV HEADER;
COPY osmc.citycover_dust_raw FROM '/var/gits/_osmc/CM/data/citycover_dust.csv' CSV HEADER;

DROP VIEW IF EXISTS osmc.vw_citycover_dust_cell
;
CREATE VIEW osmc.vw_citycover_dust_cell AS
  WITH dust2 AS
  (
    SELECT d.*,

        CASE split_part(dust_city_label,'-',1)
          WHEN 'BR' THEN osmc.decode_16h1c_br(dust_b16h)
          WHEN 'UY' THEN osmc.decode_16h1c_uy(dust_b16h)
          ELSE dust_b16h
        END AS dust_b16h_prefix,

        natcod.baseh_to_vbit(lower(        CASE split_part(dust_city_label,'-',1)
          WHEN 'BR' THEN osmc.decode_16h1c_br(dust_b16h)
          WHEN 'UY' THEN osmc.decode_16h1c_uy(dust_b16h)
          ELSE dust_b16h
        END),16) AS dust_b16h_prefix_bits,


        CASE split_part(dust_city_label,'-',1)
          WHEN 'BR' THEN osmc.decode_16h1c_br(receptor_b16h)
          WHEN 'UY' THEN osmc.decode_16h1c_uy(receptor_b16h)
          ELSE receptor_b16h
        END AS receptor_b16h_prefix,

        natcod.baseh_to_vbit(lower(        CASE split_part(dust_city_label,'-',1)
          WHEN 'BR' THEN osmc.decode_16h1c_br(receptor_b16h)
          WHEN 'UY' THEN osmc.decode_16h1c_uy(receptor_b16h)
          ELSE receptor_b16h
        END),16) AS receptor_b16h_prefix_bits,

      j.int_country_id
      ,j.geom AS city_geom
      ,g.isolabel_ext AS receptor_city_isolabel_ext


    FROM osmc.citycover_dust_raw d
    INNER JOIN optim.vw01full_jurisdiction_geom j
    ON d.dust_city=j.jurisd_local_id  AND j.isolevel=3

    INNER JOIN optim.vw01full_jurisdiction_geom g
    ON d.receptor_city=g.jurisd_local_id  AND g.isolevel=3


  ),
  dust3 AS
  (
    SELECT d.*, q.int_country_id::bit(8) || receptor_b16h_prefix_bits AS cbits_receptor_b16h, city_geom_transformed, cell_geom
    FROM dust2 d
    LEFT JOIN LATERAL
    (
      SELECT ggeohash.draw_cell_bybox(bbox,false,srid) AS cell_geom, ST_Transform(d.city_geom,r.srid) AS city_geom_transformed, int_country_id
      FROM
      (
        SELECT
          ( CASE WHEN length(dust_b16h_prefix) > 1 THEN ggeohash.decode_box2(osmc.vbit_withoutL0(dust_b16h_prefix_bits,osmc.extract_jurisdbits(cbits)),bbox) ELSE bbox END ) AS bbox,
          ST_SRID(geom) AS srid
        FROM osmc.coverage
        WHERE isolabel_ext = split_part(dust_city_label,'-',1) AND is_country IS TRUE
              AND
              (
                CASE
                WHEN split_part(dust_city_label,'-',1) IN ('CO','CM') THEN ( ( osmc.extract_L0bits(cbits) # dust_b16h_prefix_bits::bit(4) ) = 0::bit(4) ) -- 1 dígito  base16h
                ELSE                                                       ( ( osmc.extract_L0bits(cbits) # dust_b16h_prefix_bits::bit(8) ) = 0::bit(8) ) -- 2 dígitos base16h
                END
              )
        ) r
    ) q
    ON TRUE
  )
  SELECT dust3.dust_b16h, dust3.dust_city,
         dust3.receptor_city, dust3.receptor_b16h, dust3.dust_city_label,
         dust3.cbits_receptor_b16h,
         dust3.receptor_city_isolabel_ext,
         cell_geom,
         ST_Intersection(dust3.cell_geom,dust3.city_geom_transformed) as dust_geom
 FROM dust3
;


UPDATE osmc.coverage  -- aglutina a poeira ao receptor:
SET geom = t.newgeom
FROM (
  SELECT d.receptor_city, d.cbits_receptor_b16h, c.kx_prefix, c.cindex, c.cbits, d.receptor_city_isolabel_ext, ST_Union(c.geom,d.dust_geom) as newgeom
  FROM osmc.coverage c -- celula receptora
  INNER JOIN osmc.vw_citycover_dust_cell d
  ON d.cbits_receptor_b16h=c.cbits AND c.isolabel_ext = d.receptor_city_isolabel_ext
) t
WHERE coverage.cbits=t.cbits_receptor_b16h AND coverage.isolabel_ext=t.receptor_city_isolabel_ext
;

