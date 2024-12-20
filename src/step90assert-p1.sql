DO $tests$
BEGIN
    RAISE NOTICE '1. Testando cell_relate.';

    ASSERT (SELECT osmc.cell_relate(osmc.vbit_withoutL0(natcod.baseh_to_vbit(osmc.decode_16h1c('6fabr',1),16),1))) = '0010',     '1.1.  N';
    ASSERT (SELECT osmc.cell_relate(osmc.vbit_withoutL0(natcod.baseh_to_vbit(osmc.decode_16h1c('6fffv',1),16),1))) = '0011',     '1.2. NE';
    ASSERT (SELECT osmc.cell_relate(osmc.vbit_withoutL0(natcod.baseh_to_vbit(osmc.decode_16h1c('67ffv',1),16),1))) = '0001',     '1.3.  E';
    ASSERT (SELECT osmc.cell_relate(osmc.vbit_withoutL0(natcod.baseh_to_vbit(osmc.decode_16h1c('6555m',1),16),1))) = '1001',     '1.4. SE';
    ASSERT (SELECT osmc.cell_relate(osmc.vbit_withoutL0(natcod.baseh_to_vbit(osmc.decode_16h1c('6040h',1),16),1))) = '1000',     '1.5.  S';
    ASSERT (SELECT osmc.cell_relate(osmc.vbit_withoutL0(natcod.baseh_to_vbit(osmc.decode_16h1c('6000h',1),16),1))) = '1100',     '1.6. SW';
    ASSERT (SELECT osmc.cell_relate(osmc.vbit_withoutL0(natcod.baseh_to_vbit(osmc.decode_16h1c('62aah',1),16),1))) = '0100',     '1.7.  W';
    ASSERT (SELECT osmc.cell_relate(osmc.vbit_withoutL0(natcod.baseh_to_vbit(osmc.decode_16h1c('6aaar',1),16),1))) = '0110',     '1.8. NW';
    ASSERT (SELECT osmc.cell_relate(osmc.vbit_withoutL0(natcod.baseh_to_vbit(osmc.decode_16h1c('6911m',1),16),1))) = '0000',     '1.9. inside L0';

    RAISE NOTICE '2. Testando vizinhança: BR.';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('6fabr',1,1,18)) = '{2501h,2501m,6fabv,6fabm,6fabh,6faam,6faav,2500m}',      '2.1. ';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('6fffv',1,1,18)) = '{2555m,3000h,7aaar,7aaah,6fffm,6fffh,6fffr,2555h}',      '2.2. ';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('67ffv',1,1,18)) = '{6d55m,7800h,72aar,72aah,67ffm,67ffh,67ffr,6d55h}',      '2.3. ';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('6555m',1,1,18)) = '{6555v,7000r,7000h,baaar,afffv,afffr,6555h,6555r}',      '2.4. ';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('6040h',1,1,18)) = '{6040r,6040v,6040m,aaeav,aaear,aabfv,6015m,6015v}',      '2.5. ';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('6000h',1,1,18)) = '{6000r,6000v,6000m,aaaav,aaaar,9fffv,5555m,5555v}',      '2.6. ';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('62aah',1,1,18)) = '{62aar,62aav,62aam,62a8v,62a8r,57fdv,57ffm,57ffv}',      '2.7. ';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('6aaar',1,1,18)) = '{2000h,2000m,6aaav,6aaam,6aaah,5fffm,5fffv,1555m}',      '2.8. ';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('6911m',1,1,18)) = '{6911v,6914r,6914h,63ber,63bbv,63bbr,6911h,6911r}',      '2.9. ';

    ASSERT (SELECT neighbors FROM osmc.neighbors_test('d35aq',1,1,18)) = '{d370g,d371g,d35bq,d35bg,d35ag,d34fg,d34fq,d365g}',      '2.10. ';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('dfb5a2ff65a',1,1,18)) = '{dfb5a2ff670,dfb5a2ff671,dfb5a2ff65b,dfb5a2ff659,dfb5a2ff658,dfb5a2ff64d,dfb5a2ff64f,dfb5a2ff665}', '2.2. ';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('0aaar',1,1,18)) = '{NULL,NULL,0aaav,0aaam,0aaah,NULL,NULL,NULL}'     ,      '2.12. ';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('8a24v',1,1,18)) = '{8a26m,8a27h,8a25r,8a25h,8a24m,8a24h,8a24r,8a26h}',      '2.13. ';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('eaaaq',1,1,18)) = '{b000g,b001g,eaabq,eaabg,eaaag,dfffg,dfffq,a555g}',      '2.14. ';

    RAISE NOTICE '3. Testando vizinhança: CO.';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('991g',2,2,16))     = '{991q,994q,994g,93eq,93bq,93aq,990g,990q}',                                 '3.1. 991g';
    ASSERT (SELECT neighbors FROM osmc.neighbors_test('9aaaaaaq',2,2,16)) = '{c000000g,c000001g,9aaaaabq,9aaaaabg,9aaaaaag,3ffffffg,3ffffffq,6555555g}', '3.2. ';

    RAISE NOTICE '4. Testando hBig <-> AFAcodes:';
    ASSERT (SELECT osmc.hBig_to_afa_sci(37996971798872115))      = 'BR+dfc16cd39S',        '4.1. hBig -> sci';
    ASSERT (SELECT osmc.afa_sci_to_hBig('BR+dfc16cd39s'))        = '37996971798872115',    '4.2. sci -> hBig';
    ASSERT (SELECT osmc.hBig_to_afa_log(37996971798872115))      = 'BR-SP-SaoPaulo~3CDUGD', '4.3. hBig -> log';
    ASSERT (SELECT osmc.afa_log_to_hBig('BR-SP-SaoPaulo~3CDUGD')) = '37996971798872115',    '4.4. log -> hBig';
END;
$tests$ LANGUAGE plpgsql;
