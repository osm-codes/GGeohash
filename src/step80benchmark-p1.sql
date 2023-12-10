SELECT 'using data from optim.consolidated_data' as alert;

EXPLAIN ANALYZE select  count(*) from optim.consolidated_data where afa_id between 37151642052310712 and 37151642199134264;
-- Planning Time: 0.102 ms to 0.108 ms
-- Execution Time: 100.534 ms to 118.573 ms

EXPLAIN ANALYZE select  count(*) from optim.consolidated_data where geom  &&  Box2D('0103000020E61000000100000005000000CD96D9EBA64A43C0D936D35206310EC0CD96D9EBA64A43C0C17F26385B230EC033AA45306F4943C0C17F26385B230EC033AA45306F4943C0D936D35206310EC0CD96D9EBA64A43C0D936D35206310EC0'::geometry);
-- Planning Time: 0.159 ms to 0.163 ms
-- Execution Time: 208.712 ms to 211.013 ms

SELECT 'afa_id  ~2x faster tham BBOX, and advantage of no geometrytype need, only pure SQL' as conclusion;
 
