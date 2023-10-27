##
## Installing GGeohash
##

## ## ## ##
## -- VARS --
gits_basepath   := /var/gits
pg_uri_root     := postgres://postgres@localhost

info:
	echo "--------  make of GGeohash project ----------"
	echo
	echo " all pg_datalake=dl42t_main: install in dl42t_main database."
	echo

all:
	@echo "... Installing :"
	@echo "--- Running SQL NaturalCodes natcod ---"
	psql $(pg_uri_root)/$(pg_datalake) < $(gits_basepath)/_osmc/NaturalCodes/src-sql/step01def-lib_NatCod.sql
	@echo "--- Running SQL GGeohash ---"
# 	psql $(pg_uri_root)/$(pg_datalake) < $(gits_basepath)/_osmc/GGeohash/src/step01def-libHcode.sql
	psql $(pg_uri_root)/$(pg_datalake) < $(gits_basepath)/_osmc/GGeohash/src/step02def-libGGeohash.sql
	psql $(pg_uri_root)/$(pg_datalake) < $(gits_basepath)/_osmc/GGeohash/src/step03def-lib.sql
	psql $(pg_uri_root)/$(pg_datalake) < $(gits_basepath)/_osmc/GGeohash/src/step04def-ini.sql
	@echo "--- Running API GGeohash ---"
	psql $(pg_uri_root)/$(pg_datalake) < $(gits_basepath)/_osmc/WS/src/step01ggeohash-api.sql
