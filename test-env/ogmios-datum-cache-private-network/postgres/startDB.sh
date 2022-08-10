configPath = $1
port = $2
userName = $3

echo "starting db"
# Setup: other env variables
export PGHOST="$PGDATA"
# Setup: DB
[ ! -d $PGDATA ] && pg_ctl initdb -o "-U $userName" && cat $configPath >> $PGDATA/postgresql.conf
pg_ctl -o "-p $port -k $PGDATA" start
