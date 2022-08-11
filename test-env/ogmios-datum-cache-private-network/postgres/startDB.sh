configPath=$1
port=$2
userName=$3

# Setup: other env variables
export PGHOST="$PGDATA"
# Setup: DB
if [ ! -d $PGDATA ] 
then 
  pg_ctl initdb -o "-U $userName" && cat $configPath >> $PGDATA/postgresql.conf
  exitCode=$?
  if [ $exitCode -ne 0 ] 
  then 
    exit
  fi
fi 
pg_ctl -o "-p $port" -D $PGDATA -l log.txt start 
