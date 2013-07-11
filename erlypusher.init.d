#!/bin/sh

### BEGIN INIT INFO
# Provides:          erlypusher
# Required-Start:    $local_fs $network
# Required-Stop:     $local_fs $network
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: starts the erlypusher server
# Description:       starts the erlypusher using erlang system
### END INIT INFO

case "$1" in
  start)
  cd /opt/erlypusher && make start_daemon
  ;;
  stop)
  cd /opt/erlypusher && make stop_daemon
  ;;
  *)
  echo $"Usage: $0 {start|stop}" 
  exit 1
esac

exit 0
