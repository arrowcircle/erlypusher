erlypusher
==========

[![Build Status](https://travis-ci.org/arrowcircle/erlypusher.png?branch=master)](https://travis-ci.org/arrowcircle/erlypusher)

Erlang server for pusher app

# Installation
## Install Erlang from Erlang Solutions
Install Erlang from Erlang Solutions: add this content to /etc/apt/sources.list.d/erlang_solutions.list

	deb http://binaries.erlang-solutions.com/debian quantal contrib
    deb http://binaries.erlang-solutions.com/debian precise contrib
    deb http://binaries.erlang-solutions.com/debian oneiric contrib
    deb http://binaries.erlang-solutions.com/debian natty contrib
    deb http://binaries.erlang-solutions.com/debian maverick contrib
    deb http://binaries.erlang-solutions.com/debian lucid contrib
    # deb http://binaries.erlang-solutions.com/debian wheezy contrib
    deb http://binaries.erlang-solutions.com/debian squeeze contrib

Add Erlang Solutions public key:

	wget -O - http://binaries.erlang-solutions.com/debian/erlang_solutions.asc \
    | sudo apt-key add -
And install Erlang:

	sudo apt-get update
	sudo apt-get install esl-erlang git build-essential -y -q

## Install erlypusher
Clone erlypusher repo:

	cd /opt
	git clone git://github.com/arrowcircle/erlypusher.git
	cd erlypusher

Copy sample config:

	cp erlypusher.conf.sample erlypusher.conf

And edit config. Add apps and keys for erlypusher:

	{
      "port": 8080,
      "apps": [
        {
          "app_id": "appid1",
          "key": "key1",
          "secret": "secret1",
          "name": "app_name1"
        },

        {
          "app_id": "appid2",
          "key": "key2",
          "secret": "secret2",
          "name": "app_name2"
        }
      ]
    }
Add erlang cookie file. Put same file on other machines if You use it in cluster ~/.erlang.cookie

	MYERLYPUSHERCOOKIEMYERLYPUSHERCOOKIE

Add erlang hosts file (even for local usage) ~/.hosts.erlang

For local use (dont forget to add new line after last entry):

	'localhost'.

For cluster use add all cluster hosts to file.

Run erlypusher

	cd /opt/erlypusher
	make full
	make run

# Usage
## Ruby
To use with ruby gem, add this code to your initializer:

	Pusher.host = 'localhost'
	Pusher.port = 8081
	Pusher.app_id = 'appid1'
	Pusher.key = 'key1'
	Pusher.secret = 'secret1'

## JavaScrpit
To use with javascript client, add `host` and `ws_port` params to your code:

	<script src="http://js.pusher.com/2.0/pusher.min.js"></script>
	<script type="text/javascript">
	  Pusher.log = function(data) {
	    console.log('\t\t', data);
	  };
	  Pusher.host    = "127.0.0.1";
	  Pusher.ws_port = "8081";
	  var pusher = new Pusher('key1');
 	  pusher.bind('pusher:error', function(data) { console.log(data.to_json) })
	  var myChannel = pusher.subscribe('MY_CHANNEL');
	  myChannel.bind('an_event', function(data) { console.log(data) })
	</script>

If you want to use Pusher 2.0 js library you need to set `stats_host`, `sockjs_host` and `sockjs_port`:

	<script src="http://js.pusher.com/2.0/pusher.min.js"></script>
	 <script type="text/javascript">
	  Pusher.log = function(data) {
	    console.log('\t\t', data);
	  };
	  Pusher.host    = "127.0.0.1";
	  Pusher.ws_port = "8081";
	  Pusher.stats_host = '127.0.0.1:8081';
    Pusher.sockjs_host = 'localhost';
    Pusher.sockjs_http_port = '8081';
    Pusher.disableStats = true;
	  var pusher = new Pusher('key1');
 	  pusher.bind('pusher:error', function(data) { console.log(data.to_json) })
	  var myChannel = pusher.subscribe('MY_CHANNEL');
	  myChannel.bind('an_event', function(data) { console.log(data) })
	</script>

# To-do
* Change dict to proplist or orddict
* Remove old json parser from all parts and use erlson
* Chef recipe for erlypusher
* MOAR TESTS!
* Refactor all shit
* Channels handler (info, occupied)
* Presence channels subscriptions with lists of presence
* Support of sock.js



