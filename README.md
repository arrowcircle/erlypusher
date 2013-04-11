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

### Install erlypusher
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
        "app_name1": {
          "app_id": "appid1",
          "key": "key1",
          "secret": "secret1"
        },

        "app_name2": {
          "app_id": "appid2",
          "key": "key2",
          "secret": "secret2"
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

# To-do
* Channels handler (info, occupied)
* Refactor all shit
* Use jiffy for json parse and generation
* Presence channels subscriptions with lists of presence
* Support of sock.js
* Support of 2.0 pusher
* Init script for linux systems
* Chef recipe for erlypusher


