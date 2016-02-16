
![dorayaki](https://cloud.githubusercontent.com/assets/1307449/13060300/4be43692-d428-11e5-975f-3908a865d488.png)

dorayaki
=====

An OTP application

Build
-----

    $ rebar3 compile

Build & Test
------------

	$ rebar3 compile && rebar3 release
	$ _build/default/rel/dorayaki/bin/dorayaki console

Set client port to 3869
Set host port to 3868
Open nc:
 nc -t -v -l 3868
Run telnet:
 telnet localhost 3869
