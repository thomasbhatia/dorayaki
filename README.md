
![dorayaki](https://cloud.githubusercontent.com/assets/1307449/13060300/4be43692-d428-11e5-975f-3908a865d488.png)

Dorayaki [![TravisCI build](https://travis-ci.org/thomasbhatia/dorayaki.svg?branch=master)](https://travis-ci.org/thomasbhatia/dorayaki) [![License](https://img.shields.io/badge/License-BSD-blue.svg)](LICENSE)
=====

By Thomas Bhatia (thomas.bhatia@eo.io)


Description
-----------

Dorayaki is a simple Diameter Relay and in-stream Diameter message editor written in Erlang.

If you have an older Cisco access-gateways or GGSN like 7600 with sami cards and 
use it to connect to a Diameter server or OCS, you probably ran into a 
situation where the Cisco doesn't understand some Diameter results or commands, 
especially AVPS grouped within Multiple-Service-Credit-Control (MSCC).

For example, your device sends a credit-control request to an OCS and receives a reply with 
AVP 268 result-code 2001 but buried within the MSCC is the actual AVP 268 result-code 4012. 
The cisco will resend the request a few times before getting an AVP 268 result-code value of 4012. 
This generates a lot of unnecessary requests, unto 4x the traffic, so instead of 10k connections, you're 
seeing 40k connections that eat up you memory and exhaust your CPU.

Dorayaki sits at the IP layer in-between the Diameter client and server and transparently edits the Diameter messages
returned from the server. Diameter client will connect to Dorayaki IP and Client Port, Dorayaki will then connect to Diameter Server.

Dorayaki was developed and deployed in a mobile network handling 100k simultaneous active connections running on an VMware machine with 8Gb RAM. It took a considerable load off our Cisco gateways and reduced memory and CPU by at least 30%.

Currently only edits to CCA or 'answers' is supported.

### Before
![before](https://cloud.githubusercontent.com/assets/1307449/13112832/70e3c770-d584-11e5-9281-15e825aa8f40.png)

### After
![after](https://cloud.githubusercontent.com/assets/1307449/13112863/88fc9134-d584-11e5-8833-f7aca8e087a1.png)


Note
----
Dorayaki is not a Relay Agent and does not conform to RFC 6733 sec. 2.8.1. Dorayaki only modifies Diameter messages without being an active agent. I.e Diameter connections are still setup between your Diameter Client and the Diameter server. 


Build
-----

Dorayaki uses Rebar 3
    
    $ rebar3 as dev compile && rebar3 as dev release

    or

    $ rebar3 as prod compile && rebar3 as prod release


Test
------------
Eunit tests are in test/ folder, run them using rebar:

    $ rebar eunit

Run console to test server:

    $ _build/default/rel/dorayaki/bin/dorayaki console

#### Test using Seagull Traffic Generator

I've include diameter scenario files for easy end-to-end testing using [Seagull](http://gull.sourceforge.net).
You'll find it in the misc/ folder.

##### Server side:
Place the file in Seagull's diameter scenario folder:

    $ cp ccr-cca.server.xml [...]seagull/diameter/scenario/ 

Run 

    ./seagull/diameter/run/start_server_cc.ksh

##### Client side:
Execute the start_client_cc:

    ./seagull/diameter/run/start_client_cc.ksh

Docs
----

    $ rebar3 edocs

Release
-------

    $ rebar3 compile && rebar3 as prod release


Example config file
-------------------

    % Client port is where your Diameter client will connect to.
    % Server IP and Port is the Diameter server Dorayaki will connect to.

    {client, [{port, 3869}]}.

    {host, [{ip, "192.168.56.102"}, {port, 3868}]}.

    % Search header is what Dorayaki will search for in the Diameter header
    % Here we're searching for Command code 272 which is Credit-Control.
    {search_header, [{commandcode, 272}]}.

    % Search AVPs is what Dorayaki will search for within the Diameter AVPs
    % Here it's AVP 268 result-code value 2001 and AVP 456 which is MSCC and within that result-code with value 4012
    {search_avps, [{268, 2001}, {456, [{268, 4012}]}]}.

    % Replace Diameter AVP with these values.
    % Here AVP 268 result-code value will be replaced by 4012, i.e Credit-limit-reached.
    {replace_avp, [{268, 4012}]}.

    % Logging level
    % Options available are 'info' and 'debug. 
    % Debug level is very verbose, do not enable on live systems.
    {log_level, debug}.


Modules
-------

See [Modules](https://github.com/thomasbhatia/dorayaki/blob/master/apps/dorayaki/doc/README.md)



Tips & Tricks
-------------

Reload config:
    dorayaki_config_loader:dorayaki_load_config().
    

Copyright
---------
Copyright (c) 2016 Thomas Bhatia. See LICENSE for details.