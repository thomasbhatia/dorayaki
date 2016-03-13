

# Module dorayaki_diameter_processor #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Dorayaki diameter_processor public API.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#process_packet-2">process_packet/2</a></td><td>Dorayaki diameter_processor public API.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="process_packet-2"></a>

### process_packet/2 ###

<pre><code>
process_packet(Data::false | binary() | {false, term(), term(), term()} | {true, term(), term(), term()} | {true, boolean(), term(), term(), term()}, State::#state{client = port(), server = port()}) -&gt; {noreply, #state{client = port(), server = port()}}
</code></pre>
<br />

Dorayaki diameter_processor public API

