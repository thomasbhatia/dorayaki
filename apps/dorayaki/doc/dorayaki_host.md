

# Module dorayaki_host #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Dorayaki host public API.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

<pre><code>
code_change(OldVsn::term(), State::term(), Extra::term()) -&gt; {ok, term()}
</code></pre>
<br />

<a name="handle_call-3"></a>

### handle_call/3 ###

<pre><code>
handle_call(X1::term(), X2::term(), X3::term()) -&gt; {noreply, undefined}
</code></pre>
<br />

<a name="handle_cast-2"></a>

### handle_cast/2 ###

<pre><code>
handle_cast(X1::setup_socket, State::#state{client = port()}) -&gt; {noreply, #state{client = port(), server = port()}} | {stop, term(), #state{client = port()}}
</code></pre>
<br />

<a name="handle_info-2"></a>

### handle_info/2 ###

<pre><code>
handle_info(X1::{tcp_closed, term()} | {tcp, port(), binary() | maybe_improper_list(binary() | maybe_improper_list(any(), binary() | []) | byte(), binary() | []) | integer()}, State::#state{}) -&gt; &lt;&lt;_:32, _:_*8&gt;&gt; | {noreply, #state{server = port()}} | {stop, shutdown, #state{}}
</code></pre>
<br />

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(Client::term()) -&gt; {ok, #state{}}
</code></pre>
<br />

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Client::port()) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

<pre><code>
terminate(Reason::term(), State::term()) -&gt; ok
</code></pre>
<br />

