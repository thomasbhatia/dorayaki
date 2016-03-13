

# Module dorayaki_sup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Dorayaki top level supervisor.

__Behaviours:__ [`supervisor`](supervisor.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(X1::[any(), ...]) -&gt; {ok, {{one_for_one, 3, 3600}, [{term(), term(), term(), term(), term(), term()}, ...]}}
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; ignore | {error, term()} | {ok, pid()}
</code></pre>
<br />

