# helix-dap-erlang-walkthrough

An example and walkthrough for debugging an Erlang program with
[Helix](https://github.com/helix-editor/helix) and
[ErlangLS](https://github.com/erlang-ls/erlang_ls).

This walkthrough mostly mimics the
[ErlangLS DAP tutorial](https://erlang-ls.github.io/articles/tutorial-debugger/https://erlang-ls.github.io/articles/tutorial-debugger/)
with some changes. Different from the tutorial, we'll use

* `rebar3` instead of `erlang.mk`
* Helix instead of VSCode or Emacs

Just want a preview? Check out the [asciicast of the debug session].

We'll setup our local environment with everything we need to debug,
write a "hello world" Cowboy server named `dapper` and set a breakpoint
within.

## Step 0: Install

**Helix**: https://docs.helix-editor.com/install.html

Helix is available in several package managers across multiple operating
systems. Helix version 22.03 or later provides the DAP support necessary
for this walkthrough, but the newer version the better.

**ErlangLS**: https://github.com/erlang-ls/erlang_ls#quickstart

> Note: the current `master` build of ErlangLS has some bugs which
> I'll address in PRs soon. In the meantime, you can build from
> my ErlangLS fork on the
> [`nopr-flake-dap-fixes`](https://github.com/the-mikedavis/erlang_ls/tree/nopr-flake-dap-fixes)
> branch.

You'll need the `els_dap` command in `PATH`.

**Erlang**: https://github.com/erlang/otp#installation

Erlang is nearly ubiquitous in package managers. For this walkthrough I
used OTP-25 as well as `rebar3` and ErlangLS compiled with OTP-25.

**Rebar3**: https://rebar3.readme.io/docs/getting-started

---

Are you using Nix? This repository has a Nix flake and a `shell.nix`.
Use `nix develop` (flakes) or `nix-shell` to drop into a shell with
all dependencies available.

## Step 1: An example project

Let's spin up a rebar3 project we can use as an example for debugging.
This repository contains all the code you'll need for this walkthrough.

`rebar3 new app dapper` will generate a new OTP application called `dapper`.
Head into the new dapper project with `cd dapper` and (optionally) `git init`
to set up a workspace root for ErlangLS.

We'll recreate [Cowboy](https://github.com/ninenines/cowboy)'s hello-world
example using `rebar3`. So we'll start by pull in `cowboy` as a dependency:

```erlang
%% rebar3.config
%% ...
{deps, [{cowboy, "2.9.0"}]}.

%% Add `cowboy` to the list of applications started in the `rebar3 shell`
{shell, [
    {apps, [cowboy, dapper]}
]}.
```

Then we'll open up the `src/dapper_app.erl` application file and add our
cowboy route:

```erlang
%% src/dapper_app.erl
%% ...
start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [{"/", dapper_serve, []}]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    dapper_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http),
    ok.
```

This will start an HTTP server on port `8080` when we start up a
`rebar3 shell`, forwarding all requests to the `dapper_serve` module.
The change in `stop/1` stops `cowboy` from listening when `dapper`
shuts down.

Now we'll add our handler for HTTP requests, `dapper_serve`:

```erlang
%% src/dapper_serve.erl
%% @doc Serves all HTTP requests.

-module(dapper_serve).

-export([init/2]).

init(Req0, Opts) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           <<"Hello from Dapper!">>,
                           Req0),
    {ok, Req, Opts}.
```

With all of this set up, open up a shell with `rebar3 shell`. This will
spawn `cowboy`, `dapper` and any other dependent applications. Check
that the HTTP server is working as expected with `curl`:

```
$ curl -i localhost:8080
HTTP/1.1 200 OK
content-length: 18
content-type: text/plain
date: Sat, 02 Jul 2022 17:33:57 GMT
server: Cowboy

Hello from Dapper!⏎
```

Now that `dapper` is all set up, let's get down to debugging.

## Step 2: Add debugger configuration

First we'll need to modify our `rebar3.config` to enable debugging.
Add `debugger` to the list of applications to spawn and setup the
distribution settings:

```erlang
%% rebar3.config
%% ...
{shell, [
    {apps, [debugger, cowboy, dapper]}
]}.

{dist_node, [
    {setcookie, dapper_cookie},
    {sname, dapper}
]}.
```

Now when we launch a `rebar3 shell`, we'll start the `debugger` application
built in to OTP and the shell will start a node with a shortname (`sname`)
and a cookie.

The ErlangLS debug adapter will connect to the node our `rebar3 shell` starts
as a hidden node in order to set breakpoints and examine variables.

We'll also need to configure Helix with DAP configuration for Erlang. At
the time of writing, Helix does not have any default DAP configuration for
Erlang, but I'll make a PR soon 🙂.

```toml
# .helix/languages.toml
[[language]]
name = "erlang"

[language.debugger]
name = "els_dap"
transport = "stdio"
command = "els_dap"

[[language.debugger.templates]]
name = "Existing Erlang Node"
request = "attach"
completion = [ "NodeName", "Cookie", { name = "Working Directory", completion = "directory", default = "." } ]
args = { projectnode = "{0}", cookie = "{1}", cwd = "{2}" }
```

Helix allows a local language configuration `languages.toml` which is merged
in with the default language configuration and any per-user language
configuration specified in `~/.config/helix/languages.toml`. Here we add
`language.debugger` configuration to Erlang and add launch templates.
For now we'll only add one template that allows us to connect to a running
node. Later we'll add a template that will allow ErlangLS to tell helix to
spawn a `rebar3 shell` with any necessary configuration options.

You may want to keep this file checked in with hard-coded defaults for
the `NodeName` and `Cookie` inputs for convenience.

## Step 3: Debug

In one terminal, start a `rebar3 shell`. You should now see the shell's
node name in the Eshell prompts:

```
$ rebar3 shell
Erlang/OTP 25 [erts-13.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Eshell V13.0  (abort with ^G)
(dapper@mango)1> ===> Booted debugger
===> Booted cowlib
===> Booted ranch
===> Booted cowboy
===> Booted dapper
```

> `mango` is the hostname of my machine - yours should be different. When I use
> `dapper@mango` in this walkthrough, replace that atom with your node name.

In another terminal, make sure you can still `curl -i localhost:8080` and get
a reply.

In one more terminal, start up `hx`. Open up `src/dapper_serve.erl` and jump
down to line 8. Open the debug menu with `<space>d` and bring up the launch
templates with `l`. Select the "Existing Erlang Node" template we created
earlier and input the values for `NodeName`, `Cookie`, and `Working Directory`.
I used `dapper@mango`, `dapper_cookie`, and the default value (just hit
<kbd>Enter</kbd>) for the workspace directory. You can determine your
`NodeName` and `Cookie` in the `rebar3 shell`:

```erlang
(dapper@mango)1> NodeName = node().
dapper@mango
(dapper@mango)2> Cookie = erlang:get_cookie().
dapper_cookie
```

Helix should now launch a debug session. You can verify that the debug adapter
connected to `dapper` in the `rebar3 shell`:

```erlang
(dapper@mango)3> nodes(connected).
[erlang_ls_dap_dapper_119162766@mango]
```

<details><summary>You won't see the <code>erlang_ls_dap_…</code> process in <code>nodes()</code>. Why?...</summary>

The debug adapter is connecting to our node as a [hidden node](https://learnyousomeerlang.com/distribunomicon#hidden-nodes).

```erlang
(dapper@mango)3> nodes(hidden).
[erlang_ls_dap_dapper_119162766@mango]
```

</details>

Now we're all set up to debug. Let's add a breakpoint for our primary cursor's
line with `b` in the debug menu. You'll see a triangle pop up in the gutter for
line 8.

Head over to the `curl` terminal and make another `curl -i localhost:8080`
HTTP request. This one will seem to get stuck. Head back to the `hx`
terminal and notice the "Thread 7853142 stopped because of a breakpoint"
message. The pid is stopped on the breakpoint!

Check out the local variables in scope with `v` in the debug menu. Hit `c` in
the debug menu to let the process continue. If you hit `c` before the `curl`
request timed out, you should see the `curl` request succeed.

## Wrapping up

That's the basics of debugging Erlang in Helix! If you didn't follow along,
you can see an [asciicast of the debug session].

DAP support is experimental in Helix. The UI can be improved and more methods
can be implemented in the future like `Disassemble` (view Erlang ASM). Check
out the available requests in the
[DAP specification](https://microsoft.github.io/debug-adapter-protocol/specification).
If you're looking to contribute to Helix, head over to the github repository:
https://github.com/helix-editor/helix and ask questions and join development
discussions on the Helix [Matrix Space](https://matrix.to/#/#helix-community:matrix.org).

[asciicast of the debug session]: https://asciinema.org/a/505632
