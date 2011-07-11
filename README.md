Bigwig is a suite of web-based tools for the Erlang VM, like webtool.

Tools that we currently include custom versions of:

* "etop"     - see which processes are using the most cpu/heap/reds/etc
* "rb"       - browse SASL reports
* "appmon"   - explore application supervision hierarchies

Also included is a VM dashboard with release_handler details, loaded
applications, and VM settings.


Conceived for Spawnfest 2011, SMELLS LIKE BEAM SPIRIT.

USAGE
=====
Start up the dev server and head to http://localhost:40829/

    ./rebar get-deps
    ./start-dev.sh

or include as a rebar dep and start your own node:

    {bigwig, ".*", {git, "git://github.com/beamspirit/bigwig.git", {branch, "master"}}}

rebar get-deps, compile, and then make sure your config includes the following
so that binary sasl reports are generated (which are used by bigwig):

    {sasl, [
         {errlog_type, error},
         {error_logger_mf_dir, "log/sasl"},      % Log directory
         {error_logger_mf_maxbytes, 10485760},   % 10 MB max file size
         {error_logger_mf_maxfiles, 5}           % 5 files max
       ]}

then run your project:

    mkdir -p log/sasl/
    ./start-myproject-dev.sh
    1> bigwig:start().
    =INFO REPORT==== 10-Jul-2011::20:59:44 ===
    Bigwig listening on http://127.0.0.1:40829/


NOTES
=====

Erlang topology
---------------
Run Bigwig as a hidden node, on a distinct VM, and it will connect to nodes of
your chosing to monitor/examine. No need to install any applications or releases
on nodes you wish to inspect.


Web Architecture
----------------
We are using cowboy, with a combination of webservices that return JSON, and
websockets to deliver realtime updates to the browser.


JSX Json Library
----------------
Modified so term_to_json works with pids, ports, etc.
Eg, a pid is converted to {"_type": "pid", "data": "<1.2.3>"}
Used when we render complex terms in the browser, like SASL reports.


SASL Report Browsing
--------------------
'rb' is for reading SASL reports written by the log_mf_h event handler which
SASL adds to the error_logger process. The on-disk format/writing looks like
it's been around long before disk_log wrapfiles were conceived, since it sort
of does what you would nowadays use disk_log for.

The rb module is designed to print text reports to stdout.
The bigwig_report_loader module is our alternative to rb which loads the SASL
report terms from the file and returns them, which we subsequently convert to
JSON and render client side. We also use the SASL-provided renderer code and
send the familiar looking log format to the client too.

Would *love* to install our own handler that writes the reports as documents to
couch, or at the very least using disk_log, and with a better separation of
loading the terms and rendering them. No time for this yet, and at least with
our current approach we don't need people to install anything on nodes they
wish to examing using Bigwig.


eTop
----

A web interface surfacing the information provided by etop with client-side
sorting, introspection of pids and modules and the ability to pause and page
through results.


AppMon
------

A 'SpaceTree' browser of a node's application masters and supervision
hierarchies. Click nodes to browse the hierarchy. It will
automatically update unless live updates are disabled (via the button
at the top).


URL schemes, blah
=================

* `/vm`                     lists running apps, releases, process count etc

* `/rb/reports`             lists available SASL reports
* `/rb/reports?type=error`  lists SASL reports of a specific type
* `/rb/reports/1`           returns SASL report id 1
* `/rb/stream`              websocket endpoint for streaming SASL reports as JSON

* `/pid/<1.2.3>`            process_info (POST to send msg to pid)
* `/pid/NAME`               as above, but using registered process name NAME
* `/pid/global/NAME`        as above, but using global registered process name NAME
* `/module/NAME`            NAME:module_info() stuff


