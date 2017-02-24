FS Listener
===========

Backends
--------

* Mac [fsevent](https://github.com/thibaudgg/rb-fsevent)
* Linux [inotify](https://github.com/rvoicilas/inotify-tools/wiki)
* Windows [inotify-win](https://github.com/thekid/inotify-win)

NOTE: On Linux you need to install inotify-tools.

### Subscribe to Notifications

```erlang
> fs:subscribe(). % the pid will receive events as messages
> flush(). 
Shell got {<0.47.0>,
           {fs,file_event},
           {"/Users/5HT/synrc/fs/src/README.md",[closed,modified]}}
```

### List Events from Backend

```erlang
> fs:known_events(). % returns events known by your current backend
[mustscansubdirs,userdropped,kerneldropped,eventidswrapped,
 historydone,rootchanged,mount,unmount,created,removed,
 inodemetamod,renamed,modified,finderinfomod,changeowner,
 xattrmod,isfile,isdir,issymlink,ownevent]
```

### Sample Subscriber

```erlang
> fs:start_logger(). % starts a sample process that logs events with error_logger
=INFO REPORT==== 28-Aug-2013::19:36:26 ===
file_event: "/tank/proger/erlfsmon/src/4913" [closed,modified]
```

Credits
-------

* Vladimir Kirillov
* Maxim Sokhatsky

OM A HUM
