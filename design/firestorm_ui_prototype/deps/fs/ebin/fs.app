{application,fs,
             [{description,"VXZ FS Listener"},
              {vsn,"0.9.1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{fs_app,[]}},
              {env,[]},
              {modules,[fs,fs_app,fs_event_bridge,fs_server,fs_sup,fsevents,
                        inotifywait,inotifywait_win32]}]}.
