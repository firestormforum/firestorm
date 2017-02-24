using System;
using System.Threading;
using System.IO;
using System.Collections.Generic;

namespace De.Thekid.INotify
{
	// List of possible changes
	public enum Change
	{
		CREATE, MODIFY, DELETE, MOVED_FROM, MOVED_TO
	}

	/// Main class
	public class Runner
	{
		// Mappings
		protected static Dictionary<WatcherChangeTypes, Change> Changes = new Dictionary<WatcherChangeTypes, Change>();

		private List<Thread> _threads = new List<Thread>();
		private bool _stopMonitoring = false;
		private ManualResetEventSlim _stopMonitoringEvent;
		private object _notificationReactionLock = new object();
		private Arguments _args = null;

		static Runner()
		{
			Changes[WatcherChangeTypes.Created]= Change.CREATE;
			Changes[WatcherChangeTypes.Changed]= Change.MODIFY;
			Changes[WatcherChangeTypes.Deleted]= Change.DELETE;
		}

		public Runner(Arguments args)
		{
			_args = args;
		}

		/// Callback for errors in watcher
		protected void OnWatcherError(object source, ErrorEventArgs e)
		{
			Console.Error.WriteLine("*** {0}", e.GetException());
		}

		private void OnWatcherNotification(object sender, FileSystemEventArgs e)
		{
		    FileSystemWatcher w = (FileSystemWatcher)sender;
		    HandleNotification(w, e, () => Output(Console.Out, _args.Format, w, Changes[e.ChangeType], e.Name));
		}
		
		private void OnRenameNotification(object sender, RenamedEventArgs e)
		{
		    FileSystemWatcher w = (FileSystemWatcher)sender;
		    HandleNotification(w, e, () =>
		    {
		        Output(Console.Out, _args.Format, w, Change.MOVED_FROM, e.OldName);
		        Output(Console.Out, _args.Format, w, Change.MOVED_TO, e.Name);
		    });
		}
		
		private void HandleNotification(FileSystemWatcher sender, FileSystemEventArgs e, Action outputAction)
		{
		    FileSystemWatcher w = (FileSystemWatcher)sender;
		    // Lock so we don't output more than one change if we were only supposed to watch for one.
		    // And to serialize access to the console
		    lock (_notificationReactionLock)
		    {
		        // if only looking for one change and another thread beat us to it, return
		        if (!_args.Monitor && _stopMonitoring)
		        {
		            return;
		        }
		
		        if (null != _args.Exclude && _args.Exclude.IsMatch(e.FullPath))
		        {
		            return;
		        }
		
		        outputAction();
		
		        // If only looking for one change, signal to stop
		        if (!_args.Monitor)
		        {
		            _stopMonitoring = true;
		            _stopMonitoringEvent.Set();
		        }
		    }
		}

		/// Output method
		protected void Output(TextWriter writer, string[] tokens, FileSystemWatcher source, Change type, string name)
		{
			foreach (var token in tokens)
			{
				var path = Path.Combine(source.Path, name);
				switch (token[0])
				{
					case 'e':
						writer.Write(type);
						if (Directory.Exists(path))
						{
							writer.Write(",ISDIR");
						}
						break;
					case 'f': writer.Write(Path.GetFileName(path)); break;
					case 'w': writer.Write(Path.Combine(source.Path, Path.GetDirectoryName(path))); break;
					case 'T': writer.Write(DateTime.Now); break;
					default: writer.Write(token); break;
				}
			}
			writer.WriteLine();
		}

                public void Stop(object data) {
                  string s = Console.ReadLine();
		  _stopMonitoring = true;
		  _stopMonitoringEvent.Set();
                                   
                }

		public void Processor(object data) {
			string path = (string)data;
			using (var w = new FileSystemWatcher {
				Path = path,
				IncludeSubdirectories = _args.Recursive,
				Filter = "*.*"
			}) {
				w.Error += new ErrorEventHandler(OnWatcherError);

				// Parse "events" argument
				WatcherChangeTypes changes = 0;
				if (_args.Events.Contains("create"))
				{
					changes |= WatcherChangeTypes.Created;
					w.Created += new FileSystemEventHandler(OnWatcherNotification);
				}
				if (_args.Events.Contains("modify"))
				{
					changes |= WatcherChangeTypes.Changed;
					w.Changed += new FileSystemEventHandler(OnWatcherNotification);
				}
				if (_args.Events.Contains("delete"))
				{
					changes |= WatcherChangeTypes.Deleted;
					w.Deleted += new FileSystemEventHandler(OnWatcherNotification);
				}
				if (_args.Events.Contains("move"))
				{
					changes |= WatcherChangeTypes.Renamed;
					w.Renamed += new RenamedEventHandler(OnRenameNotification);
				}

				// Main loop
				if (!_args.Quiet)
				{
					Console.Error.WriteLine(
						"===> {0} for {1} in {2}{3} for {4}",
						_args.Monitor ? "Monitoring" : "Watching",
						changes,
						path,
						_args.Recursive ? " -r" : "",
						String.Join(", ", _args.Events.ToArray())
					);
				}
				w.EnableRaisingEvents = true;
				_stopMonitoringEvent.Wait();
			}
		}

		/// Entry point
		public int Run()
		{
			using (_stopMonitoringEvent = new ManualResetEventSlim(initialState: false))
			{
			    foreach (var path in _args.Paths)
			    {
			        Thread t = new Thread(new ParameterizedThreadStart(Processor));
			        t.Start(path);
			        _threads.Add(t);
			    }
                            Thread stop = new Thread(new ParameterizedThreadStart(Stop));
                            stop.Start("");
			    _stopMonitoringEvent.Wait();
			    foreach (var thread in _threads)
			    {
			        if (thread.IsAlive)
			            thread.Abort();
			        thread.Join();
			    }
			    return 0;
			}
		}

		/// Entry point method
		public static int Main(string[] args)
		{
			var p = new ArgumentParser();

			// Show usage if no args or standard "help" args are given
			if (0 == args.Length || args[0].Equals("-?") || args[0].Equals("--help"))
			{
				p.PrintUsage("inotifywait", Console.Error);
				return 1;
			}

			// Run!
			return new Runner(p.Parse(args)).Run();
		}
	}
}
