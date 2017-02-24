using System;
using System.IO;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace De.Thekid.INotify
{

	/// See also <a href="http://linux.die.net/man/1/inotifywait">inotifywait(1) - Linux man page</a>
	public class ArgumentParser
	{

		/// Helper method for parser
		protected string Value(string[] args, int i, string name)
		{
			if (i > args.Length)
			{
				throw new ArgumentException("Argument " + name + " requires a value");
			}
			return args[i];
		}

		/// Tokenizes "printf" format string into an array of strings
		protected string[] TokenizeFormat(string arg)
		{
			var result = new List<string>();
			var tokens = arg.Split(new char[]{ '%' });
			foreach (var token in tokens)
			{
				if (token.Length == 0) continue;

				if ("efwT".IndexOf(token[0]) != -1)
				{
					result.Add(token[0].ToString());
					if (token.Length > 1)
					{
						result.Add(token.Substring(1));
					}
				}
				else
				{
					result.Add(token);
				}
			}
			return result.ToArray();
		}

		private void ParseArgument(string option, string[] args, ref int i, Arguments result)
		{
			if ("--recursive" == option || "-r" == option)
			{
				result.Recursive = true;
			}
			else if ("--monitor" == option || "-m" == option)
			{
				result.Monitor = true;
			}
			else if ("--quiet" == option || "-q" == option)
			{
				result.Quiet = true;
			}
			else if ("--event" == option || "-e" == option)
			{
				result.Events = new List<string>(Value(args, ++i, "event").Split(','));
			}
			else if ("--format" == option)
			{
				result.Format = TokenizeFormat(Value(args, ++i, "format"));
			}
			else if ("--exclude" == option)
			{
				result.Exclude = new Regex(Value(args, ++i, "exclude"));
			}
			else if ("--excludei" == option)
			{
				result.Exclude = new Regex(Value(args, ++i, "exclude"), RegexOptions.IgnoreCase);
			}
			else if (option.StartsWith("--event="))
			{
				result.Events = new List<string>(option.Split(new Char[]{'='}, 2)[1].Split(','));
			}
			else if (option.StartsWith("--format="))
			{
				result.Format = TokenizeFormat(option.Split(new Char[]{'='}, 2)[1]);
			}
			else if (option.StartsWith("--exclude="))
			{
				result.Exclude = new Regex(option.Split(new Char[]{'='}, 2)[1]);
			}
			else if (option.StartsWith("--excludei="))
			{
				result.Exclude = new Regex(option.Split(new Char[]{'='}, 2)[1], RegexOptions.IgnoreCase);
			}
			else if (Directory.Exists(option))
			{
				result.Paths.Add(System.IO.Path.GetFullPath(option));
			}
		}

		/// Creates a new argument parser and parses the arguments
		public Arguments Parse(string[] args)
		{
			var result = new Arguments();
			for (var i = 0; i < args.Length; i++)
			{
				if (!args[i].StartsWith("--") && args[i].StartsWith("-") && args[i].Length > 2)
				{
					string options = args[i];
					for (var j = 1; j < options.Length; j++)
					{
						ParseArgument("-" + options.Substring(j, 1), args, ref i, result);
					}
				}
				else
				{
					ParseArgument(args[i], args, ref i, result);
				}
			}
			return result;
		}

		/// Usage
		public void PrintUsage(string name, TextWriter writer)
		{
			writer.WriteLine("Usage: " + name + " [options] path [...]");
			writer.WriteLine();
			writer.WriteLine("Options:");
			writer.WriteLine("-r/--recursive:  Recursively watch all files and subdirectories inside path");
			writer.WriteLine("-m/--monitor:    Keep running until killed (e.g. via Ctrl+C)");
			writer.WriteLine("-q/--quiet:      Do not output information about actions");
			writer.WriteLine("-e/--event list: Which events (create, modify, delete, move) to watch, comma-separated. Default: all");
			writer.WriteLine("--format format: Format string for output.");
			writer.WriteLine("--exclude:       Do not process any events whose filename matches the specified regex");
			writer.WriteLine("--excludei:      Ditto, case-insensitive");
			writer.WriteLine();
			writer.WriteLine("Formats:");
			writer.WriteLine("%e             : Event name");
			writer.WriteLine("%f             : File name");
			writer.WriteLine("%w             : Path name");
			writer.WriteLine("%T             : Current date and time");
		}
	}
}
