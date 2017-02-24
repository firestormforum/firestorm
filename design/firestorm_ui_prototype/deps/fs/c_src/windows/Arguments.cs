using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace De.Thekid.INotify
{

	public class Arguments
	{
		// Default values
		private List<string> _Events = new List<string>(new string[] { "create", "modify", "delete", "move" });
		private string[] _Format = new string[] { "w", " ", "e", " ", "f" };
		private List<string> _Paths = new List<string>();

		public bool Recursive { get; set; }
		public bool Monitor { get; set; }
		public bool Quiet { get; set; }
		public List<string> Paths {
			get { return this._Paths; }
		}
		public string[] Format {
			get { return this._Format; }
			set { this._Format = value; }
		}
		public List<string> Events
		{
			get { return this._Events; }
			set { this._Events = value; }
		}
		public Regex Exclude { get; set; }
	}
}
