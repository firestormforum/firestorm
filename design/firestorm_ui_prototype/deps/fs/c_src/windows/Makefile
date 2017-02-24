ifeq ($(OS),Windows_NT)
        BASE=$(shell cd "$(WINDIR)";pwd -W)
        CSC?=$(shell ls -1d $(BASE)/Microsoft.NET/Framework/v*|sort -rn|head -1)/csc.exe
else
        CSC?=csc
endif

MKDIR_P = mkdir -p

inotifywait.exe: c_src/windows/*.cs
	${MKDIR_P} priv
	$(CSC) //nologo //target:exe //out:priv\\$@ c_src\\windows\\*.cs

clean:
	-rm priv\\inotifywait.exe
