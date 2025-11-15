on open theFiles
	set emacsclient to "/opt/homebrew/bin/emacsclient"
	set emacs to "/opt/homebrew/bin/emacs"

	-- Convert file list to POSIX paths
	set filePaths to {}
	repeat with aFile in theFiles
		set end of filePaths to POSIX path of aFile
	end repeat

	-- Build the file arguments string
	set fileArgs to ""
	repeat with aPath in filePaths
		set fileArgs to fileArgs & " " & quoted form of aPath
	end repeat

	-- Try to open with emacsclient first (mimics the 'e' function behavior)
	-- Check exit status like the shell function does
	set cmd to quoted form of emacsclient & " -n" & fileArgs & " 2>/dev/null; echo $?"
	set exitCode to do shell script cmd

	if exitCode is "0" then
		-- Successfully opened in existing server
		return
	else
		-- If emacsclient fails (no server), start a new Emacs with the files
		do shell script quoted form of emacs & fileArgs & " > /dev/null 2>&1 &"
	end if
end open

-- Handle opening app without files (e.g., from Dock)
on run
	set emacs to "/opt/homebrew/bin/emacs"
	do shell script quoted form of emacs & " &> /dev/null &"
end run
