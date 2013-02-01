{incl_app, rpsls}.
{excl_mods, [
   %%exclude the test files
 
]}.
{level, details}.
{incl_dirs_r, ["src", "test"]}. %%test all files recursively in this directory, its a list for several direcs
{src_dirs, rpsls, ["src", "test"]}. %%sourcefile for application specified in the second arg