#include <stdlib.h>
#include <basic/Exit.h>

List<exit_func> omega::Exit_hooks;

void omega::Exit(int e) 
    {
    foreach(func, exit_func, omega::Exit_hooks, (*func)(e));

    exit(e);
    }
