#include <EXTERN.h>
#include <perl.h>
#include "my_perl.h"

// man perlembed
static bool is_my_perl_allocated = false;
static PerlInterpreter *my_perl;

int my_perl_init(){
    int argc = 0;
    char **argv = NULL;
#define N 3
    char *my_perl_argv[N+1] = {"", "-e", "use v5.32; use utf8;", NULL};
    PERL_SYS_INIT(&argc, &argv);
    my_perl = perl_alloc();
    PL_perl_destruct_level = 2;
    perl_construct(my_perl);
    if(perl_parse(my_perl, NULL, N, my_perl_argv, NULL)){
        goto error;
    }
    PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
    if(perl_run(my_perl)){
        goto error;
    }
    is_my_perl_allocated = true;
    return 0;
error:
    my_perl_free();
    return -1;
}

void my_perl_free(){
    if(is_my_perl_allocated){
        PL_perl_destruct_level = 2;
        perl_destruct(my_perl);
        my_perl_free(my_perl);
        PERL_SYS_TERM();
        is_my_perl_allocated = false;
    }
}
