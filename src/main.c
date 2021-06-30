#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
//#  include "HeXournal_stub.h"
#else
#  error "GHC required"
#endif

#include <stdlib.h>
#include <argp.h>
#include <errno.h>
#include <stdbool.h>
#include "my_perl.h"

bool verbose = false;

static int exit_errno = 0;
static inline int exit_with_code(){
    switch(exit_errno){
        case 0:
            exit(EXIT_SUCCESS);
        case ENOMEM:
            fprintf(stderr, "[error] argp_parse returned ENOMEM\n");
            exit(EXIT_FAILURE);
        default:
            exit(EXIT_FAILURE);
    }
}
static void cleanup(){
    hs_exit();
    my_perl_free();
}

extern const struct argp argp; // see argp.c

int main(int argc, char *argv[]){
    // hs_init before argp_parse to parse out GHC's +/-RTS options
    hs_init(&argc, &argv);
    if(atexit(&cleanup)){
        exit_errno = ENOTRECOVERABLE;
        exit_with_code();
    }
    int argp_errno;
    if(argp_errno = argp_parse(&argp, argc, argv, 0, NULL, NULL)){
        exit_errno = argp_errno;
        exit_with_code();
    }
    if(my_perl_init()){
        fprintf(stderr, "[warning] perl failed to initialize\n");
    }
    // jump into HeXourna.hs
    if(exit_errno){
        fprintf(stderr, "[warning] exit_errno should not be set\n");
    }
    exit_with_code();
}
