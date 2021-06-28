#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#  include "HeXournal_stub.h"
#else
#  error "GHC required"
#endif

#include <stdlib.h>
#include <argp.h>

const char *argp_program_version = "ver";
const char *argp_program_bug_address = "email";
static char doc[] = "something\nsomething2";

int main(int argc, char *argv[]){
    hs_init(&argv, &argv);
    // probably want to parse out ghc runtime options
    argp_parse(, argc, argv, , , );
    // jump into HeXourna.hs
    hs_exit();
    return EXIT_SUCCESS;
}
