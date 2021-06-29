#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
//#  include "HeXournal_stub.h"
#else
#  error "GHC required"
#endif

#include <stdlib.h>
#include <argp.h>
#include <errno.h>

#ifndef EXE_NAME
#  define EXE_NAME "hexournal"
#endif
#ifndef VERSION
#  define VERSION "0.20210628"
#endif
#ifndef BUG_ADDRESS
#  define BUG_ADDRESS "<hejohns@umich.edu>"
#endif
#define LICENSE \
    "License: GNU GPL version 2 or later <http://www.gnu.org/licenses/>\n" \
    EXE_NAME " comes with ABSOLUTELY NO WARRANTY\n" \
    "This is free software; you can redistribute and/or modify it under\n" \
    "the terms of the GPLv2+"

static const struct argp_option options[] = {
    {
        .name = "verbose",
        .key = 'v',
        .arg = NULL,
        .flags = OPTION_ARG_OPTIONAL,
        .doc = "some doc",
        .group = 0
    },
    {
        .name = NULL,
        .key = 0,
        .arg = NULL,
        .flags = 0,
        .doc = "test:",
        .group = 0
    },
    {0}
};
static error_t parser(int key, char *arg, struct argp_state *state){
    // TODO
    // we're gonna need to ARGP_NO_HELP and handle --help manually
    // so we can call argp_help
    // also usage and version etc
    // probably want to anyways for more specific help messages
    printf(":::%d\n", state->next);
    printf(":::%s\n", state->argv[state->next]);
    switch(key){
        case ARGP_KEY_ARG:
            printf("::%s\n", arg);
            break;
        case 'v':
        default:
            return ARGP_ERR_UNKNOWN;
    }
    return 0;
}
const struct argp argp = {
    .options = options,
    .parser = &parser,
    .args_doc = NULL,
    .doc = "before\vafter",
    .children = NULL,
    .help_filter = NULL,
    .argp_domain = NULL
};
const char *argp_program_version = EXE_NAME ", version " VERSION "\n\n" LICENSE;
const char *argp_program_bug_address = BUG_ADDRESS;

static int exit_errno = 0;

int main(int argc, char *argv[]){
    // hs_init before argp_parse to parse out GHC's +/-RTS options
    hs_init(&argc, &argv);
    int argp_errno;
    if(argp_errno = argp_parse(&argp, argc, argv, ARGP_NO_EXIT, NULL, NULL)){
        exit_errno = argp_errno;
        goto cleanup;
    }
    // jump into HeXourna.hs
    /* epilogue */
cleanup:
    hs_exit();
    switch(exit_errno){
        case 0:
            return EXIT_SUCCESS;
        case EINVAL:
            return EXIT_FAILURE;
        case ENOMEM:
            fprintf(stderr, "[error] argp_parse returned ENOMEM\n");
            return EXIT_FAILURE;
        default:
            return EXIT_FAILURE;
    }
}
