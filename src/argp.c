#include <stdlib.h>
#include <argp.h>
#include <stdbool.h>

extern bool verbose;

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

enum{
    nil_key_ = 0x80, // ascii max (see `struct argp_option`)
    verbose_key,
};
static const struct argp_option options[] = {
#if 0
    {
        .name = NULL,
        .key = 0,
        .arg = NULL,
        .flags = OPTION_DOC,
        .doc = "misc1:",
        .group = 0
    },
#endif
    {
        .name = "verbose",
        .key = verbose_key,
        .arg = NULL,
        .flags = 0,
        .doc = "will do something sometime",
        .group = 0
    },
    {0}
};
static error_t parser(int key, char *arg, struct argp_state *state){
    switch(key){
        case verbose_key:
            verbose = true;
            break;
        case ARGP_KEY_ARG:
            break;
        case ARGP_KEY_ERROR:
        default:
            return ARGP_ERR_UNKNOWN;
    }
    return 0;
}
const struct argp argp = {
    .options = options,
    .parser = &parser,
    .args_doc = NULL,
    .doc = "Short description\vafter text",
    .children = NULL,
    .help_filter = NULL,
    .argp_domain = NULL
};
const char *argp_program_version = EXE_NAME ", version " VERSION "\n\n" LICENSE;
const char *argp_program_bug_address = BUG_ADDRESS;
