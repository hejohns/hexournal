#!/usr/bin/env perl

use v5.28; # run perlver for actual minimum required version
use utf8;

use strict;
use warnings FATAL => qw(uninitialized);
use autodie;

use File::Basename qw(basename dirname fileparse);
use File::Find qw(find);
use File::Spec;
use Cwd qw(abs_path);

sub print_help{
    my $zero = basename $0;
    my $dir = dirname $0;
    my $help = <<~"EOF"
    Usage: ./$zero
    Description:
        this help message will print on all errrors
        ./$zero must be called with cwd = $dir
        (cause File::Find is broken...)

        header block must be contigously predicated by
        (is_comment | is_preserved_comment)
        see defintions below
    EOF
    ;
    print $help;
}

##############################
# need to set these correctly!
##############################
my $file_license_header = 'file-license-header.txt';
# avoid File::Spec->rel2abs
my $project_dir = abs_path(File::Spec->updir($0));
# map file extentions to \@header -> @header
my %map2comment = ( '.hs' => sub{$_ = shift; map {'-- ' . $_} @{$_}}
                  , '.c' => sub{$_ = shift; map {'// ' . $_} @{$_}}
);
my %is_comment = ( '.hs' => sub{shift =~ m/^--/}
                 , '.c' => sub{shift =~ m/^\/\//}
);
my %is_preserved_comment = ( '.hs' => sub{shift =~ m/^-- \*/}
                           , '.c' => sub{shift =~ m/^\/\/ \*/}
);
##############################

my @flh;

# see File::Find
sub wanted{
    # [warning] File::Find and File::Spec are very broken
    my ($filename, $dir, $file_extension) = fileparse($File::Find::name, (qr/\.hs/, qr/\.c/));
    unless(defined $file_extension and $file_extension ne ''){
        return 1;
    }
    # cause File::Find is so broken
    open(my $FH, '<:encoding(UTF-8)', $filename . $file_extension);
    my @file = <$FH>;
    my @preserved_comments;
    while(defined $file[0]){
        if($is_preserved_comment{$file_extension}($file[0])){
            push(@preserved_comments, shift @file);
        }
        elsif($is_comment{$file_extension}($file[0])){
            shift @file;
        }
        else{
            last;
        }
    }
    # cause File::Find is so broken
    (my $file_src_path = $File::Find::name) =~ s/.*\/src\/(.*)/$1/;
    say $file_src_path;
    my @file_src_path = ($file_src_path . "\n", "\n");
    my @header = ($map2comment{$file_extension}(\@file_src_path),
        @preserved_comments,
        $map2comment{$file_extension}(\@flh));
    my @file = (@header, @file);
    # remove trailing whitespace
    @file = map {$_ =~ s/\s*$//; $_ . "\n"} @file;
    close($FH);
    open($FH, '>:encoding(UTF-8)', $filename . $file_extension);
    print $FH @file;
    close($FH);
}

eval{
    open(my $flh_FH, '<:encoding(UTF-8)', $file_license_header);
    @flh = <$flh_FH>;
    find(\&wanted, qw(../src/));
};
if($@){
    &print_help();
}
