#!/usr/bin/env perl

use v5.28; # run perlver for actual minimum required version
use utf8;

use strict;
use warnings FATAL => qw(uninitialized);
use autodie;

use File::Basename qw(basename fileparse);
use File::Find qw(find);
use File::Spec;
use Cwd qw(abs_path);

sub print_help{
my $zero = basename $0;
my $help = <<~"EOF"
Usage: ./$zero
Description:
    fill this out
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
my %map_header_lines = ( hs => sub{$_ = shift; map {'-- ' . $_} @{$_}}
);
my %delete_contiguous_match = ( hs => sub{$_ = shift; $_ =~ m/^-- /}
);
##############################

my @flh;

# see File::Find
sub wanted{
    my ($filename, $dir, $file_extension) = fileparse($File::Find::name, qw(.hs));
    $file_extension = substr $file_extension, 1;
    unless(defined $file_extension and $file_extension ne ''){
        return 1;
    }
    # cause File::Find is so broken
    open(my $FH, '<:encoding(UTF-8)', $filename . ".$file_extension");
    my @file = <$FH>;
    while(defined $file[0] and $delete_contiguous_match{$file_extension}($file[0])){
        shift @file;
    }
    # cause File::Find is so broken
    (my $file_src_path = $File::Find::name) =~ s/.*\/src\/(.*)/$1/;
    say $file_src_path;
    my @header = ($file_src_path . "\n", "\n", @flh);
    @header = $map_header_lines{$file_extension}(\@header);
    my @file = (@header, @file);
    close($FH);
    open($FH, '>:encoding(UTF-8)', $filename . ".$file_extension");
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
