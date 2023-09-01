#!/usr/bin/perl

use strict;

# no strict 'subs';
use Getopt::Long;

BEGIN {
    use constant RELEASE => 0;
    use constant HOME    => (
        do { $_ = $ENV{HOME}; /\/([^\/]+)$/ }
    );

    # NOTE: cannot use $_ in the  below
    use constant SCRIPT_DIR => (
        do { $_ = `dirname $0`; chomp; $_ }
    );
    use constant RUN_DIR => (
        do { $_ = `pwd`; chomp; $_ }
    );
    if (RELEASE) {

# origin: https://stackoverflow.com/questions/5256901/can-a-perl-script-detect-whether-its-running-under-activestate-vs-strawberry-pe
        if ( ( defined &Win32::BuildNumber ) || ( defined &ActivePerl::BUILD ) )
        {
            # NOTE: the output not be visible when run in BEGIN block
            print 'This is ActivePerl ', &ActivePerl::BUILD, $/;
        }
        else {
            print 'This is Perl version ', ( $^V =~ /^v(.*)/ ), $/;
        }

    # another option is to scan @INC for some non-standard directories.
    # E.g.  when Perl is provisioned by Ansible
    # directories with 'ansbl' in the name may end up left over included in @INC

        # TODO: set extra lib path in RELEASE
    }
    else {
        unshift( @INC, SCRIPT_DIR );
        unshift( @INC, RUN_DIR );
    }
}
print join $/, map { "'" . $_ . "'" } @INC;
1;
