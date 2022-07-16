use DBI;
use strict;

my $DBHOST   = $ENV{'DBHOST'}   || 'mysql-server';
my $database = $ENV{'DATABASE'} || 'join_check';
my $dsn = "dbi:mysql:database=${database};host=mysql-server-alpine;port=3306";
my $user = $ENV{'USER'} || 'java';
my $password = $ENV{'PASSWORD'} || 'password';
my @drvs = grep ( /mysq/, DBI->available_drivers );
print STDERR join $/, @drvs, '';
my $dbh =
  DBI->connect( $dsn, $user, $password, { RaiseError => 1, AutoCommit => 0 } );
my $sql = 'select VERSION()';
print STDERR 'test' . $/;

if ( $dbh->ping ) {
    print STDERR 'database is up ' . $/;

    my $sth = $dbh->prepare($sql)
      or die "failed to prepare statement $DBI::errstr";

    $sth->execute or die "failed to execute statement $DBI::errstr";

    my $row = $sth->fetch or die "failed to fetch data $DBI::errstr";

    print STDERR $row->[0] . $/;
}
else {
    print STDERR 'database is down' . $/;
}

$dbh->disconnect;
1;
