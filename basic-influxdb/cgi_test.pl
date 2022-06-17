use CGI;
my $query = new CGI;

my $filename = $query->param('data');
my $loadtype = $query->param('type');
my $new      = $query->param('new');

$CGI::UNLINK_TMP_FILES = 0;
if ( $loadtype =~ /send/ ) {
    my $upload_filehandle = $query->upload('data');
    if ($new) {
        {
            my $file_content = do { local $/; <$upload_filehandle> };
            print "do ingestion of $file_content\n";
            # original processing:
            # print to some local file handle
            # do print to console for demo
            print $file_content;
        }
    }
    else {
        while (<$upload_filehandle>) {

            # original processing:
            # print to some local file handle
            # do print to console for demo
            print;
        }
    }
}
