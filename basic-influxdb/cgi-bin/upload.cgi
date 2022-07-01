use CGI;
use Data::Dumper;

my $query = new CGI;

my $filename = $query->param('data');
my $loadtype = $query->param('type');
my $new      = $query->param('new');

$CGI::UNLINK_TMP_FILES = 0;
print STDERR Dumper({new=>$new, loadtype => $loadtype, filename=>$filename}), $/;
if ( $loadtype =~ /send/ ) {
    my $upload_filehandle = $query->upload('data');
    if ($new) {
        {
            my $file_content = do { local $/; <$upload_filehandle> };
            print STDERR "do ingestion of $file_content", $/;
            # original processing:
            # print to some local file handle
            # do print to console for demo
            print STDERR $file_content;
        }
    }
    else {
        while (<$upload_filehandle>) {

            # original processing:
            # print to some local file handle
            # do print to console for demo
            print STDERR;
        }
    }
    &result_page;
}

sub result_page {
print $query->header ( );
print <<END_HTML;
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "DTD/xhtml1-strict.dtd">
<html xmlns="https://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Thanks!</title>
<style type="text/css">
img {border: none;}
</style>
</head>
<body>
<p>Thanks for uploading data</p>
<p><img src="/upload/$filename" alt="data" /></p>
</body>
</html>
END_HTML
}
