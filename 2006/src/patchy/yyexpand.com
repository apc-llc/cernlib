$!
$! run yexpand with PLINAME set to 'p1'
$!
$ PLINAME=p1
$!
$ yexpand 'p2' 'p3'
$!
