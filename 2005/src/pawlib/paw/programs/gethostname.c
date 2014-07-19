/*
 * return the node name
 */
#include <descrip.h>
#include <lnmdef.h>
int
gethostname( node, len )
	char *node;
	int len;
{
  $DESCRIPTOR( tabnam, "LNM$SYSTEM" );
  $DESCRIPTOR( lognam, "SYS$NODE" );
  int length = 0;
  struct {
    short buffer_length;
    short item_code;
    char  *buffer_address;
    int    *return_length;
    int    item_list_end;
  } itmlst;     /* Disabled auto initialization = { len - 1, 
		   LNM$_STRING, node, &length, 0 }; */
  char *p = node;

    /* Manual initialization code inserted by CRL on 931206 */
  itmlst.buffer_length   = (len - 1);
  itmlst.item_code       = LNM$_STRING;
  itmlst.buffer_address  = node;
  itmlst.return_length   = &length;
  itmlst.item_list_end   = 0;

  sys$trnlnm( 0, &tabnam, &lognam, 0, &itmlst );

  while( p[0] != '\\0' && p[0] != ':' )
    p++;
  p[0] = '\\0';

  return( 0 );
}
