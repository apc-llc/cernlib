/*
 * $Id: gethostn.c,v 1.1.1.1 1996/03/08 15:44:28 mclareni Exp $
 *
 * $Log: gethostn.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
#if defined(CERNLIB_TWG)
/*
 * return the node name in Wollongong TCP/IP.
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
  } itmlst = { len - 1, LNM$_STRING, node, &length, 0 };
  char *p = node;
 
  sys$trnlnm( 0, &tabnam, &lognam, 0, &itmlst );
 
  while( p[0] != '\0' && p[0] != ':' )
    p++;
  p[0] = '\0';
 
  return( 0 );
}
#endif
