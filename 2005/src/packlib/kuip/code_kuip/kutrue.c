/*
 * $Id: kutrue.c,v 1.2 1997/03/17 16:55:02 mclareni Exp $
 *
 * $Log: kutrue.c,v $
 * Revision 1.2  1997/03/17 16:55:02  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:32:52  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/03/08 15:32:58  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/16 17/08/94  12.26.34  by  Alfred Nathaniel*/
/*-- Author :    Alfred Nathaniel   14/06/92*/

#include "kuip/kuip.h"
#include "kuip/kfor.h"

static LOGICAL f77_true = ~0;
static LOGICAL f77_false = 0;

/*
 * set Fortran's notion of .TRUE. and .FALSE.
 */
void type_of_call Kutrue( LOGICAL *ftrue, LOGICAL *ffalse )
{
  f77_true = *ftrue;
  f77_false = *ffalse;
}

LOGICAL ku_true( int return_true )
{
  return return_true ? f77_true : f77_false;
}
