/*
 * $Id: signalf.c,v 1.1.1.1 1996/02/15 17:54:21 mclareni Exp $
 *
 * $Log: signalf.c,v $
 * Revision 1.1.1.1  1996/02/15 17:54:21  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE SIGNALF
  CERN PROGLIB#         SIGNALF         .VERSION KERNSUN  1.05  910318
  ORIG. 12/03/91, JZ
  FORTRAN interface routine to signal    */
#include <stdio.h>
#include <signal.h>
#include <errno.h>
int signalf_(signum,funct,flag)
      int  *signum, *flag;
      int  *funct;
{
      int  signo, istat;
      int  handler;
      void *oldhand;


      signo = *signum;
/*    printf (" entry signalf fu=%x *fu=%x fl=%x *fl=%x\n",
                              funct, *funct, flag, *flag);  */

      if (*flag < 0)          handler = (int)funct;
        else if (*flag == 0)  handler = (int)SIG_DFL;
        else if (*flag == 1)  handler = (int)SIG_IGN;
        else                  handler = *flag;

/*    printf (" handler = %x\n", handler);   */

      oldhand = signal(signo,handler);
      istat   = (int)oldhand;
      if (istat >= 0)        return istat;
      return -errno;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_CCGEN_SIGNALF
#undef CERNLIB_CCGEN_SIGNALF
#endif
#ifdef CERNLIB_TCGEN_SIGNALF
#undef CERNLIB_TCGEN_SIGNALF
#endif
