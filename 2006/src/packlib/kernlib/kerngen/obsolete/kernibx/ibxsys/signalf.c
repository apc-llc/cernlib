/*
 * $Id: signalf.c,v 1.1.1.1 1996/02/15 17:54:45 mclareni Exp $
 *
 * $Log: signalf.c,v $
 * Revision 1.1.1.1  1996/02/15 17:54:45  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE SIGNALF
  CERN PROGLIB#         SIGNALF         .VERSION KERNIBX  1.03  910315
  ORIG. 12/03/91, JZ
  FORTRAN interface routine to sigaction    */
#include <stdio.h>
#include <signal.h>
#include <errno.h>
int signalf_(signum,funct,flag)
int  *signum, *flag;
      int   *funct;
{
      int  sigaction();
      int  istat, signo;

      struct mysig {
          int       sa_handler;
          sigset_t  sa_mask;
          int       sa_flags;
         };

      struct mysig newbuf;
      struct mysig oldbuf;

      signo = *signum;

      if (*flag < 0)          newbuf.sa_handler = *funct;
        else if (*flag == 0)  newbuf.sa_handler = (int)SIG_DFL;
        else if (*flag == 1)  newbuf.sa_handler = (int)SIG_IGN;
        else                  newbuf.sa_handler = *flag;

      newbuf.sa_flags   = 0;
      sigemptyset(&newbuf.sa_mask);

      istat = sigaction(signo,&newbuf,&oldbuf);
      if (istat == 0)        return oldbuf.sa_handler;
      return -errno;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_CCGEN_SIGNALF
#undef CERNLIB_CCGEN_SIGNALF
#endif
