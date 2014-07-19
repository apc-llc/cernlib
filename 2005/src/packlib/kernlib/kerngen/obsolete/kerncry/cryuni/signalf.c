/*
 * $Id: signalf.c,v 1.1.1.1 1996/02/15 17:52:43 mclareni Exp $
 *
 * $Log: signalf.c,v $
 * Revision 1.1.1.1  1996/02/15 17:52:43  mclareni
 * Kernlib
 *
 */
#include "kerncry/pilot.h"
/*>    ROUTINE SIGNALF
  CERN PROGLIB#         SIGNALF         .VERSION KERNCRY  1.16  911111
  ORIG. 16/10/91, J-Ph. Baud
  FORTRAN interface routine to sigaction    */
#include <signal.h>
int SIGNALF(signum,funct,flag)
      int  *signum, *flag;
      void (*funct)();
{
      int  sigaction();
      int  istat, signo;

      struct sigaction newbuf;
      struct sigaction oldbuf;

      signo = *signum;

      if        (*flag < 0)    newbuf.sa_handler = funct;
        else if (*flag == 0)   newbuf.sa_handler = SIG_DFL;
        else if (*flag == 1)   newbuf.sa_handler = SIG_IGN;
        else                   newbuf.sa_handler = (void (*)())*flag;

      newbuf.sa_flags   = 0;
      sigemptyset(&newbuf.sa_mask);

      istat = sigaction(signo,&newbuf,&oldbuf);
      if (istat == 0)        return (int)oldbuf.sa_handler;
      return -1;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_CCGEN_SIGNALF
#undef CERNLIB_CCGEN_SIGNALF
#endif
