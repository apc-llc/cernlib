/*
 * $Id: signalf.c,v 1.1.1.1 1996/02/15 17:50:56 mclareni Exp $
 *
 * $Log: signalf.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:56  mclareni
 * Kernlib
 *
 */
#include "kernalt/pilot.h"
/*>    ROUTINE SIGNALF
  CERN PROGLIB#         SIGNALF         .VERSION KERNSGI  1.04  930120
  ORIG. 24/05/93, JZ
  FORTRAN interface routine to sigvec    */
#include <signal.h>
 
int signalf_(signum,funct,flag)
      int  *signum, *flag;
      int  (*funct)();
{
      int  istat, signo;
 
      struct sigvec newbuf;
      struct sigvec oldbuf;
 
      signo = *signum;
 
      if        (*flag < 0)    newbuf.sv_handler = funct;
        else if (*flag == 0)   newbuf.sv_handler = SIG_DFL;
        else if (*flag == 1)   newbuf.sv_handler = SIG_IGN;
        else                   newbuf.sv_handler = (int (*)())*flag;
 
      newbuf.sv_flags = 0;
      newbuf.sv_mask  = 0;
 
      istat = sigvec(signo,&newbuf,&oldbuf);
      if (istat == 0)        return (int)oldbuf.sv_handler;
      return -1;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_CCGEN_SIGNALF
#undef CERNLIB_CCGEN_SIGNALF
#endif
#ifdef CERNLIB_CCGEN_SIGBSD
#undef CERNLIB_CCGEN_SIGBSD
#endif
#ifdef CERNLIB_CCGEN_SIGPOSIX
#undef CERNLIB_CCGEN_SIGPOSIX
#endif
