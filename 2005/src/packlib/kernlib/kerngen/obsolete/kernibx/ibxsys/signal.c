/*
 * $Id: signal.c,v 1.1.1.1 1996/02/15 17:54:45 mclareni Exp $
 *
 * $Log: signal.c,v $
 * Revision 1.1.1.1  1996/02/15 17:54:45  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE SIGNAL
  CERN PROGLIB#         SIGNAL          .VERSION KERNIBX  1.02  900801
  ORIG. 20/07/90, JZ + RH
  FORTRAN interface routine to sigaction    */
#include <stdio.h>
#include <signal.h>
      int signal_(asigno,funct,aflag)
      long *asigno, *aflag;
      void  (**funct)();
{
      int  sigaction();
      int  istat, signo;

      struct sigaction {
          void   (*sa_handler)();
          sigset_t  sa_mask;
          int       sa_flags;
         };

      struct sigaction buf;

      signo = *asigno;
      buf.sa_handler = *funct;
      buf.sa_flags   = SA_RESTART;
      sigemptyset(&buf.sa_mask);

      istat = sigaction(signo,&buf,NULL);
      return istat;
}
/*> END <----------------------------------------------------------*/
