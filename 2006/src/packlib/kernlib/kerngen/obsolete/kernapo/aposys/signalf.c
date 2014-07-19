/*
 * $Id: signalf.c,v 1.1.1.1 1996/02/15 17:51:09 mclareni Exp $
 *
 * $Log: signalf.c,v $
 * Revision 1.1.1.1  1996/02/15 17:51:09  mclareni
 * Kernlib
 *
 */
#include "kernapo/pilot.h"
#if defined(CERNLIB_QSYSBSD)
/*>    ROUTINE SIGNALF
  CERN PROGLIB#         SIGNALF         .VERSION KERNAPO  1.22  910313
  ORIG. 11/03/91, JZ
  Fortran interface routine to sigvec
  (using sigvec rather than signal to be sure not to pick it
   from the Fortran interface library)

      IOLD = SIGNALF (ISIGNO, FUNC, IFLAG)

                   ISIGNO : signal number
                     FUNC : signal handler if IFLAG = -1
                    IFLAG : < 0 : FUNC is the handler
                            = 0 : use default action
                            = 1 : ignore signal
                            > 1 : adr returned by earlier call

          IOLD returns the previous handler and can be used
          to restore it later.
*/
#include <stdio.h>
#include <errno.h>
#if defined(CERNLIB_QX_SC)
long signalf_(signum, proc, flag)
#endif
#if defined(CERNLIB_QXNO_SC)
long signalf(signum, proc, flag)
#endif
      long *signum, *flag;
      long *proc;
{
      struct mysig {
          int sv_handler;
          int sv_mask;
          int sv_flags;   };

      struct mysig newbuf;
      struct mysig oldbuf;
      int  sigvec();
      int  siguse, istat;

      siguse = *signum;
/*    printf (" entry Signalf %d flag:  %d \n", siguse, *flag);  */

      if (*flag < 0)
        { newbuf.sv_handler = *proc;
/*        printf ("       proc, *proc: %x %x \n", proc, *proc);  */
        }
        else
        { newbuf.sv_handler = *flag;
/*        printf ("       mode: %x \n", *flag);  */
        }

      newbuf.sv_mask    = 0;
      newbuf.sv_flags   = 0;

/*    printf (" struct newbuf = %x %x %x\n",
              newbuf.sv_handler, newbuf.sv_mask, newbuf.sv_flags);   */

      istat = sigvec(siguse, &newbuf, &oldbuf);
/*    printf (" sigvec returns status %d \n", istat);  */

      if (istat == 0)   return oldbuf.sv_handler;
      return -errno;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_CCGEN_SIGNALF
#undef CERNLIB_CCGEN_SIGNALF
#endif
#ifdef CERNLIB_TCGEN_SIGNALF
#undef CERNLIB_TCGEN_SIGNALF
#endif
#endif
