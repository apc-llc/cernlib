/*
 * $Id: getarg.c,v 1.1.1.1 1996/02/15 17:53:30 mclareni Exp $
 *
 * $Log: getarg.c,v $
 * Revision 1.1.1.1  1996/02/15 17:53:30  mclareni
 * Kernlib
 *
 */
#include "kernnxt/pilot.h"
/*>    ROUTINE GETARG
  CERN PROGLIB#         GETARG          .VERSION KERNNXT  1.00  901105
  ORIG.  Written for NeXT OS 1.0 by Paul Kunz, SLAC, August 1990
 *
 * ==> iargc modified to give # arguments, not counting the 0'th. This
 * ==> is to be compatible with iargc on most other Unix machines.
 * ==> (F.Dittus, 92/09/22)
 *
 * getarg, iargc - return to Fortran a command line argument
 *
 * Usage:
 *
 *    character*N c
 *    integer i,j
 *    integer function iargc
 *
 *          call getarg(i,c)
 *          j = iargc
 *
 *    getarg returns the i-th command line argument of the current
 *    process, filled with trailing blanks as needed.
 *    The 0th command line argument is the full pathname of
 *    the application's executable file.
 *
 *    iargc returns the index of the last argument.
 *
 * restrictions:
 *    It is up to the FORTRAN programmer to provide
 *    a character array that is big enough.
 *
 * notes:
 *    The 3rd + 4th dummy arguments are passed implicitly
 *    by the Absoft compiler.
 */
 
#if defined(CERNLIB_QX_SC)
int iargc_()
#endif
#if defined(CERNLIB_QXNO_SC)
int iargc()
#endif
#if defined(CERNLIB_QXCAPT)
int IARGC()
#endif
{
  extern int NXArgc;     /* argc saved in crt0  */
 
      return (NXArgc)-1;
}
 
#if defined(CERNLIB_QX_SC)
int getarg_( iarg, carray, dummy, esize )
#endif
#if defined(CERNLIB_QXNO_SC)
int getarg( iarg, carray, dummy, esize )
#endif
#if defined(CERNLIB_QXCAPT)
int GETARG( iarg, carray, dummy, esize )
#endif
 
long *iarg;        /* pointer to fortran integer*4  */
char *carray;      /* pointer to fortran character array  */
int dummy;         /* dummy size for iarg  */
int esize;         /* size of fortran character array  */
 
{
  extern int    NXArgc;        /* argc saved in crt0  */
  extern char **NXArgv;        /* argv saved in crt0  */
  int i, j;
  char *c;
 
      i = *iarg;
      j = esize;
      if ( i >= 0 && i < NXArgc ) {
          c = NXArgv[i];
          while (*c) {    /* copy characters until null */
              *carray++ = *c++;
              j--;
          }
      }
      for ( ; j > 0 ; j-- )    /* add any trailing blanks needed */
          *carray++ = ' ';
}
/*> END <----------------------------------------------------------*/
