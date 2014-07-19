*
* $Id: hcntpaw.inc,v 1.1.1.1 1996/01/16 17:07:52 mclareni Exp $
*
* $Log: hcntpaw.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:52  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCNTPAW_INC
#define CERNLIB_HBOOK_HCNTPAW_INC
*
*
* hcntpaw.inc
*
*CMZ :  4.21/10 01/03/94  11.54.50  by  Rene Brun
*-- Author :    Fons Rademakers   10/04/93
*
*-- ATTENTION: MAXCR4 must be 50007 in order for variables in $HCBONT to
*--            fit into PAWCR4. When changing any of these parameters
*--            check also $HCBONT.
*
      PARAMETER      (MAXCR8 = 1000, MAXCR4 = 50007, MAXC32 = 1000)
      REAL*8          RVAR8(MAXCR8)
      REAL            RVAR4(MAXCR4)
      INTEGER         IVAR4(MAXCR4)
      LOGICAL         LVAR4(MAXCR4)
      CHARACTER*32    CVAR32(MAXC32)
*
      COMMON /PAWCR8/ RVAR8
      COMMON /PAWCR4/ RVAR4
      COMMON /PAWC32/ CVAR32
*
      EQUIVALENCE (IVAR4, RVAR4)
      EQUIVALENCE (LVAR4, RVAR4)
*

#endif
