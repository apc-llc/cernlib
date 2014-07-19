*
* $Id: igparf.F,v 1.1.1.1 1996/02/14 13:10:36 mclareni Exp $
*
* $Log: igparf.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:36  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/00 01/11/93  10.24.15  by  O.Couet
*-- Author :    G.Richards   06/12/91
      SUBROUTINE IGPARF(IARCH,CHOPT)
#if defined(CERNLIB_PHIGS)
#include "higz/hiatt.inc"
#endif
      CHARACTER*(*) CHOPT
#if defined(CERNLIB_PHIGS)
      IF (IDIM.NE.3) RETURN
*
      IF (CHOPT.EQ.'S') THEN
         CALL PSCNRS (2,2)
         CALL POPARF (1,IARCH)
         CALL PARAST (1)
         CALL PCLARF (1)
      ENDIF
*
      IF (CHOPT.EQ.'D') THEN
         CALL PSCNRS(1,1)
         CALL POPARF (1,IARCH)
         CALL PDASAR(1)
         CALL PCLARF (1)
      ENDIF
*
      IF (CHOPT.EQ.'R') THEN
         CALL PSCNRS(2,2)
         CALL POPARF (1,IARCH)
         CALL PQSID(IN,IERR,INUM,ISTRID)
         ISTCNT=101
         DO 10 I=1,INUM
            CALL PQSID(I,IERR,INUM,ISTRID)
            IF (ISTRID.GT.ISTCNT) ISTCNT=ISTRID
   10    CONTINUE
         ISTCNT=ISTCNT+1
         CALL PRAST (1)
         CALL PCLARF(1)
      ENDIF
#endif
      END
