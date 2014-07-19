*
* $Id: palunf.F,v 1.1.1.1 1996/03/01 11:38:41 mclareni Exp $
*
* $Log: palunf.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:41  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/00 27/10/93  10.25.15  by  Alfred Nathaniel
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PALUNF(LUN,IKASE,IFREE)
*
*             If ICASE=1 check if LUN is free
*             If ICASE=2 check if LUN is used
*             If ICASE=3 return first free unit starting at LUN
*
#include "hbook/hcunit.inc"
#include "paw/pawlun.inc"
      LOGICAL     PRNT
      CHARACTER*8 CHCASE(10)
      SAVE CHCASE
      DATA CHCASE/'FZIN    ','FZOUT   ','HBOOK   ','HIGZ    '
     +           ,'RZ      ','KUIP    ','COMIS   ','GKS     '
     +           ,'FORTRAN ','USER    '/
*________________________________________________________
*
      ICASE = IKASE
      IF(LUN.LE.0.OR.LUN.GT.128)THEN
         CALL HBUG('Invalid Logical unit number','PALUNF',0)
         IFREE=6
         IF(ICASE.NE.1)IFREE=0
         GO TO 99
      ENDIF
      IFREE=IABS(LUNIT(LUN))
      PRNT = .TRUE.
      IF (ICASE .LT. 0) THEN
         PRNT  = .FALSE.
         ICASE = -ICASE
      ENDIF
      IF(ICASE.EQ.1)THEN
         IF(IFREE.EQ.0)GO TO 99
         IFR=MIN(IFREE,10)
         CALL KUALFA
         IF (PRNT) WRITE(LOUT,1000)LUN,CHCASE(IFR)
      ELSEIF(ICASE.EQ.2)THEN
         IF(IFREE.NE.0)GO TO 99
         CALL KUALFA
         IF (PRNT) WRITE(LOUT,2000)LUN
      ELSE
         IFREE=0
         DO 10 I=LUN,99
            IF(LUNIT(I).EQ.0)THEN
               IFREE=I
               GO TO 99
            ENDIF
  10     CONTINUE
      ENDIF
*
  99  RETURN
 1000 FORMAT(' Unit ',I3,' already used by ',A)
 2000 FORMAT(' Unit ',I3,' is not active')
      END
