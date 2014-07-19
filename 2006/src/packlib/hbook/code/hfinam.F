*
* $Id: hfinam.F,v 1.1.1.1 1996/01/16 17:07:36 mclareni Exp $
*
* $Log: hfinam.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 15/12/92  21.48.30  by  Rene Brun
*-- Author :
      SUBROUTINE HFINAM(IDD,FITN,NPAR)
*.==========>
*.           Stores names of parameters in fit bank
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
      CHARACTER*(*) FITN(*)
      CHARACTER*8 FIT1
*.___________________________________________
      CALL HFIND(IDD,'HFINAM')
      IF(LCID.LE.0)GO TO 99
      CALL HDCOFL
      IF(I1.EQ.0)THEN
         CALL HBUG('Routine can only be called for 1-D histos',
     +              'HFINAM',IDD)
         GO TO 99
      ENDIF
*
      CALL UCTOH('HFNA',IHFNA,4,4)
      IF(IQ(LCONT-2).EQ.0)GO TO 90
      LFUNC=LQ(LCONT-1)
      IF(LFUNC.EQ.0)GO TO 90
      IF(IQ(LFUNC-2).EQ.0)GO TO 90
      LHFIT=LQ(LFUNC-1)
      IF(LHFIT.EQ.0)GO TO 90
      L=LQ(LHFIT)
   5  IF(L.EQ.0)GO TO 90
      IF(IQ(L-4).EQ.IHFNA)THEN
         NP=MIN(NPAR,IQ(L-1)/2)
         DO 10 I=1,NP
            FIT1=FITN(I)
            CALL UCTOH(FIT1,IQ(L+2*I-1),4,8)
  10     CONTINUE
         GO TO 99
      ELSE
         L=LQ(L)
         GO TO 5
      ENDIF
*
*        Fit name bank does not exist
*
  90  CALL HBUG('Cannot store names','HFINAM',IDD)
  99  END
