*
* $Id: hparnt.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hparnt.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/02 14/05/93  12.07.43  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/01/92
      SUBROUTINE HPARNT(IDN, CHROUT)
*.==========>
*.
*.           Get address and parameters of new Ntuple IDN.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*(*) CHROUT
*
      LCID = 0
      NIDN  = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),IDN)
      IF (NIDN .LE. 0) THEN
         CALL HRIN(IDN,9999,0)
         NIDN = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),IDN)
         IF (NIDN .LE. 0) THEN
            CALL HBUG('Unknown N-tuple',CHROUT,IDN)
            IDN = 0
            RETURN
         ENDIF
      ENDIF
      LCID = LQ(LTAB-NIDN)
      I4 = JBIT(IQ(LCID+KBITS),4)
      IF (I4 .EQ. 0) THEN
         CALL HBUG('Not a N-tuple',CHROUT,IDN)
         IDN = 0
         RETURN
      ENDIF
      IF (IQ(LCID-2) .NE. ZLINK) THEN
         CALL HBUG('Old N-tuple, this routine works only for new '//
     +             'N-tuples',CHROUT,IDN)
         IDN = 0
         RETURN
      ENDIF
*
*-- when disk resident N-tuple make sure that all the buffers
*-- and the header are flushed on disk
*
      IF (IQ(LCID+ZNPRIM) .GT. 0) THEN
         CALL HNBFWR(IDN)
         CALL HNHDWR(IDN)
      ENDIF
*
      END
