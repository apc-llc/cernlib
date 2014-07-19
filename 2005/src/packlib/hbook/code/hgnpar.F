*
* $Id: hgnpar.F,v 1.1.1.1 1996/01/16 17:07:39 mclareni Exp $
*
* $Log: hgnpar.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:39  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 09/10/92  14.52.30  by  Unknown
*-- Author :
      SUBROUTINE HGNPAR(IDN,CHROUT)
*.==========>
*.           Get address and parameters of Ntuple IDN.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      CHARACTER*(*) CHROUT
      INTEGER KEYS(2)
*.___________________________________________
*
      LCIDN=0
      NIDN=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),IDN)
      IF(NIDN.LE.0)THEN
         CALL HRIN(IDN,9999,0)
         NIDN=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),IDN)
         IF(NIDN.LE.0)THEN
            CALL HBUG('Unknown N-tuple',CHROUT,IDN)
            RETURN
         ENDIF
      ENDIF
      LCIDN=LQ(LTAB-NIDN)
      I4=JBIT(IQ(LCIDN+KBITS),4)
      IF(I4.EQ.0)THEN
         CALL HBUG('Not a N-tuple',CHROUT,IDN)
         RETURN
      ENDIF
      IF (IQ(LCIDN-2) .NE. 2) THEN
         CALL HBUG('New N-tuple, this routine works only for old '//
     +             'N-tuples',CHROUT,IDN)
         RETURN
      ENDIF
*
*          Make Reference links for memory-resident Ntuple
*
      NCHRZ=IQ(LCIDN+11)
      IF(NCHRZ.EQ.0)THEN
         NMORE=IQ(LCIDN+5)+3-IQ(LCIDN-3)
         IF(NMORE.GT.0)THEN
            CALL MZPUSH(IHDIV,LCIDN,NMORE,0,' ')
            LC=LQ(LCIDN-1)
            IF(IQ(LCIDN+5).GE.1)THEN
               DO 10 IB=1,IQ(LCIDN+5)
                  LQ(LCIDN-3-IB)=LC
                  LC=LQ(LC)
                  IF(LC.EQ.0)GO TO 999
   10          CONTINUE
            ENDIF
            GO TO 999
         ENDIF
      ENDIF
*
*          Make sure header is stored on disk
*
      LC=LQ(LCIDN-1)
      IF(JBIT(IQ(LC),1).NE.0)THEN
         CALL SBIT0(IQ(LC),1)
         KEYS(1) = IDN
         KEYS(2) = 0
         CALL HRZOUT(IHDIV,LCIDN,KEYS,ICYCLE,' ')
      ENDIF
*
  999 END
