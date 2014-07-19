*
* $Id: hgnf.F,v 1.1.1.1 1996/01/16 17:07:39 mclareni Exp $
*
* $Log: hgnf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:39  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 26/01/94  13.46.18  by  Rene Brun
*-- Author :
      SUBROUTINE HGNF(IDN,IDNEVT,X,IERROR)
*.==========>
*.           Return in array X the variables of event IDNEVT
*.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcdire.inc"
      COMMON/QUEST/IQUEST(100)
      DIMENSION X(*)
      INTEGER   KEYS(2)
*.___________________________________________
*
      LC=LQ(LCIDN-1)
      NEVB=IQ(LC-1)/IQ(LCIDN+2)
      IBANK=(IDNEVT-1)/NEVB + 1
*
*           Load bank containing this event
*
      IF(IQ(LCIDN+11).EQ.0)THEN
         LC=LQ(LCIDN-3-IBANK)
      ELSE
         IF(IQ(LCIDN).EQ.IBANK.OR.IQ(LCIDN+6).EQ.0)GO TO 20
         IF(IBANK.LE.IQ(LCIDN+6))THEN
            LKEY=LQ(LC)
            IF(LKEY.GT.0)THEN
*-*  case where bank with keys serial numbers exist
               KEYS(1)=IQ(LKEY+IBANK)
               CALL HRZIN(IHDIV,LCIDN,-1,KEYS,99999,'RS')
            ELSE
*-*  case where bank with keys serial numbers does not exist
               IF(ICHTYP(ICDIR).EQ.1)THEN
                  KEYS(1) = IQ(LCIDN+5)+10000*IBANK
                  KEYS(2) = 0
               ELSE
                  KEYS(1) = IQ(LCIDN+5)
                  KEYS(2) = IBANK
               ENDIF
               CALL HRZIN(IHDIV,LCIDN,-1,KEYS,99999,'R')
               IF(IQUEST(1).NE.0)GO TO 90
            ENDIF
         ELSE
            IOFSET=IDN-IQ(LCIDN+5)
            CALL HDELET(IDN)
            CALL HRIN(IDN-IOFSET,99999,IOFSET)
            NIDN=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),IDN)
            LCIDN=LQ(LTAB-NIDN)
         ENDIF
         LC=LQ(LCIDN-1)
         IQ(LCIDN)=IBANK
      ENDIF
*
  20  IERROR=0
      IAD=IQ(LCIDN+2)*(IDNEVT-NEVB*(IBANK-1)-1)
      DO 30 I=1,IQ(LCIDN+2)
         X(I)=Q(LC+IAD+I)
  30  CONTINUE
      RETURN
*
*            Error
*
  90  IERROR=1
      END
