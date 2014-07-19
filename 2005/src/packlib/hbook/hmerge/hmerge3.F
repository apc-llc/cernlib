*
* $Id: hmerge3.F,v 1.1.1.1 1996/01/16 17:08:10 mclareni Exp $
*
* $Log: hmerge3.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:10  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/04 07/06/94  09.07.24  by  Rene Brun
*-- Author :    Rene Brun         26/05/94
      SUBROUTINE HMERGE3(IDN,IOFSET,IDNEVT,X,IERROR)
*.==========>
*.           Return in array X the variables of event IDNEVT
*.
*..=========> ( R.Brun )
*
#include "hbook/hcbook.inc"
#include "hbook/hcdire.inc"
*
      COMMON/QUEST/IQUEST(100)
      DIMENSION KEYS(2)
      DIMENSION X(*)
*.___________________________________________
*
      LCIDN=0
      NIDN=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),IDN)
      IF(NIDN.LE.0)THEN
         CALL HRIN(IDN-IOFSET,99999,IOFSET)
         NIDN=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),IDN)
         IF(NIDN.LE.0)GO TO 90
      ENDIF
      LCIDN=LQ(LTAB-NIDN)
      IF(LCIDN.LE.0)GO TO 90
*
      IF(IDNEVT.LE.0)GO TO 90
      IF(IDNEVT.GT.IQ(LCIDN+3))THEN
         IERROR=-1
         RETURN
      ENDIF
      LC=LQ(LCIDN-1)
      NEVB=IQ(LC-1)/IQ(LCIDN+2)
      IBANK=(IDNEVT-1)/NEVB + 1
*
*           Load bank containing this event
*
      IF(IQ(LCIDN+11).EQ.0)THEN
         DO 10 I=2,IBANK
            IF(LQ(LC).NE.0)LC=LQ(LC)
  10     CONTINUE
      ELSE
         IF(IQ(LCIDN).EQ.IBANK.OR.IQ(LCIDN+6).EQ.0)GO TO 20
         IF(IBANK.LE.IQ(LCIDN+6))THEN
            IF(ICHTYP(ICDIR).EQ.1)THEN
               KEYS(1) = IDN+10000*IBANK-IOFSET
               KEYS(2) = 0
            ELSE
               KEYS(1) = IDN-IOFSET
               KEYS(2) = IBANK
            ENDIF
            CALL HRZIN(IHDIV,LCIDN,-1,KEYS,99999,'R')
            IF(IQUEST(1).NE.0)GO TO 90
         ELSE
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
