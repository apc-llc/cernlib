*
* $Id: hptit.F,v 1.2 1998/11/10 09:18:55 couet Exp $
*
* $Log: hptit.F,v $
* Revision 1.2  1998/11/10 09:18:55  couet
* - mods for Y2K
*
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 19/04/93  10.19.58  by  Rene Brun
*-- Author :
      SUBROUTINE HPTIT(ICAS,NUM,XMIN,XMAX)
*.==========>
*.             THIS ROUTINE PRINTS GLOBAL TITLE,TITLE OF HISTOGRAM,
*.            NATURE OF HISTOGRAM AND THE LIMITS IN CASE OF SLICES
*.            OR BANDES. 2 DIFFERENT FORMATS,PRINTER AND TTY.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcvers.inc"
      COMMON/HFORM/IA(127),IDU
#include "hbook/hcprin.inc"
#include "hbook/hcrlf.inc"
*.___________________________________________
      CALL VBLANK(IA,NCOLPA)
*
      IF(ICAS.EQ.3)THEN
         CALL UCTOH('PROJECTION X',IA,1,12)
      ELSEIF(ICAS.EQ.4)THEN
         CALL UCTOH('PROJECTION Y',IA,1,12)
      ELSEIF(ICAS.EQ.5)THEN
         CALL UCTOH('SLICE X',IA,1,7)
      ELSEIF(ICAS.EQ.6)THEN
         CALL UCTOH('SLICE Y',IA,1,7)
      ELSEIF(ICAS.EQ.7)THEN
         CALL UCTOH('BAND X',IA,1,6)
      ELSEIF(ICAS.EQ.8)THEN
         CALL UCTOH('BAND Y',IA,1,6)
      ENDIF
*
*             NO ENTRIES CASE
*
      IF(NOENT.EQ.2)THEN
         CALL HFORMA(2)
         IF(ICAS.LE.4)THEN
            WRITE(LOUT,8000)ID,(IA(I),I=1,12)
         ELSE
            WRITE(LOUT,9000)ID,(IA(I),I=1,12),NUM
         ENDIF
         GO TO 99
      ENDIF
*
*             PAGE EJECT OR NOT PAGE EJECT
*
      IF(KSQUEZ.NE.0)WRITE(LOUT,5000)
      IEJECT=1
      IF(KSQUEZ.NE.0)IEJECT=0
      IF(LGTIT.NE.0)THEN
         NWT=IQ(LGTIT-1)
         IF(NWT.GT.20)NWT=20
         CALL VBLANK(IA(20),80)
         DO 5 I=1,NWT
            K=20+4*(I-1)
            CALL UBLOW(IQ(LGTIT+I),IA(K),4)
   5     CONTINUE
         IF(IEJECT.EQ.0)WRITE(LOUT,1000)(IA(I),I=20,99)
         IF(IEJECT.NE.0)WRITE(LOUT,1500)CRLF,(IA(I),I=20,99)
         IEJECT=0
         DO 10 I=1,80
            J2=NWT*4+21-I
            IF(IA(J2).NE.IDG(41))GO TO 20
  10     CONTINUE
  20     CONTINUE
         DO 30 J1=20,J2
  30     IA(J1)=IDG(39)
         WRITE(LOUT,1000)(IA(I),I=20,99)
         CALL HFORMA(2)
      ENDIF
*
*
      IF(I1.NE.0)THEN
         J1=LCID+KTIT1
      ELSE
         J1=LCID+KTIT2
      ENDIF
      J2=IQ(LCID-1)+LCID
      NWTITL=J2-J1+1
      NWT=NWTITL
      NWLINE=20
      CALL VBLANK(IA(20),80)
      J3=NWLINE
      IF(NWT.LT.NWLINE)J3=NWT
      DO 35 I=1,J3
         K=20+4*(I-1)
         CALL UBLOW(IQ(J1+I-1),IA(K),4)
  35  CONTINUE
      IF(NHT.EQ.1)THEN
         IF(IEJECT.EQ.0)WRITE(LOUT,1000)(IA(I),I=20,99)
         IF(IEJECT.NE.0)WRITE(LOUT,1500)CRLF,(IA(I),I=20,99)
      ELSE
         IF(IEJECT.EQ.0)WRITE(LOUT,4000)(IA(I),I=20,99),IH,NHT
         IF(IEJECT.NE.0)WRITE(LOUT,4500)CRLF,(IA(I),I=20,99),IH,NHT
      ENDIF
*
  40  NWT=NWT-J3
      IF(NWT.GT.0)THEN
         CALL VBLANK(IA(20),80)
         J1=J1+J3
         J3=NWLINE
         IF(NWT.LT.NWLINE)J3=NWT
         DO 45 I=1,J3
            K=20+4*(I-1)
            CALL UBLOW(IQ(J1+I-1),IA(K),4)
  45     CONTINUE
         WRITE(LOUT,1000)(IA(I),I=20,99)
         GO TO 40
      ENDIF
*
      CALL HFORMA(2)
      WRITE(LOUT,2000)ID,(IA(I),I=1,12),CHDATE,NH
      CALL HFORMA(2)
*
      IF(XMIN.EQ.0..AND.XMAX.EQ.0.)GO TO 99
      IF(MOD(ICAS,2).EQ.0)THEN
         WRITE(LOUT,7000)(IA(I),I=1,12),NUM,XMIN,XMAX
         CALL HFORMA(2)
      ELSE
         WRITE(LOUT,7100)(IA(I),I=1,12),NUM,XMIN,XMAX
         CALL HFORMA(2)
      ENDIF
*
 1000 FORMAT(' ',80A1)
 1500 FORMAT(A,80A1)
 2000 FORMAT(' ','HBOOK',5X,'ID =',I10,13X,12A1,15X,'DATE  ',A10,
     +12X,'NO =',I6)
 4000 FORMAT(' ',80A1,5X,'PART',I2,' OF ',I2)
 4500 FORMAT(A,80A1,5X,'PART ' ,I2,' OF ',I2)
 5000 FORMAT(//)
 7000 FORMAT(' ',12A1,'NO =',I4,5X,'XMIN=',E12.4,2X,'XMAX=',E12.4)
 7100 FORMAT(' ',12A1,'NO =',I4,5X,'YMIN=',E12.4,2X,'YMAX=',E12.4)
 8000 FORMAT(' ','****** HBOOK   NO ENTRIES FOR HISTOGRAM ID=    ',
     1 I8,5X,12A1)
 9000 FORMAT(' ','****** HBOOK   NO ENTRIES FOR HISTOGRAM ID=    ',
     1 I8,5X,12A1,2X,'NO=',I6)
  99  RETURN
      END
