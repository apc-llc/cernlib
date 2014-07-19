*
* $Id: cskinq.F,v 1.1.1.1 1996/02/26 17:16:23 mclareni Exp $
*
* $Log: cskinq.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:23  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 17/02/94  16.35.22  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSKINQ(I)
***-----------------------
*    it is interpreter's command inquire
***-----------------------
*      if i=0 -> lunop  exist
*      if i=1 -> fileop exist
*      returns: i=1 if o.k.
*               i=0 if error
#include "comis/cspar.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
#include "comis/csopen.inc"
#include "comis/csfmt.inc"
#include "comis/csichv.inc"
      CHARACTER ACC*12, BLNK*8, DIR*8, FM*12, FMD*8, NAM*80,
     +          SEQ*8, UNFMD*8
      LOGICAL EXST,NMD,OD
      IF(I.EQ.0)THEN
        INQUIRE(UNIT=LUNOP,ACCESS=ACC,BLANK=BLNK,DIRECT=DIR,
     +   ERR=77,EXIST=EXST,FORM=FM,FORMATTED=FMD,IOSTAT=IST,
     +   NAME=NAM,NAMED=NMD,NEXTREC=NR,NUMBER=NUM,OPENED=OD,
     +   RECL=LRC,SEQUENTIAL=SEQ,UNFORMATTED=UNFMD          )
      ELSEIF(I.EQ.1)THEN
        INQUIRE(FILE=FILEOP,ACCESS=ACC,BLANK=BLNK,DIRECT=DIR,
     +   ERR=77,EXIST=EXST,FORM=FM,FORMATTED=FMD,IOSTAT=IST,
     +   NAME=NAM,NAMED=NMD,NEXTREC=NR,NUMBER=NUM,OPENED=OD,
     +   RECL=LRC,SEQUENTIAL=SEQ,UNFORMATTED=UNFMD          )
      ENDIF
      IF(JIOSTA.NE.0)IA(JIOSTA-JTOPA)=IST
      DO 55 K=1,14
        J=JADRLN(K,1)
        N=JADRLN(K,2)
        IF(J.EQ.0)GO TO 55
        GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14),K
 1      CONTINUE
        IF(N.LE.12)THEN
          CALL CCOPYS(MJSCHA(ACC),J,N)
        ELSE
*         put blanks
          CALL CSSETC(J,N,ICHBLN)
          CALL CCOPYS(MJSCHA(ACC),J,12)
        ENDIF
        GO TO 55
 2      CONTINUE
        IF(N.LE.8)THEN
          CALL CCOPYS(MJSCHA(BLNK),J,N)
        ELSE
*         put blanks
          CALL CSSETC(J,N,ICHBLN)
          CALL CCOPYS(MJSCHA(BLNK),J,8)
        ENDIF
        GO TO 55
 3      CONTINUE
        IF(N.LE.8)THEN
          CALL CCOPYS(MJSCHA(DIR),J,N)
        ELSE
*         put blanks
          CALL CSSETC(J,N,ICHBLN)
          CALL CCOPYS(MJSCHA(DIR),J,8)
        ENDIF
        GO TO 55
 4      CONTINUE
        IF(EXST)THEN
          IA(J-JTOPA)=1
        ELSE
          IA(J-JTOPA)=0
        ENDIF
        GO TO 55
 5      CONTINUE
        IF(N.LE.12)THEN
          CALL CCOPYS(MJSCHA(FM),J,N)
        ELSE
*         put blanks
          CALL CSSETC(J,N,ICHBLN)
          CALL CCOPYS(MJSCHA(FM),J,12)
        ENDIF
        GO TO 55
 6      CONTINUE
        IF(N.LE.8)THEN
          CALL CCOPYS(MJSCHA(FMD),J,N)
        ELSE
*         put blanks
          CALL CSSETC(J,N,ICHBLN)
          CALL CCOPYS(MJSCHA(FMD),J,8)
        ENDIF
        GO TO 55
 7      CONTINUE
        IF(N.LE.80)THEN
          CALL CCOPYS(MJSCHA(NAM),J,N)
        ELSE
*         put blanks
          CALL CSSETC(J,N,ICHBLN)
          CALL CCOPYS(MJSCHA(NAM),J,80)
        ENDIF
        GO TO 55
 8      CONTINUE
        IF(NMD)THEN
          IA(J-JTOPA)=1
        ELSE
          IA(J-JTOPA)=0
        ENDIF
        GO TO 55
 9      CONTINUE
          IA(J-JTOPA)=NR
        GO TO 55
 10     CONTINUE
          IA(J-JTOPA)=NUM
        GO TO 55
 11     CONTINUE
        IF(OD)THEN
          IA(J-JTOPA)=1
        ELSE
          IA(J-JTOPA)=0
        ENDIF
        GO TO 55
 12     CONTINUE
          IA(J-JTOPA)=LRC
        GO TO 55
 13     CONTINUE
        IF(N.LE.8)THEN
          CALL CCOPYS(MJSCHA(SEQ),J,N)
        ELSE
*         put blanks
          CALL CSSETC(J,N,ICHBLN)
          CALL CCOPYS(MJSCHA(SEQ),J,8)
        ENDIF
        GO TO 55
 14     CONTINUE
        IF(N.LE.8)THEN
          CALL CCOPYS(MJSCHA(UNFMD),J,N)
        ELSE
*         put blanks
          CALL CSSETC(J,N,ICHBLN)
          CALL CCOPYS(MJSCHA(UNFMD),J,8)
        ENDIF
 55   CONTINUE
      I=1
      DO 66 K=1,14
        JADRLN(K,1)=0
        JADRLN(K,2)=0
 66   CONTINUE
      JIOSTA=0
      RETURN
 77   CONTINUE
      I=0
      IF(JIOSTA.NE.0)IA(JIOSTA-JTOPA)=IST
      DO 88 K=1,14
        JADRLN(K,1)=0
        JADRLN(K,2)=0
 88   CONTINUE
      JIOSTA=0
      END
