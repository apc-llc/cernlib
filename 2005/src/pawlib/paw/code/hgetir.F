*
* $Id: hgetir.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: hgetir.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/02 11/05/95  11.56.25  by  O.Couet
*-- Author :
      SUBROUTINE HGETIR(ID,CHR,IC1,IC2,KONE,IAXIS,ICRANG)
*
*         To decode histogram range encoded in CHR
*
#include "paw/quest.inc"
      CHARACTER*(*) CHR
      CHARACTER*14 CHNUM
*
      ICRANG = 0
      NCH = LENOCC(CHR)
      IF (NCH.LT.2) RETURN
      ICOL = INDEX(CHR,':')
      IF (ICOL.EQ.0) RETURN
*
*      Decode the lower limit of the range
*
      IF (ICOL.GT.1) THEN
         IREAL = INDEX(CHR(1:ICOL-1),'.')+INDEX(CHR(1:ICOL-1),'E')
     +          +INDEX(CHR(1:ICOL-1),'-')+INDEX(CHR(1:ICOL-1),'+')
         IF (IREAL.EQ.0) THEN
            CALL KICTOI(CHR(1:ICOL-1),IK1)
            IF (IQUEST(1).EQ.0) THEN
               IC1 = IK1
            ELSE
               GOTO 999
            ENDIF
         ELSE
            CHNUM = ' '
            CHNUM (16-ICOL:) = CHR(1:ICOL-1)
            READ (CHNUM,'(G14.7)',ERR=999) RVAL
            CALL HVTOB(ID,RVAL,IC1,IAXIS)
         ENDIF
      ENDIF
*
*      Decode the upper limit of the range
*
      IF (ICOL.LT.NCH) THEN
         IREAL = INDEX(CHR(ICOL+1:NCH),'.')+INDEX(CHR(ICOL+1:NCH),'E')
     +          +INDEX(CHR(ICOL+1:NCH),'-')+INDEX(CHR(ICOL+1:NCH),'+')
         IF (IREAL.EQ.0) THEN
            CALL KICTOI(CHR(ICOL+1:NCH),IK2)
            IF (IQUEST(1).EQ.0) THEN
               IC2 = IK2
            ELSE
               GOTO 999
            ENDIF
         ELSE
            CHNUM = ' '
            CHNUM (15+ICOL-NCH:) = CHR(ICOL+1:NCH)
            READ (CHNUM,'(G14.7)',ERR=999) RVAL
            CALL HVTOB(ID,RVAL,IC2,IAXIS)
         ENDIF
      ENDIF
*
*      If a valid range has been found, ICRANG is set to 1
*
      ICRANG=1
*
  999 END
