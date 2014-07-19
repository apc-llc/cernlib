*
* $Id: kisndv.F,v 1.1.1.1 1996/03/08 15:32:55 mclareni Exp $
*
* $Log: kisndv.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.02/00 15/04/93  18.27.32  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   15/04/93
      SUBROUTINE KISNDV(CHVEC,ISTAT)
      CHARACTER*(*) CHVEC
*
* Send a vector to the Piaf server
*
#include "kuip/kcgen.inc"
#include "kuip/kcques.inc"
      CHARACTER CHMAIL*80

      CALL KUVECT(CHVEC,LLOW,LHIGH)
      IF(LLOW.EQ.0) THEN
         CHMAIL=' UNKNOWN VECTOR '//CHVEC
         CALL CZPUTA(CHMAIL,ISTAT)
      ELSE
         ITYPE=IQUEST(14)
         IF(ITYPE.EQ.1) THEN
*--- real vector
            CHMAIL='R'
            IOCH=3
         ELSE
            CHMAIL='I'
            IOCH=2
         ENDIF
         LENTOT=IQUEST(31)*IQUEST(32)*IQUEST(33)
         NUH=LENTOT
         IF(NUH.GT.400) NUH=400
         WRITE(CHMAIL(7:),'(4I10)') NUH,IQUEST(31),IQUEST(32),IQUEST(33)
         CALL CZPUTA(CHMAIL,ISTAT)
*--- FZ user header is restricted to 400 words
         DO 10 I=1,LENTOT,400
            NUH=LENTOT-I+1
            IF(NUH.GT.400) NUH=400
            CALL FZOUT(998,IXKUIP,0,0,'Z',IOCH,NUH,IQ(LLOW+I-1))
            ISTAT=IQUEST(1)
 10      CONTINUE
      ENDIF

      END
