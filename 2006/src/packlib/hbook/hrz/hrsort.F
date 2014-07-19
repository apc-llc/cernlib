*
* $Id: hrsort.F,v 1.1.1.1 1996/01/16 17:08:08 mclareni Exp $
*
* $Log: hrsort.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.12.30  by  Rene Brun
*-- Author :    Rene Brun   25/08/93
      SUBROUTINE HRSORT(CHOPT)
*.==========>
*.           To sort the current RZ directory in increasing order of IDs
*.           If option 'S' directory in memory saved on file
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcdire.inc"
      COMMON /ZEBQ/  IQFENC(4), LQZ(100)
      DIMENSION IQZ(92)
      EQUIVALENCE (IQZ(1),LQZ(9))
      COMMON /MZCA/  NQSTOR,NQOFFT(16),NQOFFS(16),NQALLO(16), NQIAM
     +,              LQATAB,LQASTO,LQBTIS, LQWKTB,NQWKTB,LQWKFZ
     +,              MQKEYS(3),NQINIT,NQTSYS,NQM99,NQPERM,NQFATA,NQCASE
     +,              NQTRAC,MQTRAC(48)
      EQUIVALENCE (KQSP,NQOFFS(1))
      COMMON/QUEST/IQUEST(100)
      CHARACTER*(*) CHOPT
*.___________________________________________
*
      CALL HUOPTC (CHOPT,'S',IOPTS)
      CALL RZCDIR(' ',' ')
*
*-- Get the pointers to the RZ directory
*-- WARNING!! LCDIRZ might point in a primary store different from /PAWC/
      LCDIRZ=IQUEST(11)
      IF(LCDIRZ.EQ.0)GO TO 99
*
      NKEYS = IQUEST(7)
      NWKEY = IQUEST(8)
      LK    = IQUEST(13)
*
*--   store list of IDs in temporary bank in working space
      CALL MZBOOK(IHWORK,LHWORK,LHWORK,1,'HWOR',0,0,NKEYS,2,0)
      DO 20 I=1,NKEYS
         KI=LK+(NWKEY+1)*(I-1)
         IQ(LHWORK+I)=IQZ(KQSP+LCDIRZ+KI+1)
   20 CONTINUE
*
*--  Sort IDs in increasing order
      NMODS=0
      DO 50 I=1,NKEYS-1
         KI=LK+(NWKEY+1)*(I-1)
         IPINT=IQZ(KQSP+LCDIRZ+KI)
         IF(JBIT(IQZ(KQSP+LCDIRZ+IPINT+1),4).NE.0)GO TO 50
         J =I+1
   30    CONTINUE
         IF(IQ(LHWORK+I).GT.IQ(LHWORK+J))THEN
            KJ=LK+(NWKEY+1)*(J-1)
            NMODS=NMODS+1
            IDS      =IQ(LHWORK+J)
            IQ(LHWORK+J)=IQ(LHWORK+I)
            IQ(LHWORK+I)=IDS
            IPINT=IQZ(KQSP+LCDIRZ+KJ)
            IQZ(KQSP+LCDIRZ+KJ ) =IQZ(KQSP+LCDIRZ+KI)
            IQZ(KQSP+LCDIRZ+KI ) =IPINT
            DO 40 II = 1,NWKEY
               IKD = IQZ(KQSP+LCDIRZ+KJ+II)
               IQZ(KQSP+LCDIRZ+KJ+II) = IQZ(KQSP+LCDIRZ+KI+II)
               IQZ(KQSP+LCDIRZ+KI+II) = IKD
   40       CONTINUE
         ENDIF
         IF(J.EQ.NKEYS)GO TO 50
         J=J+1
         GO TO 30
   50 CONTINUE
*
      CALL MZDROP(IHWORK,LHWORK,' ')
*
*--   save directory in memory on RZ file
      LHWORK=0
      IF(NMODS.EQ.0)GO TO 99
      IF(JBIT(IQZ(KQSP+LCDIRZ),1).NE.0)GO TO 99
      IF(IOPTS.NE.0)THEN
         IFLAG=0
         CALL RZMODS('HRSORT',IFLAG)
         IF(IFLAG.NE.0)GO TO 99
         CALL RZSAVE
      ENDIF
*
   99 END
