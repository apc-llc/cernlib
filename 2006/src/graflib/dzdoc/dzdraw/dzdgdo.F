*
* $Id: dzdgdo.F,v 1.1.1.1 1996/03/04 16:13:17 mclareni Exp $
*
* $Log: dzdgdo.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:17  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGDO(IXST,L,KEYVEC,CLINE,IFC,ILC,ICYCLE)
#include "dzdoc/bkwrp.inc"
*         From DZEBRA
#include "zebra/zbcdk.inc"
#include "zebra/zunit.inc"
#include "dzdprm.inc"
      INTEGER KEYVEC(2)
      CHARACTER*(*) CLINE
      IC1000 = 10000000
      IDVER1=IDVERS
      NCYCLE=-10
      CLINE=' '
      IFC=1
      ILC=1
2     CALL RZIN(IXST,L,2,KEYVEC,IC1000,'DC')
      IF (IQUEST(1).NE.0 .OR. L .EQ. 0)GOTO 5
      NWGEN=IQ(KQS+L+11)
*     WRITE(*,*)' NWGEN',NWGEN
      IP0 = L+KQS+1
      CALL DZDGGI(IQ(IP0),'VE',' ',CLINE,IFC,ILC)
      IF(IDVER1.EQ.0)GOTO 5
      IF(IC1000.LT.1000000)THEN
         IF(IQUEST(6).NE.IC1000)THEN
            IDVDOC=0
***  is this ok?
            GOTO 25
         ENDIF
      ENDIF
      IF(NCYCLE.LT.-1)NCYCLE=IQUEST(50)-1
*     only 1 cycle stored
      IF(NCYCLE.LT.0)THEN
         IF(IFC.EQ.0)THEN
            IFC=1
            ILC=1
         ENDIF
         GOTO 5
      ENDIF
25    IF(IDVER1.NE.0)THEN
*        no version tag present
         IF(IFC.LE.0)THEN
            IDVDOC=0
            GOTO 4
         ENDIF
*        convert to integer,remove dots
         IPC=IFC-1
         DO 3 IC=IFC,ILC
*           is it a number?
            IF(INDEXN(CLINE(IC:IC)).NE.0)THEN
               IPC=IPC+1
               CLINE(IPC:IPC)=CLINE(IC:IC)
            ENDIF
3        CONTINUE
         IF(IPC.GE.IFC)THEN
            CALL DZDCTI(CLINE(IFC:IPC),IDVDOC)
            ILC=IPC
         ELSE
            IDVDOC=0
         ENDIF
4         IF(IDVDOC.EQ.IDVER1)THEN
            ICYCLE=IQUEST(6)
            GOTO 5
         ELSE
            NCYCLE=NCYCLE-1
            IF(NCYCLE.LT.0)THEN
               ICYCLE=-1
               IF(IDVER1.GT.IDVDOC)THEN
*                 last cycle again
                  IC1000=10000000
                  IDVER1=0
               ELSE
                  GOTO 5
               ENDIF
            ELSE
               IC1000=IQUEST(6)-1
            ENDIF
            IF(L.NE.0)CALL MZDROP(IXST,L,'L')
            L=0
            GOTO 2
         ENDIF
      ENDIF
5     CONTINUE
      IF(IDVERS.EQ.0)ICYCLE=0
      END
***********************************************************************
