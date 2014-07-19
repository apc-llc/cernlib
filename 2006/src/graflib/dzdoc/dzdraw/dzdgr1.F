*
* $Id: dzdgr1.F,v 1.1.1.1 1996/03/04 16:13:18 mclareni Exp $
*
* $Log: dzdgr1.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:18  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGR1(CL,IVAL,IREPC,NEXTRA,IVR,MV,NV)
      CHARACTER*(*) CL
      INTEGER IVAL,IREPC,NEXTRA,IVR(2),MV,NV, IFILIT
      IREPC=IVAL
      NC=LENOCC(CL)
      IP = 0
   10 IP=IP+1
*     need at least 2 characters
      IF(IP.GE.NC)GOTO 999
*     operator expected: + - * /
 
      IF(CL(IP:IP).EQ.' ')GOTO 10
      IF(CL(IP:IP).EQ.'!')GOTO 999
      CALL DZDGIV(CL(IP+1:80),IV,NCV)
      IF(IV.LE.0)GOTO 999
      NEXTRA=0
      IFILIT=0
      IF(MV.GT.0 .AND. NV.LT.MV)THEN
         NV=NV+1
         IFILIT=1
         IVR(NV)=IV
      ENDIF
      IF    (CL(IP:IP).EQ.'+')THEN
          IFLEXT=0
          IREPC=IREPC+IV
          IF(IFILIT.NE.0)IFILIT=1
      ELSEIF(CL(IP:IP).EQ.'-')THEN
         IREPC=IREPC-IV
         IFLEXT=0
         IF(IFILIT.NE.0)IFILIT=2
      ELSEIF(CL(IP:IP).EQ.'*')THEN
         IREPC=IREPC*IV
         IFLEXT=0
         IF(IFILIT.NE.0)IFILIT=3
      ELSEIF(CL(IP:IP).EQ.'/')THEN
         IREPCS=IREPC
         IREPC=IREPC/IV
          IF(IFILIT.NE.0)IFILIT=4
*         IREPC=FLOAT(IREPC)/FLOAT(IV)+0.5
*        is rep count integer?
         IF(IFLEXT.EQ.1)THEN
            NREST=IREPCS-IREPC*IV
            IF(IREPC.GT.0)THEN
               NEXTRA=NREST/IREPC
               IF(NEXTRA.NE.0)THEN
                  IF(NEXTRA*IREPC.NE.NREST)THEN
                     NEXTRA=-1000000
                     IREPC=-1
                  ELSE
                     IF(NEXTRA.LT.0)THEN
                        NEXTRA=-IREPCS/IREPC
                     ENDIF
                  ENDIF
               ENDIF
            ELSE
               IREPC=-1
            ENDIF
         ENDIF
      ELSE
         IREPC=-1
         GOTO 999
      ENDIF
      IF(IFILIT.NE.0)CALL SBYT(IFILIT,IVR(NV),25,8)
      IP=IP+NCV
      GOTO 10
  999 END
