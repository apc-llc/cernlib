*
* $Id: csdvln.F,v 1.1.1.1 1996/02/26 17:16:21 mclareni Exp $
*
* $Log: csdvln.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:21  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/08 04/08/94  14.10.02  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSDVLN(KEYWD)
***--------------------------------------------
*       translater's routine
***--------------------------------------------
       INTEGER CSLTGB,CSLTGP,CSLTLI,CSLTGI,CSKIDN,CSITLI,CSITGP
#include "comis/cspar.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
      PARAMETER (  KLSFT=0,   KNDOL=10)
      PARAMETER (  KTINT=1,   KLINT=1, KTREAL=2,  KLREAL=1,
     +  KTCHAR=3,  KTLOG=4,   KLLOG=1, KTDOU=5 )
      COMMON /CSGSCM/IGSST,JGSST,NGSST,CSJUNK(3)
#include "comis/cstvrs.inc"
      NHDOLL=ICHAR('$')
      IF(KEYWD.EQ.9)THEN
*   EXTERNAL I,I...
        I=CSLTGI(IPVS)
        IF(I.NE.0)GO TO 701
        I=CSLTLI(IPVS)
        IF(I.NE.0)THEN
          IF(IABS(NUMGI).GT.1.OR.MODEGI.NE.0)GO TO 701
*                                            ---------
          IF(IABS(ITYPGI).EQ.KTCHAR)GO TO 711
*                                    --------
          MODEGI=-2
          IF(NUMGI.EQ.-1)THEN
            CALL CSRTLI(I)
            RETURN
          ENDIF
        ELSE
          MODEGI=-2
          ITYPGI=CSKIDN(JID,LENEGI)
        ENDIF
        IP=CSLTGP(IPVSP)
        IF(IP.EQ.0)THEN
           ITYPGP=ITYPGI
           IP=CSITGP(IPVSP)
        ELSE
*ITYPGP==ITYPGI?
           IF(ITYPGI.GT.0)ITYPGP=ITYPGI
           CALL CSRTGP(IP)
        ENDIF
        ISHGI=IP
        IF(I.NE.0)THEN
          CALL CSRTLI(I)
        ELSE
          I=CSITLI(IPVS)
        ENDIF
        RETURN
      ELSEIF(KEYWD.EQ.10)THEN
*    USE I,I...
        IF(NGSST.GT.0)THEN
          IF(MKCHAR(JGSST).EQ.NHDOLL)THEN
            CALL CCOPYS(JID,JID+1,5)
            CALL CSSETC(JID,1,NHDOLL)
            JGSST=JGSST+1
            NGSST=NGSST-1
          ENDIF
        ENDIF
        I=CSLTGB(IPVS)
        IF(I.EQ.0)GO TO 702
*                ----------
         NUMGB=I+KSADGB
         CALL CSBUSE(NUMGB)
      ELSEIF(KEYWD.EQ.11)THEN
*SAVE I,I,I
        CONTINUE
      ENDIF
      RETURN
 701  IGSST=-19
      RETURN
 702  IGSST=-2
      RETURN
 711  IGSST=-11
      END
