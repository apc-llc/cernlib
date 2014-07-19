*
* $Id: csdio.F,v 1.1.1.1 1996/02/26 17:16:21 mclareni Exp $
*
* $Log: csdio.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:21  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/23 24/10/93  11.53.34  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSDIO(IPCE,IOWR)
***--------------------------------------------
*       translater's routine
***--------------------------------------------
      PARAMETER( KLACK=48, KDCV=49, KLAL=51,KAA=86,KACA=89,
     + KTVW=111, KIOV=151, KIOA=152, KSVDL=153,KSVCXL=196,
     + KAKV=162)

#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
*un+SEQ,CSTABPS.
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/csdpvs.inc"
#include "comis/cskucs.inc"
      PARAMETER ( KLCMLX=2 )
      COMMON/CSGSCM/IGSST,JGSST,NGSST,CSJUNK(3)
#include "comis/cstvrs.inc"
      CHARACTER*(KLENID) KUVNAME
      INTEGER CSKUVI
      GO TO(2111,2112,2113,2114,2111,2115),IB1
*            EXP  CON  VAR  EA   FUN  NA
      IGSST=-9
      RETURN
*EXP
 2111 IF(IT1.EQ.3)GO TO 721
*               -----------
      IF(IOWR.EQ.1)GO TO 2117
      KOD(IPCE+1)=LSFT
      KOD(IPCE+2)=KLAL
      KOD(IPCE+3)=LSFT
      IF(IT1.EQ.5)THEN
        KOD(IPCE)=KSVDL
        LSFT=LSFT+KDLEN
      ELSEIF(IT1.EQ.7)THEN
        KOD(IPCE)=KSVCXL
        LSFT=LSFT+KLCMLX
      ELSE
        KOD(IPCE)=KTVW
        LSFT=LSFT+1
      ENDIF
      IPCE=IPCE+4
      GO TO 2118
* CONSTANT
 2112 IF(IT1.NE.3)THEN
         KOD(IPCP)=KOD(IPCP)+1
      ELSE
        KOD(IPCP)=KLACK
          KOD(IPCE)=KDCV
        KOD(IPCE+1)=LSFT
        LSFT=LSFT+2
        IPCE=IPCE+2
      ENDIF
      IF(IOWR.EQ.1)GO TO 2117
      GO TO 2118
*VARIABLE
 2113 IF(IT1.NE.3)THEN
        KOD(IPCP)=KOD(IPCP)+1
      ELSE
        KOD(IPCP)=KOD(IPCP)+1
        KOD(IPCE)=KDCV
        KOD(IPCE+1)=LSFT
        LSFT=LSFT+2
        IPCE=IPCE+2
      ENDIF
      GO TO 2118
* EL.ARRAY
 2114 KOD(IPCP)=KOD(IPCP)+1
      IF(IT1.EQ.3)THEN
        KOD(IPCE)=KDCV
        KOD(IPCE+1)=LSFT
        LSFT=LSFT+2
        IPCE=IPCE+2
      ENDIF
      GO TO 2118
* NAME OF ARRAY
 2115 IDES=ISEM(ITS-2)
      CALL CSLDLI(IDES)
      IF(NUMGI.GT.0)THEN
        IF(IQ(NUMGI).EQ.JKUVBS)THEN
          CALL CSGTIDL(IDES,KUVNAME,NCKUV)
          IGSST=CSKUVI(KUVNAME)
          KOD(IPCE)=KAKV
          IPCE=IPCE+1
          KOD(IPCE)=IGSST
        ELSE
          KOD(IPCE)=KAA
        ENDIF
      ELSE
        KOD(IPCE)=KAA
      ENDIF
      IF(IT1.EQ.3)THEN
        IPCE=IPCE-7
        KOD(IPCE)=KACA
      ENDIF
      KOD(IPCE+1)=MODEGI
      IPCE=IPCE+2
      ITS=ITS-2
      KOD(IPCE)=KIOA
      KOD(IPCE+1)=IT1
      KOD(IPCE+2)=MODEGI
      IPCE=IPCE+3
      RETURN
 2117 IGSST=-1
      RETURN
 2118 KOD(IPCE)=KIOV
      KOD(IPCE+1)=IT1
      IPCE=IPCE+2
      RETURN
 721  IGSST=-21
      END
