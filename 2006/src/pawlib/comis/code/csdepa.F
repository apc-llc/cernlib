*
* $Id: csdepa.F,v 1.1.1.1 1996/02/26 17:16:21 mclareni Exp $
*
* $Log: csdepa.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:21  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/23 24/10/93  11.54.27  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSDEPA(IPCE,IPCB,NPARAM,IOST,IKEA,IKCHSS)
***--------------------------------------------
*       translater's routine
***--------------------------------------------
       INTEGER CSJTJP
      PARAMETER( KLK=45, KLAK=46, KLACK=48,  KDCV=49, KLAL=51 )
      PARAMETER( KLAP=55, KAA=86, KACA=89, KCONT=109, KTVW=111)
      PARAMETER( KSVDL=153, KSVCXL=196, KAKV=162)
      PARAMETER ( KLCMLX=2 )
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
*un+SEQ,CSTABPS.
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/csdpvs.inc"
#include "comis/cskucs.inc"
      COMMON/CSGSCM/IGSST,JGSST,NGSST,CSJUNK(3)
#include "comis/cstvrs.inc"
      CHARACTER*(KLENID) KUVNAME
      INTEGER CSKUVI
      IPOPT=0
      GO TO(2024,2025,2026,2027,2028,2029,2030,2031,2033),IB1
*            EXP  CON  VAR  EA   FUN  NA   NF   BL  SKIP
      GO TO 709
*      --------
*EXP
 2024 IF(IT1.EQ.3)GO TO 721
*                -----------
      KOD(IPCE+1)=LSFT
      KOD(IPCE+2)=KLAL
      KOD(IPCE+3)=LSFT
      IF(IT1.EQ.5)THEN
        KOD(IPCE)=KSVDL
#if defined(CERNLIB_ALIGN)
        IF(MOD(LSFT,2).EQ.1)THEN
          LSFT=LSFT+1
          KOD(IPCE+1)=LSFT
          KOD(IPCE+3)=LSFT
        ENDIF
#endif
        LSFT=LSFT+KDLEN
      ELSEIF(IT1.EQ.7)THEN
        KOD(IPCE)=KSVCXL
        LSFT=LSFT+KLCMLX
      ELSE
        KOD(IPCE)=KTVW
        LSFT=LSFT+1
      ENDIF
      IPCE=IPCE+4
*      ITS=ITS-2
      GO TO 2032
* CONSTANT
 2025 IF(IT1.EQ.3)THEN
        KOD(IPCP)=KLACK
        KOD(IPCE)=KDCV
        KOD(IPCE+1)=LSFT
        LSFT=LSFT+2
        IPCE=IPCE+2
#if defined(CERNLIB_ALIGN)
      ELSEIF(IT1.EQ.5)THEN
        IF(MOD(IPCP,2).EQ.1)THEN
          L=IPCE-IPCP
          CALL CCOPYA(KOD(IPCP),KOD(IPCP+1),L)
          KOD(IPCP)=KCONT
          IPCP=IPCP+1
          IPCE=IPCE+1
        ENDIF
        KOD(IPCP)=KOD(IPCP)+1
#endif
      ELSE
        KOD(IPCP)=KOD(IPCP)+1
      ENDIF
      IPOPT=1
*      ITS=ITS-2
      GO TO 2032
*VARIABLE
 2026 IF(NUMGI.EQ.0)THEN
        IPOPT=1
        IF(IT1.EQ.3)IPOPT=IKCHSS
      ELSEIF(NUMGI.GT.0)THEN
        IPOPT=1
        IF(IQ(NUMGI+1).GE.3)IPOPT=0
        IF(IT1.EQ.3 .AND. IPOPT.NE.0)IPOPT=IKCHSS
      ENDIF
      IF(IT1.NE.3)THEN
        KOD(IPCP)=KOD(IPCP)+1
      ELSE
        KOD(IPCP)=KOD(IPCP)+1
        KOD(IPCE)=KDCV
        KOD(IPCE+1)=LSFT
        LSFT=LSFT+2
        IPCE=IPCE+2
      ENDIF
*      ITS=ITS-2
      GO TO 2032
* EL.ARRAY
 2027 IF(IKEA.NE.0)THEN
*        IDES=ISEM(ITS-4)
        CALL CSLDLI(IDES)
        IF(NUMGI.EQ.0)THEN
          IPOPT=1
        ELSEIF(NUMGI.GT.0)THEN
          IF(IQ(NUMGI+1).LT.3)IPOPT=1
        ENDIF
      ENDIF
      KOD(IPCP)=KOD(IPCP)+1
      IF(IT1.EQ.3)THEN
        KOD(IPCE)=KDCV
        KOD(IPCE+1)=LSFT
        LSFT=LSFT+2
        IPCE=IPCE+2
      ENDIF
      GO TO 2032
*#F(X)
 2028 GO TO 2024
* NAME OF ARRAY
 2029 IDES=ISEM(ITS-4)
      CALL CSLDLI(IDES)
      IF(NUMGI.EQ.0)THEN
        IPOPT=1
      ELSEIF(NUMGI.GT.0)THEN
        IF(IQ(NUMGI+1).LT.3 .AND.IQ(NUMGI).NE.JKUVBS)IPOPT=1
      ENDIF
      IF(NUMGI.GT.0 .AND. IT1.NE.3)THEN
        IF(IQ(NUMGI).EQ.JKUVBS)THEN
          CALL CSGTIDL(IDES,KUVNAME,NCKUV)
          IGSST=CSKUVI(KUVNAME)
          KOD(IPCE)=KAKV
          KOD(IPCE+1)=IGSST
          KOD(IPCE+2)=MODEGI
          IPCE=IPCE+3
          IF(IOST.NE.0)THEN
            KOD(IPCE)=KTVW
            KOD(IPCE+1)=LSFT
            LSFT=LSFT+1
            KOD(IPCE+2)=KLK
            KOD(IPCE+3)=MODEGI
            IPCE=IPCE+4
          ENDIF
        ELSE
          KOD(IPCE)=KAA
          IF(IOST.NE.0)KOD(IPCE)=KLK
          KOD(IPCE+1)=MODEGI
          IPCE=IPCE+2
        ENDIF
      ELSEIF(IT1.EQ.3)THEN
        IPCE=IPCE-7
        KOD(IPCE)=KACA
        IF(IOST.NE.0)KOD(IPCE)=KLK
        KOD(IPCE+1)=MODEGI
        IPCE=IPCE+2
      ELSE
        KOD(IPCE)=KAA
        IF(IOST.NE.0)KOD(IPCE)=KLK
        KOD(IPCE+1)=MODEGI
        IPCE=IPCE+2
      ENDIF
      CALL CCOPYA(ISEM(ITS-2),ISEM(ITS-4),2)
      ITS=ITS-2
      GO TO 2032
* NAME OF FUN
 2030 KOD(IPCE)=KLK
      MODEGI=ISEM(ITS-4)
      IF(MODEGI.LT.0)THEN
        KOD(IPCE)=KLAP
        MODEGI=-MODEGI
      ENDIF
      KOD(IPCE+1)=MODEGI
      IPCE=IPCE+2
      CALL CCOPYA(ISEM(ITS-2),ISEM(ITS-4),2)
      ITS=ITS-2
      GO TO 2032
*[BLOCK]
 2031 CONTINUE
*
*
        GO TO 2032
 2033 KOD(IPCE)=KLAK
      KOD(IPCE+1)=0
      IPCE=IPCE+2
*     GO TO 2032
 2032 ITS=ITS-2
      ISEM(ITS-3)=ISEM(ITS-3)+1
      I=IPCE-IPCB
      IF(NPARAM.EQ.0)THEN
        J=CSJTJP(ISEM(ITS-6),IPCE)
        IF(J.LT.0)GO TO 727
*                 ---------
      ELSEIF(IOST.NE.0)THEN
        IF(ISEM(ITS-1).EQ.10 .OR. ISEM(ITS-1).EQ.11)THEN
          J=CSJTJP(ISEM(ITS-6),IPCE)
          IF(J.LT.0)GO TO 727
*                 ---------
        ENDIF
      ENDIF
      ISEM(ITS-5)=IB1*100+IT1
      IF(IPOPT.NE.0)ISEM(ITS-5)=-(IB1*100+IT1)
      ISEM(ITS-6)=I
      RETURN
 709  IGSST=-9
      RETURN
 721  IGSST=-21
      RETURN
 727  IGSST=-27
      END
