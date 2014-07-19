*
* $Id: csctli.F,v 1.1.1.1 1996/02/26 17:16:21 mclareni Exp $
*
* $Log: csctli.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:21  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 02/11/94  12.06.56  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSCTLI(IPCE,IL,ISFTG,ITYPE,KEYWD)
***--------------------------------------------
*       translater's routine
***--------------------------------------------
       INTEGER CSLTGP
        PARAMETER (KDFA=70, KDFAS=71, KDFCA=72, KDFCAS=73)
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/cssysd.inc"
#include "comis/csdpvs.inc"
      PARAMETER (  KTINT=1,   KLINT=1, KTREAL=2,  KLREAL=1,
     +  KTCHAR=3,  KTLOG=4,   KLLOG=1, KTDOU=5 )
      PARAMETER (KSDIM=3)
      COMMON/CSGSCM/IGSST,JGSST,NGSST,CSJUNK(3)
#include "comis/cstvrs.inc"
       GO TO (965,965,966,967,967),KEYWD
*
 965  IF(MODEGI.GE.0 .AND. NUMGI.EQ.0)THEN
*         VAR OR ARR          LOCAL
        ITYPE=IABS(ITYPGI)
*cb        IF(ITYPE.EQ.KTCHAR)THEN
*cb          IF(KGLO.EQ.1)GO TO 712
*E                     ---------
*cb          IF(KGLO.EQ.0)IQ(NUMGB+1)=IQ(NUMGB+1)+2
*cb          KGLO=2
*cb        ELSE
*cb          IF(KGLO.EQ.2)GO TO 712
*E                     ---------
*cb          IF(KGLO.EQ.0)IQ(NUMGB+1)=IQ(NUMGB+1)+1
*cb          KGLO=1
*cb        ENDIF
        IF(MODEGI.GT.1)THEN
          IF(NDIM.GT.0)GO TO 701
*E                        ------
*  T - ARR  EL - VAR
          IQ(MODEGI)=LENEGI+KON2
          N=IQ(MODEGI+KSDIM)
          LENT=IQ(MODEGI+N+3)*LENEGI
        ELSE
          IF(NDIM.GT.0)THEN
*  T - VAR  EL - ARR
            IF(KDA.LE.0)GO TO 710
*E                      ---------
            INFVEC(1)=LENEGI+KON2
            INFVEC(2)=0
            L=NDIM+4
            MODEGI=MHLOC(L+1)+1
            CALL CCOPYA(INFVEC(1),IQ(MODEGI),L)
            LENT=NEL*LENEGI
          ELSE
*  VAR  VAR
            LENT=LENEGI
*cb            MODEGI=1
          ENDIF
        ENDIF
        NUMGI=NUMGB
*cb        ISHGI=-(ISFTG+1)
*add one line
        ISHGI=0
        IF(ITYPE.EQ.KTCHAR)THEN
          IF(LENEGI.EQ.0)GO TO 713
*                            -----
*cb          LENT=(LENT-1)/NBYTPW+1
        ENDIF
*cb        ISFTG=ISFTG+LENT
* TEST LENGTH OF BLOCK
*cb        CALL CSTLGB(NUMGB,ISFTG)
      ELSE
        GO TO 701
*E      ---------
      ENDIF
      CALL CSRTLI(IL)
      RETURN
*DIM
*cb 966  IF(MODEGI.NE.0 .OR. IABS(NUMGI).GT.1)GO TO 701
*         NOT VAR         NOT(DUMMY OR LOCAL) ------
 966  IF(MODEGI.NE.0)GO TO 701
      IF(KDA.LE.0 .AND.NUMGI.NE.-1) GO TO 710
*                                     ------
      ITYPE=IABS(ITYPGI)
      INFVEC(1)=LENEGI+KON2
      INFVEC(2)=0
      INFVEC(4)=NDIM
      L=NDIM+4
      MODEGI=MHLOC(L+1)+1
      CALL CCOPYA(INFVEC(1),IQ(MODEGI),L)
      CALL CSRTLI(IL)
      IF(NUMGI.EQ.-1)THEN
        IF(IPCE+4.GE.LAST)GO TO 727
*                           -------
        KOD(IPCE+1)=ISHGI
        KOD(IPCE+2)=MODEGI
        IF(KDA.EQ.0)THEN
          KOD(IPCE)=KDFAS
          IF(ITYPE.EQ.KTCHAR)THEN
            KOD(IPCE)=KDFCAS
            KOD(IPCE+3)=LENEGI
            IPCE=IPCE+1
          ENDIF
        ELSE
          KOD(IPCE)=KDFA
          IF(ITYPE.EQ.KTCHAR)THEN
            KOD(IPCE)=KDFCA
            KOD(IPCE+3)=LENEGI
            IPCE=IPCE+1
          ENDIF
        ENDIF
        IPCE=IPCE+3
      ENDIF
      RETURN
 967  IF(ITYPGI.GT.0)GO TO 701
*                     --------
      ITYPGI=ITYPE
      LENEGI=LENEL
      IF(NDIM.GT.0)GO TO 966
      IF((NUMGI.EQ.-2.OR.MODEGI.EQ.-2).AND.ITYPE.EQ.KTCHAR)GO TO 711
* ERR: NO CHAR FUN
      IF(MODEGI.EQ.-2)THEN
        IP=CSLTGP(IPVSP)
        IF(ITYPGP.LT.0)ITYPGP=ITYPE
        CALL CSRTGP(IP)
      ELSEIF(MODEGI.GT.1)THEN
        IQ(MODEGI)=LENEGI+KON2
      ENDIF
      CALL CSRTLI(IL)
      RETURN
 701  IGSST=-19
      RETURN
 710  IGSST=-10
      RETURN
 711  IGSST=-11
      RETURN
*cb 712  IGSST=-12
*cb      RETURN
 713  IGSST=-13
      RETURN
 727  IGSST=-27
      END
