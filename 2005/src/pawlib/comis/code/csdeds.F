*
* $Id: csdeds.F,v 1.1.1.1 1996/02/26 17:16:21 mclareni Exp $
*
* $Log: csdeds.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:21  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 01/10/93  10.58.11  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSDEDS(I)
***--------------------------------------------
*       translater's routine
***--------------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
#include "comis/cssysd.inc"
      PARAMETER (  KLSFT=0,   KNDOL=10)
      PARAMETER (  KTINT=1,   KLINT=1, KTREAL=2,  KLREAL=1,
     +  KTCHAR=3,  KTLOG=4,   KLLOG=1, KTDOU=5 )
      PARAMETER (KSDIM=3)
      COMMON /CSGSCM/IGSST,JGSST,NGSST,CSJUNK(3)
#include "comis/cstvrs.inc"
#if defined(CERNLIB_ALIGN)
      CHARACTER*(KLENID) NAMEDP
#endif
 801  IF(I.EQ.0)RETURN
      CALL CSLDLI(I)
      IF(NUMGI.GE.0 .AND. MODEGI.GE.0)THEN
*                VAR OR ARR
        IF(ISHGI.LT.0)THEN
          IF(LEQUGI.EQ.0)THEN
            IF(NUMGI.GT.0)THEN
* SET ADDRESS for global
              ISHGI=-(ISHGI+1)
#if defined(CERNLIB_ALIGN)
              IF(IABS(ITYPGI).EQ.KTDOU)THEN
                IF(MOD(ISHGI,2).EQ.1)THEN
                  CALL CSGTIDL(I,NAMEDP,NC)
                  PRINT *,' Non aligned double ',NAMEDP
                ENDIF
              ENDIF
#endif
            ELSE
* SET ADDRESS for local
              IF(MODEGI.GT.1)THEN
* ARR
                N=IQ(MODEGI+KSDIM)
                LENT=LENEGI*IQ(MODEGI+N+3)
              ELSE
* VAR
                LENT=LENEGI
              ENDIF
              IF(IABS(ITYPGI).EQ.KTCHAR)THEN
                LENT=(LENT-1)/NBYTPW+1
#if defined(CERNLIB_ALIGN)
              ELSEIF(IABS(ITYPGI).EQ.KTDOU)THEN
                IF(MOD(LSFT,2).EQ.1)LSFT=LSFT+1
#endif
              ENDIF
              ISHGI=LSFT
              LSFT=LSFT+LENT
            ENDIF
            CALL CSRTLI(I)
          ELSE
* EQU CHAIN
            IF(NUMGI.GT.0)THEN
* EQU WITH GLOBAL
              KGLO=NUMGI
              IBOFS=-(ISHGI+1+IOFSGI)
            ELSE
*    PASS 1
              KGLO=0
              KMIN=IOFSGI
              IQEL=LEQUGI
 802          IF(I.NE.IQEL)THEN
                CALL CSLDLI(IQEL)
                IF(NUMGI.GT.0)THEN
                  KGLO=NUMGI
                  IBOFS=-(ISHGI+1+IOFSGI)
*                 STOP PASS 1!!
                ELSE
                  KMIN=MIN0(KMIN,IOFSGI)
                  IQEL=LEQUGI
                  GO TO 802
                ENDIF
              ENDIF
            ENDIF
* SECOND PASS EQU.CHAIN
            IQEL=I
            IF(KGLO.GT.0)THEN
* SET ADDRESS GLOBAL VARS
 803          CALL CSLDLI(IQEL)
              IF(NUMGI.GT.0)THEN
                KA=-ISHGI-1
                ISHGI=IBOFS+IOFSGI
                IF(KA.NE.ISHGI .OR. KGLO.NE.NUMGI)GO TO 706
*                                                -------
* ERROR IN EQU
              ELSE
                ISHGI=IBOFS+IOFSGI
                NUMGI=KGLO
                IF(ISHGI.LT.0)GO TO 706
* NEG ADDRESS IN GLOBAL
              ENDIF
              LENT=LENEGI
              IF(MODEGI.GT.1)THEN
                N=IQ(MODEGI+KSDIM)
                LENT=LENT*IQ(MODEGI+N+3)
              ELSE
                MODEGI=1
              ENDIF
* NOT YET     IF(ITYPGI.EQ.KTCHAR)LENT=(LENT-1)/NBYTPW+1
              CALL CSTLGB(NUMGI,ISHGI+LENT)
#if defined(CERNLIB_ALIGN)
              IF(IABS(ITYPGI).EQ.KTDOU)THEN
                IF(MOD(ISHGI,2).EQ.1)THEN
                  CALL CSGTIDL(IQEL,NAMEDP,NC)
                  PRINT *,' Non aligned double ',NAMEDP
                ENDIF
              ENDIF
#endif
              CALL CSRTLI(IQEL)
              IQEL=LEQUGI
              IF(IQEL.NE.I)GO TO 803
            ELSE
* SET ADDRESS TO LOCAL
              KMIN=LSFT-KMIN
 804          CALL CSLDLI(IQEL)
              ISHGI=KMIN+IOFSGI
              LENT=LENEGI
              IF(MODEGI.GT.1)THEN
                N=IQ(MODEGI+KSDIM)
                LENT=LENT*IQ(MODEGI+N+3)
              ELSE
                MODEGI=1
              ENDIF
* NOT YET     IF(ITYPGI.EQ.KTCHAR)LENT=(LENT-1)/NBYTPW+1
              LSFT=MAX0(LSFT,ISHGI+LENT)
#if defined(CERNLIB_ALIGN)
              IF(IABS(ITYPGI).EQ.KTDOU)THEN
                IF(MOD(ISHGI,2).EQ.1)THEN
                  CALL CSGTIDL(IQEL,NAMEDP,NC)
                  PRINT *,' Non aligned double ',NAMEDP
                ENDIF
              ENDIF
#endif
              CALL CSRTLI(IQEL)
              IQEL=LEQUGI
              IF(IQEL.NE.I) GO TO 804
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      I=IQ(I)
      GO TO  801
 706  IGSST=-6
      END
