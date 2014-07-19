*
* $Id: ugboot.F,v 1.2 1996/02/22 13:24:04 ravndal Exp $
*
* $Log: ugboot.F,v $
* Revision 1.2  1996/02/22 13:24:04  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:21  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGBOOT
 
C------------------------------------------------------------------
C       CKECK THAT THE CURRENT PARTICLE SHOULD BE REPLACED
C       BY PRESHOWER OR NOT.
C
C       IF YES DO IT. THE USER SHOULD TAKE CARE OF THE DEPOSITED ENERGY
C       IN EACH CELL.
C
C       CALLED BY GUSTEP
C------------------------------------------------------------------
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"
#include "celoss.inc"
#include "bootwk.inc"
 
      DIMENSION BUF(2,40),IBUF(2,40),XREGIO(3)
      EQUIVALENCE (BUF,IBUF)
      DIMENSION RNDM(2)
 
      IF (ISTOP.NE.0) RETURN
C------------------------------------------------------------------
C     IS THIS MEDIUM MARKED FOR BOOTSTRAP ?
C------------------------------------------------------------------
      NMED =IQ(JBOOT+1)
      IF (NMED .EQ.0) RETURN
      DO 100 IMED=1,NMED
         JBMED=LQ(JBOOT-IMED)
         NAME =IQ(JBMED+1)
         IF (NAME.EQ.NAMES(NLEVEL)) GOTO 200
100   CONTINUE
      RETURN
C------------------------------------------------------------------
C     PARTICLE ENERGY BELOW ECUT ?
C------------------------------------------------------------------
200   ECUT = Q(JBMED+2)*EINCID
      IF (GETOT.LT.ECUT) GOTO 300
      RETURN
C------------------------------------------------------------------
C     PARTICLE MARKED FOR BOOTSTRAP IN THIS MEDIUM ?
C------------------------------------------------------------------
300   NTYPE=IQ(JBMED+4)
      DO 400 ITYPE=1,NTYPE
         IF (IQ(JBMED+ITYPE+4).EQ.IPART) GOTO 500
400   CONTINUE
      RETURN
C------------------------------------------------------------------
C     IS THERE SHOWERS THERE ?
C------------------------------------------------------------------
500   JBADM=LQ(JBMED-ITYPE)
      NENRG=IQ(JBADM+10)
      DO 600 IENRG=1,NENRG
         JBSHO=LQ(JBADM-IENRG)
         ENSHO= Q(JBSHO+1)/1000.0
         DEVIA=ABS(ENSHO-GETOT)/ENSHO
         IF (DEVIA.LT.Q(JBMED+3)) GOTO 700
600   CONTINUE
      RETURN
C------------------------------------------------------------------
C     LET'S GO BOOTSTRAP
C------------------------------------------------------------------
700   NBSHO=IQ(JBSHO+2)
      CALL HFILL(1000,FLOAT(IENRG),0.,1.)
      LUNIT=IQ(JBMED+ITYPE+NTYPE+4)
      NXBIN=IQ(JBADM+4)
      NYBIN=IQ(JBADM+5)
      NZBIN=IQ(JBADM+6)
      XBIN = Q(JBADM+7)
      YBIN = Q(JBADM+8)
      ZBIN = Q(JBADM+9)
      IXY  =IQ(JBADM+12)
      XRI  = Q(JBADM+13)
      YRI  = Q(JBADM+14)
      ZRI  = Q(JBADM+15)
C------------------------------------------------------------------
C     DIRECTION COSINES AND PARTICLE COORDINATES
C------------------------------------------------------------------
      XP=VECT(1)
      YP=VECT(2)
      ZP=VECT(3)
      UP=VECT(4)
      VP=VECT(5)
      WP=VECT(6)
C------------------------------------------------------------------
C     ROTATION MATRIX ELEMENTS
C------------------------------------------------------------------
      P=UP*UP
      T=UP*VP
      R=VP*VP
      S= 1.0/(1.0+WP)
      T11=1.0-P*S
      T12=-T*S
      T13=UP
      T21=T12
      T22=1.0-R*S
      T23=VP
      T31=-T13
      T32=-T23
      T33=WP
C------------------------------------------------------------------
C     RANDOM ROTATION MATRIX ELEMENTS
C------------------------------------------------------------------
      CALL GRNDM(RNDM,2)
      PHIROT=6.28*RNDM(1)
      COSROT=COS(PHIROT)
      SINROT=SIN(PHIROT)
C------------------------------------------------------------------
C     NORMALIZE FOR THE DEVIATION FROM STORED SHOWER ENERGY
C------------------------------------------------------------------
      ENORM=GETOT/ENSHO
C------------------------------------------------------------------
C     RANDOMLY PICKS UP A SHOWER FROM THE FILE
C------------------------------------------------------------------
      INSHO=RNDM(2)*NBSHO+1
      IREC1=IQ(JBSHO+INSHO+2)
      IREC2=IQ(JBSHO+INSHO+3)
      NREC=IREC2-IREC1
      DO 900 IREC=IREC1,IREC2-1
      READ(LUNIT,REC=IREC) IBUF
      DO 800 IND=1,40
C------------------------------------------------------------------
C     THE CONTENT OF A CELL , NUMBER 'IREGIO' , IS 'EREGIO'
C------------------------------------------------------------------
      IREGIO=IBUF(2,IND)
C------------------------------------------------------------------
      EREGIO= BUF(1,IND)/1000.0*ENORM
      IF ((IREGIO.GT.0).AND.(EREGIO.GT.0.)) THEN
             I=(IREGIO-2)/IXY+1
             J=(IREGIO-2-IXY*(I-1))/NXBIN+1
             K=IREGIO-1-IXY*(I-1)-NXBIN*(J-1)
             AR=(K-0.5)*XBIN-XRI
             BR=(J-0.5)*YBIN-YRI
             XR= COSROT*AR + SINROT*BR
             YR=-SINROT*AR + COSROT*BR
             ZR=(I-0.5)*ZBIN-ZRI
             XREGIO(1)=T11*XR+T12*YR+T13*ZR+XP
             XREGIO(2)=T21*XR+T22*YR+T23*ZR+YP
             XREGIO(3)=T31*XR+T32*YR+T33*ZR+ZP
C---------------------------------------------------------
C            HERE ATTACH BINS TO 'PHYSICAL' CRYSTALS
C            ( BEFORE THAT UPDATES THE VOLUME COMMON )
C            YOU CAN INSERT HERE YOUR OWN ROUTIN FOR COUNTING
C            DEPOSITED ENERGY
C---------------------------------------------------------
             CALL GMEDIA(XREGIO,NUMED)
 
C ***          Energy deposited
              IF(NUMED.EQ.1) THEN
                 NL  = NUMBER(NLEVEL)
                 NR  = NUMBER(NLEVEL-1)
                 DEDL(NL) = DEDL(NL) + EREGIO
                 DEDR(NR) = DEDR(NR) + EREGIO
              ENDIF
C---------------------------------------------------------
C      SOME DEBUG MAY BE HERE
C---------------------------------------------------------
       ENDIF
800    CONTINUE
900    CONTINUE
C------------------------------------------------------------------
C      PARTICLE DISCARDED ( ITS ENERGY BECOMES ZERO )
C------------------------------------------------------------------
       GEKIN=0.0
       ISTOP=2
       IF(IDEBUG*ISWIT(2).EQ.2) PRINT 1239,GETOT
1239   FORMAT(' BOOTSTRAP FOR ',F12.3,' GEV SHOWER USED')
 
      END
