*
* $Id: fmassq.F,v 1.1.1.1 1996/03/22 16:42:46 mclareni Exp $
*
* $Log: fmassq.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:46  mclareni
* Phtools
*
*
      FUNCTION FMASSQ(I1,I2,I3,I4,I5,I6)
C=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
C--  CALLED FROM - USER (OPTIONAL)
C--  PURPOSE - CALCULATES EFFECTIVE MASS (SQUARED) OF  UP TO 6 PARTICLES
      COMMON/UTIL/PLACE(150),WEIGHT(150),NP,TECM,AMASS(18),PCM(5,18),WT
     +/BEAM  / PRM       ,TARGM    ,PBM      ,ETA      ,GAM      ,DPBM
     +/CONTRL/ NEVTOT    ,TLIMIT   ,TLIM2    ,NDUMP    ,IEVENT
     +        ,DAT1      ,DAT2     ,IPAGE
     +/HISTR / NHIST     ,NGRP     ,NHDIM    ,LTITLE   ,LOCP(150)
     +/TAPES / NTPNCH   ,NTPBIN   ,KONSOL
     +/SCATR / IPL1(20)  ,IPL2(20) ,IWTS(20) ,LOCS(20)
      DIMENSION I(6)
      EMSQ = 0.0
      IF(I1.LE.0) GOTO 400
      I(1) = I1
      IF(I2.LE.0) GOTO 400
      I(2) = I2
      K = 2
      IF(I3.LE.0) GOTO 200
      I(3) = I3
      K = 3
      IF(I4.LE.0) GOTO 200
      I(4) = I4
      K = 4
      IF(I5.LE.0) GOTO 200
      I(5) = I5
      K = 5
      IF(I6.LE.0) GOTO 200
      I(6) = I6
      K = 6
  200 PX = 0.0
      PY = 0.0
      PZ = 0.0
      E  = 0.0
      DO 250  IDO =1, K
      INEW = I(IDO)
      PX = PX + PCM(1,INEW)
      PY = PY + PCM(2,INEW)
      PZ = PZ + PCM(3,INEW)
  250 E  =  E + PCM(4,INEW)
      EMSQ = E**2 - PX**2 - PY**2 - PZ**2
      FMASSQ = EMSQ
  400 RETURN
      END
