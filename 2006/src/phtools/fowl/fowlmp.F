*
* $Id: fowlmp.F,v 1.1.1.1 1996/03/22 16:42:46 mclareni Exp $
*
* $Log: fowlmp.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:46  mclareni
* Phtools
*
*
      SUBROUTINE FOWLMP
C     ********
C     * FOWL * = A GENERAL MONTE-CARLO PHASE SPACE PROGRAM  ************
C     ********
      COMMON / / NDIMEN,BINS(100)
      COMMON/UTIL/PLACE(150),WEIGHT(150),NP,TECM,AMASS(18),PCM(5,18),WT
     +/BEAM  / PRM       ,TARGM    ,PBM      ,ETA      ,GAM      ,DPBM
     +/CONTRL/ NEVTOT    ,TLIMIT   ,TLIM2    ,NDUMP    ,IEVENT
     +        ,DAT1      ,DAT2     ,IPAGE
     +/HISTR / NHIST     ,NGRP     ,NHDIM    ,LTITLE   ,LOCP(150)
     +/TAPES / NTPNCH   ,NTPBIN   ,KONSOL
     +/SCATR / IPL1(20)  ,IPL2(20) ,IWTS(20) ,LOCS(20)
C-------------------------------------------------
C--  IPAGE=1 MEANS A PAGE THROW BEFORE EACH HISTOGRAM
      IPAGE = 0
      NHDIM = 150
      LTITLE = 8
      IEVENT = 0
      NGRP = 200
      WRITE(6,5010)
      WRITE(6,5011)NHDIM,NDIMEN
      CALL REDATA
      IF(NEVTOT.LT.NGRP) NGRP = NEVTOT
      N2 = MIN (NEVTOT/NGRP, 30000)
      CALL START
      DO  500 I2= 1, N2
      DO  400  I= 1, NGRP
      IEVENT = IEVENT + 1
      CALL BEAMIN
      CALL GENEV
      CALL USER
      CALL TRANCH
  400 CONTINUE
      CALL TIMEX(TIMSEC)
      IF(TIMSEC.GE.TLIMIT*60.) GOTO 600
  500 CONTINUE
  600 CONTINUE
      CALL FINISH
      CALL FOWLPR
      STOP
 5010 FORMAT('1'/40X,'  STANDARD FOWL  --  VERSION OF 24/10/81')
 5011 FORMAT(32X,' PROGRAM DIMENSIONS=',I4,' HISTOGRAMS USING',I6,
     +    ' BIN LOCATIONS'/33X,61('*'))
      END
