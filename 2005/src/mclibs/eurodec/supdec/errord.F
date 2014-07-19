*
* $Id: errord.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: errord.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE ERRORD(IMES,TEXT,CODE)
C.----------------------------------------------------------------------
C.
C.    ROUTINE FOR ERROR MESSAGE HANDLING
C.    MESSAGE NUMBER: IMES, MESSAGE TEXT: TEXT, MESSAGE CODE: CODE
C.    LAST UPDATE: 05/04/89
C.
C.----------------------------------------------------------------------
#include "eurodec/inpout.inc"
#include "eurodec/ptable.inc"
#include "eurodec/dtable.inc"
      CHARACTER TEXT*(*)
      SAVE IERROR
      DATA IERROR/ 0/
      WRITE(LUN2,9000)
      WRITE(LUN2,9360)
      WRITE(LUN2,9370)
      GOTO (10,20,30,40),IMES-30
      GOTO (50,60,70,80),IMES-40
      GOTO (90,100,110,120,130,140,150,160,170),IMES-50
      GOTO (180,190,200),IMES-60
      GOTO (210,220,230,240,250,260,270,280,290),IMES-70
      GOTO (300,310,320,330,340,350),IMES-80
      WRITE(LUN2,9380) IMES
      GOTO 380
   10 WRITE(LUN2,9010)
      GOTO 360
   20 WRITE(LUN2,9020) CODE
      GOTO 360
   30 WRITE(LUN2,9030) CODE
      GOTO 360
   40 WRITE(LUN2,9040)
      GOTO 360
   50 WRITE(LUN2,9050) IFIX(CODE)
      GOTO 370
   60 WRITE(LUN2,9060)
      GOTO 370
   70 WRITE(LUN2,9070) IFIX(CODE)
      GOTO 370
   80 WRITE(LUN2,9080) TEXT,CODE
      GOTO 360
   90 WRITE(LUN2,9090) IFIX(CODE)
      GOTO 360
  100 WRITE(LUN2,9100) IFIX(CODE)
      GOTO 360
  110 WRITE(LUN2,9110) IFIX(CODE)
      GOTO 360
  120 WRITE(LUN2,9120) TEXT,IFIX(CODE)
      GOTO 360
  130 WRITE(LUN2,9130) IFIX(CODE)
      GOTO 360
  140 WRITE(LUN2,9140) TEXT
      GOTO 360
  150 WRITE(LUN2,9150) TEXT,CODE
      GOTO 360
  160 WRITE(LUN2,9160) TEXT
      GOTO 360
  170 WRITE(LUN2,9170) TEXT,IFIX(CODE)
      GOTO 360
  180 WRITE(LUN2,9180) IFIX(CODE)
      GOTO 360
  190 WRITE(LUN2,9190) IFIX(CODE)
      GOTO 360
  200 WRITE(LUN2,9200) IFIX(CODE)
      GOTO 380
  210 WRITE(LUN2,9210) TEXT,IFIX(CODE)
      GOTO 360
  220 WRITE(LUN2,9220) IFIX(CODE)
      GOTO 360
  230 WRITE(LUN2,9230) TEXT,IFIX(CODE)
      GOTO 360
  240 WRITE(LUN2,9240) TEXT,IFIX(CODE)
      GOTO 360
  250 WRITE(LUN2,9250) TEXT,IFIX(CODE)
      GOTO 360
  260 WRITE(LUN2,9260) IFIX(CODE)
      GOTO 360
  270 WRITE(LUN2,9270) IFIX(CODE)
      GOTO 360
  280 WRITE(LUN2,9280) IFIX(CODE)
      GOTO 360
  290 WRITE(LUN2,9290) TEXT,IFIX(CODE)
      GOTO 360
  300 WRITE(LUN2,9300) TEXT,CODE
      GOTO 360
  310 WRITE(LUN2,9310) TEXT,CODE
      GOTO 360
  320 WRITE(LUN2,9320) TEXT,CODE
      GOTO 360
  330 WRITE(LUN2,9330) TEXT
      WRITE(LUN2,*) (IDC(J,IFIX(CODE)),J=1,NDP(IFIX(CODE)))
      GOTO 380
  340 WRITE(LUN2,9340) TEXT
      WRITE(LUN2,*) (IDC(J,IFIX(CODE)),J=1,NDP(IFIX(CODE)))
      GOTO 360
  350 WRITE(LUN2,9350) IFIX(CODE)
      GOTO 360
  360 CONTINUE
      WRITE(LUN2,9370)
      WRITE(LUN2,9360)
      STOP
  370 CALL PRTEVT(0)
  380 IERROR=IERROR+1
      IF (IERROR.LT.10) RETURN
      WRITE(LUN2,9370)
      WRITE(LUN2,9360)
      WRITE(LUN2,9390)
      STOP
 9000 FORMAT(1H1)
 9010 FORMAT(1H ,'**** FUNPRE: Failure to find Trapezoid: HELP!! ****')
 9020 FORMAT(1H ,'**** FUNPRE: Negative Range of X, X= ',F15.6,' ****')
 9030 FORMAT(1H ,'**** FUNPRE: Negative Fie Value, F= ',F15.6,'  ****')
 9040 FORMAT(1H ,'**** DGAUSS: Too high Accuracy required !!     ****')
 9050 FORMAT(1H ,'**** CLRSTK: Error in flavour Matching, left:',I3,
     &'****')
 9060 FORMAT(1H ,'**** ADJUST: Error ******** DFZ = 0 ********   ****')
 9070 FORMAT(1H ,'**** ADJUST: You tried ',I3,
     &' Times to fragment, undertake Action! ****')
 9080 FORMAT(1H ,'**** ',A,': Insufficient Phase Space for ',
     &       'Fragmentation, Q0**2 = ',F15.6,' (fatal) ****')
 9090 FORMAT(1H ,'**** Format Error on Input Record:',I5,'       ****')
 9100 FORMAT(1H ,'**** Error: End on Input File at Record:',I5,' ****')
 9110 FORMAT(1H ,'**** Error: Particle Table array Length>',I5,' ****')
 9120 FORMAT(1H ,'**** Daughter of ',A,' missing at Line:',I5,' ****')
 9130 FORMAT(1H ,'**** Error: Decay Table array Length >',I5,'   ****')
 9140 FORMAT(1H ,'**** Particle Name: ',A,' has not been defined ****')
 9150 FORMAT(1H ,'**** Br. Ratios ',A,' add up to : ',F6.4,' ****')
 9160 FORMAT(1H ,'**** Error: File - ',A,' - cannot be opened ****')
 9170 FORMAT(1H ,'**** Particle Name: ',A,
     &' multiply defined in Record: ',I5,' ****')
 9180 FORMAT(1H ,'**** Error: Array Length of /RESULT/ >',I5,' ****')
 9190 FORMAT(1H ,'**** Error: Array Length of /HADGEN/ >',I5,' ****')
 9200 FORMAT(1H ,'**** Particle Id: ',I5,' has not been defined ****')
 9210 FORMAT(1H ,'**** Error: ',A,' is unknown Keyword in Record:',I5,
     &' ****')
 9220 FORMAT(1H ,'**** Error: Missing Qualifier in Record:',I5,' ****')
 9230 FORMAT(1H ,'**** Error: ',A,' is wrong Qualifier in Record:',I5,
     &' ****')
 9240 FORMAT(1H ,'**** Error: ',A,
     &' is wrong Value/Value Length in Record:',I5,' ****')
 9250 FORMAT(1H ,'**** Error: Weak Mixing for ',A,
     &' is not forseen, Record:',I5,' ****')
 9260 FORMAT(1H ,'**** Error: Unable to read String in Record:',I5,
     &' ****')
 9270 FORMAT(1H ,'**** Error: Decay Mode missing in Record:',I5, ' ****'
     &)
 9280 FORMAT(1H ,'**** Error: Decay Mode not in Decay Table, Record:',
     &I5,' ****')
 9290 FORMAT(1H ,'**** Error: ',A,' has incorrect Charge, Record:',I5,
     &' ****')
 9300 FORMAT(1H ,'**** Error: ',A,' has incorrect Mass, Record:',I5,
     &' ****')
 9310 FORMAT(1H ,'**** Error: ',A,' has incorrect Lifetime, Record:',I5,
     &' ****')
 9320 FORMAT(1H ,'**** Error: ',A,' has incorrect Width, Record:',I5,
     &' ****')
 9330 FORMAT(1H ,'**** Error: Insufficient Phase Space for ',A,
     &' decaying into: ****')
 9340 FORMAT(1H ,'**** Error: Insufficient Phase Space for ',A,
     &' decaying into (fatal): ****')
 9350 FORMAT(1H ,'**** Error: Seed for Random Number Generator out of ',
     &       'Range: ',I10,' (fatal) ****')
 9360 FORMAT(1H ,80('*'))
 9370 FORMAT(1H )
 9380 FORMAT(1H ,'**** Funny Error Message: ',I4,' ! What to do? ****')
 9390 FORMAT(1H ,
     &'**** 10 Error Messages generated, I stop this Run! ****')
      END
