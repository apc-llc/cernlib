*
* $Id: ttrla.F,v 1.1.1.1 1996/02/15 17:55:02 mclareni Exp $
*
* $Log: ttrla.F,v $
* Revision 1.1.1.1  1996/02/15 17:55:02  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TTRLA

#include "kerngent/mkcde.inc"
      DIMENSION DC(10), EC(12), SC(6), TC(12), UC(12), VC(12), WC(10),
     1          XC(10), AC(10), YC(12)
      DIMENSION SAC(12),ASC(12),SATC(12),ATSC(12) ,
     1          ATSAC(6), QSQC(10)
     2         ,PCKC(10),UPCKC(16)
      INTEGER TINF(4), TINF2(15), TINF4 (15)
      DATA DC/1.,2.,3.,0.,0.,0.,4.,5.,0.,6./,
     1     EC/4.,7.,3.,3.,6.,4.,0.,5.,5.,2.,1.,2./,
     2     SC/1384.,1712.,2201.,858.,1075.,538./,
     3     TC/18.,23.,19.,19.,23.,22.,0.,0.,0.,12.,6.,12./,
     4    UC/30.,36.,0.,18.,34.,37.,0.,30.,17.,16.,0.,12./,
     5     VC/4.0,7.0,3.0,17.,32.,18.,0.,0.,0.,43.,64.,44./,
     6     WC/74.,66.,61.,50.,50.,50.,21.,20.,15.,9./,
     7     XC/77.,62.,69.,17.,23.,10.,52.,45.,11.,38./,
     8     YC/4.,29.,0.,69.,6.,24.,0.,74.,5.,16.,0.,42./,
     9     AC/1.,0.,0.,2.,0.,13.,4.,0.,23.,77./

      DATA ASC/30.,44.,0.,69.,34.,49.,0.,74.,17.,26.,0.,42./
      DATA SAC/18.,23.,19.,27.,37.,28.,0.,0.,0.,43.,64.,44./
      DATA SATC/30.,34.,17.,44.,49.,26.,0.,0.,0.,69.,74.,42./
      DATA ATSC/18.,27.,0.,43.,23.,37.,0.,64.,19.,28.,0.,44./
      DATA ATSAC/239.,331.,447.,248.,345.,257./
      DATA QSQC/1265.,1594.,2009.,0.,0.,0.,1940.,2446.,0.,2980./
      DATA UPCKC/1.,2.,4.,7.,2.,3.,5.,8.,4.,5.,6.,9.,7.,8.,9.,10./
      DATA PCKC/1.,2.,3.,4.,5.,6.,7.,8.,9.,10./
      DATA TINF/1000,4H    ,1,4H    /
      DATA TINF2/4HLA  ,4HLTA ,4HAL  ,4HALT ,4HAAT ,4HATA ,4HASAT,
     +          4HATSA,4HQSQ ,4HSA  ,4HAS  ,4HSAT ,4HATS ,4HPCK ,4HUPCK/
      DATA TINF4/4H443 ,4H443 ,4H344 ,4H344 ,4H4*3 ,4H4*3 ,4H3443,
     +          4H3443,4H4444,4H443 ,4H344 ,4H443 ,4H344 ,4H4*4 ,4H4*4 /
C----------------------------------------------------------------------

      CALL NEWGUY ('TRLA-TRLTA-TRAL-TRALT.','TTRLA   ')
      ZERLEV = ZEROV(2)

      CALL TRLA (DC,EC,A,4,3)
      CALL MVERIF (111,A,VC,12)

      CALL UCOPY(EC,A,12)
      CALL TRLA (DC,A,A,4,3)
      CALL MVERIF(112,A,VC,12)

      CALL TRLTA (DC,EC,A,4,3)
      CALL MVERIF (121,A,TC,12)

      CALL UCOPY(EC,A,12)
      CALL TRLTA(DC,A,A,4,3)
      CALL MVERIF(122,A,TC,12)

      CALL TRAL (EC,DC,A,3,4)
      CALL MVERIF (131,A,UC,12)

      CALL UCOPY(EC,A,12)
      CALL TRAL (A,DC,A,3,4)
      CALL MVERIF(132,A,UC,12)

      CALL TRALT (EC,DC,A,3,4)
      CALL MVERIF (141,A,YC,12)

      CALL UCOPY(EC,A,12)
      CALL TRALT(A,DC,A,3,4)
      CALL MVERIF(142,A,YC,12)
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

      IF (ITIMES .EQ. 0)   GO TO 180

      TINF(2)=TINF2(1)
      TINF(4)=TINF4(1)
      CALL TIMED (TIMERD)
      DO 150 J=1,1000
  150 CALL TRLA (DC,EC,A,4,3)
      CALL TIMING (TINF)

      TINF(2)=TINF2(2)
      TINF(4)=TINF4(2)
      CALL TIMED (TIMERD)
      DO 50  J=1,1000
   50 CALL TRLTA (DC,EC,A,4,3)
      CALL TIMING (TINF)

      TINF(2)=TINF2(3)
      TINF(4)=TINF4(3)
      CALL TIMED (TIMERD)
      DO 110 J=1,1000
  110 CALL TRAL(EC,DC,A,3,4)
      CALL TIMING (TINF)

      TINF(2)=TINF2(4)
      TINF(4)=TINF4(4)
      CALL TIMED (TIMERD)
      DO 100 J=1,1000
  100 CALL TRALT (EC,DC,A,3,4)
      CALL TIMING (TINF)
C----------------------------------------------------------------------

  180 CALL NEWGUY ('TRAAT-TRATA.','TTRLA   ')
      CALL TRAAT (EC,A,4,3)
      CALL MVERIF (211,A,WC,10)

      CALL TRATA (EC,A,4,3)
      CALL MVERIF (221,A,XC,10)
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

      IF (ITIMES .EQ. 0)   GO TO 280
      TINF(2)=TINF2(5)
      TINF(4)=TINF4(5)
      CALL TIMED (TIMERD)
      DO 200 J=1,1000
  200 CALL TRAAT (EC,A,4,3)
      CALL TIMING (TINF)

      TINF(2)=TINF2(6)
      TINF(4)=TINF4(6)
      CALL TIMED (TIMERD)
      DO 250 J=1,1000
  250 CALL TRATA (EC,A,4,3)
      CALL TIMING (TINF)
C----------------------------------------------------------------------

  280 CALL NEWGUY ('TRASAT-TRATSA-TRQSQ.','TTRLA   ')
      CALL TRASAT (EC,AC,A,3,4)
      CALL MVERIF (311,A,SC,6 )

      CALL TRATSA(EC,DC,A,3,4)
      CALL MVERIF(321,A,ATSAC,6)

       CALL TRQSQ(DC,AC,A,4)
      CALL MVERIF(331,A,QSQC,10)
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

      IF (ITIMES .EQ. 0) GOTO 380
      TINF(2)=TINF2(7)
      TINF(4)=TINF4(7)
      CALL TIMED (TIMERD)
      DO 300 J=1,1000
  300 CALL TRASAT (AC,EC,A,4,3)
      CALL TIMING (TINF)

      TINF(2)=TINF2(8)
      TINF(4)=TINF4(8)
      CALL TIMED (TIMERD)
      DO 310  J=1,1000
  310 CALL TRATSA(EC,AC,A,3,4)
      CALL TIMING(TINF)

      TINF(2)=TINF2(9)
      TINF(4)=TINF4(9)
      CALL TIMED (TIMERD)
      DO 320  J=1,1000
  320  CALL TRQSQ(DC,AC,A,4)
      CALL TIMING(TINF)
C----------------------------------------------------------------------

  380 CALL NEWGUY ('TRSA-TRAS-TRSAT-TRATS.','TTRLA   ')

      CALL TRSA(DC,EC,A,4,3)
      CALL MVERIF(411,A,SAC,12)

      CALL TRAS(EC,DC,A,3,4)
      CALL MVERIF(421,A,ASC,12)

      CALL TRSAT(DC,EC,A,4,3)
      CALL MVERIF(431  ,A,SATC,12)

      CALL TRATS(EC,DC,A,3,4)
      CALL MVERIF(441,A,ATSC,12)
C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

      IF (ITIMES .EQ. 0) GOTO 480
      TINF(2)=TINF2(10)
      TINF(4)=TINF4(10)
      CALL TIMED (TIMERD)
      DO 400  J=1,1000
  400 CALL TRSA(DC,EC,A,4,3)
      CALL TIMING(TINF)

      TINF(2)=TINF2(11)
      TINF(4)=TINF4(11)
      CALL TIMED (TIMERD)
      DO 410  J=1,1000
  410 CALL TRAS(EC,DC,A,3,4)
      CALL TIMING(TINF)

      TINF(2)=TINF2(12)
      TINF(4)=TINF4(12)
      CALL TIMED (TIMERD)
      DO 420 J=1,1000
  420 CALL TRSAT(DC,EC,A,4,3)
      CALL TIMING(TINF)

      TINF(2)=TINF2(13)
      TINF(4)=TINF4(13)
      CALL TIMED (TIMERD)
      DO 430  J=1,1000
  430 CALL TRATS(EC,DC,A,3,4)
      CALL TIMING(TINF)
C----------------------------------------------------------------------

  480 CALL NEWGUY ('TRPCK-TRUPCK.','TTRLA   ')

       CALL TRPCK(UPCKC,A,4)
       CALL MVERIF(511,A,PCKC,10)

       CALL TRUPCK(PCKC,B,4)
       CALL MVERIF(521,B,UPCKC,16)

      CALL TRPCK(B,B,4)
       CALL MVERIF(512,B,PCKC,10)

       CALL TRUPCK(A,A,4)
       CALL MVERIF(522,A,UPCKC,16)

C-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

      IF(ITIMES.EQ.0) GOTO 580
      TINF(2)=TINF2(14)
      TINF(4)=TINF4(14)
      CALL TIMED (TIMERD)
      DO 500 J=1,1000
  500 CALL TRPCK(A,B,4)
      CALL TIMING(TINF)

      TINF(4)=TINF4(15)
      TINF(2)=TINF2(15)
      CALL TIMED (TIMERD)
      DO 510 J=1,1000
  510 CALL TRUPCK(A,B,4)
      CALL TIMING(TINF)

  580 CONTINUE
      RETURN
      END
