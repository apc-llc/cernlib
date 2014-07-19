*
* $Id: freeze.F,v 1.1.1.1 1996/01/11 14:14:37 mclareni Exp $
*
* $Log: freeze.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:37  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE FREEZE(HFLAQ)
C     ************************
C-- FREEZES DECAY OF ALL QUANTA OR OF CHARM, BOTTOM, TOP PARTICLES
C-- ACCORDING TO THE VALUE OF HFLAQ
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data2.inc"
#include "cojets/data3.inc"
#include "cojets/decpar.inc"
#include "cojets/itapes.inc"
#include "cojets/khadro.inc"
#include "cojets/spyfor.inc"
C
      CHARACTER*5 HFLAQ
C
      CALL SBLOCK
C
      IF(HFLAQ.EQ.'CH   ') GO TO 40
      IF(HFLAQ.EQ.'BT   ') GO TO 50
      IF(HFLAQ.EQ.'TP   ') GO TO 60
      IF(HFLAQ.EQ.'Q    ') GO TO 100
      GO TO 500
C
C-- CHARM
40    CONTINUE
      SPYC=1.
      DO 41 L=27,32
41    IDB(L)=0
      DO 42 L=67,72
42    IDB(L)=0
      RETURN
C
C-- BOTTOM
50    CONTINUE
      SPYB=1.
      DO 51 L=33,40
51    IDB(L)=0
      DO 52 L=73,80
52    IDB(L)=0
      RETURN
C
C-- TOP
60    CONTINUE
      SPYT=1.
      DO 61 L=41,50
61    IDB(L)=0
      DO 62 L=81,90
62    IDB(L)=0
      RETURN
C
C-- QUANTA
100   CONTINUE
      SPYQ=1.
      KHADRO=0
      RETURN
C
C-- ABNORMAL EXIT
500   WRITE(ITLIS,501) HFLAQ
501   FORMAT(1H1,1X,24HINCORRECT CALL TO FREEZE
     1  //1X,27HCALLED WITH 1ST ARGUMENT =   ,A1
     2   //1X,32HONLY 'CH','BT','TP','Q' ACCEPTED     )
      STOP
      END
