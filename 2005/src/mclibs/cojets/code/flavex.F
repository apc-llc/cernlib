*
* $Id: flavex.F,v 1.1.1.1 1996/01/11 14:14:36 mclareni Exp $
*
* $Log: flavex.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:36  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE FLAVEX(HFLA2)
C     ************************
C-- FORCES FLAVOR EXCITATION PRODUCTION OF FLAVOR HFLA (=1HC,1HB,1HT)
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data3.inc"
#include "cojets/flapre.inc"
#include "cojets/itapes.inc"
#include "cojets/nflav.inc"
#include "cojets/qcds.inc"
C
      CHARACTER*2 HFLA2
      CHARACTER*5 HFLA
C
      IF(FLAPRE.GT.0) GO TO 600
      IF(KPRHEV.GT.0) GO TO 700
C
      HFLA=HFLA2
      KFRFLA=0
      IF(HFLA.EQ.'CH   ') KFRFLA=4
      IF(HFLA.EQ.'BT   ') KFRFLA=5
      IF(HFLA.EQ.'TP   ') KFRFLA=6
      IF(KFRFLA.EQ.0) GO TO 500
      NFLAV=MAX(NFLAV,KFRFLA)
      KPRHEV=2
      RETURN
C
C-- ABNORMAL EXIT
500   WRITE(ITLIS,501) HFLA
501   FORMAT(5(/),1X,A4,40H IS INCORRECT ARGUMENT IN CALL TO FLAVEX
     2   //1X,28HONLY 'CH','BT','TP' ACCEPTED
     2 //1X,20HEXECUTION TERMINATED       )
      STOP
600   WRITE(ITLIS,601)
601   FORMAT(5(/),1X,47HCALL TO FLAVEX INCORRECTLY PLACED AFTER CALL TO,
     17H PREGEN    ,
     2 //1X,20HEXECUTION TERMINATED       )
      STOP
700   WRITE(ITLIS,701)
701   FORMAT(5(/),1X,35HINCORRECT MULTIPLE CALLS TO FUSION,
     1 ,' FLAVEX, HEAVYQ, GSPLIT'
     2 //1X,20HEXECUTION TERMINATED       )
      STOP
      END
