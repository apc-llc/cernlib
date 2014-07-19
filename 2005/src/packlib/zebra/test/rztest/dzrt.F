*
* $Id: dzrt.F,v 1.1.1.1 1996/03/06 10:47:04 mclareni Exp $
*
* $Log: dzrt.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:04  mclareni
* Zebra
*
*
#if defined(CERNLIB_INTER)
#include "test_include/pilot.h"
      SUBROUTINE DZRT(NCOM,NPAR)
*
********************************************************************************
*                                                                              *
*        Dispatching routine for the RZ interactive test program               *
*           Debug menu                                                         *
*                                                                              *
*  Author  : R.Brun                                                            *
*  Written : 02.04.86                                                          *
*  Last mod: 26.04.86                                                          *
*                                                                              *
********************************************************************************
*
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(20000)
      DIMENSION LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
C
#include "zebra/rzclun.inc"
*     COMMON/RZCLUN/LUN,LREC,ISAVE,NHPWD,IHPWD(2)
C
      CHARACTER*4 NAME
      CHARACTER*32 CHOPT
*
*     ---------------------------------------------------------------------
*
      CALL ZCBRON
C
      GO TO (10,20,30,40,50,60,70,80,90,100,110,120,130,140
     +      ,150),NCOM
C
C           DZSURV
C
  10  CONTINUE
      IPRINT=2
      GO TO 41
C
C           DZSNAP
C
  20  CONTINUE
      IDIV=2
      IF(NPAR.GT.0)CALL ZCGETI(' Give division number $ ',IDIV)
      CHOPT='M'
      IF(NPAR.GT.1)CALL ZCGETC('$',CHOPT,NCH)
      CALL DZSNAP('DZSNAP',IDIV,CHOPT)
      GO TO 99
C
C           DZVERI
C
  30  CONTINUE
      IDIV=0
      IF(NPAR.GT.0)CALL ZCGETI('$',IDIV)
      CHOPT='CLSU'
      IF(NPAR.GT.1)CALL ZCGETC('$',CHOPT,NCH)
      CALL DZVERI('DZVERI',IDIV,CHOPT)
      GO TO 99
C
C           DZSHOW
C
  40  CONTINUE
      IPRINT=1
  41  NUMBER=0
      IXSTOR=0
      CALL UCTOH('U   ',IFORM,4,4)
      CALL ZCGETC('BANK NAME $',NAME,NCH)
      IF(NPAR.GT.1)CALL ZCGETI('$',NUMBER)
      LOC=LZLOC(IXSTOR,NAME,NUMBER)
      IF(LOC.LE.0)GO TO 99
      IF(IPRINT.EQ.2)THEN
         CALL DZSURV('DZSURV',IXSTOR,LOC)
      ENDIF
      IF(IPRINT.EQ.1)THEN
         CHOPT='BHV'
         IF(NPAR.GT.2)CALL ZCGETC('$',CHOPT,NCH)
         CALL DZSHOW('DZBANK',IXSTOR,LOC,CHOPT,0,0,0,0)
      ENDIF
      GO TO 99
C
C           DZLINK
C
  50  CONTINUE
      IXSTOR=0
      CHOPT=' '
      IF(NPAR.GT.0)CALL ZCGETC('$',CHOPT,NCH)
      CALL DZLINK('DZLINK',IXSTOR,CHOPT)
      GO TO 99
C
C           DZSTOR
C
  60  CONTINUE
      IXSTOR=0
      CALL DZSTOR('DZSTOR',IXSTOR)
      GO TO 99
C
C           MZLOGL
C
  70  CONTINUE
      IXSTOR=0
      LEVEL=0
      CALL ZCGETI(' GIVE LOG LEVEL ===> $',LEVEL)
      CALL MZLOGL(IXSTOR,LEVEL)
      GO TO 99
C
C           MZBOOK
C
  80  IF(LEV.EQ.0)THEN
         CALL MZBOOK(IXSTOR,LEV,LEV,1,'LEV1',0,0,10,2,0)
      ENDIF
      NDATA=IQ(LEV-1)
      CALL ZCGETI(' Give number of data words $ ',ND)
      NPUSH=ND-NDATA
      IF(NPUSH.NE.0)CALL MZPUSH(IXSTOR,LEV,0,NPUSH,'I')
      DO 81 I=1,ND
         IQ(LEV+I)=I
  81  CONTINUE
      GO TO 99
C
C          PZIN
C
  90  CONTINUE
      CALL DZSHOW('PZIN',IXSTOR,LEVIN,'BHV',0,0,0,0)
      GO TO 99
C
C          SDIVISION
C
 100  CONTINUE
      IF(NPAR.GT.0)CALL ZCGETI('$',IDIV)
      GO TO 99
C
C          ZFREE
C
 110  CONTINUE
      LUNP=LUN
      IF(NPAR.GT.0)CALL ZCGETI('$',LUNP)
      LOC=LZLOC(20,'RZFR',LUNP)
      IF(LOC.NE.0)CALL DZSHOW('FREE RECORDS',20,LOC,'BHV',0,0,0,0)
      GO TO 99
C
C          ZUSED
C
 120  CONTINUE
      LUNP=LUN
      IF(NPAR.GT.0)CALL ZCGETI('$',LUNP)
      LOC=LZLOC(20,'RZUS',LUNP)
      IF(LOC.NE.0)CALL DZSHOW('USED RECORDS',20,LOC,'BHV',0,0,0,0)
      GO TO 99
C
C          ZPURG
C
 130  CONTINUE
      LUNP=LUN
      IF(NPAR.GT.0)CALL ZCGETI('$',LUNP)
      LOC=LZLOC(20,'RZPU',LUNP)
      IF(LOC.NE.0)CALL DZSHOW('PURGED RECORDS',20,LOC,'BHV',0,0,0,0)
      GO TO 99
C
C          ZNEWD
C
 140  CONTINUE
      GO TO 99
C
C          BTREE
C
 150  CONTINUE
      IF(LEV.NE.0)THEN
         CALL MZDROP(IXSTOR,LEV,' ')
      ENDIF
      NLEV=10
      IF(NPAR.GT.0)CALL ZCGETI('$',NLEV)
      IMODE=1
      IF(NPAR.GT.1)CALL ZCGETI('$',IMODE)
      IF(NLEV.GT.500)NLEV=500
      CALL MZBOOK(IXSTOR,LEV,LEV,1,'LEV1',NLEV+1,NLEV,10,2,0)
      DO 152 I=1,NLEV
         NWORDS=I
         IF(IMODE.NE.1)NWORDS=I*I
         CALL MZBOOK(IXSTOR,LBANK,LEV,-I,'LEV2',1,0,NWORDS,2,0)
         DO 151 J=1,I
            IQ(LBANK+J)=J
 151     CONTINUE
 152  CONTINUE
      LQ(LEV-NLEV-1)=LBANK
      GO TO 99
C
  99  CALL ZCBROF
      END
#endif
