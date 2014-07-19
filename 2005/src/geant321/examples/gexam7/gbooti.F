*
* $Id: gbooti.F,v 1.2 1996/02/22 13:24:00 ravndal Exp $
*
* $Log: gbooti.F,v $
* Revision 1.2  1996/02/22 13:24:00  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:20  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GBOOTI
C---------------------------------------------------------------------
C  CALLED WHEN *BOOT CARD IS READ
C    TO CREATE DIRECTORIES OF THE SHOWER FILES
C---------------------------------------------------------------------
#include "bootdt.inc"
#include "bootwk.inc"
#include "geant321/gcbank.inc"
      DIMENSION BUF(80),IBUF(80),LOPEN(10)
      EQUIVALENCE (BUF(1),IBUF(1))
CCCCC LOGICAL READY
*
      DATA IOBMED,IOBADM,IOBSHO / 0,0,0 /
      DATA NOPEN / 0 /
*
C---------------------------------------------------------------------
C  INITIALIZE ZEBRA STRUCTURE IF IT WAS NOT YET
C  CREATE BOOTSTRAP MOTHER BANK IF IT DOES NOT EXIST
C---------------------------------------------------------------------
      CALL GZINIT
      IF(JBOOT.EQ.0) THEN
         CALL MZLINK(IXSTOR,'/BOOTWK/',JBOOT,JBOOT,JBOOT)
         CALL MZBOOK(IXCONS,LBANK,JBOOT,1,'BOOT',10,10,1,2,0)
*
         CALL MZFORM('BMED','1H 2F -I', IOBMED)
         CALL MZFORM('BADM','6I 3F 1I 1F 1I 3F',IOBADM)
         CALL MZFORM('BSHO','1F -I', IOBSHO)
      ENDIF
C--------------------------------------------------------------
C     NEW VOLUME ASSIGNED TO BOOTSTRAP PROCEDURE
C--------------------------------------------------------------
      NMED=IQ(JBOOT+1)
      NMED=NMED+1
      IQ(JBOOT+1)=NMED
C--------------------------------------------------------------
C     CREATE BOOTSTRAP VOLUME BANK CONTAINING :
C       -  THE NAME OF THE VOLUME IN WHICH BOOTSTRAP IS USED
C       -  THE ENERGY CUT-OFF
C       -  THE MAXIMUM DEVIATION FROM STORED SHOWER ENERGY
C       -  THE PARTICLE TYPES TO BE REPLACED BY PREFABRICATED
C          SHOWERS
C       -  THE LOGICAL UNITS FOR READING THE CORRESPONDING
C          DIRECT ACCES FILES
C--------------------------------------------------------------
      CALL MZBOOK(IXCONS,JBMED,JBOOT,-NMED,'BMED',10,10,24,IOBMED,0)
      IQ(JBMED+1)=MEDIUM
      Q (JBMED+2)=ECUT
      Q (JBMED+3)=RANGE
      NTYPE=0
      DO 900 I=1,10
900   IF(LTYPE(1,I)*LTYPE(2,I).GT.0) NTYPE=NTYPE+1
      IQ(JBMED+4)=NTYPE
      DO 1000 ITYPE=1,NTYPE
      JBMED = LQ(JBOOT - NMED)
      IND1=JBMED+4+ITYPE
      IND2=JBMED+4+ITYPE+NTYPE
C--------------------------------
C     PARTICLE TYPE
C--------------------------------
      IQ(IND1)=LTYPE(1,ITYPE)
C--------------------------------
C     LOGICAL UNIT
C--------------------------------
      IQ(IND2)=LTYPE(2,ITYPE)
      LUNACT=LTYPE(2,ITYPE)
CCCCC INQUIRE (LUNACT,OPENED=READY)
      LUN = IUCOMP(LUNACT,LOPEN,NOPEN)
      IF(LUN.LE.0) THEN
         NOPEN = NOPEN + 1
         LOPEN(NOPEN) = LUNACT
      ENDIF
      READ(LUNACT,REC=1) IBUF
      LENDIR=IBUF(1)
      IQI   =IBUF(2)
      IRI   =IBUF(3)
      NXBIN =IBUF(4)
      NYBIN =IBUF(5)
      NZBIN =IBUF(6)
      XBIN  = BUF(7)
      YBIN  = BUF(8)
      ZBIN  = BUF(9)
      NENRG =IBUF(10)
      CUTCEL= BUF(11)
      IXY   = NXBIN*NYBIN
      I     = (IRI-2)/IXY+1
      J     = (IRI-2-IXY*(I-1))/NXBIN+1
      K     = (IRI-1-IXY*(I-1)-NXBIN*(J-1))
      XRI   = (K-0.5)*XBIN
      YRI   = (J-0.5)*YBIN
      ZRI   = (I-0.5)*ZBIN
C----------------------------------------------------------------
C     DIRECTORY BANK ( HIGHER LEVEL )
C        LENDIR     = NUMBER OF DIRECTORY RECORDS
C        IQI        = CHARGE
C        IRI        = INCIDENT REGION NUMBER IN EGS
C        NXBIN      = NUMBER OF X BINS IN EGS RECTANGULAR SYSTEM
C        XBIN       = X BINSIZE
C        NENRG      = NUMBER OF DIFFERENT ENERGY GENERATED
C        CUTCEL     = THE MINIMUM ENERGY OF A CELL THAT WAS STORED
C        XRI        = X COORDINATE OF THE SHOWER ORIGIN
C----------------------------------------------------------------
      JBMED  = LQ(JBOOT - NMED)
      CALL MZBOOK (IXCONS,JBADM,JBMED,-ITYPE,'BADM',20,20,15,IOBADM,0)
      IQ(JBADM+1) = LENDIR
      IQ(JBADM+2) = IQI
      IQ(JBADM+3) = IRI
      IQ(JBADM+4) = NXBIN
      IQ(JBADM+5) = NYBIN
      IQ(JBADM+6) = NZBIN
       Q(JBADM+7) = XBIN
       Q(JBADM+8) = YBIN
       Q(JBADM+9) = ZBIN
      IQ(JBADM+10)= NENRG
       Q(JBADM+11)= CUTCEL
      IQ(JBADM+12)= IXY
       Q(JBADM+13)= XRI
       Q(JBADM+14)= YRI
       Q(JBADM+15)= ZRI
      DO 100 IENRG=1,NENRG
C---------------------------------------------------------------
C     CREATES SHOWER BANKS ( LOWER LEVEL OF DIRECTORY STRUCTURE )
C        ENERGY   = ENERGY OF THE PREFABRICATED SHOWERS
C        NSHWER   = NUMBER OF THE GENERATED SHOWERS ( MAX. 78 )
C     STORES THE RECORD NUMBERS AT WHICH SHOWERS BEGIN AND
C     ALSO THE RECORD NUMBER FOLLOWING THE LAST SHOWER
C---------------------------------------------------------------
      JBMED  = LQ(JBOOT - NMED)
      JBADM  = LQ(JBMED - ITYPE)
      CALL MZBOOK(IXCONS,JBSHO,JBADM,-IENRG,'BSHO',0,0,80,IOBSHO,0)
      READ(LUNACT,REC=IENRG+1) IBUF
      ENERGY =  BUF(1)
      NSHWER= IBUF(2)
       Q(JBSHO+1) =ENERGY
      IQ(JBSHO+2) =NSHWER
      DO 10 ISHWER=1,NSHWER+1
10    IQ(JBSHO+ISHWER+2)=IBUF(ISHWER+2)
100   CONTINUE
1000  CONTINUE
 
      CALL VZERO(MEDIUM,23)
 
      END
