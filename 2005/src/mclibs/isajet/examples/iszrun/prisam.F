C-
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LISAMI= bank address
C-  NISAM = bank number
C-  CFL   = flag to control printout
C-          'ALL' for all banks, 'LINEAR' for one linear structure
C-          'ONE' for one bank only
C-          LISAMI must be provided for 'LINEAR',
C-          LISAMI or NISAM may be provided for 'ONE',
C-          LISAMI and NISAM ignored for 'ALL'
C-  IFL   = not used
C-
C-     DH 10-1990
C-
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/izisam.inc"
#include "isajet/zebcom.inc"
      INTEGER PRUNIT,LISAMI,NISAM,IFL
      CHARACTER CFL*(*)
      INTEGER LZLOC,LISP1,LISAM
      INTEGER LISAQ,LISAJ
      INTEGER NP,NV,NQ,NJ,K,K1,K2,IOR
      CHARACTER*8 NAME,LABEL
C
      LISAM=LISAMI
C
C          Print titles
C
        WRITE(PRUNIT,100)
C
      IF(CFL.EQ.'ONE') THEN
        IF(LISAM.LE.0.AND.NISAM.GT.0) LISAM=LZLOC(IXMAIN,'ISAM',NISAM)
      ENDIF
C
      IF(CFL.EQ.'ALL') THEN
        LISAM=LQ(LISAE-IZISAM)
      ENDIF
C
    1 IF(LISAM.GT.0) THEN
C
C          find the parent jet, parton and particle
        NJ=0
        NQ=0
        NP=0
        LISAQ=LQ(LISAM-2)
        LISAJ=LQ(LISAM-3)
        LISP1=LQ(LISAM-1)
        IF(LISAJ.GT.0) NJ=IQ(LISAJ-5)
        IF(LISAQ.GT.0) NQ=IQ(LISAQ-5)
        IF(LISP1.GT.0) NP=IQ(LISP1-5)
C
C   Print contents of bank
C
        K1=LISAM+5
        K2=LISAM+11
        NAME=LABEL(IQ(LISAM+1))
        WRITE(PRUNIT,101) NP,NAME(1:4),NJ,NQ,IQ(LISAM+2),
     A  IQ(LISAM+3),(Q(K),K=K1,K2)
C
        IF(CFL.NE.'ONE') THEN  ! find next bank in linear structure
          LISAM=LQ(LISAM)
          GOTO 1
        ENDIF
C
      ENDIF
C
C
      RETURN
  100 FORMAT('0',/,' LEPTON BANKS (ISAM)',/,
     1 '  NO. NAME   JET# PAR# SRCE TRIG',6X,'PX',8X,'PY',8X,'PZ',9X,'E'
     2 ,7X,'PHI',5X,'THETA',7X,'ETA')
  101 FORMAT(I4,2X,A4,1X,4I5,2X,4F10.3,3F10.4)
      END
