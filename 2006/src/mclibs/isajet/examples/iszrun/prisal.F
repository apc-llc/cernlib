C-
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LISALI= bank address
C-  NISAL = bank number
C-  CFL   = flag to control printout
C-          'ALL' for all banks, 'LINEAR' for one linear structure
C-          'ONE' for one bank only
C-          LISALI must be provided for 'LINEAR',
C-          LISALI or NISAL may be provided for 'ONE',
C-          LISALI and NISAL ignored for 'ALL'
C-  IFL   = not used
C-
C-     SDP  Jan,1986 , Rev. Feb,1986
C-
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
      INTEGER PRUNIT,LISALI,NISAL,IFL
      CHARACTER CFL*(*)
      INTEGER LZLOC,LISP1,LISAL,GZISAL
      INTEGER LISAQ,LISAJ
      INTEGER NP,NV,NQ,NJ,K,K1,K2,IOR
      CHARACTER*8 NAME,LABEL
C
      LISAL=LISALI
C
C          Print titles
C
        WRITE(PRUNIT,100)
C
      IF(CFL.EQ.'ONE') THEN
        IF(LISAL.LE.0.AND.NISAL.GT.0) LISAL=LZLOC(IXMAIN,'ISAL',NISAL)
      ENDIF
C
      IF(CFL.EQ.'ALL') THEN
        LISAL=GZISAL()
      ENDIF
C
    1 IF(LISAL.GT.0) THEN
C
C          find the parent jet, parton and particle
        NJ=0
        NQ=0
        NP=0
        LISAQ=LQ(LISAL-2)
        LISAJ=LQ(LISAL-3)
        LISP1=LQ(LISAL-1)
        IF(LISAJ.GT.0) NJ=IQ(LISAJ-5)
        IF(LISAQ.GT.0) NQ=IQ(LISAQ-5)
        IF(LISP1.GT.0) NP=IQ(LISP1-5)
C
C   Print contents of bank
C
        K1=LISAL+2
        K2=LISAL+9
        NAME=LABEL(IQ(LISAL+1))
        WRITE(PRUNIT,101) NP,NAME(1:4),NJ,NQ,(Q(K),K=K1,K2)
C
        IF(CFL.NE.'ONE') THEN  ! find next bank in linear structure
          LISAL=LQ(LISAL)
          GOTO 1
        ENDIF
C
      ENDIF
C
C
      RETURN
  100 FORMAT('0',/,' LEPTON BANKS (ISAL)',/,
     1 '  NO. NAME   JET# PAR#',6X,'PX',8X,'PY',8X,'PZ',9X,'E'
     2 ,6X,'MASS',7X,'PHI',5X,'THETA',7X,'ETA')
  101 FORMAT(I4,2X,A4,1X,2I5,2X,5F10.3,3F10.4)
      END
