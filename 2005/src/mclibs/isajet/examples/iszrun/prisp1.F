C-
C-
C-  INPUT:
C-  PRUNIT = unit number for printout
C-  LISP1I = bank address
C-  NISP1  = bank number
C-  CFL    = flag to control printout
C-           'ALL' for all banks, 'LINEAR' for one linear structure
C-           'ONE' for one bank only
C-            LISP1I must be provided for 'LINEAR',
C-            LISP1I or NISP1 may be provided for 'ONE',
C-            LISP1I and NISP1 ignored for 'ALL'
C-  IFL    = not used
C-
C-     SDP  Jan,1986 , Rev. Feb,1986
C-
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
#include "isajet/izisp1.inc"
      INTEGER PRUNIT,LISP1I,NISP1,IFL
      CHARACTER CFL*(*)
      INTEGER LZLOC,LISV1,LISP1,GZISV1,GZISP1
      INTEGER LISAQ,LISAJ,IDAUV
      INTEGER NP,NV,NQ,NJ,K,K1,K2,IOR
      CHARACTER*8 NAME,LABEL
C
      LISP1 = LISP1I
C
C          Print titles
C
      WRITE(PRUNIT,100)
C
      IF ( CFL.EQ.'ONE' ) THEN
        IF ( LISP1.LE.0 .AND. NISP1.GT.0 )
     &     LISP1 = LZLOC(IXMAIN,'ISP1',NISP1)
      ENDIF
C
      IF ( CFL.EQ.'ALL' ) THEN
        LISV1=GZISV1()
        IF ( LISV1.LE.0 ) RETURN
        LISP1=LQ(LISV1-IZISP1)
      ENDIF
C
    1 IF ( LISP1.GT.0 ) THEN
C
C          find the parent jet and parton
        NJ = 0
        NQ = 0
        LISAQ = LQ(LISP1-2)
        LISAJ = LQ(LISP1-3)
        IF ( LISAJ.GT.0 ) NJ = IQ(LISAJ-5)
        IF ( LISAQ.GT.0 ) NQ = IQ(LISAQ-5)
C
C               find daughter vertex
        IDAUV = LQ(LISP1-4)
        IF ( IDAUV.GT.0 ) IDAUV = IQ(IDAUV-5)
C
C          find the supporting vertex
        IOR = LQ(LISP1+1)
        NV = IQ(IOR-5)
C
C   Print contents of bank
C
        NP = IQ(LISP1-5)
        K1 = LISP1+2
        K2 = LISP1+9
        NAME = LABEL(IQ(LISP1+1))
        WRITE(PRUNIT,101) NP,NAME(1:4),NV,NJ,NQ,IDAUV,(Q(K),K=K1,K2)
C
        IF ( CFL.NE.'ONE' ) THEN  ! FIND NEXT BANK IN LINEAR STRUCTURE
          LISP1 = LQ(LISP1)
          GOTO 1
        ENDIF
C
      ENDIF
C
      IF ( CFL.EQ.'ALL' ) THEN    ! FIND NEXT VERTEX
        LISV1 = LQ(LISV1)
        IF ( LISV1.GT.0 ) THEN
          LISP1 = LQ(LISV1-IZISP1)
          GOTO 1
        ENDIF
      ENDIF
C
      RETURN
  100 FORMAT('0',/,' PARTICLE BANKS (ISP1)',/,
     1 '  NO. NAME   VTX# JET# PAR# DAUV',7X,'PX',8X,'PY',8X,'PZ',9X,'E'
     2 ,6X,'MASS',7X,'PHI',5X,'THETA',7X,'ETA')
  101 FORMAT(I4,2X,A4,2X,4I5,2X,5F10.3,3F10.4)
      END
