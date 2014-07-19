C-
C-
C-  INPUT:
C-  PRUNIT = unit number for printout
C-  LISP2I = bank address
C-  NISP2  = bank number
C-  CFL    = flag to control printout
C-           'ALL' for all banks, 'LINEAR' for one linear structure
C-           'ONE' for one bank only
C-            LISP2I must be provided for 'LINEAR',
C-            LISP2I or NISP2 may be provided for 'ONE',
C-            LISP2I and NISP2 ignored for 'ALL'
C-  IFL    = not used
C-
C-     SDP  Jan,1986 , Rev. Feb,1986
C-     SK   Apr,1986 , modified from SDP's PRISP1
C-
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
#include "isajet/izisv2.inc"
#include "isajet/izisp2.inc"
      INTEGER PRUNIT,LISP2I,NISP2,IFL
      CHARACTER CFL*(*)
      INTEGER LZLOC,LISV2,LISP2
      INTEGER LISAQ,LISAJ,IDAUV
      INTEGER NP,NV,K,K1,K2,IOR
      CHARACTER*8 NAME,LABEL
C
      LISP2 = LISP2I
C
C          Print titles
C
      WRITE(PRUNIT,100)
C
      IF ( CFL.EQ.'ONE' ) THEN
        IF ( LISP2.LE.0 .AND. NISP2.GT.0 )
     &     LISP2 = LZLOC(IXMAIN,'ISP2',NISP2)
      ENDIF
C
      IF ( CFL.EQ.'ALL' ) THEN
        IF ( LISAE.NE.0 ) LISV2 = LQ(LISAE-IZISV2)
        IF ( LISV2.NE.0 ) LISP2 = LQ(LISV2-IZISP2)
        IF ( LISAE.LE.0 .OR. LISV2.LE.0 ) RETURN
      ENDIF
C
    1 IF ( LISP2.GT.0 ) THEN
C
C               find daughter vertex
        IDAUV = LQ(LISP2-4)
        IF ( IDAUV.GT.0 ) IDAUV = IQ(IDAUV-5)
C          find the supporting vertex
        IOR = LQ(LISP2+1)
        NV = IQ(IOR-5)
C
C   Print contents of bank
C
        NP = IQ(LISP2-5)
        K1 = LISP2+2
        K2 = LISP2+9
        NAME = LABEL(IQ(LISP2+1))
        WRITE(PRUNIT,101) NP,NAME(1:4),NV,IDAUV,(Q(K),K=K1,K2)
C
        IF ( CFL.NE.'ONE' ) THEN  ! FIND NEXT BANK IN LINEAR STRUCTURE
          LISP2 = LQ(LISP2)
          GOTO 1
        ENDIF
C
      ENDIF
C
      IF ( CFL.EQ.'ALL' ) THEN    ! FIND NEXT VERTEX
        LISV2 = LQ(LISV2)
        IF ( LISV2.GT.0 ) THEN
          LISP2 = LQ(LISV2-IZISP2)
          GOTO 1
        ENDIF
      ENDIF
C
      RETURN
  100 FORMAT(/' PARTICLE BANKS (ISP2)'/
     1 '  NO. NAME   VTX# DAUV',7X,'PX',8X,'PY',8X,'PZ',9X,'E'
     2 ,6X,'MASS',7X,'PHI',5X,'THETA',7X,'ETA')
  101 FORMAT(I4,2X,A4,2X,2I5,2X,5F10.3,3F10.4)
      END
