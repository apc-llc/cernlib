C-
C-  INPUT:
C-  PRUNIT = unit number for printout
C-  LISV2I = bank address
C-  NISV2  = bank number
C-  CFL    = flag to control printout
C-           'ALL' for all banks, 'LINEAR' for one linear structure
C-           'ONE' for one bank only
C-            LISV2I must be provided for 'LINEAR',
C-            LISV2I or NISV2 may be provided for 'ONE',
C-            LISV2I and NISV2 ignored for 'ALL'
C-  IFL    = not used
C-
C-     SDP  Jan,1986 , Rev. Feb,1986
C-     SK   Apr,1986,  Modified from SDP's PRISV1.
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
#include "isajet/izisv2.inc"
      CHARACTER CFL*(*)
      INTEGER LZLOC
      INTEGER PRUNIT,LISV2I,NISV2,IFL
      INTEGER NV,K,K1,K2,K3,IORV,IORP,LISV2
      CHARACTER*8 NAME,LABEL
C
C          Print titles
C
      WRITE(PRUNIT,100)
C
      LISV2 = LISV2I
      IF ( CFL.EQ.'ALL' ) THEN
        LISV2 = LQ(LISAE-IZISV2)
      ENDIF
C
    1 IF ( LISV2.GT.0 ) THEN
C
C       find parent vertex
        IORV = LQ(LISV2-2)
        IF ( IORV.GT.0) IORV = IQ(IORV-5)
        IORP = LQ(LISV2-3)
        IF ( IORP.GT.0) IORP = IQ(IORP-5)
C
C   Print contents of bank
C
        NV = IQ(LISV2-5)
        K1 = LISV2+2
        K2 = LISV2+9
        K3 = LISV2 + 10
        NAME = LABEL(IQ(LISV2+1))
        WRITE(PRUNIT,101) NV,NAME(1:4),IORV,IORP,(Q(K),K=K1,K2),IQ(K3)
C
        IF ( CFL.NE.'ONE' ) THEN
          LISV2 = LQ(LISV2)
          GOTO 1
        ENDIF
C
      ENDIF
C
      RETURN
  100 FORMAT(//' VERTEX BANK (ISV2)'/
     1 ' NO.  NAME IORV IORP'
     2 ,7X,'PX',8X,'PY',8X,'PZ',9X,'E',6X,'MASS'
     3 ,8X,'X',9X,'Y',9X,'Z',5X,'VTX TYPE')
  101 FORMAT(I4,2X,A4,2I5,2X,5F10.3,3F10.4,3X,I5)
      END
