C-
C-  INPUT:
C-  PRUNIT = unit number for printout
C-  LISP3I = bank address
C-  NISP3  = bank number
C-  CFL    = flag to control printout
C-           'ALL' for all banks, 'LINEAR' for one linear structure
C-           'ONE' for one bank only
C-            LISP3I must be provided for 'LINEAR',
C-            LISP3I or NISP3 may be provided for 'ONE',
C-            LISP3I and NISP3 ignored for 'ALL'
C-  IFL    = not used
C-
C-     SDP  Jan,1986 , Rev. Feb,1986
C-     SK   Apr,1986   Modified from SDP's PRISV1.
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
#include "isajet/izisp3.inc"
      CHARACTER CFL*(*)
      INTEGER LZLOC,IDAUV
      INTEGER PRUNIT,LISP3I,NISP3,IFL
      INTEGER NV,K,K1,K2,K3,IORV,IORP,LISP3
      CHARACTER*8 NAME,LABEL
C
C          Print titles
C
      WRITE(PRUNIT,100)
C
      LISP3 = LISP3I
      IF ( CFL.EQ.'ALL' ) THEN
        LISP3 = LQ(LISAE-IZISP3)
      ENDIF
C
    1 IF ( LISP3.GT.0 ) THEN
C
C       find parent vertex
        IORV = 0
        IF ( LQ(LISP3-2).NE.0 ) IORV = IQ(LQ(LISP3-2)-5)
        IORP = 0
        IF ( LQ(LISP3-3).NE.0 ) IORP = IQ(LQ(LISP3-3)-5)
C
C               find daughter vertex
        IDAUV = LQ(LISP3-4)
        IF ( IDAUV.GT.0 ) IDAUV = IQ(IDAUV-5)
C
C   Print contents of bank
C
        NV = IQ(LISP3-5)
        K1 = LISP3+2
        K2 = LISP3+9
        K3 = LISP3+10
        NAME = LABEL(IQ(LISP3+1))
        WRITE(PRUNIT,101) NV,NAME(1:4),IORV,IORP,IDAUV,(Q(K),K=K1,K2),
     &    IQ(K3)
C
        IF ( CFL.NE.'ONE' ) THEN
          LISP3 = LQ(LISP3)
          GOTO 1
        ENDIF
C
      ENDIF
C
      RETURN
  100 FORMAT(//' PARTICLE BANK (ISP3)'/
     1 ' NO.  NAME IORV IORP DAUV'
     2 ,7X,'PX',8X,'PY',8X,'PZ',9X,'E',6X,'MASS'
     2 ,8X,'X',9X,'Y',9X,'Z',5X,'TRK TYPE')
  101 FORMAT(I4,2X,A4,3I5,2X,5F10.3,3F10.4,3X,I5)
      END
