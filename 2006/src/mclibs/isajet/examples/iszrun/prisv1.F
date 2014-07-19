C-
C-  INPUT:
C-  PRUNIT = unit number for printout
C-  LISV1I = bank address
C-  NISV1  = bank number
C-  CFL    = flag to control printout
C-           'ALL' for all banks, 'LINEAR' for one linear structure
C-           'ONE' for one bank only
C-            LISV1I must be provided for 'LINEAR',
C-            LISV1I or NISV1 may be provided for 'ONE',
C-            LISV1I and NISV1 ignored for 'ALL'
C-  IFL    = not used
C-
C-     SDP  Jan,1986 , Rev. Feb,1986
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
      CHARACTER CFL*(*)
      INTEGER LZLOC,GZISV1
      INTEGER PRUNIT,LISV1I,NISV1,IFL
      INTEGER NV,K,K1,K2,IORIV,K3,K4,LISV1
      CHARACTER*8 NAME,LABEL
C
C          Print titles
C
      WRITE(PRUNIT,100)
C
      LISV1 = LISV1I
      IF ( CFL.EQ.'ONE' ) THEN
        IF ( LISV1.LE.0 .AND. NISV1.GT.0 )
     &    LISV1 = LZLOC(IXMAIN,'ISV1',NISV1)
      ENDIF
      IF ( CFL.EQ.'ALL' ) THEN
        LISV1 = GZISV1()
      ENDIF
C
    1 IF ( LISV1.GT.0 ) THEN
C
C       find parent vertex
        IORIV = LQ(LISV1-2)
        IF ( IORIV.NE.0 ) IORIV = IQ(IORIV-5)
C
C   Print contents of bank
C
        NV = IQ(LISV1-5)
        K1 = LISV1+2
        K2 = LISV1+9
        NAME = LABEL(IQ(LISV1+1))
        WRITE(PRUNIT,101) NV,NAME(1:4),IORIV,(Q(K),K=K1,K2)
C
        IF ( CFL.NE.'ONE' ) THEN
          LISV1 = LQ(LISV1)
          GOTO 1
        ENDIF
C
      ENDIF
C
      RETURN
  100 FORMAT('0',//,' VERTEX BANK (ISV1)',/,
     1 '  NO.  NAME ORIV',7X,'PX',8X,'PY',8X,'PZ',9X,'E',6X,'MASS'
     2 ,8X,'X',9X,'Y',9X,'Z')
  101 FORMAT(I4,2X,A4,I5,2X,5F10.3,3F10.4)
      END
