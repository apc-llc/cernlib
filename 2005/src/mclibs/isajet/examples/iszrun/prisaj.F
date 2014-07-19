C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LISAJI= bank address
C-  NISAJ = bank number
C-  CFL   = flag to control printout
C-          'ALL' for all banks, 'LINEAR' for one linear structure
C-          'ONE' for one bank only
C-          LISAJI must be provided for 'LINEAR',
C-          LISAJI or NISAJ may be provided for 'ONE',
C-          LISAJI and NISAJ ignored for 'ALL'
C-  IFL   = 0  print everything
C-          1  print only 4-momenta and mass
C-
C-     SDP  Jan,1986 , Rev. Feb,1986
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/zebcom.inc"
      CHARACTER CFL*(*)
      INTEGER LZLOC,LISAJ,GZISAJ
      INTEGER PRUNIT,LISAJI,NISAJ,IFL
      INTEGER K,K1,K2,NJ
      CHARACTER*8 NAME,LABEL
C
      LISAJ=LISAJI
      IF(CFL.EQ.'ALL') THEN
        LISAJ=GZISAJ()
      ENDIF
C
      IF(CFL.EQ.'ONE') THEN
        IF(LISAJ.LE.0) LISAJ=LZLOC(IXMAIN,'ISAJ',NISAJ)
      ENDIF
C
C          Print titles
C
      WRITE(PRUNIT,100)
C
    1 IF(LISAJ.GT.0) THEN
C
C   Print contents of bank
C
        NJ=IQ(LISAJ-5)
        K1=LISAJ+2
        K2=LISAJ+9
        IF(IFL.EQ.1) K2=LISAJ+6
        NAME=LABEL(IQ(LISAJ+1))
        WRITE(PRUNIT,101) NJ,NAME,(Q(K),K=K1,K2)
C
        IF(CFL.NE.'ONE') THEN
          LISAJ=LQ(LISAJ)
          GOTO 1
        ENDIF
C
      ENDIF
      RETURN
  100 FORMAT('0',///,' JET BANKS (ISAJ)',/,
     1 ' NO.  NAME ',10X,'PX',8X,'PY',8X,'PZ',9X,'E'
     2 ,6X,'MASS',7X,'PHI',5X,'THETA',7X,'ETA')
  101 FORMAT(I4,2X,A8,2X,5F10.3,3F10.4)
      END
