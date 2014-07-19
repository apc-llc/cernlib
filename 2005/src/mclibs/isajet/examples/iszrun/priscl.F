C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LISCLI= bank address
C-  NISCL = bank number
C-  CFL   = flag to control printout
C-          'ALL' for all banks, 'LINEAR' for one linear structure
C-          'ONE' for one bank only
C-          LISCLI must be provided for 'LINEAR',
C-          LISCLI or NISCL may be provided for 'ONE',
C-          LISCLI and NISCL ignored for 'ALL'
C-  IFL   = 0  print everything
C-          1  print only indices, no. of hadrons and gammas,
C-             e.m. and hadronic energy
C-
C-     SDP  Jan,1986 , Rev. Feb,1986
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/izisac.inc"
#include "isajet/iziscl.inc"
#include "isajet/zebcom.inc"
      CHARACTER CFL*(*)
      INTEGER LZLOC,LISAC,LISCL
      INTEGER PRUNIT,LISCLI,NISCL,IFL
      INTEGER K,K1,K2,IETA,IPHI,NCH,NG,NCELLS
C
      LISCL=LISCLI
      IF(CFL.EQ.'ALL') THEN
        LISAC=LQ(LISAE-IZISAC)
        LISCL=LQ(LISAC-IZISCL)
      ENDIF
C
      IF(CFL.EQ.'ONE') THEN
        IF(LISCL.LE.0) LISCL=LZLOC(IXMAIN,'ISCL',NISCL)
      ENDIF
C
C          Print titles
C
      IF(IFL.EQ.0) WRITE(PRUNIT,100)
      IF(IFL.EQ.1) WRITE(PRUNIT,101)
      NCELLS=0
C
    1 IF(LISCL.GT.0) THEN
C
C   Print contents of bank
C
        NCELLS=NCELLS+1
        IF(NCELLS.EQ.100) THEN
          NCELLS=0
          WRITE(PRUNIT,103)   ! print header
        ENDIF
        IPHI=IQ(LISCL+1)/10000
        IETA=MOD(IQ(LISCL+1),10000)
        NCH=IQ(LISCL+2)/10000
        NG=MOD(IQ(LISCL+2),10000)
        K1=LISCL+3
        K2=LISCL+10
        IF(IFL.EQ.1) K2=LISCL+4
        WRITE(PRUNIT,102) IETA,IPHI,NCH,NG,(Q(K),K=K1,K2)
C
        IF(CFL.NE.'ONE') THEN
          LISCL=LQ(LISCL)
          GOTO 1
        ENDIF
C
      ENDIF
      RETURN
  100 FORMAT('0',///,' PSEUDOCALORIMETER BANKS (ISCL)',/,
     $ ' IETA IPHI    NCH  NG',7X,'EEM',6X,'EHAD'
     $ ,6X,'SINT',6X,'COST',6X,'SINP',6X,'COSP',7X,'PHI',7X,'ETA')
  101 FORMAT('0',///,' PSEUDOCALORIMETER BANKS (ISCL)',/,
     $ ' IETA IPHI    NCH  NG',7X,'EEM',6X,'EHAD')
  103 FORMAT('0',/,' IETA IPHI    NCH  NG',7X,'EEM',6X,'EHAD'
     $ ,6X,'SINT',6X,'COST',6X,'SINP',6X,'COSP',7X,'PHI',7X,'ETA')
  102 FORMAT(2I5,2X,2I5,2X,2F10.3,6F10.4)
      END
