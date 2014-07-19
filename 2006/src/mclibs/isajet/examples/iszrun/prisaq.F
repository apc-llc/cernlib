C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LISAQI= bank address
C-  NISAQ = bank number
C-  CFL   = flag to control printout
C-          'ALL' for all banks, 'LINEAR' for one linear structure
C-          'ONE' for one bank only
C-          LISAQI must be provided for 'LINEAR',
C-          LISAQI or NISAQ may be provided for 'ONE',
C-          LISAQI and NISAQ ignored for 'ALL'
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
      INTEGER PRUNIT,LISAQI,NISAQ,IFL
      CHARACTER CFL*(*)
      INTEGER LZLOC,LISAQ,GZISAQ
      INTEGER NQ,NJ,NPJ,K,K1,K2
      CHARACTER*8 NAME,LABEL
C
      LISAQ=LISAQI
      IF(CFL.EQ.'ONE') THEN
        IF(LISAQ.EQ.0) THEN
          IF(NISAQ.EQ.0) GOTO 98      ! error exit
          LISAQ=LZLOC(IXMAIN,'ISAQ',NISAQ)
        ENDIF
      ENDIF
C
      IF(CFL.EQ.'ALL') THEN
        LISAQ=GZISAQ()   ! find pointer to first bank
      ENDIF
C
C        print title
C
      IF(IFL.EQ.0) WRITE (PRUNIT,100)
      IF(IFL.NE.0) WRITE (PRUNIT,101)
C
    1 IF(LISAQ.GT.0) THEN
C
C
C ****  find parent (ISAJ) jet
C
        NJ=LQ(LISAQ-1)
        IF(NJ.NE.0) NJ=IQ(NJ-5)
C
C ****  find associated PJET if PJET links exists
C
        NPJ = 0
        IF( IQ(LISAQ-3) .GT. 1) THEN
          NPJ=LQ(LISAQ-2)
          IF(NPJ.NE.0) NPJ=IQ(NPJ-5)
        END IF
C
C   Print contents of bank
C
        NQ=IQ(LISAQ-5)
        K1=LISAQ+2
        K2=LISAQ+9
        IF(IFL.NE.0) K2=LISAQ+6
        NAME=LABEL(IQ(LISAQ+1))
        WRITE(PRUNIT,102) NQ,NAME(1:4),NJ,NPJ,(Q(K),K=K1,K2)
C
        IF(CFL.NE.'ONE') THEN
          LISAQ=LQ(LISAQ)               ! pointer to next bank
          GOTO 1
        ENDIF
C
      ENDIF
C
      RETURN
   98 PRINT 111,LISAQ,NISAQ
      RETURN
   99 PRINT 112,LISAQ
      RETURN
  100 FORMAT('0',///,' PARTON BANKS (ISAQ)',/,
     1 ' NO.  NAME  JET   PJET',5X,'PX',8X,'PY',8X,'PZ',9X,'E'
     2 6X,'MASS',7X,'PHI',5X,'THETA',7X,'ETA')
  101 FORMAT('0',///,' PARTON BANKS (ISAQ)',/,
     1 ' NO.  NAME   JET  PJET',5X,'PX',8X,'PY',8X,'PZ',9X,'E'
     2 6X,'MASS')
  102 FORMAT(I3,4X,A4,2I4,5F10.3,3F10.4)
  111 FORMAT('0',//,'  FOR A SINGLE BANK PRINTOUT OF ISAQ YOU MUST',
     1 ' DEFINE POINTER OR BANK NUMBER',/,' THEY ARE NOW SET TO',2I10)
  112 FORMAT('0',//,' FOR PRINTOUT OF LINEAR ARRAY OF ISAQ',
     1 ' YOU MUST DEFINE POINTER',/,' IT IS NOW SET TO',I10)
      END
