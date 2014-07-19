C-
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LISACI= bank address (not used)
C-  NISAC = bank number  (not used)
C-  CFL   = not used
C-
C-  IFL   = not used
C-
C-     SDP  May, 1986
C-
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/izisac.inc"
#include "isajet/zebcom.inc"
      INTEGER PRUNIT,LISACI,NISAC,IFL
      INTEGER NCPHI,NCY
      REAL DELPHI,DELY,YCMIN,YCMAX
      CHARACTER CFL*(*)
      INTEGER LISAC
C
      IF(LISAE.LE.0) RETURN
        LISAC=LQ(LISAE-IZISAC)
C
    1 IF(LISAC.GT.0) THEN
C
C   Print contents of bank
C
        NCPHI=IQ(LISAC+1)    ! number of phi cells
        NCY=IQ(LISAC+2)      !   "        y    "
        DELPHI=Q(LISAC+3)   ! phi cell size
        DELY=Q(LISAC+4)     !  y    "   "
        YCMIN=Q(LISAC+5)    ! minimum y
        YCMAX=Q(LISAC+6)    ! maximum y
        WRITE(PRUNIT,101) NCPHI,NCY,DELPHI,DELY,YCMIN,YCMAX
C
      ENDIF
C
C
      RETURN
  101 FORMAT('0',///,' Calorimeter description bank ISAC',//,
     $' Number of phi cells=',I4,10X,'Number of y cells=',I4,/,
     $' phi cell size (rad)=',F10.5,10X,'y cell size=',F10.5,/,
     $' minimum y=',F10.3,10X,'maximum y=',F10.3)
      END
