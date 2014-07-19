C-              banks 'ISJT'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LISJT  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NISJT  [I] : Bank number, used only if CFL='ONE' and LISJT = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LISJT point to a bank, or if <0, NISJT is
C-                                  the bank number.
C-                          'LINEAR' : LISJT points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: not used
C-
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  21-JULY-1989  Serban Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
C
#include "isajet/zebcom.inc"
C
      INTEGER PRUNIT, LISJT, NISJT, IFL
      CHARACTER*(*) CFL
      INTEGER LISJT1, GZISJT, LZLOC, J,LSUP,NJET,ND
C----------------------------------------------------------------------
C
      LISJT1 = LISJT
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LISJT .LE. 0 ) GOTO 990
C
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LISJT .LE. 0 ) THEN
          IF( NISJT .EQ. 0 ) GOTO 980          ! Error exit
          LISJT1 = LZLOC( IXMAIN, 'ISJT', NISJT )
        ENDIF
C
      ELSEIF( CFL .EQ. 'ALL' ) THEN
        LISJT1=GZISJT()
        IF ( LISJT1.LE.0 ) THEN
          WRITE(PRUNIT,981)
          GOTO 999                    ! no banks
        ENDIF
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRISJT ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
      WRITE( PRUNIT, 1101 ) ' No. ',' ET ',' Px ',' Py ',' Pz ',' E ',
     &  ' Mass ',' Phi ',' Theta ',' Eta '
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LISJT1
C
      IF(LISJT1.EQ.0) GOTO 999
      NJET=IQ(LISJT1-5)
      ND=IQ(LISJT1-1)
      WRITE( PRUNIT, 1100 ) NJET,(Q( LISJT1 + J ) , J = 1, ND)
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR'.OR.CFL.EQ.'ALL' ) THEN
        LISJT1 = LQ( LISJT1 )
        IF( LISJT1 .NE. 0 ) GOTO 1
      ELSE
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LISJT
 2000 FORMAT(/' ** PRISJT ** called for LINEAR without valid bank '
     &        'pointer, LISJT =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LISJT, NISJT
 2100 FORMAT(/'  ** PRISJT ** called for ONE without bank pointer and '
     &        'bank number, LISJT =',I10,' NISJT =', I10/)
      GOTO 999
  981 FORMAT(/' ** PRISJT ** no ISJT banks'/)
 1101 FORMAT(/'ISAJET TOY CALORIMETER JETS (ISJT)',
     &  /A5,1X,9A8,2A4,1X,A8)
 1100 FORMAT(I3,3X,9F8.2)
      END
