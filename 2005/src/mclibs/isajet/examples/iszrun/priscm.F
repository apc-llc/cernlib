C-              banks 'ISCM'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LISCM  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NISCM  [I] : Bank number, used only if CFL='ONE' and LISCM = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LISCM point to a bank, or if <0, NISCM is
C-                                  the bank number.
C-                          'LINEAR' : LISCM points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  13-DEC-1989 10:20:16.28  Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
C
#include "isajet/zebcom.inc"
#include "isajet/iziscm.inc"
#include "isajet/izisab.inc"
C
      INTEGER PRUNIT, LISCM, NISCM, IFL
      CHARACTER*(*) CFL
      CHARACTER LINE*80,STRING*4,CR*1
      INTEGER N
      INTEGER LISCM1, GZISCM, LZLOC, J
C----------------------------------------------------------------------
      LISCM1 = LISCM
      CR = CHAR(13)
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LISCM .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LISCM .LE. 0 ) THEN
          IF( NISCM .EQ. 0 ) GOTO 980          ! Error exit
          LISCM1 = LZLOC( IXMAIN, 'ISCM', NISCM )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LISCM1 = GZISCM( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRISCM ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LISCM1
C
      LINE = ' '
      N = 0
      WRITE( PRUNIT, 1200 )
      DO   J = 1, IQ( LISCM1-1)
        CALL UHTOC ( IQ( LISCM1 + J ),4,STRING,4)
        IF (N.GT.0) THEN
          LINE(1:N+4)  = LINE(1:N)//STRING(1:4)
        ELSE
          LINE(1:4)  = STRING(1:4)
        END IF
        IF (INDEX(STRING,CR ) .GT. 0 ) THEN
          WRITE( PRUNIT, 1100 ) LINE
          LINE = ' '
          N = 0
        ELSE
          N = N + 4
        END IF
      END DO
 1200 FORMAT(/' ISAJET COMMAND FILE       ** ISCM  BANK ** ',/1X)
 1100 FORMAT(A80)
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LISCM1 = LQ( LISCM1 )
        IF( LISCM1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LISCM1 = GZISCM()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LISCM
 2000 FORMAT(/' ** PRISCM ** called for LINEAR without valid bank '
     &        'pointer, LISCM =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LISCM, NISCM
 2100 FORMAT(/'  ** PRISCM ** called for ONE without bank pointer and '
     &        'bank number, LISCM =',I10,' NISCM =', I10/)
      GOTO 999
      END
