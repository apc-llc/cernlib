C-              banks 'PJHD'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LPJHD  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NPJHD  [I] : Bank number, used only if CFL='ONE' and LPJHD = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LPJHD point to a bank, or if <0, NPJHD is
C-                                  the bank number.
C-                          'LINEAR' : LPJHD points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   7-NOV-1989 17:57:58.00  Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
C
#include "isajet/zebcom.inc"
#include "isajet/izpjhd.inc"
C
      INTEGER PRUNIT, LPJHD, NPJHD, IFL
      CHARACTER*(*) CFL
      INTEGER LPJHD1, GZPJHD, LZLOC, J,K,L
C----------------------------------------------------------------------
      LPJHD1 = LPJHD
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LPJHD .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LPJHD .LE. 0 ) THEN
          IF( NPJHD .EQ. 0 ) GOTO 980          ! Error exit
          LPJHD1 = LZLOC( IXMAIN, 'PJHD', NPJHD )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LPJHD1 = GZPJHD( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRPJHD ** Illegal value of CFL = ',A/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LPJHD1
C
      IF (IQ(LPJHD+2) .EQ. 1) THEN
        WRITE( PRUNIT,'(/A)') 'PJET CONE (R) ALGORITHM - (PJHD)'
        WRITE( PRUNIT, 1101 )  'NV','N ALG','NPJET','R CONE CUT',
     &  'JET ET CUT', ' MAX ITER ', ' IR ','MUON'
      ELSE IF (IQ(LPJHD+2) .EQ. 2) THEN
        WRITE( PRUNIT,'(/A)') ' PJET ANGLE ALGORITHM - (PJHD) '
        WRITE( PRUNIT, 1101 )  'NV','N ALG','NPJET','ANG CUT(rad)',
     &  'JET ET CUT', ' MAX ITER ', ' IR ',' MUON'
      ELSE
        WRITE( PRUNIT,'(/A)') ' PJET UNDEFINED ALGORITHM - (PJHD) '
        WRITE( PRUNIT, 1101 )  'NV','N ALG','NPJET',' CUT ',
     &  'JET ET CUT', ' MAX ITER ', ' IR ','MUON'
      END IF
      WRITE( PRUNIT, 1100 ) ( IQ( LPJHD1 + J ) , J = 1,3),
     &  ( Q( LPJHD1+K) , K = 4,5), ( IQ( LPJHD1 + L ) , L = 6,8)
 1100 FORMAT( /1X,I5,2I8,2F15.3,3I8)
 1101 FORMAT( 1X,A5,2A8,2A15,3A8)
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LPJHD1 = LQ( LPJHD1 )
        IF( LPJHD1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LPJHD1 = GZPJHD()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LPJHD
 2000 FORMAT(/' ** PRPJHD ** called for LINEAR without valid bank ',
     &        'pointer, LPJHD =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LPJHD, NPJHD
 2100 FORMAT(/'  ** PRPJHD ** called for ONE without bank pointer and ',
     &        'bank number, LPJHD =',I10,' NPJHD =', I10/)
      GOTO 999
      END
