C-              banks 'PJET'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LPJETI [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NPJET  [I] : Bank number, used only if CFL='ONE' and LPJET = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LPJET point to a bank, or if <0, NPJET is
C-                                  the bank number.
C-                          'LINEAR' : LPJET points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   7-NOV-1989 18:10:09.84  Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
C
#include "isajet/izpjet.inc"
#include "isajet/izpjpt.inc"
#include "isajet/zebcom.inc"
      INTEGER PRUNIT,LPJETI,NPJET,IFL
      CHARACTER CFL*(*)
      INTEGER LZLOC,LPJET,LPJHD,GZPJHD
      INTEGER LPJPT,MAXQ
      PARAMETER (MAXQ=20)
      INTEGER NQ,NJ,K,J,IP,NP(MAXQ)
C
      LPJET=LPJETI
      IF(CFL.EQ.'ONE') THEN
        IF(LPJET.EQ.0) THEN
          IF(NPJET.EQ.0) GOTO 98      ! error exit
          LPJET=LZLOC(IXMAIN,'PJET',NPJET)
          LPJPT = LQ(LPJET-IZPJPT)
        ENDIF
      ENDIF
C
      IF(CFL.EQ.'ALL') THEN
        LPJHD  = GZPJHD ()           ! find PJHD main bank
        CALL PRPJHD(PRUNIT,LPJHD, 0, 'ONE', IFL )
        LPJET  =LQ(LPJHD - IZPJET)   ! find pointer to first bank
        LPJPT = LQ(LPJET-IZPJPT)
      ENDIF
C
C        print title
C
   10 IF ( IQ(LPJHD+2).EQ.1 ) WRITE (PRUNIT,1102) IQ(LPJET+1)
      IF ( IQ(LPJHD+2).EQ.2 ) WRITE (PRUNIT,1103) IQ(LPJET+1)
      WRITE( PRUNIT, 1101 ) ' No. ',' ET ',' Px ',' Py ',' Pz ',' E ',
     &  ' Mass ',' Phi ',' Theta ',' Eta ',' CLASS ',' NEXT ',' PJPT '
C
      IP = 0
    1 IF(LPJET.GT.0) THEN
C
C   Print contents of bank
C
        IP = IP + 1
        DO K = 2, IQ(LPJPT-3)
          NP ( K ) = LQ( LPJPT-K )
          IF (NP(K).NE. 0) NP(K) = IQ(NP(K)-5) ! ISAQ BANK NUMBER
        END DO
        WRITE( PRUNIT, 1100 ) IP , Q(LPJET+2),
     &  (Q(LPJET+J), J = 3, 10) , IQ( LPJET+11), IQ(LPJET+12),
     &  (NP(K) , K = 2, IQ(LPJPT-3) )
C
        IF(CFL.NE.'ONE') THEN
          LPJET=LQ(LPJET)               ! pointer to next bank
          LPJPT = LQ(LPJET-IZPJPT)
          GOTO 1
        ENDIF
C
      ELSE  IF(CFL.EQ.'ALL') THEN
        LPJHD  = LQ(LPJHD) ! find NEXT PJHD  bank
        IF( LPJHD .GT. 0) THEN
          CALL PRPJHD(PRUNIT,LPJHD, 0, 'ONE', IFL )
          LPJET  =LQ(LPJHD - IZPJET)   ! find pointer to first bank
          LPJPT = LQ(LPJET-IZPJPT)
          GOTO 10
        END IF
      ENDIF
C
      RETURN
   98 PRINT 111,LPJET,NPJET
      RETURN
   99 PRINT 112,LPJET
      RETURN
  111 FORMAT('0',//,'  FOR A SINGLE BANK PRINTOUT OF PJET YOU MUST',
     1 ' DEFINE POINTER OR BANK NUMBER',/,' THEY ARE NOW SET TO',2I10)
  112 FORMAT('0',//,' FOR PRINTOUT OF LINEAR ARRAY OF PJET',
     1 ' YOU MUST DEFINE POINTER',/,' IT IS NOW SET TO',I10)
 1100 FORMAT(I3,3X,9F8.2,2I4,4X,15I3,2(/85X,15I3))
 1101 FORMAT(A5,1X,9A8,2A4,1X,A8)
 1102 FORMAT(/,
     &  ' PARTON JETS BANK(PJET)  -  R CONE ALGORITHM  - VERSION',I5)
 1103 FORMAT(/,
     &  ' PARTON JETS BANK(PJET)  -  CM ANGLE ALGORITHM- VERSION', I5)
      END
