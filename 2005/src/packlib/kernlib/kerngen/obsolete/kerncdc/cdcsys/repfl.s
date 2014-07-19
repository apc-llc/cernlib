*
* $Id: repfl.s,v 1.1.1.1 1996/02/15 17:51:36 mclareni Exp $
*
* $Log: repfl.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:36  mclareni
* Kernlib
*
*
          IDENT REPFL
*
* CERN PROGLIB# Z039    REPFL           .VERSION KERNCDC  0.1   740511
*
          ENTRY REPFL
          EXT   SYSERR.
*         30-NOV-73
*
          USE   /EXCHPAK/
 JCONDT   BSS   1
 JCODE    BSS   1
 LREP     BSS   1
 XCHC     BSS   16
 PARITY   BSS   2
          USE
*
*------   REPFL,  ENTRY POINT FOR FORTRAN LIBRARY ERRORS
*
 DSYST    VFD   36/6HSYSTEM,24/SYSERR. TRACE-WORD -SYSTEM-
          RJ    DQREC
-         VFD   30/DSYST
*
          VFD   30/5HREPFL,30/DQREC
 DQREC    EQ    DSYST+2            TRACE SIMULATION, ENTRY -REPFL-
-         VFD   30/0
 REPFL    DATA  0
          SB7   1              B7= 1
          SA4   B1             X4= ERR. NO
          MX7   0
          BX6   X4
          SA6   JCONDT             JCONDT= ERR NO
          SA7   A6+B7              JCODE = 0
*
          SA3   A7+B7              SET RJ TO RECOVERY ROUTINE
          SA4   RJINST2
          LX3   30
          BX6   X3+X4
          SA6   RJ2
*
          SA5   B1+B7          X5= ADR(CODE WORD)
          SA5   X5             X5= CODE WORD
          BX6   X5
          SA6   A3+B7              STORE CODE 1
          SA7   A6+B7              ZERO CODE 2
          SA7   A7+B7              ZERO CODE 3
          SA7   A7+B7
          SA7   A7+B7
          SA7   A7+B7
*
          MX0   54
          SB6   5              B6= 5 LOOP COUNTER
 LOOP     BX6   -X0*X6
          ZR    X6,EXIT
          SA5   A5+B7          X5= NEXT CODE WORD
          SB6   B6-B7
          BX6   X5
          SA6   A6+B7
          EQ    B0,B6,LOOP
 EXIT     BSS   0
 RJ2      DATA  0
          EQ    REPFL
*
 RJINST2  VFD   6/1,54/DQREC-1
          END
