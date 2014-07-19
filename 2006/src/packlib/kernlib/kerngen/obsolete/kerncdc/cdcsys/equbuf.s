*
* $Id: equbuf.s,v 1.1.1.1 1996/02/15 17:51:33 mclareni Exp $
*
* $Log: equbuf.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:33  mclareni
* Kernlib
*
*
          IDENT EQUBUF
*
* CERN PROGLIB# Z260    EQUBUF          .VERSION KERNCDC  2.12  840712
*
          ENTRY EQUBUF
          EXT   FNNUM,GETFIT.
          SST
*
*     CALL EQUBUF (LUNEXT,LUNINT,CLOPT,NERROR)
*         CLOPT    CLOSING OPTION: N, R, U  - ABSENT=U
*         NERROR   RETURN RM ERROR   - ABSENT:  ERROR IGNORED
*
          USE   /SLATE/
 DUMMY    BSS   10
 SAVEA0   BSS   1              A0 SAVED
 ARGE     BSS   1              NEW FILE NAME
 LARG3    BSS   1              ADR OF 3RD ARGUMENT
 LARG4    BSS   1              ADR OF 4TH ARGUMENT
 FITADR   BSS   26             ADR OF FIT
          USE
*
 TRACE    VFD   36/6HEQUBUF,24/EQUBUF
 EQUBUF   DATA  0
          SB1   1
*                              X1= LARG1 = ADR(ARG1=LUNEXT)
          SA2   A1+B1          X2= LARG2 = ADR(ARG2=LUNINT)
          SA3   A2+B1          X3= LARG3 = ADR(ARG3=CLOPT)
          SA5   X1             X5= ARG1 = LUNEXT
          BX1   X2             X1= ADR(LUNINT)
          SX6   A0
          SA6   SAVEA0
          BX6   X5
          SA6   A6+B1              SAVE ARG1 INTO ARGE
          BX6   X3
          SA6   A6+B1              SAVE LARG3
          MX7   0
          SA7   A6+B1              SET  LARG4 = 0
          ZR    X3,ALLSVD
          SA4   A3+B1          X4= LARG4 = ADR(ARG4=NERROR)
          ZR    X4,ALLSVD
          SA7   X4                 SET ARG4=0
          BX7   X4
          SA7   A6+B1              SAVE LARG4
*
 ALLSVD   BSS   0
          MX6   1
          BX1   X1+X6
+RJGETFIT
-         VFD   30/TRACE
          BX7   X1
          SA7   FITADR             SAVE ADR OF FIT
          SB1   1              B1= 1 RESET TO BE SAFE
          STORE X1,EX=RMERROR
          FETCH X1,OC,X4
          SB3   X4
          NE    B3,B1,CLEAN        JUMP IF FILE NOT OPEN
          SA3   LARG3
          ZR    X3,UNLD            NO ARG3
          SA4   X3             X4= ARG3
          MX0   6
          BX6   X0*X4              ISOLATE FIRST CHAR IN ARG3
          LX6   6
          SX5   X6-1RR
          SX4   X6-1RN
          ZR    X5,REWD
          ZR    X4,NORE
*
 UNLD     CLOSEM X1,U
          JP    CLEAN
 REWD     CLOSEM X1,R
          JP    CLEAN
 NORE     CLOSEM X1,N
*
*--       OPEN/CLOSE DUMMY FILE TO CLEAR FIT
*
 CLEAN    BSS   0
****            SA1   FITADR
****            SA2   DUMYLFN
****            STORE X1,LFN=X2
****            OPENM X1,I-O,N
****            SA1   FITADR
****            CLOSEM X1,U
#if defined(CERNLIB_CDC76SYS)
*
*--       CLEAR FIT - SET TO ZERO WORDS  FIT+1 TO FIT+14
*
          SA1   FITADR
          FETCH X1,EO,X2           SAVE EO INTO X2
          MX6   0
          SB1   1
          SB2   14
 LOOP76   SA6   X1+B2
          SB2   B2-B1
          NE    B2,B0,LOOP76
          STORE X1,EO=X2
#endif
*
*--       SET LFN INTO FIT
*
 SETLFN   SA1   ADRARGE            CONVERT FILE-NAME TO 7LXXXXXXX
+RJFNNUM
-         VFD   30/TRACE
          SA1   FITADR         X1= ADR OF FIT
          STORE X1,LFN=X6          STORE FILE NAME
          STORE X1,EX=0            CANCEL ERROR EXIT
          STORE X1,CF=N            STORE -NO REWIND ON CLOSE-
          STORE X1,OF=N            STORE -NO REWIND ON OPEN-
          SA4   SAVEA0             RESTORE A0
          SA0   X4
          JP    EQUBUF
 ADRARGE  VFD   42/0,18/ARGE
****   DUMYLFN  VFD   42/7HZQZZQQZ,18/0
*
*----     HANDLE RM ERROR
*
 RMERROR  DATA  0
          SA1   FITADR
          FETCH X1,ES,X6
          SA2   LARG4
          ZR    X2,QUIT
          SA6   X2                 STORE ERROR NUMBER
 QUIT     STORE X1,ES=0
          JP    RMERROR
          END
