*
* $Id: equbuf.s,v 1.1.1.1 1996/02/15 17:51:32 mclareni Exp $
*
* $Log: equbuf.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:32  mclareni
* Kernlib
*
*
          IDENT EQUBUF
*
* CERN PROGLIB# Z260    EQUBUF          .VERSION KERNCDC  1.16  820129
*
          ENTRY EQUBUF
          EXT   ABEND
          SST
*      MODEL  MICRO  1,,*74*  OR  1,,*76*
#include "defmodel.inc"
*
*     CALL EQUBUF (LUNEX,LUNIN,OPTION,NERROR)
*       OPTION MAY BE  N    NO REWIND
*                      R    REWIND
*                      U    UNLOAD / DEFAULT IF 3RD ARG NOT GIVEN.
*       NERROR IS THE RETURN CODE (OPTIONAL)
*             IT RETURNS THE RM ERROR IF ANY (0 OTHERWISE)
*             IF A RM OCCURS WITHOUT NERROR SPECIFIED, THE JOB IS ABORTED.
*             A DAYFILE MESSAGE IS PRINTED FOR ANY RM-ERROR
*
 NAME     VFD   42/0HEQUBUF,18/EQUBUF
 ARG3     BSS   1                  ADDRESS OF 3RD ARG. IF ANY.
 ADRC     BSSZ  1                  ADDRESS OF 4TH ARG. IF ANY.
 RC       BSSZ  1                  TO SAVE FIES
 EXNAME   BSS   1
 MSG1     DATA  H*EQUBUF - FILE NOT DECLARED =*   *BL
 FILE1    BSSZ  1
 MSG2     DATA  H*EQUBUF - RM ERROR =*            *BL
 RMNR     DATA  H*      ON *                      *BL
 LFN      BSSZ  1
 TEMPA0   BSSZ  1
 FICF     BSS   1
 #MD      IFC   LT, "MODEL" 75          *BL
 LNFIT    SET   18
 FIT0     SET   6
 #MD      ELSE
 LNFIT    SET   14
 FIT0     SET   0
 #MD      ENDIF
 EQUBUF   EQ    *+1BS17
          SX6   A0
          MX0   42
          SA6   TEMPA0             SAVE A0
*
*        ASSIGN TO THE EXTERNAL FILE THE SAME FIT AS THE INTERNAL FILE.
          SB7   1
          SA4   A1+B7
          SA2   X1             X2= NAME OF EXTERNAL FILE.
          SA3   A4+B7          X3= ADDRESS OF 3RD ARGUMENT.
          SA1   X4             X1= NAME OF INTERNAL FILE.
          MX6   0
          ZR    X3,SAVP4           JUMP IF ONLY 2 ARG
          SA4   A3+B7
          ZR    X4,SAVP4           JUMP IF ONLY 3 ARGS
          MX7   0
          BX6   X4
          SA7   RC                 PRESET RC TO ZERO
 SAVP4    BSS   0
          SA6   ADRC               SAVE ADDRESS OF 4TH ARG
          BX6   X2
          RJ    GETNAME
          BX2   X6
          BX6   X1
          RJ    GETNAME
          BX1   X6
          SA4   B7+B7              GET FIRST LOGICAL FILE NAME
 SEARCH   ZR    X4,NOTDEC
          BX7   X0*X4
          BX5   X7-X1
          ZR    X5,FOUND
          SA4   A4+B7
          EQ    SEARCH
*
 NOTDEC   SA7   FILE1
          MESSAGE MSG1
*
 ABT      BSS   0
          RJ    ABEND
*
 FOUND    SX1   X4             X1= FIT ADDRESS.
          LX7   X2
 #MD      IFC   LT, "MODEL" 75          *BL
          SA0   X1                 SAVE X1 WHICH WILL BE DESTROYED BY 6R
 #MD      ENDIF
          BX6   X3
          SA7   EXNAME
          SA6   ARG3
          STORE X1,EX=RMERROR
          FETCH X1,CF,X6
          SA6   FICF               SAVE THE ORIGINAL CF
          FETCH X1,OC,X4
          SX3   X4-1
          ZR    X4,CHFIT           FILE NEVER OPENED
          NZ    X3,CLEANFT         FILE ALREADY CLOSED
          SA3   ARG3
          ZR    X3,UNLOAD
          SA4   X3
          MX0   6
          BX6   X4*X0
          LX6   6
          SX5   X6-1RR
          SX4   X6-1RN
          ZR    X5,REWIND
          ZR    X4,NOREW
*
 UNLOAD   CLOSEM X1,U
          EQ    CLEANFT
 NOREW    CLOSEM X1,N
          EQ    CLEANFT
 REWIND   CLOSEM X1,R
*
 CLEANFT  BSS   0
 #MD      IFC   LT, "MODEL" 75          *BL
          SX1   A0                 RESTORE X1 DESTROYED BY 6RM
*
*         CLEAN THE FET
          FETCH X1,FET,X2
          FETCH X1,FWB,X3
          MX6   0
          SB5   -1
          SB6   X3+B5
          SB7   X2             B7= FET START ADDRESS
 LOOPFET  BSS   0
          SA6   B6
          SB6   B6+B5
          GE    B6,B7,LOOPFET
 #MD      ENDIF
*
*        CLEAN THE FIT
          MX6   0
          SB5   1
          FETCH X1,EO,X2           SAVE FIEO IN X2
          SB1   LNFIT
          SB2   FIT0
 LOOPFT   SA6   X1+B1
          SB1   B1-B5
          NE    B1,B2,LOOPFT
 #MD      IFC   LT, "MODEL" 75          *BL
          FETCH X1,FET,X5          FIT+1
          BX7   X5
          SA7   X1+1
          STORE X1,DX=0            FIT+3
          MX6   0
          SA6   X1+4               FIT+4
          SA6   X1+2               FIT+2
          STORE X1,LT=ANY
          FETCH X1,FWB,X5          FIT+5
          BX6   X5
          SA6   X1+5
          STORE X1,SDS=YES
 #MD      ENDIF
          STORE X1,EO=X2
          STORE X1,OF=N
*
 CHFIT    SA2   EXNAME
          BX6   X2
          SA6   X1
          SA4   FICF
          STORE X1,CF=X4           RESTORE THE ORIGINAL CF IN THE FIT
          SA2   ADRC
          SA5   TEMPA0
          SA0   X5                 RESTORE A0
          ZR    X2,EQUBUF          NO RC, RETURN
          SA3   RC
          BX6   X3
          SA6   X2                 RETURN RC
          EQ    EQUBUF
*
*        CONVERT THE TAPE NUMBER TO DISPLAY CODE AND APPEND TO *TAPE*
*         X6 HOLDS THE RESULT, A4, X4, X5, X7 ARE DESTROYED.
 GETNAME  DATA  0
          BX4   X0*X6
          MX5   0
          NZ    X4,GETNAME         IT IS ALREADY A FILE NAME
          SA4   =4LTAPE
          SX7   X6-10
          PL    X7,GET2            IF THE TAPE NUMBER IS 10-99
          SX7   X6+1R0             N IN DISPLAY CODE
          LX7   60-5*6
          BX6   X4+X7              APPEND TO *TAPE*
          EQ    GETNAME
*
*        CONVERT A TWO DIGIT TAPE NUMBER TO DISPLAY CODE
 GET2     SX7   X6-20
          SX6   X6-10
          SX5   X5+100B
          PL    X7,GET2            THE NUMBER IS NOT YET REDUCED
          IX5   X6+X5
          SX7   X5+2R00            NN IN DISPLAY CODE
          LX7   60-6*6
          BX6   X4+X7              APPEND TO *TAPE*
          EQ    GETNAME
*
 RMERROR  DATA  0
 #MD      IFC   LT, "MODEL" 75          *BL
          SX1   A0                 RESTORE X1 DESTROYED BY 6RM
 #MD      ENDIF
          FETCH X1,IRS,X6
          SA6   RC                 SAVE FIES
          BX3   X6
*
*         TRANSFORM INTEGER IN DISPLAY CODE
          MX6   0
          MX0   60-3
          SB5   3                  ONLY 3 DIGITS IN RM ERROR
          LX3   51
          SB1   1
          SB4   1R0
 COD1     BSS   0
          LX3   3
          BX4   -X0*X3
          SX5   X4+B4
          LX6   6
          IX6   X6+X5
          SB5   B5-B1
          NZ    B5,COD1
          LX6   42
          MX0   18
          SA5   RMNR
          BX4   -X0*X5
          BX6   X0*X6
          IX6   X4+X6
          SA6   A5                 STORE RMNR
          SA2   X1
          BX6   X2
          SA6   LFN                SAVE LFN NAME
          MESSAGE MSG2
          SA2   ADRC
          ZR    X2,ABT             NO RC, ABORT
 #MD      IFC   LT, "MODEL" 75          *BL
          SX1   A0                 RESTORE X1 DESTROYED BY 6RM
 #MD      ENDIF
          STORE X1,ES=0            CLEAR FIES
          EQ    RMERROR
          END
#ifdef CERNLIB_CDCSYS_EQUBUF
#undef CERNLIB_CDCSYS_EQUBUF
#endif
