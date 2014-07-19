*
* $Id: suopen.s,v 1.1.1.1 1996/04/01 15:03:19 mclareni Exp $
*
* $Log: suopen.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:19  mclareni
* Mathlib gen
*
*
#if (defined(CERNLIB_NOSBE))&&(defined(CERNLIB_F4))
          IDENT SUOPEN
          SST
          ENTRY SUOPEN
          EXT   GETFIT.
*
*     CALL SUOPEN (LUN,JREW,JOUT)   OPEN FILE, JREW=0 REWIND,
*                                              JOUT=0 INPUT FILE
*
 TRACE    VFD   36/6HSUOPEN,24/SUOPEN
 HOME     SA5   SVA0
          SA0   X5
 SUOPEN   DATA  0
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA2   X2
          SA3   X3
          BX6   X2
          BX7   X3
          SA6   SVMODE             SAVE JREW
          SA7   A6+B7              SAVE JOUT
          SX6   A0
          SA6   A7+B7              SAVE A0
+RJGETFIT
-         VFD   30/TRACE
          BX6   X1
          SA6   SVFIT              SAVE FIT-ADR
          FETCH X1,OC,X3           CHECK FILE IS OPEN
          SX3   X3-1
          ZR    X3,RUNNING
*
 OPEN     SA2   SVMODE         X2= JREW  = 0 IF REWIND
          SA3   SVMODE+1       X3= JOUT  .NE. 0 IF OUTPUT
          STORE X1,BT=C
          STORE X1,CM=NO
*         STORE X1,MRL=5120
          SX6    B7      1 = FILE OF BINARY TYPE
          LX6    18
          SA6    X1+35   STORE TYPE IN FIT EXTENSION
          NZ    X3,OUTF
          STORE X1,RT=S
          NZ    X2,RESIN
          OPENM X1,INPUT,R,SQ
          EQ    CHECK
 RESIN    OPENM X1,INPUT,N,SQ
          EQ    CHECK
*
 OUTF     STORE X1,RT=U
          NZ    X2,RESOUT
          OPENM X1,OUTPUT,R,SQ
          EQ    CHECK
 RESOUT   OPENM X1,OUTPUT,N,SQ
*
*     CHECK RECORD-TYPE NOT OVERWRITTEN BY FILE-CARD
*
 CHECK    EQ    HOME
 SVMODE   DATA  0
          DATA  0
 SVA0     DATA  0
 SVFIT    DATA  0
 MSG      VFD   60/10HFILE-CARD         *BL
          VFD   60/10HNOT ALLOWE        *BL
          VFD   60/10HD FOR   -         *BL
 FN       DATA  0
*
*  FILE IS ALREADY OPEN, CHECK RT-CHANGE, CHECK REWIND
*
 RUNNING  FETCH X1,RT,X5
          SA3   SVMODE+1
          ZR    X3,WANTW
          SX5   X5-7
 WANTW    NZ    X5,CLS
          SA3   SVMODE
          NZ    X3,HOME
 CLS      CLOSEM X1,N,FILE,SQ
          SA1   SVFIT
          EQ    OPEN
          END
#endif
