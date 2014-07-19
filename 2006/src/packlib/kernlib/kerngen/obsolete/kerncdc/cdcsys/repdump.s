*
* $Id: repdump.s,v 1.1.1.1 1996/02/15 17:51:35 mclareni Exp $
*
* $Log: repdump.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:35  mclareni
* Kernlib
*
*
          IDENT REPDUMP
*
* CERN PROGLIB# Z038    REPDUMP         .VERSION KERNCDC  0.1   740511
*
          ENTRY REPDUMP
          EXT   REPDMP2
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
          VFD   42/7HREPDUMP,18/REPDUMP
 REPDUMP  DATA  0
          SB7   1              B7= 1
          SA5   X1             X5= LUN
          SA2   A1+B7          X2= ADR(NBEF)
          NZ    X5,LUN
          SA5   LUNDEF
 LUN      BX6   X5
          SA6   PLUN               SET LUN AS PARAMETER
          SA5   XCHC
          LX5   24
          SB1   X5             B1= P
          EQ    B1,B0,NODUMP
*
*----     LOAD   X2=LADR  &  X3=NTOT
*
          ZR    X2,NODUMP          PARAMETER 2 MISSING
          SA3   A2+B7
          SA2   X2             X2= NBEF
          SA3   X3             X3= NTOT
          NG    X2,NODUMP
          NG    X3,NODUMP
*
*----     VERIFY  LADR &  LADR+NTOT  ARE IN RANGE
*
          SB2   X2
          SB2   B1-B2          B2= LADR = P - NBEF
          GE    B2,B0,OK1
          SB2   B7
 OK1      MX7   0
          SA7   LADR
          MEMORY CM,LADR,RECALL
          SA1   LADR
          LX1   30
          SB6   X1-4           B6= FLU = FL - 4
          GE    B1,B6,NODUMP       IF P    OUT OF RANGE
          GE    B2,B6,NODUMP       IF LADR OUT OF RANGE
          SB5   B2+X3          B5= LADR + NTOT
          LT    B5,B6,OK2
          SX3   B6-B2              NTOT= FLU - LADR IF OUT OF RANGE
          NG    X3,NODUMP
 OK2      SX6   X3-4000
          NG    X6,SDUMP           NTOT .GE. 4000 IS NOT ALLOWED
*
 NODUMP   MX3   0
 SDUMP    SX6   B2
          BX7   X3
          SA6   PARAM+1
          SA6   PADR
          SA7   PNTOT
          SA1   PARAM
+RJREPDMP2
-         VFD   30/REPDUMP-1
          EQ    REPDUMP
*
 LUNDEF   VFD   36/6HOUTPUT,24/0
 PLUN     DATA  0
 PNTOT    DATA  0
 PADR     DATA  0
 LADR     EQU   PADR
*
 PARAM    VFD   42/0,18/PLUN
          DATA  0
          VFD   42/0,18/PNTOT
          VFD   42/0,18/PADR
          DATA  0
          END
