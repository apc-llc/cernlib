*
* $Id: detach.s,v 1.1.1.1 1996/02/15 17:51:29 mclareni Exp $
*
* $Log: detach.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:29  mclareni
* Kernlib
*
*
          IDENT DETACH
*
* CERN PROGLIB# K511    DETACH          .VERSION KERNCDC  1.16  820129
*
          ENTRY DETACH
          EXT   GETFIT.
          SST
*
*     CALL DETACH (LUN)
*     TO RETURN A DISK FILE TO THE SYSTEM DURING EXECUTION
*
*     SHORT VERSION OF RETRNF FOR DISK FILES, 3-MAY-78, JZ
*
 TRACE.   VFD   42/0HDETACH,18/DETACH
 TEMPA0   BSS   1
 LFIT     BSS   1
*
 HOME     BSS   0
 HOMEX    BSS   0
          SA5   TEMPA0
          SA0   X5
 DETACH   BSSZ  1
          MX6   1
          BX1   X1+X6
 +        RJ    GETFIT.
 -        VFD   30/TRACE.
          SX6   A0
          SA6   TEMPA0
          BX6   X1
          SA6   LFIT               SAVE ADR OF FIT
          STORE X1,EX=ERROR
          FETCH X1,OC,X2
          SX3   X2-1
          ZR    X3,OPENED
          OPENM X1,I-O,R
          SA1   LFIT           X1= LFIT RESTORED
*
 OPENED   BSS   0
          CLOSEM X1,U
          SA1   LFIT           X1= LFIT RESTORED
          EQ    HOME
*
 ERROR    BSS   1
          SA1   LFIT           X1= LFIT RESTORED
          STORE X1,ES=0            CLEAR FIES
          EQ    HOMEX
          END
#ifdef CERNLIB_CDCSYS_DETACH
#undef CERNLIB_CDCSYS_DETACH
#endif
