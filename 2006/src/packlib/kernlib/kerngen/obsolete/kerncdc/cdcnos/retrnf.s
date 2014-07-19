*
* $Id: retrnf.s,v 1.1.1.1 1996/02/15 17:51:29 mclareni Exp $
*
* $Log: retrnf.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:29  mclareni
* Kernlib
*
*
          IDENT RETRNF
*
* CERN PROGLIB# K510    RETRNF          .VERSION KERNCDC  1.18  820317
*
          ENTRY RETRNF
          EXT   GETFIT.
          SST
*
*     CALL RETRNF (LUN,ERROR)      RETURN FILE LUN
*     DERIVED FROM P=CDCNOS, D=DETACH
*
 TRACE.   VFD   42/0HRETRNF,18/RETRNF
 TEMPA0   BSS   1
 LFIT     BSS   1
*
 HOME     BSS   0
 HOMEX    BSS   0
          SA5   TEMPA0
          SA0   X5
 RETRNF   BSSZ  1
          SA2   A1+1
          MX6   0
          SA6   X2             RETURN  ERROR=0
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
#ifdef CERNLIB_CDCSYS_RETRNF
#undef CERNLIB_CDCSYS_RETRNF
#endif
