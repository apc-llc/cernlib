*
* $Id: detach.s,v 1.1.1.1 1996/02/15 17:51:33 mclareni Exp $
*
* $Log: detach.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:33  mclareni
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
#if defined(CERNLIB_CDC60SYS)
 TEMPA0   BSS   1
#endif
 FICF     BSS   1
 #NEVOPN# EQU   0
*
 HOME     BSS   0
 HOMEX    BSS   0
          STORE X1,RC=0
          STORE X1,BN=0
          SA4   FICF
          STORE X1,CF=X4           RESTORE THE ORIGINAL CF IN THE FIT
          STORE X1,OC=NEVOPN       SET FIOC TO NEVER OPENED
#if defined(CERNLIB_CDC60SYS)
          SA5   TEMPA0
          SA0   X5
#endif
 DETACH   BSSZ  1
          MX6   1
          BX1   X1+X6
 +        RJ    GETFIT.
 -        VFD   30/TRACE.
#if defined(CERNLIB_CDC60SYS)
          SX6   A0
          SA6   TEMPA0
          SA0   X1                 SAVE X1 WHICH WILL BE DESTROYED BY 6R
#endif
          STORE X1,EX=ERROR
          FETCH X1,CF,X6
          SA6   FICF               SAVE THE ORIGINAL CF
          FETCH X1,OC,X2
          SX3   X2-1
          ZR    X3,OPENED
          OPENM X1,I-O,R
#if defined(CERNLIB_CDC60SYS)
          SX1   A0                 RESTORE X1 DESTROYED BY 6RM
#endif
*
 OPENED   BSS   0
          CLOSEM X1,U
#if defined(CERNLIB_CDC60SYS)
          SX1   A0                 RESTORE X1 DESTROYED BY 6RM
#endif
          EQ    HOME
*
 ERROR    BSS   1
#if defined(CERNLIB_CDC60SYS)
          SX1   A0                 RESTORE X1 DESTROYED BY 6RM
#endif
          STORE X1,ES=0            CLEAR FIES
          EQ    HOMEX
          END
