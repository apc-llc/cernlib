*
* $Id: loafit.s,v 1.1.1.1 1996/02/15 17:51:29 mclareni Exp $
*
* $Log: loafit.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:29  mclareni
* Kernlib
*
*
          IDENT LOAFIT
*
* CERN PROGLIB#         LOAFIT          .VERSION KERNCDC  1.16  820129
*
          ENTRY LOAFIT
          EXT   UCOPY,GETFIT.
*
*     CALL LOAFIT (LUN)    COPIES FIT FOR LUN INTO MFIT IN /COMFIT/
*
          USE   /COMFIT/
 LUN      BSS   1
 LFIT     BSS   1
 MFIT     BSS   40
 MWK      BSS   40
          USE
*
 TRACE    VFD   36/6HLOAFIT,24/LOAFIT
 LOAFIT  DATA  0
          MX6   1
          BX1   X1+X6
+RJGETFIT
          BX6   X1
          SA6   LFIT
          SA6   PARI
*
          SA1   PARI
+RJUCOPY
-         VFD   30/TRACE
          EQ    LOAFIT
*
 NF       DATA  35
 PARI     DATA  0
          VFD   42/0,18/MFIT
          VFD   42/0,18/NF
          DATA  0
          END
