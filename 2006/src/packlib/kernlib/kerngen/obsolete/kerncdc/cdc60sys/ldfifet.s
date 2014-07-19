*
* $Id: ldfifet.s,v 1.1.1.1 1996/02/15 17:51:32 mclareni Exp $
*
* $Log: ldfifet.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:32  mclareni
* Kernlib
*
*
          IDENT LDFIFET
*
* CERN PROGLIB#         LDFIFET         .VERSION KERNCDC  1.16  820129
*
          ENTRY LDFIFET
          EXT   UCOPY,GETFIT.
*
          USE   /FITFET/
 LUN      BSS   1
 LFIT     BSS   1
 MFIT     BSS   16
 LFET     BSS   1
 MFET     BSS   10
 MWK      BSS   53
          USE
*
 TRACE    VFD   42/7HLDFIFET,18/LDFIFET
 LDFIFET  DATA  0
          MX6   1
          BX1   X1+X6
+RJGETFIT
          BX6   X1
          SA6   LFIT
          SA6   PARI
          MX0   42
          SA2   X1+1
          BX6   -X0*X2
          SA6   LFET
          SA6   PARE
*
          SA1   PARI
+RJUCOPY
-         VFD   30/TRACE
          SA1   PARE
+RJUCOPY
-         VFD   30/TRACE
          EQ    LDFIFET
*
 N16      DATA  16
 N10      DATA  10
 PARI     DATA  0
          VFD   42/0,18/MFIT
          VFD   42/0,18/N16
          DATA  0
 PARE     DATA  0
          VFD   42/0,18/MFET
          VFD   42/0,18/N10
          DATA  0
          END
