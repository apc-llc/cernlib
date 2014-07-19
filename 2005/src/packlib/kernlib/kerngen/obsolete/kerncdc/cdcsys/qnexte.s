*
* $Id: qnexte.s,v 1.1.1.1 1996/02/15 17:51:35 mclareni Exp $
*
* $Log: qnexte.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:35  mclareni
* Kernlib
*
*
          IDENT QNEXTE
*
* CERN PROGLIB# Z041    QNEXTE          .VERSION KERNCDC  2.13  840827
* ORIG.  1981, JZ, CERN
*
          ENTRY QNEXTE
          EXT   QNEXT
*
 TRACE    VFD   36/6HQNEXTE,24/QNEXTE
 QNEXTE   DATA  0
          SA1   SAVEEE
          NZ    X1,REENT
*
*--       FIRST ENTRY
*
          SA1   QNEXTE         SAVE ENTRY/EXIT LINE
          SX6   A0             SAVE A0
          SA6   SAVEA0
          BX7   X1
          SA7   SAVEEE
          JP    GO
*
*--       RE-ENTRY
*
 REENT    BX7   X1             RESTORE ENTRY/EXIT LINE
          SA7   QNEXTE
*
*--       TRANSFER TO QNEXT
*
 GO       RJ    QNEXT
-         VFD   30/TRACE
          SA2   SAVEA0
          SA0   X2
 SAVEEE   DATA  0
*
 SAVEA0   DATA  0
          END
#ifdef CERNLIB_TCGEN_QNEXTE
#undef CERNLIB_TCGEN_QNEXTE
#endif
