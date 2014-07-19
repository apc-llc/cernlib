*
* $Id: cdmain.F,v 1.1.1.1 1996/02/28 16:23:34 mclareni Exp $
*
* $Log: cdmain.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:34  mclareni
* Hepdb, cdlib, etc
*
*
#include "sys/CERNLIB_machine.h"
#include "_hepdb/pilot.h"
      PROGRAM CDMAIN
*     ==============
*
************************************************************************
*                                                                      *
*        PROG. CDMAIN                                                  *
*                                                                      *
*   Main Program of the Interactive HEPDB based on the KUIP Package    *
*                                                                      *
************************************************************************
*
      PARAMETER       (L3CORQ=200000, L3CORH=200000, L3CORK=70000)
      COMMON /GCBANK/ FENCCD(22), LQ(L3CORQ)
      COMMON /PAWC/   PAW(L3CORH)
      PARAMETER       (L3WKST=1, NARGL3=100)
      COMMON /L3FLAG/ IFTML3, IGYFL3, IHFIL3, IHISL3, INTXL3, IPRTL3
     +              , IPR2L3, IWKTL3, IXERL3, MOP3L3, ARGSL3(NARGL3)
      INTEGER         IARGL3(NARGL3)
      INTEGER         IFTML3, IGYFL3, IHFIL3, IHISL3, INTXL3, IPRTL3
     +              , IPR2L3, IWKTL3, IXERL3, MOP3L3
      REAL            ARGSL3
      EQUIVALENCE     (IARGL3(1), ARGSL3(1))
      PARAMETER       (MAXJCX=20, MAXVCX= 20, LUKYCX=88, LUDACX=89)
      COMMON /CXLINK/ ISTOCX, KEYSCX(100), MASKCX(100), L3PRCX, LURZCX
     +              , LFRSCX, LJOICX, LKJNCX(MAXJCX), LDJNCX(MAXJCX)
     +              , LVIWCX, LKVWCX(MAXVCX), LDVWCX(MAXVCX), LASTCX
*
      DATA            LUNER /19/, LUNMF /10/
*
*     ------------------------------------------------------------------
*
* *** Interface to C-routines for KUIP
*
      CALL INITC
*
* *** Initialize ZEBRA
*
      CALL MZEBRA (-1)
*
*  ** Initialize DB-Store, User-Division, User-Links
*
      CALL CDXINI (L3CORQ)
*
*  ** Initialize PAW and KUIP
*
      CALL HLIMIT (-L3CORH)
      CALL KUINIT (L3CORK)
      INTXL3 = 1
      NWORD  = 0
      CALL IGINIT (NWORD)
      CALL KUOPEN (LUNMF, 'dbase.metafile', 'UNKNOWN', ISTAT)
      CALL KUOPEN (LUNER, 'gkserror.log', 'UNKNOWN', ISTAT)
      CALL HERMES (LUNER)
      CALL IGWKTY (IWKTL3)
      CALL HPLINT (IWKTL3)
      CALL IGSA   (L3WKST)
*
*  ** Define the Exit Routine
*
      CALL TIMEST (99999.)
*
*  *  Create the Interactive Command structure
*
      CALL HERMES (L3PRCX)
      CALL VECDEF
      CALL CDINXI
      CALL HISDEF
      CALL FUNDEF
      CALL NTUDEF
      CALL ZEBDEF
      CALL GRADEF
      CALL PICDEF
      CALL FORDEF
      CALL NETDEF
      CALL PVEDEF
*
*  *  Set prompt
*
      CALL KUEXEC ('SET/PROMPT ''DB >''')
*
*  *  Execute LOGON Macro
*
      CALL KUEXEC ('EXEC DBLOGN')
*
      CALL KUSPY ('NEVER')
*
      CALL KUWHAT
*
      CALL CDEND ('*', 'A', IRC)
      STOP
*                                                             END CDMAIN
      END
 
