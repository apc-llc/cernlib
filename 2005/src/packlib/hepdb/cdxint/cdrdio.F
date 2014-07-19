*
* $Id: cdrdio.F,v 1.1.1.1 1996/02/28 16:24:51 mclareni Exp $
*
* $Log: cdrdio.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:51  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRDIO (PATHN, KEYX, NDAT, CHIDH, LENCH, CHOPT, IRC)
*     ===============================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDRDIO (PATHN, KEYX, NDAT*, CHIDH*, LENCH*, CHOPT, IRC*)*
*                                                                      *
*   Reads the I/O-Descriptor of the Data-Bank from the display-file    *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     KEYX     Key-vector                                              *
*     NDAT     Number of Data in the Data-bank associated to the key   *
*     CHIDH    IO Characteristics of the Data-bank                     *
*     LENCH    Length of CHIDH                                         *
*     CHOPT    Character string with any of the following characters   *
*          E   Display only example (template)                         *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDUPKY                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/cxlink.inc"
      CHARACTER       CFNAM*32, PATHN*(*), STRFIL*40
      CHARACTER       IOO*1, ION*1, IOC*2, CHIDH*(*), CHOPT*(*)
      DIMENSION       KEYX(100)
*
*     ------------------------------------------------------------------
*
      NDAT  = 0
      IRC   = 0
*
*  ** Open the file to editing
*
      CALL KUPROC ('Give Data File-Name (e.g. DFNAME) ',CFNAM, LFNAM)
#if !defined(CERNLIB_IBMVM)
      CFNAM  = CFNAM(1:LFNAM)//'.FILEXT'
#endif
#if defined(CERNLIB_UNIX)
      CALL CUTOL (CFNAM)
#endif
#if defined(CERNLIB_IBMVM)
      CFNAM  = CFNAM(1:LFNAM)//'.FILEXT.A'
#endif
      CALL CDOPFL (LUDACX, CFNAM, 'UNKNOWN', ISTAT)
      IF (ISTAT.NE.0)     GO TO 999
*
*  ** Prepares the Data file for PATHN and KEYX vector
*
      CALL CDDISD (LUDACX, PATHN, KEYX, CHOPT, IRC)
      CALL CDCLFL (LUDACX)
      IF (IRC.NE.0)       GO TO 999
*
*  ** Edits the file containing the Data
*
      CALL KUEDIT (CFNAM, IST)
*
*  ** Decode the Key part of the edited file
*
      LENCH = 0
      CALL CDOPFL (LUDACX, CFNAM, 'OLD', ISTAT)
      IF (ISTAT.NE.0)     GO TO 999
      READ (LUDACX, 1001, ERR=20, END=20)
*
*  ** Build-up I/O descriptor
*
      IT    = -1
      IOO   = ' '
   10 CONTINUE
      READ (LUDACX, 1002, ERR=20, END=20) STRFIL
      NCH   = LENOCC (STRFIL)
      NCH   = MIN0 (NCH, 80)
      ION   = ' '
      DO 15 J = 1, NCH
        IF (STRFIL(J:J).NE.' ') THEN
          ION = STRFIL(J:J)
          JST = J + 6
          GO TO 16
        ENDIF
   15 CONTINUE
      JST   = 7
      ION   = ' '
   16 LWRDH = 1
      DO 17 J = JST, NCH
        IF (STRFIL(J:J).NE.' ') THEN
          LCDAT = NCH-J+1
          LWRDH = (LCDAT-1)/4 + 1
          GO TO 18
        ENDIF
   17 CONTINUE
   18 CONTINUE
      IF (IOO.NE.ION) THEN
        IF (IT.EQ.-1) THEN
          IT = 0
          IS = 1
          IF (ION.EQ.'H') IS = LWRDH
        ELSE
          WRITE (IOC, 1003) IS
          NDAT = NDAT + IS
          IF (IT.EQ.0) THEN
            CHIDH = IOC//IOO
          ELSE
            CHIDH = CHIDH(1:IT)//IOC//IOO
          ENDIF
          IF (ION.EQ.' ') GO TO 20
          IS = 1
          IF (ION.EQ.'H') IS = LWRDH
          CHIDH(IT+4:IT+4) = ' '
          IT = IT + 4
        ENDIF
        IOO = ION
      ELSE
        INCR = 1
        IF (ION.EQ.'H') INCR = LWRDH
        IS = IS + INCR
      ENDIF
      GO TO 10
   20 CONTINUE
      LENCH = IT + 3
      IF (NDAT.EQ.0) CALL CDCLFL (LUDACX)
*
 1001 FORMAT (////)
 1002 FORMAT (A40)
 1003 FORMAT (I2)
*                                                             END CDRDIO
  999 END
