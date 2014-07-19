*
* $Id: dbmdip.F,v 1.1.1.1 1996/02/28 16:25:00 mclareni Exp $
*
* $Log: dbmdip.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:00  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBMDIP (PATHN, NWKEY, CHFOR, CHTAG, MXKP)
*     ====================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBMDIP (PATHN, NWKEY, CHFOR, CHTAG, MXKP)               *
*                                                                      *
*   Creates Directories with the conventions of DB package for         *
*   partitioned data sets                                              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Path name of the directory                              *
*     NWKEY    Number of words associated to the keys at the lowest    *
*              level (If some directory in the pathname at a higher    *
*              level does not exist, it is created with 9 keys)        *
*     CHFOR    Character variable describing each element of the key   *
*              vector at the lowest level (at higher level it is set   *
*              to default, i.e., 'IIIIIIIII')                          *
*              The first 7 characters should be 'I'                    *
*     CHTAG    Character array defined as CHARACTER*8 (NWKEY)          *
*              The first 7 elements should be :  'SERIAL_#'            *
*                                                'POINTER '            *
*                                                'STR_VALI'            *
*                                                'END_VALI'            *
*                                                'PRG_VERS'            *
*                                                'FLAGS   '            *
*                                                'INS_TIME'            *
*     MXKP     Maximum number of objects in each partition             *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 43 : Too many key elements                           *
*               = 44 : Cannot find the top directory name              *
*                      (wrong initialisation)                          *
*               = 45 : Illegal Path name                               *
*               = 48 : Error in directory search sequence              *
*                                                                      *
************************************************************************
*
#include "hepdb/ckkeys.inc"
#include "hepdb/cuserf.inc"
#include "dxused.inc"
      COMMON /QUEST/  IQUEST(100)
      CHARACTER       CTAG(100)*8, CFOR*100, CHOPT*4
      CHARACTER*(*)   CHFOR, PATHN, CHTAG(*)
*
*     ------------------------------------------------------------------
*
* *** Check on format and tags of the DB system keys
*
      NKEYS  = NWKEY - NSYSDX
      DO I = NSYSDX+1, NWKEY
        CTAG(I-NSYSDX) = CHTAG(I)
      ENDDO
      NCF    = LEN(CHFOR)
      IF (NCF.GT.NSYSDX) THEN
        CFOR = CHFOR(NSYSDX+1:NCF)
      ELSE
        CFOR = ' '
      ENDIF
      CHOPT  = 'CP'
      CALL CDMDIR (PATHN,NKEYS, CFOR,CTAG,MXKP, IPRECU,DELTCU,CHOPT,IRC)
      IQUEST(1) = IRC
*                                                             END DBMDIP
  999 END
