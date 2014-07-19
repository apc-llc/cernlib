*
* $Id: dbplnt.F,v 1.1.1.1 1996/02/28 16:25:04 mclareni Exp $
*
* $Log: dbplnt.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:04  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBPLNT (NTUP, PATHS, NPATH, NOBJS, KOBJS, NKEYX,
     +                   KEYXS, KEYS, CHOPT)
*     ===========================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBPLNT (NTUP, PATHS, NPATH, NOBJS, KOBJS, NKEYX,        *
*                      KEYXS, KEYS, CHOPT)                             *
*                                                                      *
*   Fills an N-tuple with data and key elements from multiple          *
*   directories as requested by the user                               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     NTUP     Integer identifier of the ntuple                        *
*     PATHS    Character array describing the path names               *
*     NPATH    Number of paths given in PATHS                          *
*     NOBJS    Array giving the number of data elements to be stored   *
*              for PATHS(i)                                            *
*     KOBJS    Vector specifying the element indices to be stored for  *
*              PATHS(i)                                                *
*     NKEYX    Array giving the number of key elements to be stored    *
*              with data for PATHS(i)                                  *
*     KEYXS    Array giving key element indices to be stored with the  *
*              data. For a given path, data elements follow the key    *
*              elements                                                *
*     KEYS     Vector of keys. Only the elements declared in CHOPT are *
*              considered to contain useful information. The vector as *
*              well as CHOPT refers to the PATHS(1). Selections for    *
*              all subsequent paths is done on the basis of start      *
*              validity period from path 1 and a correlation table     *
*              between the user keys.                                  *
*     CHOPT    Character string with any of the following characters   *
*          T   Transforms the Keys 3,4,7 to seconds/minutes passed     *
*              since midnight January 1, 1980                          *
*          3   selects objects with start validity time > KEYS(3)      *
*          4   selects objects with start validity time < KEYS(4)      *
*          5   specific Program version number required                *
*          7   selects objects with insertion      time < KEYS(7)      *
*          n   consider user key n (where 7 < n < 30)                  *
*                                                                      *
*   Called by user,   DBAUXI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =161 : Illegal path name                               *
*               =162 : No keys or data in the directory                *
*               =163 : Illegal number of objects                       *
*               =164 : Illegal number of path names                    *
*               =165 : Illegal object element indices                  *
*               =166 : Illegal key element indices                     *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "dxused.inc"
      PARAMETER       (NKYMX=100)
      CHARACTER       CHOPF*28
      CHARACTER*(*)   PATHS(*), CHOPT
      DIMENSION       KEYS(9), KEYXS(9), KOBJS(9), NKEYX(9), NOBJS(9)
      DIMENSION       KEYXT(NKYMX)
*
*     ------------------------------------------------------------------
*
* *** Reformat CHOPT
*
      CALL DBOPTS (CHOPT, IRC)
      IF (IRC.NE.0) THEN
        IQUEST(1) = IRC
        GO TO 999
      ENDIF
      IOPXDX = 1
      CALL DBOPTM (CHOPF)
*
* *** Reformat the keys
*
      CALL DBCKEY (KEYS, KEYSDX, MXKYDX)
      NKTOT  = 0
      DO I = 1, NPATH
        NKTOT  = NKTOT + NKEYX(I)
      ENDDO
      DO I = 1, NKTOT
        IF (KEYXS(I).EQ.1) THEN
          KEYXT(I) = IDHKSN
        ELSE IF (KEYXS(I).EQ.2) THEN
          KEYXT(I) = IDHPTR
        ELSE IF (KEYXS(I).EQ.3) THEN
          KEYXT(I) = NOF1CK + 1
        ELSE IF (KEYXS(I).EQ.4) THEN
          KEYXT(I) = NOF1CK + 2
        ELSE IF (KEYXS(I).EQ.5) THEN
          KEYXT(I) = IDHUSI
        ELSE IF (KEYXS(I).EQ.6) THEN
          KEYXT(I) = IDHFLG
        ELSE IF (KEYXS(I).EQ.7) THEN
          KEYXT(I) = IDHINS
        ELSE
          KEYXT(I) = KEYXS(I) - NSYSDX + NOF2CK + 2
        ENDIF
      ENDDO
*
* *** Prepare object and key vector specifiers for different paths
*
      CALL CDNTPL (NTUP, PATHS, NPATH, NOBJS, KOBJS, NKEYX,
     +                   KEYXT, MASKDX, KEYSDX, CHOPF, IRC)
      IQUEST(1) = IRC
*                                                             END DBPLNT
  999 END
