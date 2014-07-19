*
* $Id: fmmss.F,v 1.1.1.1 1996/03/07 15:18:21 mclareni Exp $
*
* $Log: fmmss.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:21  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMMSS(GENAM,CHURL,CHOPT,IRC)
*
*     Check if 'GENAM' with URL 'CHURL' accessible on current node
*
      CHARACTER*(*)    GENAM,CHURL
      CHARACTER*255    CHHOME
      LOGICAL          IEXIST
#include "fatmen/slate.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatopts.inc"
 
      IRC  = 0
 
      LGN  = LENOCC(GENAM)
      LURL = LENOCC(CHURL)
 
      IURL = INDEX(CHURL(1:LURL),'://')
      IF(IURL.EQ.0) THEN
         IRC = 1
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9001) GENAM(1:LGN),CHURL(1:LURL)
 9001 FORMAT(' FMMSS. genam ',A,' has invalid URL ',A)
         GOTO 999
      ENDIF
*
*     Lachman OSM
*
      IF(CHURL(1:IURL-1).EQ.'OSM') THEN
         CALL GETENVF('OSMHOME',CHHOME)
         IF(IS(1).EQ.0) THEN
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9002)
 9002 FORMAT(' FMMSS. env. variable OSMHOME not defined.',
     +       ' Looking for /osm...')
            INQUIRE(FILE='/osm',EXIST=IEXIST)
            IF(IEXIST) THEN
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9003)
 9003 FORMAT(' FMMSS. OSMHOME defaulted to /osm')
            ELSE
               IF(IDEBFA.GE.0) WRITE(LPRTFA,9004)
 9004 FORMAT(' FMMSS. files with URL like ',A,
     +       ' cannot be accessed on this node.')
               IRC = 99
               GOTO 999
            ENDIF
         ELSE
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9005) CHHOME(1:IS(1))
 9005 FORMAT(' FMMSS. $OSMHOME = ',A)
         ENDIF
      ENDIF
*
*     Unitree ftp access (NFS access as for local files)
*
      IF(CHURL(1:IURL-1).EQ.'UNITREE') THEN
         WRITE(LPRTFA,9006) CHURL(1:IURL-1)
         IRC = 99
         GOTO 999
      ENDIF
 
*
*     Adstar ADSM
*
      IF(CHURL(1:IURL-1).EQ.'ADSM') THEN
         WRITE(LPRTFA,9006) CHURL(1:IURL-1)
         IRC = 99
         GOTO 999
      ENDIF
 
*
*     E-MASS (NFS ok as for Unitree?)
*
      IF(CHURL(1:IURL-1).EQ.'EMASS') THEN
         WRITE(LPRTFA,9006) CHURL(1:IURL-1)
         IRC = 99
         GOTO 999
      ENDIF
 
 9006 FORMAT(' FMMSS. support for ',A,' not yet available')
  999 END
