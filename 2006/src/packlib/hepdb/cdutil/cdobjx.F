*
* $Id: cdobjx.F,v 1.1.1.1 1996/02/28 16:24:38 mclareni Exp $
*
* $Log: cdobjx.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:38  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDOBJX(PATH,IRC)
*
      CHARACTER*(*) PATH
#include "hepdb/cdunit.inc"
#include "hepdb/quest.inc"
#include "hepdb/cdoccm.inc"
      LPATH = LENOCC(PATH)
*
*     Bump directory count
*
      NDIRT  = NDIRT + 1
      NFIL   = 0
      IRC    = 0
*
*     Get number of directories below CWD (filled by CDSCAN)
*
      NDIR   = IQUEST(12)
      NFIL   = IQUEST(14)
      NFOUND = NFOUND + NFIL
*
*     Display only empty (zero files) directories
*
      IF((IOPTZ.NE.0).AND.(NFIL.NE.0)) RETURN
*
*     Display only lowest level directories
*
      IF((IOPTL.NE.0).AND.(NDIR.NE.0)) RETURN
 
      IF((IOPTD.NE.0).OR.(IOPTF.NE.0)) THEN
         WRITE(LOUTCD,*)
         WRITE(LOUTCD,*) 'Directory: ',PATH(1:LPATH)
      ENDIF
 
      IF(IOPTD.NE.0) WRITE(LOUTCD,*) ' subdirectories: ',NDIR
*
*     Number of objects
*
      IF(IOPTF.NE.0) WRITE(LOUTCD,*) ' objects: ',NFIL
 
      END
