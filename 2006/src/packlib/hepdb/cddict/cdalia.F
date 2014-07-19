*
* $Id: cdalia.F,v 1.1.1.1 1996/02/28 16:24:08 mclareni Exp $
*
* $Log: cdalia.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:08  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDALIA (CHPATH, CHALIA, CHOPT, IRC)
*
*     Enter, retrieve, delete or print an alias definition
*
      CHARACTER*(*) CHPATH,CHALIA
      CHARACTER*255 CHDIR
      CHARACTER*8   ALIAS
#include "hepdb/cduscm.inc"
#include "hepdb/quest.inc"
#include "hepdb/hdbopts.inc"
 
      IRC = 0
*
*     Save current directory
*
      CALL RZCDIR(CHDIR,'R')
      LDIR = LENOCC(CHDIR)
 
      IF(IOPTD.NE.0)     THEN
         CALL CDEALI (CHPATH, '        ', 1, IRC)
      ELSEIF(IOPTP.NE.0) THEN
         LPATH  = LENOCC(CHPATH)
         CALL CDGALI (CHPATH, ALIAS, IRC)
         LALIAS = LENOCC(ALIAS)
         IF(IRC.EQ.0) THEN
            IF(LALIAS.EQ.0) THEN
               WRITE(LPRTCD,9001) CHPATH(1:LPATH)
9001  FORMAT(' CDALIA. no alias found for path ',A)
            ELSE
               WRITE(LPRTCD,9002) CHPATH(1:LPATH),
     +                         ALIAS(1:LALIAS)
9002  FORMAT(' CDALIA. alias for path ',A,' is ',A)
            ENDIF
         ENDIF
      ELSEIF(IOPTG.NE.0) THEN
*
*     Get the alias definition for the specified path
*
         CALL CDGALI (CHPATH, CHALIA, IRC)
      ELSEIF(IOPTR.NE.0) THEN
*
*     Retrieve the equivalence name for the specified alias
*
         CALL CDRALI (CHALIA, CHPATH, IRC)
      ELSEIF(IOPTS.NE.0) THEN
         CALL CDEALI (CHPATH, CHALIA, 0, IRC)
         LPATH  = LENOCC(CHPATH)
         IF(IRC.EQ.0) WRITE(LPRTCD,9003) CHPATH(1:LPATH),
     +                                   CHALIA(1:LENOCC(CHALIA))
9003  FORMAT(' CDALIA. alias for path ',A,' is ',A,
     +       ' (for this session)')
      ELSEIF(IOPTU.NE.0) THEN
         CALL CDEALI (CHPATH, CHALIA, 1, IRC)
         LPATH  = LENOCC(CHPATH)
         IF(IRC.EQ.0) WRITE(LPRTCD,9004) CHPATH(1:LPATH),
     +                                   CHALIA(1:LENOCC(CHALIA))
9004  FORMAT(' CDALIA. alias for path ',A,' is ',A,
     +       ' (entered into database)')
      ENDIF
*
*     Restore current directory
*
      CALL RZCDIR(CHDIR(1:LDIR),' ')
*                                                             END CDALIA
      END
