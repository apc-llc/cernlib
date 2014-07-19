*
* $Id: cdload.F,v 1.2 1996/03/12 13:08:00 cernlib Exp $
*
* $Log: cdload.F,v $
* Revision 1.2  1996/03/12 13:08:00  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLOAD(LUNFZ,CHFILE,IRC)
 
      CHARACTER*(*) CHFILE
#include "hepdb/cdnamc.inc"
#include "hepdb/quest.inc"
      CHARACTER*132 TEXT
*-----------------------------------------------------------------------
*
      LFILE  = LENOCC(CHFILE)
#if defined(CERNLIB_VAXVMS)
      ISTART = INDEXB(CHFILE(1:LFILE),']')+1
#endif
#if !defined(CERNLIB_VAXVMS)
      ISTART = 1
#endif
#if !defined(CERNLIB_IBMVM)
      OPEN(LUNFZ,IOSTAT=ISTAT,FILE=CHFILE(1:LFILE),FORM='FORMATTED',
     +     STATUS='OLD')
#endif
#if defined(CERNLIB_IBMVM)
      OPEN(LUNFZ,IOSTAT=ISTAT,FILE='/'//CHFILE(1:LFILE),
     +     STATUS='OLD',FORM='FORMATTED')
#endif
      IF(ISTAT.NE.0) THEN
         IRC = ISTAT
         WRITE (TEXT,9001) CHFILE(ISTART:LFILE), ISTAT
         CALL CDLOGF (-1,TEXT)
         RETURN
      ELSE
         CALL FZFILE (LUNFZ, 0, 'AIP')
         IF ( IQUEST(1) .NE. 0 ) THEN
            IRC = IQUEST(1)
            WRITE (TEXT,9002) CHFILE(ISTART:LFILE), IRC
            CALL CDLOGF (-2,TEXT)
         ELSE
            WRITE (TEXT,9003) CHFILE(ISTART:LFILE)
#if defined(CERNLIB__DEBUG)
            CALL CDLOGF (0,TEXT)
#endif
            PRINT *,' Loading the file ',CHFILE(1:LFILE)
            CALL CDFZUP(LUNFZ,'F',IRC)
            IF(IRC.EQ.0) THEN
               IRC = 0
               WRITE (TEXT,9004) CHFILE(ISTART:LFILE)
               CALL CDLOGF (0,TEXT)
            ELSE
               WRITE (TEXT,9005) CHFILE(ISTART:LFILE), IRC
               CALL CDLOGF (-11,TEXT)
            ENDIF
            CALL FZENDI(LUNFZ,'T')
         ENDIF
      ENDIF
 
      CLOSE(LUNFZ)
 
 9001 FORMAT (' Error opening journal file: ',A,'. IOSTAT=',I7)
 9002 FORMAT (' Error at FZFILE of ',A,'. IQUEST (1)=',I7)
 9003 FORMAT (' Opened the file: ',A)
 9004 FORMAT (' Loaded the file: ',A)
 9005 FORMAT (' Error loading the file: ',A,'. IRC=',I7)
      END
