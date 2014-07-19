*
* $Id: hmerge.F,v 1.1.1.1 1996/01/16 17:08:10 mclareni Exp $
*
* $Log: hmerge.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:10  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/07 28/06/94  16.53.03  by  Rene Brun
*-- Author :    Rene Brun, J.C. Marin and Fons Rademakers   26/05/94
      SUBROUTINE HMERGE(NFILES,FILES,FILOUT)
*.==========>
*.
*.  Subroutine to merge the NFILES HBOOK files with identical objects
*.  and directories into FILOUT.
*.
*..=========> (Rene Brun, J.C. Marin, A.A.Rademakers )
*
      INTEGER         IQUEST
      COMMON/QUEST/   IQUEST(100)
*
      INTEGER         NFILES
      CHARACTER*(*) FILES(*), FILOUT
*
      INTEGER         LRECL, ISTAT
      INTEGER         LENOCC
*
      EXTERNAL        HMERGE1, HMERGE2
*____________________________________________________________________
*
      IF (NFILES .LE. 0) THEN
         WRITE(6,'('' No INPUT file. Job aborted'')')
         GOTO 99
      ENDIF
*
*         Open 1st file to get record length
*
*         Auto-record length detection does not work without option X
*         on Alpha/OSF/VMS
*
      LRECL = 0
      CALL HROPEN(21,'FILE',FILES(1),'X',LRECL,ISTAT)
      IF (ISTAT .NE. 0) THEN
         PRINT *, 'Cannot open file ', FILES(1)(1:LENOCC(FILES(1)))
         GOTO 99
      ENDIF
      CALL HRENDC('FILE')
*
*         Create big file
*
      IQUEST(10) = 64000
      CALL HROPEN(22,'BIGF',FILOUT(1:LENOCC(FILOUT)),'QN',LRECL,ISTAT)
      IF (ISTAT .NE. 0) THEN
         PRINT *, 'Cannot create file ', FILOUT(1:LENOCC(FILOUT))
         GOTO 99
      ENDIF
*
*         Open 1st file to read characteristics
*
      CALL HROPEN(21,'FILE',FILES(1),' ',LRECL,ISTAT)
      IF (ISTAT .NE. 0) THEN
         PRINT *, 'Cannot open file ', FILES(1)(1:LENOCC(FILES(1)))
         GOTO 99
      ENDIF
      CALL RZSCAN('//FILE', HMERGE1)
      CALL HRENDC('FILE')
*
*         Loop on all files
*
      DO 50 IFILE = 1, NFILES
         CALL HROPEN(21,'FILE',FILES(IFILE),' ',LRECL,ISTAT)
         IF (ISTAT .NE. 0) THEN
            PRINT *, 'Cannot open file ',
     +                FILES(IFILE)(1:LENOCC(FILES(IFILE)))
            GOTO 99
         ENDIF
         PRINT *, 'Processing file ',
     +             FILES(IFILE)(1:LENOCC(FILES(IFILE)))
         CALL RZSCAN(' ', HMERGE2)
         CALL HRENDC('FILE')
50    CONTINUE
*
*         Save into the big file and close file
*
      CALL HCDIR('//BIGF',' ')
      CALL HROUT(0,ICYCLE,'T')
      IF (IQUEST(1) .NE. 0) THEN
         PRINT *, 'Error saving merged file'
         GOTO 99
      ENDIF
      CALL HRENDC('BIGF')
      PRINT *, 'Merge completed successfully'
      IQUEST(1)=0
      GO TO 999
*
*         Abnormal end
*
99    CONTINUE
      IQUEST(1)=1
*
999   END
