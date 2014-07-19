*
* $Id: mergin.F,v 1.4 1999/03/26 15:09:52 couet Exp $
*
* $Log: mergin.F,v $
* Revision 1.4  1999/03/26 15:09:52  couet
* - It is now possible to change the record length used to open the input
*   files of the command HMERGE.
*
* Revision 1.3  1997/11/24 10:49:06  couet
* - KUHOME is now called before the HROPEN(s) call(s). This allows to have
*   a HMERGE command like:
*
* PAW > hmerge ~/paw/out.hbook in1.hbook $HOME/paw/in2.hbook
*
* Revision 1.2  1996/04/15 08:51:37  couet
* Filecase problem in the OUTFILE of the HMERGE command fixed.
*
* Revision 1.1.1.1  1996/03/01 11:38:38  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/14 25/09/95  17.16.56  by  O.Couet
*-- Author :    Rene Brun   13/07/94
      SUBROUTINE MERGIN
*.==========>
*.
*.  Subroutine to merge HBOOK files with identical objects
*.  and directories into FILOUT.
*.
*..=========> (Rene Brun)
*
      COMMON /QUEST/ IQUEST(100)
*
      CHARACTER*128 FILES, FILOUT
*
      EXTERNAL HMERGE1, HMERGE2
*
      CHARACTER*16 CHREC
      SAVE LRECL
      DATA LRECL /0/
*____________________________________________________________________
*
*
*         Open 1st file to get record length
*
*         Auto-record length detection does not work without option X
*         on Alpha/OSF/VMS
*
      CALL KUGETF(FILOUT,NOCH)
*
      CHREC = FILOUT(1:5)
      CALL CLTOU(CHREC)
      IF (CHREC(1:5).EQ.'LRECL') THEN
         CALL KUGETF(CHREC,NCHREC)
         CALL KICTOI(CHREC(1:NCHREC),LRECL)
         IF (IQUEST(1).NE.0) THEN
            PRINT*, 'Invalid record length value'
            LRECL = 0
         ENDIF
         IF (LRECL.NE.0) THEN
            PRINT*, 'Next HMERGE will use LRECL =',LRECL
         ELSE
            PRINT*, 
     +      'Next HMERGE will use the auto-record length detection'
         ENDIF
         RETURN
      ENDIF
*
      CALL KUGETF(FILES,NCH)
      CALL KUGETL(FILES,NCH)
*
      CALL KUHOME(FILES,NCH)
      CALL HROPEN(21,'FILE',FILES,'XP',LRECL,ISTAT)
      IF (ISTAT .NE. 0) GOTO 99
      CALL HRENDC('FILE')
*
*         Create big file
*
      IQUEST(10) = 64000
      CALL KUHOME(FILOUT,NCH)
      CALL HROPEN(22,'BIGF',FILOUT,'PQN',LRECL,ISTAT)
      IF (ISTAT .NE. 0) THEN
         GOTO 99
      ELSE
         PRINT *, 'Output file ', FILOUT(1:NCH) ,' created'
      ENDIF
*
*         Open 1st file to read characteristics
*
      CALL HROPEN(21,'FILE',FILES,'P',LRECL,ISTAT)
      IF (ISTAT .NE. 0) GOTO 99
      CALL RZSCAN('//FILE', HMERGE1)
      CALL HRENDC('FILE')
*
*         Loop on all input files
*
  50  CALL HROPEN(21,'FILE',FILES,'P',LRECL,ISTAT)
      IF (ISTAT .NE. 0) GOTO 99
      PRINT *, 'Processing file ', FILES(1:NCH)
      CALL RZSCAN(' ', HMERGE2)
      CALL HRENDC('FILE')
      FILES=' '
      CALL KUGETL(FILES,NCH)
      IF(NCH.GT.0)THEN
         CALL KUHOME(FILES,NCH)
         GO TO 50
      ELSE
         PRINT *, 'All input files scanned'
      ENDIF
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
