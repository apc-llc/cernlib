*
* $Id: hmergin.F,v 1.1.1.1 1996/01/16 17:08:10 mclareni Exp $
*
* $Log: hmergin.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:10  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.21.16  by  Rene Brun
*-- Author :    Rene Brun, J.C. Marin and Fons Rademakers   26/05/94
      SUBROUTINE HMERGIN
*.==========>
*.
*.  Main subroutine to merge HBOOK files with identical objects
*.  and directories.
*.
*..=========> (Rene Brun, J.C. Marin, A.A.Rademakers )
*
      INTEGER         MXFILES
      PARAMETER      (MXFILES=99)
      INTEGER         NFILES
      CHARACTER*80   FILES
      COMMON /FILES/  NFILES, FILES(MXFILES)
*
      CHARACTER*80   FILOUT
*____________________________________________________________________
*
*         Get output file name
*
      FILOUT = ' '
      WRITE(6,1000)
 1000 FORMAT(' Give output file name ===>')
      READ(5,'(A)') FILOUT
      IF (LENOCC(FILOUT) .LE. 0) THEN
         WRITE(6,'('' No OUTPUT file. Job aborted'')')
         GOTO 99
      ENDIF
*
*         Get all input file names
*
      NFILES = 0
    5 WRITE(6,2000)
 2000 FORMAT(' Give input file name or terminate with a blank ==>')
      FILES(NFILES+1) = ' '
      READ(5,'(A)') FILES(NFILES+1)
      NCH = LENOCC(FILES(NFILES+1))
      IF (NCH .GT. 0) THEN
         NFILES = NFILES + 1
         IF(NFILES.LT.MXFILES)GOTO 5
      ENDIF
*
*     call the main merger
*
      CALL HMERGE(NFILES,FILES,FILOUT)
*
   99 END
