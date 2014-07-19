*
* $Id: gtopen.F,v 1.1.1.1 1996/04/01 15:01:12 mclareni Exp $
*
* $Log: gtopen.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:12  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE GTOPEN
#include "iorc.inc"
C     **   This subroutine returns the number (LOUT) of the output  **
C     **   stream for the verbose messages printed when running     **
C     **   the GENLIB testing routines. The program also opens up   **
C     **   the file (if selected) into which the long messages will **
C     **   be put.                                                  **
      CHARACTER      QUEST1*1, QUEST2*1
      CHARACTER*(*) OUTFIL
#include "outfil.inc"
#if defined(CERNLIB_VERBOSE)
      DATA QUEST1/'Y'/,QUEST2/'Y'/
#endif
#if !defined(CERNLIB_VERBOSE)
      DATA QUEST1/'N'/,QUEST2/'N'/
#endif
#if defined(CERNLIB_INTERACT)
100   PRINT *,'Do you wish to see verbose messages on screen ? (Y/N)'
      READ(5,'(A1)') QUEST1
      IF (QUEST1 .EQ. 'y') QUEST1='Y'
      IF (QUEST1 .EQ. 'n') QUEST1='N'
      IF (QUEST1 .NE. 'Y' .AND. QUEST1 .NE. 'N') GOTO 100
#endif
C     So here we have a valid prompt Y or N
      IF (QUEST1 .EQ. 'N') THEN
        LOUT=1
#if defined(CERNLIB_INTERACT)
200     FORMAT(//' Do you want the verbose messages stored in a file ?')
300     PRINT 200
        READ(5,'(A1)') QUEST2
        IF (QUEST2 .EQ. 'y') QUEST2='Y'
        IF (QUEST2 .EQ. 'n') QUEST2='N'
        IF (QUEST2 .NE. 'Y' .AND. QUEST2 .NE. 'N') GOTO 300
#endif
        IF (QUEST2 .EQ. 'N') THEN
C          The verbose text file will be trashed upon exit
#if defined(CERNLIB_IBM)
           CALL FILEINF(I,'RECFM','VBA','LRECL',137)
#endif
           OPEN (UNIT=LOUT,FORM='FORMATTED',STATUS='SCRATCH')
        ELSE
           OPEN (UNIT=LOUT,FILE=OUTFIL,STATUS='UNKNOWN',ERR=400)
        ENDIF
      ELSE
C       Here verbose output is wanted, so LOUT points to the screen
        LOUT=6
      ENDIF
C     CALL MTLSET('     ',LOUT,100,100)
      CALL MTLSET('      ',LOUT,-1,-1)
      CALL KERSET('           ',LOUT,-1,-1)
400   RETURN
      END
