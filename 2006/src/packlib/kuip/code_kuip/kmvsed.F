*
* $Id: kmvsed.F,v 1.1.1.1 1996/03/08 15:32:55 mclareni Exp $
*
* $Log: kmvsed.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
#if defined(CERNLIB_IBMMVS)
*CMZ :  2.05/15 01/08/94  10.35.04  by  Alfred Nathaniel
*-- Author :    Alfred Nathaniel   06/11/92
      SUBROUTINE KMVSED(FILE,IST)
*
*******************************************************************************
*
* Called by KUEDIT to invoke the editor on MVS
*
* IST is set to 0 on normal exit, 1 on abort (quit), 2 on any error case
*
*     Input :
*
      CHARACTER*(*) FILE
*
*     Output :
*
      INTEGER IST
*
********************************************************************************
*
#if defined(CERNLIB_GSI)
      CHARACTER*46   CNAMEH
#endif
#if !defined(CERNLIB_NEWLIB)
      CHARACTER*80   TEMP
      CHARACTER*48   CNAME
      CHARACTER*20   PREFIX

#endif
#if defined(CERNLIB_NEWLIB)
      CHARACTER*44   CNAME
      CHARACTER*20   PREFIX
      CHARACTER*80   MVSCOM(20)
      INTEGER        SPACE(2)
#endif

      LENGTH = LENOCC(FILE)

#if !defined(CERNLIB_NEWLIB)

*--       CHECK THAT THE FILE EXISTS (IF NOT, CREATE IT)

      LDUM=99
30    CONTINUE
      TEMP=' '
      CALL KUINQF(TEMP,LDUM)
      IF (TEMP.EQ.' ') GO TO 20
      LDUM=LDUM-1
      GO TO 30
20    CONTINUE

      CALL KUOPEN ( LDUM, FILE, 'UNKNOWN', ISTAT )
      IF ( ISTAT .NE. 0 )               THEN
           IST = 2
           PRINT *, '*** KUEDIT: CANNOT OPEN FILE= ', FILE
           RETURN
      ENDIF

*--       AND CLOSE IT, IT WILL BE OPEN BY ISPF EDITOR

      CALL KUCLOS(LDUM,'KEEP',IRC)
   10 CONTINUE

*--       IBMMVS = CALL THE ISPF EDITOR

*--       WARNING = ROUTINE ISPLNK IS FOUND IN LIBRARY:
*--            ISP.V2R2M0.ISPLOAD (STRASBOURG NAME...) WHICH MUST
*--            BE ADDED AT THE LINK LEVEL

*--       DON'T ADD PREFIX IF THE FIRST CHARACTER OF FILE NAME IS A DOT

      IF ( FILE(1:1) .EQ. '.' )         THEN
           CNAME  = FILE(2:LENGTH)
           LENGTH = LENGTH - 1
      ELSE
           CALL KPREFI ( PREFIX, NCHPRE )
           CNAME  = PREFIX(1:NCHPRE)//FILE
           LENGTH = LENGTH + NCHPRE
      ENDIF
#endif
#if defined(CERNLIB_GSI)
*--   NOW ADD APOSTROPHES ALWAYS
      CNAMEH = ''''//CNAME(1:LENGTH)//''''
      CNAME = CNAMEH
      LENGTH = LENGTH + 2
#endif
#if !defined(CERNLIB_NEWLIB)

      IST   = 0
* return after occurence of error
      ISTAT = ISPLNK ( 'CONTROL' , 'ERRORS' , 'RETURN' )
      IF ( ISTAT .NE. 0 )               THEN
           PRINT *, '*** SYSTEM FAILURE IN CALLING THE ISPF/PDF'
     +,             ' CONTROL ERROR RETURN, RC= ', ISTAT
           IST = 2
           RETURN
      ENDIF
* define dataset names
      ISTAT = ISPLNK ( 'VDEFINE', '(DSN)', CNAME, 'CHAR', LENGTH )
      IF ( ISTAT .NE. 0 )               THEN
           IST = 2
           PRINT *, '*** KUEDIT: SYSTEM FAILURE IN CALLING THE EDITOR'
     +,             ' STEP VDEFINE - STATUS ', ISTAT
           RETURN
      ENDIF
* call the editor
      ISTAT = ISPLNK ( 'EDIT ', CNAME )
      IF ( ISTAT .NE. 0                 .AND.
     +     ISTAT .NE. 4 )               THEN
           IST = 2
           PRINT *, '*** KUEDIT: SYSTEM FAILURE IN CALLING THE EDITOR'
     +,             ' STEP EDIT - STATUS ', ISTAT
           RETURN
      ENDIF
* If not saved, ISTAT=4 (if text was not altered or edit sess. canceled)
      IF ( ISTAT .EQ. 4) THEN
         IST = 1
         PRINT *, 'KUEDIT: FILE NOT SAVED !!'
      ENDIF
* reset into line mode
      ISTAT = ISPLNK ( 'CONTROL' , 'DISPLAY' , 'LINE' , 1)
      IF ( ISTAT .NE. 0 )               THEN
           PRINT *, '*** SYSTEM FAILURE IN CALLING THE ISPF/PDF'
     +,             ' CONTROL DISPLAY LINE 1, RC= ', ISTAT
           RETURN
      ENDIF
#endif
#if defined(CERNLIB_NEWLIB)
*--
*     Test for enough memory for shell
      CALL $SPACEM(SPACE(1),SPACE(2))
      IF (SPACE(2) .LT. 200 000) THEN
         PRINT *, '***ERROR*** Not enough storage available for EDIT'
     +,           '. Command rejected'
         RETURN
      ENDIF

      IF ( FILE(1:2) .EQ. 'FT' .AND. FILE(5:8) .EQ. 'F001') THEN

*--       EDIT temporary FORTRAN file

           MVSCOM( 1)='((CMZ_EDIT)) '//FILE(1:8)
           MVSCOM( 2)='PUT EDITFILE FILE '//FILE(1:8)//' SEQ'
           GOTO 900
      ENDIF

*--       DON'T ADD PREFIX IF THE FIRST CHARACTER OF FILE NAME IS A DOT

      IF ( FILE(1:1) .EQ. '.' )         THEN
           CNAME  = FILE(2:LENGTH)
           LENGTH = LENGTH - 1
      ELSE
           CALL KPREFI ( PREFIX, NCHPRE )
           CNAME  = PREFIX(1:NCHPRE)//FILE
           LENGTH = LENGTH + NCHPRE
      ENDIF
      MVSCOM( 1)='((KU_EDIT)) '''//CNAME(1:LENGTH)//''''
      MVSCOM( 2)='PUT EDITFILE INTO '''//CNAME(1:LENGTH)//''' SEQ'

  900 CALL NEWLIB(MVSCOM, 1)
      MVSCOM( 1)='V OFF OFF'
*     MVSCOM( 2)=    already set above
      MVSCOM( 3)='SC EDITFILE'
      MVSCOM( 4)='V ON'
      MVSCOM( 5)='QUIT'
      CALL NEWLIB(MVSCOM,5)
#endif

999   END

#endif
