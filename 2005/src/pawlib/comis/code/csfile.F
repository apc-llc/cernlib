*
* $Id: csfile.F,v 1.4 1998/03/11 09:56:51 couet Exp $
*
* $Log: csfile.F,v $
* Revision 1.4  1998/03/11 09:56:51  couet
* - new mod for LINUX:
*
* appl comis quit
* !file a.sl
* quit
*
*  didn't work
*
* Revision 1.3  1998/01/13 12:45:29  couet
* - Mods for LINUX
*
* Revision 1.2  1998/01/12 09:55:46  couet
* - Mods for LINUX (shared libraries)
*
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          13/02/96  14.51.39  by  Unknown
*-- Author : V.Berezhnoi
      SUBROUTINE CSFILE(FILENAME)
      CHARACTER*(*)FILENAME
      COMMON/CSERRNO/IERR
#if defined(CERNLIB_SHL)
#include "comis/cshlnm.inc"
      CHARACTER LIBNAME*256,LINE*256,PATH*256,NAME*256,
     +          CPID*8,FEXT*32
      INTEGER SYSTEMF
      LOGICAL EXIST1
      L=LENOCC(FILENAME)
*???      CALL CUTOL(FILENAME(:L))
      ICASE=0
      IERR=0
      CALL CSPARSFN(FILENAME(:L),PATH,NAME,FEXT)
      LX=LENOCC(FEXT)
      IF(FEXT.EQ.'.csl')THEN
        ICASE=4
      ELSEIF(FEXT.EQ.'.sl')THEN
        ICASE=3
      ELSEIF(FEXT.EQ.'.c')THEN
        ICASE=2
      ELSEIF(FEXT(LX-1:LX).EQ.'77')THEN
        ICASE=1
        INQUIRE(FILE=FILENAME(:L),EXIST=EXIST1)
        IF(.NOT.EXIST1) FEXT=FEXT(1:LX-2)
      ENDIF
      IF(ICASE.GT.0)THEN
        CALL GETPIDF(IPID)
        CPID=' '
        WRITE(CPID,'(I8)')IPID
        LP=LOG10(REAL(IPID))+1
        CPID=CPID(9-LP:)
        LCHPATH=LENOCC(CHPATH)
        LPATH=LENOCC(PATH)
        LNAME=LENOCC(NAME)
        LEXT=LENOCC(FEXT)
        LIBNAME=CHPATH(:LCHPATH)// NAME(:LNAME) // '_' // CPID(:LP)
        LL=LENOCC(LIBNAME)
        IF(ICASE.EQ.1)THEN
*---- file...77
          IF(LPATH.GT.0)THEN
            LINE=PATH(:LPATH)//NAME(:LNAME)//FEXT(:LEXT)
          ELSE
            LINE=NAME(:LNAME)//FEXT(:LEXT)
          ENDIF
          CALL CSRMSL(LIBNAME(:LL))
          CALL CSF77(LINE,LIBNAME(:LL)//'.f',IERR)
          IF(IERR.NE.0)THEN
         PRINT *,' error during conversion to f77::File: ',
     +          FILENAME(:L)
            RETURN
          ENDIF
        ELSEIF(ICASE.EQ.2)THEN
*---- file.c
          LINE='cp '//FILENAME(:L)//' '//LIBNAME(:LL)//'.c'
          LE=LENOCC(LINE)
          IERR=SYSTEMF(LINE(:LE))
          IF(IERR.NE.0)RETURN
          CALL CSRMSL(LIBNAME(:LL))
        ELSEIF(ICASE.EQ.3)THEN
*---- file.sl
          IL=L-3
          LIBNAME= FILENAME(:IL)
          LL=IL
          CALL CSRMSL(LIBNAME(:LL))
*-- ' ' means don't prodused output file
#if defined(CERNLIB_LINUX)
          LINE = LIBNAME
          LINE(LENOCC(LINE)+1:) = '.f'
          CALL CSF77(LINE,' ',IERR)
#else
          CALL CSF77(FILENAME(:IL)//'.f',' ',IERR)
#endif
          IF(IERR.NE.0)THEN
          PRINT *,' error during translation csf77-file: '
     +            ,FILENAME(:L)
            RETURN
          ENDIF
        ELSEIF(ICASE.EQ.4)THEN
*---- file.csl
          IL=L-4
          LIBNAME= FILENAME(:IL)
          LL=IL
          CALL CSRMSL(LIBNAME(:LL))
        ENDIF
        IF (INDEX(LIBNAME(:LL),'/') .EQ. 0) THEN
           LINE = LIBNAME
           LIBNAME = './'//LINE
           LL = LL + 2
        ENDIF
        CALL CSCRSL(LIBNAME(:LL),CPID(:LP),FEXT(:LEXT),IERR)
        RETURN
      ENDIF
#endif
      IERR=0
      CALL CSFILX(FILENAME)
      END
