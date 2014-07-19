*
* $Id: kprefi.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kprefi.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
#if defined(CERNLIB_IBMMVS)
*CMZ :  1.68/00 20/02/92  14.32.31  by  Fons Rademakers
*-- Author :
      SUBROUTINE KPREFI(PREFIX, NCHPRE)
************************************************************************
*                                                                      *
* KPREFI                                                               *
*                                                                      *
* KPREFI returns the prefix to be added on each file name.             *
*                                                                      *
* PREFIX  (C)    character string containing prefix (output)           *
* NCHPRE  (I)    number of characters of prefix     (output)           *
*                                                                      *
************************************************************************
*
      LOGICAL        INTRAC

      CHARACTER*(*)  PREFIX
#if !defined(CERNLIB_GSI)
      CHARACTER*80   USERID

      DATA           USERID / '        ' /
#endif
#if defined(CERNLIB_GSI)
      CHARACTER*8    CUID

#endif
#if defined(CERNLIB_NEWLIB)
      CHARACTER*16   NAMES
#endif
*
*-- extract the current user id from system information
*
#if defined(CERNLIB_NEWLIB)
      CALL JOBNAM(NAMES)
      USERID(1:6)=NAMES
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))&&(defined(CERNLIB_GSI))
      CALL USERID(CUID)
      PREFIX = CUID(1:4)//'.'
      NCHPRE = 5
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))&&(!defined(CERNLIB_GSI))
      IF ( INTRAC(DUMMY) )              THEN
*-- interactive mode
           NCH   = 8
           ISTAT = ISPLNK ( 'VCOPY', '(ZUSER)', NCH, USERID, 'MOVE' )
           IF ( ISTAT .NE. 0 )          THEN
                PRINT *, '*** KPREFI: SYSTEM FAILURE WHEN '
     +,                  'EXTRACTING USERID - STATUS ', ISTAT
                IF ( USERID .EQ. '        ' )
     +               CALL KDIALO ('PLEASE TYPE YOUR USERID ', USERID, N)
           ENDIF
      ELSE
*-- batch mode
           CALL KPREFB ( USERID )
           USERID(7:8) = '  '
      ENDIF
*
*-- build a prefix starting from the userid
*
*-- the following construction is installation dependant
*-- and function of local file name conventions
*-- at strasbourg, from userid = 'G057AM', we build
*-- the prefix = 'G057.KAM.'
*
      PREFIX = USERID(1:4)//'.K'//USERID(5:6)//'.'
      NCHPRE = 9
#endif
#if defined(CERNLIB_NEWLIB)
      PREFIX = USERID(1:6)//'.'
      NCHPRE =7
#endif

      RETURN
      END

#endif
