*
* $Id: csincl.F,v 1.2 1997/05/13 11:56:05 couet Exp $
*
* $Log: csincl.F,v $
* Revision 1.2  1997/05/13 11:56:05  couet
* - new routine csinc1 to define a unique include file name
*
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 23/09/94  18.16.16  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      LOGICAL FUNCTION CSINCL(LINE,NCH,LIBRD,INCLUN)
***---------------------------------
      INTEGER INCLUN(10)
#include "comis/cslun.inc"
#include "comis/csinc.inc"
      COMMON/QUEST/IQUEST(100)
#if defined(CERNLIB_IBM)
         CHARACTER *80 VMIBM
#endif
         CHARACTER *(*) LINE, NAME*72
      CSINCL=.FALSE.
      IS=INDEX(LINE,'INCLUDE')
      IF(IS.LE.0)RETURN
      DO 88 I=1,IS-1
        IF(LINE(I:I).NE.' ')RETURN
 88   CONTINUE
      IS=IS+7
      DO 99 I=IS,NCH
        IF(LINE(I:I).NE.' ')GO TO 100
 99   CONTINUE
      CALL CSSOUT(' INCLUDE: no file name')
      RETURN
 100  IF(LINE(I:I).EQ.'''' .OR. LINE(I:I).EQ.'"')THEN
        NAME=LINE(I+1:NCH-1)
      ELSEIF(LINE(I:I).EQ.'=' .OR. LINE(I:I).EQ.'(')THEN
        RETURN
      ELSE
        NAME=LINE(I:NCH)
      ENDIF
*
*-* Special PAW case when NAME has a ?. This means gets the ntuple definition
*-* via the PAW utility UWFUNC written to junk file comis.inc
#if defined(CERNLIB_PAW)
      IF(INDEX(NAME,'?').NE.0)THEN
         IQUEST(99)=-1
         CALL PAWUWF
         IQUEST(99)=0
         NAME=INCNAM
      ENDIF
#endif
      CALL CSLUNF(LUN)
      IF(LUN.EQ.0)RETURN
      LIBRD=LIBRD+1
      IF(LIBRD.GT.10)THEN
          CALL CSSOUT
     +    ('COMIS: to deeply nested INCLUDE directives')
          RETURN
      ENDIF
      INCLUN(LIBRD)=LUN
      LUNLIB=LUN
#if defined(CERNLIB_VAX)
      OPEN( LUNLIB,FILE=NAME,SHARED,READONLY,STATUS='OLD',ERR=1)
#endif
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
*      OPEN( LUNLIB,FILE=NAME,STATUS='UNKNOWN' ,ERR=1)
      OPEN( LUNLIB,FILE=NAME,STATUS='OLD' ,ERR=1)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_PAW))
      VMIBM=NAME
      L=LENOCC(NAME)
      DO 77 I=1,L
         IF(VMIBM(I:).EQ.'.')VMIBM(I:I)=' '
  77  CONTINUE
      IS=INDEX(VMIBM,'/')
      IF(IS.NE.0)VMIBM(IS:IS)=' '
      OPEN( LUNLIB,FILE='/'//VMIBM,STATUS='UNKNOWN' ,ERR=1)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_PAW))
      CALL KUOPEN ( LUNLIB, NAME, 'OLD', ISTAT )
      IF ( ISTAT .NE. 0 )               GO TO 1
#endif
#if defined(CERNLIB_IBMMVS)
      CALL KUOPEN ( LUNLIB, NAME, 'OLD', ISTAT )
      IF ( ISTAT .NE. 0 )               GO TO 1
#endif
***      LIBRD=1
      CSINCL=.TRUE.
      RETURN
  1   CALL CSSOUT('FILE WAS NOT OPEN')
      CALL CSSOUT(NAME)
      CALL CSCLOS(LUNLIB)
      LIBRD=LIBRD-1
      IF(LIBRD.GT.0)LUNLIB=INCLUN(LIBRD)
      END
