*
* $Id: mhlattr.F,v 1.2 1996/09/11 14:59:48 couet Exp $
*
* $Log: mhlattr.F,v $
* Revision 1.2  1996/09/11 14:59:48  couet
* - Hgetnt and Hgetn2 (old qp routines) are now replaced by hntld
*
* Revision 1.1.1.1  1996/03/01 11:39:05  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/07 07/06/94  19.09.13  by  Fons Rademakers
*-- Author :
      SUBROUTINE MHLATTR(CHPATH,IOBJ,TITLE,NX,XMIN,XMAX,
     +                                     NY,YMIN,YMAX,ISW,NTUP)
************************************************************************
*                                                                      *
* MHLATTR                                                              *
*                                                                      *
* Get attributes of IOBJ in directory CHPATH.                          *
* If NX<0 then IOBJ is a chain. If object not found NY<0.              *
*                                                                      *
************************************************************************
*
#include "hbook/hcbook.inc"
#include "hbook/hcdire.inc"
#include "paw/pawchn.inc"
#include "paw/pntold.inc"
#include "paw/quest.inc"
*
      CHARACTER*(*) CHPATH, TITLE
      CHARACTER*128 CHWOLD
      CHARACTER*64  CHTAG
      CHARACTER*8   BLOCK, CHID
      CHARACTER*1   TYPE
      INTEGER       IOBJ, PCHEVT, PISCHN
      LOGICAL       NTUP, ISCHN
*
      IF (LHBOOK.EQ.0) GOTO 99
*
*             Save Current directory
*
      CALL HCDIR(CHWOLD,'R')
*
*             If path is a chain but not the current chain then return
*
      ISCHN = .FALSE.
      IF (PISCHN(CHPATH, LENOCC(CHPATH)) .NE. 0) THEN
         LC = LENOCC(CURCHN)
         IF (CHPATH(3:LC+2) .NE. CURCHN(1:LC)) GOTO 99
         ISCHN = .TRUE.
      ENDIF
*
*             Set Current Working directory to CHPATH
*
      CALL HCDIR(CHPATH,' ')
      IF (IQUEST(1) .EQ. 1) GOTO 99
*
*             Get attributes of object
*
      CALL HLATTR(IOBJ,TYPE,TITLE,NX,XMIN,XMAX,NY,YMIN,YMAX,ISW,IER)
      IF (IER .NE. 0) GOTO 99
*
      NXT = NX
      IF (ISCHN .AND. TYPE.EQ.'N') THEN
         NCHEVT = PCHEVT(CHPATH, LENOCC(CHPATH), IOBJ, 0, 2)
         NX = -NX
         NY = NCHEVT
      ENDIF
*
      IF (TYPE .EQ. 'N' .AND. NTUP) THEN
*
*-- copy N-tuple header to PAWC (HNTVAR only works when header in PAWC)
*
         CALL IZITOC(IOBJ, CHID)
         CALL HNTLD(CHID)
         IF (IQUEST(1) .LT. 0) GOTO 40
*
*-- NCHEVT is calculated in HNTLD
*
         IF (ISCHN) NY = NCHEVT
*
         DO 10 I = 1, NXT
            CALL HNTVDEF(IOBJ, I, CHTAG, BLOCK, ITYPE)
            CALL LIST_ADD_NTUPLE_VAR(CHTAG, LENOCC(CHTAG), 0)
  10     CONTINUE
      ENDIF
*
*          Restore Current Directory
*
  40  CALL HCDIR(CHWOLD,' ')
*
      RETURN
*
  99  NY = -1
      RETURN
*
      END
