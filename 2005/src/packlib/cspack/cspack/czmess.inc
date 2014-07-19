*
* $Id: czmess.inc,v 1.1.1.1 1996/03/08 15:44:18 mclareni Exp $
*
* $Log: czmess.inc,v $
* Revision 1.1.1.1  1996/03/08 15:44:18  mclareni
* Cspack
*
*
#ifndef CERNLIB_CSPACK_CZMESS_INC
#define CERNLIB_CSPACK_CZMESS_INC
*
*
* czmess.inc
*
*
*     Process server messages
*
      IRC = 0
10    CONTINUE
      CALL CZGETA(CHMAIL,ISTAT)
      LCH = LENOCC(CHMAIL)
      IF(CHMAIL(1:1).EQ.'0') THEN
*
*     Nop
*
      ELSEIF(CHMAIL(1:1).EQ.'1') THEN
         PRINT *,CHMAIL(2:LCH)
      ELSEIF(CHMAIL(1:1).EQ.'2') THEN
         PRINT *,CHMAIL(2:LCH)
         GOTO 10
      ELSEIF(CHMAIL(1:1).EQ.'3') THEN
         PRINT *,CHMAIL(2:LCH)
         IQUEST(1) = 1
         IRC       = 1
      ELSEIF(CHMAIL(1:1).EQ.'4') THEN
         IQUEST(1) = 1
         IRC       = 1
      ELSEIF(CHMAIL(1:1).EQ.'E') THEN
         IQUEST(1) = -1
         IRC       = -1
      ELSEIF(CHMAIL(1:1).EQ.'V') THEN
*
*        Number of bytes read from a variable length read
*
         READ(CHMAIL(2:11),'(I10)') NGOT
         GOTO 10
      ELSE
         PRINT *,'Unknown server message ',CHMAIL
         IQUEST(1) = 1
         IRC       = 1
      ENDIF
*

#endif
