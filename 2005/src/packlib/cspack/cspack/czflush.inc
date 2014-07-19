*
* $Id: czflush.inc,v 1.1.1.1 1996/03/08 15:44:17 mclareni Exp $
*
* $Log: czflush.inc,v $
* Revision 1.1.1.1  1996/03/08 15:44:17  mclareni
* Cspack
*
*
#ifndef CERNLIB_CSPACK_CZFLUSH_INC
#define CERNLIB_CSPACK_CZFLUSH_INC
*
*
* czflush.inc
*
*
*     Flush server messages
*
10    CONTINUE
      CALL CZGETA(CHMAIL,ISTAT)
      LCH = LENOCC(CHMAIL)
      IF(CHMAIL(1:1).EQ.'0') THEN
*
*        Nop
*
         ELSEIF(CHMAIL(1:1).EQ.'1') THEN
         ELSEIF(CHMAIL(1:1).EQ.'2') THEN
         GOTO 10
         ELSEIF(CHMAIL(1:1).EQ.'3') THEN
         IQUEST(1) = 1
         IRC       = 1
         ELSEIF(CHMAIL(1:1).EQ.'E') THEN
         IQUEST(1) = -1
         IRC       = -1
         ELSEIF(CHMAIL(1:1).EQ.'V') THEN
         GOTO 10
         ELSE
         IQUEST(1) = 1
         IRC       = 1
         ENDIF
*

#endif
