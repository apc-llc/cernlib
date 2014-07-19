*
* $Id: cstype.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: cstype.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.19/00 18/01/95  15.56.16  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSTYPE(IADDR,TEXT)
***--------------------------------
* input:  IADDR -- cs-address of routine (iaddr=csaddr('name'))
* output: TEXT(1:1)='I' for integer function
*                  ='R' for real    function
*                  ='C' for character function
*                  ='L' for logical function
*                  ='D' for double pr. function
*                  ='S' for subroutine
*                  ='X' for complex function
*                  ='?' for unknown or error
***--------------------------------
#include "comis/cstabps.inc"
         COMMON/MDPOOL/IQ(100)
         CHARACTER *(*) TEXT,REPL*8
         DATA REPL/'IRCLDSX?'/
      IF(IADDR.GT.0)THEN
        I=IABS(IQ(IADDR+KSTPGP))
        IF(I.GT.8)I=8
        TEXT(1:1)=REPL(I:I)
      ELSE
        TEXT(1:1)='?'
      ENDIF
      END
