*
* $Id: cdgimgna.F,v 1.1.1.1 1996/02/28 16:24:18 mclareni Exp $
*
* $Log: cdgimgna.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:18  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CD_G_IMGNAME (NAME, LENGTH)
*     ======================================
*
************************************************************************
*                                                                      *
*        SUBR. CD_G_IMGNAME (NAME*, LENGTH*)                           *
*                                                                      *
*   Finds the executing image name in the character variable NAME      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     NAME     Name of the task                                        *
*     LENGTH   Length of the character string NAME                     *
*                                                                      *
*   Called by CD_CHK_CLIENT                                            *
*                                                                      *
************************************************************************
*
      CHARACTER*(*)   NAME
      INCLUDE '($JPIDEF)'
      STRUCTURE /ITMLST/
         UNION
            MAP
               INTEGER*2 BUFLEN
               INTEGER*2 CODE
               INTEGER*4 BUFADR
               INTEGER*4 RETLENADR
            END MAP
            MAP
               INTEGER*4 END_LIST
            END MAP
         END UNION
      END STRUCTURE
      RECORD /ITMLST/ JPI_LIST(2)
      INTEGER*4 STATUS,SYS$GETJPIW
*
*     ------------------------------------------------------------------
*
      JPI_LIST(1).BUFLEN    = 80
      JPI_LIST(1).CODE      = JPI$_IMAGNAME
      JPI_LIST(1).BUFADR    = %LOC(NAME)
      JPI_LIST(1).RETLENADR = %LOC(LENGTH)
      JPI_LIST(2).END_LIST  = 0
 
      STATUS = SYS$GETJPIW (,,,JPI_LIST,,,)
      IF (.NOT. STATUS) CALL LIB$SIGNAL (%VAL(STATUS))
*
* *** Parse full image name and strip off device,directory,file type
* *** and version.
*
      CALL STR$UPCASE ( NAME(1:LENGTH), NAME(1:LENGTH) )
      DO 10 J=1,LENGTH
        IF(NAME(J:J).EQ.']') LOC_OFFSET = J
   10 CONTINUE
      DO 20 J=LOC_OFFSET+1,LENGTH
        IF(NAME(J:J).EQ.'.') LOC_BORDER = J
   20 CONTINUE
      LENGTH1 = LOC_BORDER - LOC_OFFSET - 1
      DO 30 J=1,LENGTH1
        NAME(J:J) = NAME(LOC_OFFSET+J:LOC_OFFSET+J)
   30 CONTINUE
      LENGTH = LENGTH1
*                                                       END CD_G_IMGNAME
      END
