*
* $Id: fathead.F,v 1.1.1.1 1996/03/07 15:17:39 mclareni Exp $
*
* $Log: fathead.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:39  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATHEAD
*
*     Print headers of journal file on unit 1
*
*     Unix usage: fathead filename
*        or       for i in $FMCNDIV/done/*
*                    do
*                       fathead $i
*                    done
*
      COMMON/PAWC/PAW(50000)
      COMMON/QUEST/ IQUEST(100)
      CHARACTER*255 GENAM,CHFILE
      CHARACTER*4   CHCOMM
      DIMENSION     IUHEAD(400)
      DIMENSION     IOCR(100)
      PARAMETER     (JBIAS=2)
      PARAMETER     (IEVENT=0)
*
*     Get the file name (if given)
*
      CALL GETARG(1,CHFILE)
      LFILE = LENOCC(CHFILE)
 
      CALL HLIMIT(50000)
 
      CALL FZFILE(1,0,'IA')
 
      IF(LFILE.EQ.0) THEN
         OPEN(1,ACCESS='SEQUENTIAL', FORM='FORMATTED', STATUS='OLD')
      ELSE
#if defined(CERNLIB_IBMVM)
      CALL CTRANS('.',' ',CHFILE,1,LFILE)
#endif
         OPEN(1,ACCESS='SEQUENTIAL', FORM='FORMATTED', STATUS='OLD',
#if !defined(CERNLIB_IBMVM)
     +        FILE=CHFILE(1:LFILE))
#endif
#if defined(CERNLIB_IBMVM)
     +        FILE='/'//CHFILE(1:LFILE))
#endif
      ENDIF
      NREC = 0
    1 CONTINUE
      NHEAD = 400
      CALL FZIN(1,0,LSUP,2,'S',NHEAD,IUHEAD)
      IF(IQUEST(1).EQ.0) THEN
         NREC  = NREC + 1
         GENAM = ' '
         CALL UHTOC(IUHEAD(1),4,CHCOMM,4)
         CALL UHTOC(IUHEAD(2),4,GENAM,255)
         PRINT *,CHCOMM,GENAM(1:LENOCC(GENAM))
         IF(NHEAD.GT.80) THEN
            GENAM = ' '
            PRINT 9001,IUHEAD(81),IUHEAD(82)
            CALL UHTOC(IUHEAD(91),4,GENAM,255)
            PRINT *,'CHFNFA: ',GENAM(1:LENOCC(GENAM))
         ENDIF
9001     FORMAT(' IHOWFA: ',Z8,' ITIMFA: ',I6)
         GOTO 1
      ENDIF
      PRINT *,IQUEST(1)
      PRINT *,'NUMBER OF RECORDS = ',NREC
      CALL FZENDI(1,'T')
*
   99 END
