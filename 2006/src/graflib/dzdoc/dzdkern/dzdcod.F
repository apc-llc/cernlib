*
* $Id: dzdcod.F,v 1.1.1.1 1996/03/04 16:12:56 mclareni Exp $
*
* $Log: dzdcod.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:56  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDCOD(CHBANK,CWHATI,CHOPT,CHPRE,LUN,IFI,ILI)
      CHARACTER*8 CHBANK
      CHARACTER*4 CWHATI,CWHAT
      CHARACTER*(*) CHOPT,CHPRE
 
      CHARACTER*2 CHOINT
 
*     CWHAT:   'BANK', 'LINK', 'DATA'
 
*     CHOPT:   'B'   mzbook 'BANK'
*              'L'   mzlift 'BANK'
*              'A'   'LINK' assignments
*              'O'   'DATA' word offsets or 'LINK' offsets
*              'N'   no parameter statements
*              'T'   do it for a bank tree
*              '2'   skip first 2 char for data words
*              'R'   put REAL
*              'C'   put COMMON/CO..../
*              'H'   piut CALL HBNAME...
 
*     IFLAG: 1 - 99 things concerning 'BANK'
 
*            1 make keeps for INTEGER declarations
*            2 continue INTEGER decl
*            3 make keeps for booking +KEEP..
*            4 contiue booking no +KEEP..
*            5 COMMON with header
*            6  continue COMMON
*            7  make INTEGER for MMIDBK bank descriptor vector
*            8  continue with  MMIDBK
*            9  make COMMON for MMIDBK
*           10  continue with  MMIDBK
*           11  fill bank descriptor vector MMIDBK
*           12  continue with  MMIDBK
*           13  gen MZLIFT instead of MZBOOK
*           14  continue MZLIFT
*
*           101 - 199 things concerning 'LINK'
 
*           101  INTEGER decl start
*           102               continue
*           103  PARAMETER start
*           104            continue
*           105 link assignments start
*           106                  continue
*
*           201 - 299 things concerning 'DATA'
 
*           201  data word offsets INTEGER decl
*           203                    PARAMETER
 
      IF(INDEX(CHOPT,'T').GT.0)THEN
          CHOINT='T'
      ELSE
          CHOINT='S'
      ENDIF
      CWHAT=CWHATI
      CALL CLTOU(CWHAT)
      IF(CWHAT.EQ.'BANK')THEN
 
*         do INTEGER declarations (links)
          CALL DZDCO1(CHBANK,1,CHOINT,' ',LUN,IFI,ILI)
*         finish line with decl
          CALL DZDPLN(LUN,' ',99)
*         COMMON declarations (links)
          CALL DZDCO1(CHBANK,5,CHOINT,' ',LUN,IFI,ILI)
*         finish line
          CALL DZDPLN(LUN,' ',99)
          IF(INDEX(CHOPT,'L').NE.0)THEN
*             do INTEGER decl for MMIDBK
              CALL DZDCO1(CHBANK,7,CHOINT,' ',LUN,IFI,ILI)
              CALL DZDPLN(LUN,' ',99)
*             do COMMON decl for MMIDBK
              CALL DZDCO1(CHBANK,9,CHOINT,' ',LUN,IFI,ILI)
              CALL DZDPLN(LUN,' ',99)
*             fill MMIDBK
              CALL DZDCO1(CHBANK,11,CHOINT,' ',LUN,IFI,ILI)
              CALL DZDPLN(LUN,' ',99)
*             do MZLIFTs
              CALL DZDCO1(CHBANK,13,CHOINT,' ',LUN,IFI,ILI)
              CALL DZDPLN(LUN,' ',99)
          ELSE
*            do MZBOOKS
              CALL DZDCO1(CHBANK,3,CHOINT,' ',LUN)
*             finish line with bookings
              CALL DZDPLN(LUN,' ',99)
          ENDIF
      ELSEIF(CWHAT.EQ.'LINK')THEN
          IF(INDEX(CHOPT,'O').NE.0)THEN
*             INTEGER statement (Linkoffsets)
              CALL DZDCO1(CHBANK,101,CHOINT,CHPRE,LUN,IFI,ILI)
*             finish line
              CALL DZDPLN(LUN,' ',99)
*             PARAMETER statement
              CALL DZDCO1(CHBANK,103,CHOINT,CHPRE,LUN,IFI,ILI)
*             finish line
              CALL DZDPLN(LUN,')',1)
              CALL DZDPLN(LUN,' ',99)
          ELSEIF(INDEX(CHOPT,'A').NE.0)THEN
*             link assignment statements
              CALL DZDCO1(CHBANK,105,CHOINT,CHPRE,LUN,IFI,ILI)
          ENDIF
      ELSEIF(CWHAT.EQ.'DATA')THEN
*         data word offsets  INTEGER statements
          CALL DZDCO1(CHBANK,201,CHOPT,CHPRE,LUN,IFI,ILI)
          CALL DZDPLN(LUN,' ',99)
*         data word offsets PARAMETER
          IF(INDEX(CHOPT,'N').EQ.0 .AND.
     +       INDEX(CHOPT,'=').EQ.0 .AND.
     +       INDEX(CHOPT,'C').EQ.0 .AND.
     +       INDEX(CHOPT,'H').EQ.0  )THEN
             CALL DZDCO1(CHBANK,203,CHOPT,CHPRE,LUN,IFI,ILI)
             CALL DZDPLN(LUN,')',1)
             CALL DZDPLN(LUN,' ',99)
          ENDIF
      ENDIF
      CALL DZDPLN(0,' ',0)
      END
***********************************************************************
