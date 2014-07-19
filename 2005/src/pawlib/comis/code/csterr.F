*
* $Id: csterr.F,v 1.1.1.1 1996/02/26 17:16:21 mclareni Exp $
*
* $Log: csterr.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:21  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 27/09/94  13.22.14  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSTERR(IERR)
***---------------------------
*    types translater's errors
***---------------------------
#include "comis/cspar.inc"
#include "comis/csrec.inc"
#include "comis/cspnts.inc"
      GO TO (1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
     + 18,19,20,21,22,23,24,25,26,27,28,29,30 ,31,32,33,
     + 34,35,36,37,38,39               ),1-IERR
      WRITE(*,*)'CS ERROR?',IERR
 1    REC1=' syntax error'
         GO TO 77
 2    REC1=' no common or global block with name:'
      CALL CCOPYS(JID,JSR1+40,NCIDEN)
         GO TO 77
 3    REC1=' comis does not support yet this statement'
         GO TO 77
 4    REC1=' common and global block with same name'
         GO TO 77
 5    REC1=' multiple definition of statement label'
         GO TO 77
 6    REC1=' error in equivalence'
         GO TO 77
 7    REC1=' adjustable array bounds must be dummy arguments'
         GO TO 77
 8    REC1=' lower bound greater than upper bound'
         GO TO 77
 9    REC1=' assignment to a non-variable or COMPILER ERROR'
         GO TO 77
 10   REC1=' non constant subscript'
         GO TO 77
 11   REC1=' comis does not support character functions'
          GO TO 77
 12   REC1=' mixed numeric and character'
         GO TO 77
 13   REC1=' non pozitive length of string'
         GO TO 77
 14   REC1=' do_variable has wrong type'
         GO TO 77
 15   REC1=' wrong number syntax'
       GO TO 77
 16   REC1=' do - level > 10'
       GO TO 77
 17   REC1=' non variable or array'
        GO TO 77
 18   REC1=' character value required'
         GO TO 77
 19   REC1=' multiple declaration of name'
         GO TO 77
 20   REC1=' arithmetic value required'
         GO TO 77
 21   REC1=' argument - character expression - not allowed'
         GO TO 77
 22   REC1=' non integer expression'
         GOTO 77
 23   REC1=' number of subscripts does not match array declaration'
         GO TO 77
 24   REC1=' undefined variable'
         GO TO 77
 25   REC1=' non constant expression'
         GO TO 77
 26   REC1=' operation not allowed on these data types'
         GO TO 77
 27   REC1='COMPILER STACK OVERFLOW'
         GO TO 77
 28   REC1=' DO or IF block not closed'
         GO TO 77
 29   REC1=' missing apostrophe'
         GO TO 77
 30   GO TO 78
 31   REC1=' array subscript out of range'
       GO TO 77
 32   REC1=' comis does not support character equivalence'
       GO TO 77
 33   REC1=' number of names exceeds number of values'
        GO TO 77
 34   REC1=' number of values exceeds number of names'
        GO TO 77
 35   REC1=' invalid repeat counter'
         GO TO 77
 36   REC1=' too long hollerith constant'
        GO TO 77
 37   REC1=' more then 7 dimensions'
        GO TO 77
 38   REC1=' error in call to intrinsic'
        GO TO 77
 39   REC1=' missing the END statement'
        GO TO 77
 77   CALL CSSOUT(REC1)
 78   END
