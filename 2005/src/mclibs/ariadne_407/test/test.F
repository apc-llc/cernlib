*
* $Id: test.F,v 1.1.1.1 1996/03/08 16:51:07 mclareni Exp $
*
* $Log: test.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:07  mclareni
* Ariadne
*
*
 
C...ARiadne TEST by calling artest with argument IPRINT= 0 or 1
 
C...Performs various tests on Ariadne
 
      EXTERNAL LUDATA,PYDATA
      IPRINT=0

      CALL ARTEST(IPRINT)
      END
