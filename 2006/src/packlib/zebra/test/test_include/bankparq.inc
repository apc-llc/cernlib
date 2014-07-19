*
* $Id: bankparq.inc,v 1.1.1.1 1996/03/06 10:47:03 mclareni Exp $
*
* $Log: bankparq.inc,v $
* Revision 1.1.1.1  1996/03/06 10:47:03  mclareni
* Zebra
*
*
*  BANK PARAMETERS + EXPLANATION
*
* bankparq.inc
*

      PARAMETER ( NBKHDQ = 1 )
      PARAMETER ( NBKCTQ = 9 )
      PARAMETER ( NBKOHQ = NBKHDQ + NBKCTQ )
      PARAMETER ( MBKLNQ = 0 )
      PARAMETER ( JBKSKQ = 1 )
      PARAMETER ( NBKSKQ = 16 )
      PARAMETER ( NBKMXQ = 2**NBKSKQ-1 )
      PARAMETER ( MBKIOQ = 1 + MBKLNQ )
      PARAMETER ( JBKIOQ = NBKSKQ + 1 )
      PARAMETER ( NBKIOQ = 16 )
      PARAMETER ( NBKJMQ = 12 )

      PARAMETER ( JBKUSQ = 1 )
      PARAMETER ( NBKUSQ = 18 )
      PARAMETER ( JBKSYQ = NBKUSQ + 1 )
      PARAMETER ( NBKSYQ = 32 - NBKUSQ )

