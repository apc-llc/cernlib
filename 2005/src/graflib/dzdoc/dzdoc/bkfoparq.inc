*
* $Id: bkfoparq.inc,v 1.1.1.1 1996/03/04 16:12:58 mclareni Exp $
*
* $Log: bkfoparq.inc,v $
* Revision 1.1.1.1  1996/03/04 16:12:58  mclareni
* Dzdoc/Zebpack
*
*
#ifndef CERNLIB_DZDOC_BKFOPARQ_INC
#define CERNLIB_DZDOC_BKFOPARQ_INC
*
*   Description of the bank IO characteristic codes
*
* bkfoparq.inc
*
 
C--     Definition of the bank IO characteristic types (MZBOOK/MZLIFT)
 
C--               Undefined --> bank cannot be transported
      PARAMETER ( IFOUNQ = 0)
C--               Bit pattern
      PARAMETER ( IFOBIQ = 1)
C--               Integer
      PARAMETER ( IFOINQ = 2)
C--               Floating point
      PARAMETER ( IFOFLQ = 3)
C--               Double precision
      PARAMETER ( IFODOQ = 4)
C--               Hollerith
      PARAMETER ( IFOHOQ = 5)
C--               Self describing sector
      PARAMETER ( IFOSEQ = 7)
C--               Retrieve by name
      PARAMETER ( IFONAQ = 9)
C--               Copy from linear chain
      PARAMETER ( IFOLIQ = 11)
C--               Multiplication factor for self-describing sector
      PARAMETER ( JFOSEQ = 16)
 

#endif
