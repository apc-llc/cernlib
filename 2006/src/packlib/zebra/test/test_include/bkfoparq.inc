*
* $Id: bkfoparq.inc,v 1.1.1.1 1996/03/06 10:47:03 mclareni Exp $
*
* $Log: bkfoparq.inc,v $
* Revision 1.1.1.1  1996/03/06 10:47:03  mclareni
* Zebra
*
*
*   Description of the bank IO characteristic codes
*
* bkfoparq.inc
*


      CHARACTER*1 KFOTYP(0:11)
      PARAMETER ( IFOUNQ = 0 , KFOTYP(IFOUNQ)='U' )
      PARAMETER ( IFOBIQ = 1 , KFOTYP(IFOBIQ)='B' )
      PARAMETER ( IFOINQ = 2 , KFOTYP(IFOINQ)='I' )
      PARAMETER ( IFOFLQ = 3 , KFOTYP(IFOFLQ)='F' )
      PARAMETER ( IFODOQ = 4 , KFOTYP(IFODOQ)='D' )
      PARAMETER ( IFOHOQ = 5 , KFOTYP(IFOHOQ)='H' )
      PARAMETER ( IFOSEQ = 7 , KFOTYP(     6)='*' , KFOTYP(IFOSEQ)='S')
      PARAMETER ( IFONAQ = 9 , KFOTYP(     8)='D' , KFOTYP(IFONAQ)='N')
      PARAMETER ( IFOLIQ = 11, KFOTYP(    10)='*' , KFOTYP(IFOLIQ)='L')
      PARAMETER ( JFOSEQ = 16)

