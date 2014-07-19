*
* $Id: csdbmass.s,v 1.1.1.1 1996/02/26 17:16:47 mclareni Exp $
*
* $Log: csdbmass.s,v $
* Revision 1.1.1.1  1996/02/26 17:16:47  mclareni
* Comis
*
*
*CMZ :  1.19/00 03/04/95  11.34.00  by  Unknown
*-- Author :
*************************************
* from 04.03.94
* f77 version for mjchar,mkchar,cschar
* mjchar==locb
* mkchar returns ichar(ch)
* cschar(j,nh) now nh=ichar(ch)
*************************************
***--------------------
*   FUNCTION MJCHAR(I)
*   gets byte-address
***--------------------
*MJCHAR  START  0
*+SELF,IF=QMIBMXA.
*MJCHAR  AMODE ANY
*MJCHAR  RMODE ANY
*+SELF.
*       USING  *,15
*       L      0,0(1)
*       N      0,K
*       OI     12(13),X'80'
*       BR     14
*       DS     0F
*K      DC     X'7FFFFFFF'
*       END
*MKCHAR  START 0
***----------------------------------------------
*   FUNCTION MKCHAR(J)
*   gets character from j-byte address (say X)
*   puts character into MKCHAR and adds 3 blanks
*   results is /4HX   /
***----------------------------------------------
*+SELF,IF=QMIBMXA.
*MKCHAR  AMODE ANY
*MKCHAR  RMODE ANY
*+SELF.
*       USING  *,15
*       ST     2,28(13)
*       L      2,0(1)
*       L      2,0(2)
*       IC     0,0(2)
*       SLL    0,24
*       O      0,IBL
*       L      2,28(13)
*       OI     12(13),X'80'
*       BR     14
*       DS     0F
*IBL    DC     X'00404040'
*       END
*CSCHAR  START  0
***----------------------------------------------
*   SUBROUTINE CSCHAR(J,NH)
*   puts first character from NH to j-byte address
***----------------------------------------------
*+SELF,IF=QMIBMXA.
*CSCHAR  AMODE ANY
*CSCHAR  RMODE ANY
*+SELF.
*       USING  *,15
*       STM    2,3,28(13)
*       LM     2,3,0(1)
*       L      2,0(2)
*       MVC    0(1,2),0(3)
*       LM     2,3,28(13)
*       OI     12(13),X'80'
*       BR     14
*       END
***************************
CSBCLL  START  0
#if defined(CERNLIB_QMIBMXA)
CSBCLL  AMODE ANY
CSBCLL  RMODE ANY
#endif
       L      15,0(1)
       L      15,0(15)
       LA     1,4(1)
       BR     15
       END
CIFNCL START  0
#if defined(CERNLIB_QMIBMXA)
CIFNCL  AMODE ANY
CIFNCL  RMODE ANY
#endif
       L      15,0(1)
       L      15,0(15)
       LA     1,4(1)
       BR     15
       END
CLFCLL START  0
#if defined(CERNLIB_QMIBMXA)
CLFCLL  AMODE ANY
CLFCLL  RMODE ANY
#endif
       L      15,0(1)
       L      15,0(15)
       LA     1,4(1)
       BR     15
       END
CSLCAL START  0
#if defined(CERNLIB_QMIBMXA)
CSLCAL  AMODE ANY
CSLCAL  RMODE ANY
#endif
       USING  *,15
       STM    2,3,28(13)
       LR     2,1
       L      3,4(2)
       L      3,0(3)
       LTR    3,3
       BNP    NOPAR
       S      3,K1
       SLL    3,2
       L      1,8(2)
       AR     3,1
       OI     0(3),X'80'
NOPAR  L      15,0(2)
       L      15,0(15)
       LM     2,3,28(13)
       BR     15
K1     DC     F'1'
       END
CSCALI START  0
#if defined(CERNLIB_QMIBMXA)
CSCALI  AMODE ANY
CSCALI  RMODE ANY
#endif
       USING  *,15
       STM    2,3,28(13)
       LR     2,1
       L      3,4(2)
       L      3,0(3)
       LTR    3,3
       BNP    NOPAR
       S      3,K1
       SLL    3,2
       L      1,8(2)
       AR     3,1
       OI     0(3),X'80'
NOPAR  L      15,0(2)
       L      15,0(15)
       LM     2,3,28(13)
       BR     15
K1     DC     F'1'
       END
CSCALR START  0
#if defined(CERNLIB_QMIBMXA)
CSCALR  AMODE ANY
CSCALR  RMODE ANY
#endif
       USING  *,15
       STM    2,3,28(13)
       LR     2,1
       L      3,4(2)
       L      3,0(3)
       LTR    3,3
       BNP    NOPAR
       S      3,K1
       SLL    3,2
       L      1,8(2)
       AR     3,1
       OI     0(3),X'80'
NOPAR  L      15,0(2)
       L      15,0(15)
       LM     2,3,28(13)
       BR     15
K1     DC     F'1'
       END
CSCALD START  0
#if defined(CERNLIB_QMIBMXA)
CSCALD  AMODE ANY
CSCALD  RMODE ANY
#endif
       USING  *,15
       STM    2,3,28(13)
       LR     2,1
       L      3,4(2)
       L      3,0(3)
       LTR    3,3
       BNP    NOPAR
       S      3,K1
       SLL    3,2
       L      1,8(2)
       AR     3,1
       OI     0(3),X'80'
NOPAR  L      15,0(2)
       L      15,0(15)
       LM     2,3,28(13)
       BR     15
K1     DC     F'1'
       END
