*
* $Id: zjumpbe.s,v 1.1.1.1 1996/03/08 12:01:16 mclareni Exp $
*
* $Log: zjumpbe.s,v $
* Revision 1.1.1.1  1996/03/08 12:01:16  mclareni
* Zbook
*
*
#if defined(CERNLIB_BESM6)
 ZJUMP    :  , NAME ,
             , STI  , 8
           8 , WTC  ,
             , UJ   ,
             , END  ,
#endif
