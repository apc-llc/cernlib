/*
 * $Id: mkdims.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkdims.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */
/*** String and array dimension ***/
#ifndef _mkdims_
#define _mkdims_

/*  N.B. The following values MUST be the SAME as in FORTRAN (KUCDES) !!!     */
#define MAXSTR            80  /*  most string length                          */
#define MAX_APP_SHELL     20  /*  max. number of application shell windows    */
                              /*  (create_kuipxxx)                            */

/*  C definitions                                                             */
#define MAX_MENU_LEVEL   16   /*  max. number of menu levels                  */
#define MAX_parm         20   /*  max. number of command parameters           */
#define MAX_opt          40   /*  max. number of options for option menu      */
#define MAX_button       600  /*  max. number of buttons in a panel           */
                              /*  cf. KUIP : 30 lin. X 20 col.                */
#define MAX_string        80  /*  max. length for string                      */
#define MAX_stringl      240  /*  max. length for long string                 */

#endif /* _mkdims_ */

