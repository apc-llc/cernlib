/*
 * $Id: jumpcn.c,v 1.2 1997/03/14 12:02:25 mclareni Exp $
 *
 * $Log: jumpcn.c,v $
 * Revision 1.2  1997/03/14 12:02:25  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:34:44  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/02/26 17:16:55  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
/*CMZ :  1.18/03 11/05/94  15.54.01  by  Rene Brun*/
/*-- Author :*/
/*>    ROUTINE jumpcN
  CERN PROGLIB# Z043    jumpcN          .VERSION KERNFOR  4.36  930602
  ORIG. 21/04/88 JZ+FCA
C
C-    To transfer to the user routine TARGET (say) with 2 parameters
C-    two steps are needed :

C- 1) EXTERNAL TARGET              to get the address of TARGET
C-    IADR = JUMPAD (TARGET)

C- 3) CALL jumpc2 (IADR,par1,par2)      to transfer
*/

#if defined(CERNLIB_QCCINDAD)
#define IADR *iadr
#endif

#if !defined(CERNLIB_QCCINDAD)
#define IADR iadr
#endif

#if defined(CERNLIB_QX_SC)
#define jumpc0 jumpc0_
#define jumpc1 jumpc1_
#define jumpc2 jumpc2_
#define jumpc3 jumpc3_
#define jumpc4 jumpc4_
#define jumpc5 jumpc5_
#endif
#if defined(CERNLIB_QXCAPT)
#define jumpc0 JUMPC0
#define jumpc1 JUMPC1
#define jumpc2 JUMPC2
#define jumpc3 JUMPC3
#define jumpc4 JUMPC4
#define jumpc5 JUMPC5
#endif
void type_of_call jumpc0(iadr)
     void (type_of_call **IADR)();
{
    (**IADR)();
    return;
}

void type_of_call jumpc1(iadr,ipara)
     void (type_of_call **IADR)();
     char *ipara;
{
    (**IADR)(ipara);
    return;
}

void type_of_call jumpc2(iadr, ipara, iparb)
     void (type_of_call **IADR)();
     char *ipara, *iparb;
{
    (**IADR)(ipara, iparb);
    return;
}
void type_of_call jumpc3(iadr, ipara, iparb, iparc)
     void (type_of_call **IADR)();
     char *ipara, *iparb, *iparc;
{
    (**IADR)(ipara, iparb, iparc);
    return;
}
void type_of_call jumpc4(iadr, ipara, iparb, iparc, ipard)
     void (type_of_call **IADR)();
     char *ipara, *iparb, *iparc, *ipard;
{
    (**IADR)(ipara, iparb, iparc, ipard);
    return;
}
void type_of_call jumpc5(iadr, ipara, iparb, iparc, ipard,ipare)
     void (type_of_call **IADR)();
     char *ipara, *iparb, *iparc, *ipard, *ipare;
{
    (**IADR)(ipara, iparb, iparc, ipard, ipare);
    return;
}
/*> END <----------------------------------------------------------*/
