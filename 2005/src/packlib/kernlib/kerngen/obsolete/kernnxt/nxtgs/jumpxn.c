/*
 * $Id: jumpxn.c,v 1.1.1.1 1996/02/15 17:53:30 mclareni Exp $
 *
 * $Log: jumpxn.c,v $
 * Revision 1.1.1.1  1996/02/15 17:53:30  mclareni
 * Kernlib
 *
 */
#include "kernnxt/pilot.h"
/*>    ROUTINE JUMPXN
  CERN PROGLIB# Z042    JUMPXN          .VERSION KERNNXT  1.00  901105
  ORIG. 21/04/88 JZ+FCA
 
      To transfer to the user routine TARGET (say) with 2 parameters
      three steps are needed :
 
   1) EXTERNAL TARGET              to get the address of TARGET
      IADR = JUMPAD (TARGET)
 
   2) CALL JUMPST (IADR)           to set the tranfer address
 
   3) CALL JUMPX2 (par1,par2)      to transfer
*/
static void (*tarsub)();
/* ----   jumpst   ---------------------------------------------  */
#if defined(CERNLIB_QX_SC)
int jumpst_(iadr)
#endif
#if defined(CERNLIB_QXNO_SC)
int jumpst(iadr)
#endif
#if defined(CERNLIB_QXCAPT)
int JUMPST(iadr)
#endif
void (**iadr)();
{
   tarsub = *iadr;
}
/* ----   jumpxn   ---------------------------------------------  */
char *ipara, *iparb;
#if defined(CERNLIB_QX_SC)
int jumpx0_()
#endif
#if defined(CERNLIB_QXNO_SC)
int jumpx0()
#endif
#if defined(CERNLIB_QXCAPT)
int JUMPX0()
#endif
{
    (*tarsub)();
    return;
}
 
#if defined(CERNLIB_QX_SC)
int jumpx1_(ipara)
#endif
#if defined(CERNLIB_QXNO_SC)
int jumpx0(ipara)
#endif
#if defined(CERNLIB_QXCAPT)
int JUMPX1(ipara)
#endif
{
    (*tarsub)(ipara);
    return;
}
 
#if defined(CERNLIB_QX_SC)
int jumpx2_(ipara,iparb)
#endif
#if defined(CERNLIB_QXNO_SC)
int jumpx@(ipara,iparb)
#endif
#if defined(CERNLIB_QXCAPT)
int JUMPX2(ipara,iparb)
#endif
{
    (*tarsub)(ipara, iparb);
    return;
}
/* ----   jumpad   ---------------------------------------------  */
#if defined(CERNLIB_QX_SC)
void *jumpad_(ifun)
#endif
#if defined(CERNLIB_QXNO_SC)
void *jumpad(ifun)
#endif
#if defined(CERNLIB_QXCAPT)
void *JUMPAD(ifun)
#endif
char **ifun;
{
    return  *ifun;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_TCGEN_JUMPXN
#undef CERNLIB_TCGEN_JUMPXN
#endif
