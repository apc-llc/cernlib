/*
 * $Id: qnexte.c,v 1.1.1.1 1996/02/15 17:53:31 mclareni Exp $
 *
 * $Log: qnexte.c,v $
 * Revision 1.1.1.1  1996/02/15 17:53:31  mclareni
 * Kernlib
 *
 */
#include "kernnxt/pilot.h"
/*>    ROUTINE QNEXTE
  CERN PROGLIB# Z041    QNEXTE          .VERSION KERNNXT  1.00  901105
  */
#include <setjmp.h>
#if defined(CERNLIB_QX_SC)
void qnext_();
static void (* unext_)() = 0;
static long init_ = 1;
static jmp_buf env_; 
void qnset_(ufun_)
void (* ufun_)();
{
   unext_ = ufun_;
   init_ = 1;
   return;
}
#endif
#if defined(CERNLIB_QXNO_SC)
void qnext();
static void (* unext)() = 0;
static long init = 1;
static jmp_buf env;
void qnset(ufun)
void (* ufun)();
{
   unext = ufun;
   init = 1;
   return;
}
#endif
#if defined(CERNLIB_QXCAPT)
void QNEXT()
static void (* UNEXT)() = 0;
static long INIT = 1;
static jmp_buf ENV;
void QNSET(UFUN)
void (* UFUN)();
{
   UNEXT = UFUN;
   INIT = 1;
   return;
}
#endif
 
#if defined(CERNLIB_QX_SC)
void qnexte_()
{
if (init_)
{
   if (unext_ == 0) unext_ = qnext_;
   init_ = 0;
   setjmp(env_);
   (* unext_)();
}
else
   longjmp(env_,1);
}
#endif
#if defined(CERNLIB_QXNO_SC)
void qnexte()
{
if (init)
{
   if (unext == 0) unext = qnext;
   init = 0;
   setjmp(env);
   (* unext)();
}
else
   longjmp(env,1);
}
#endif
#if defined(CERNLIB_QXCAPT)
void QNEXTE()
{
if (INIT)
{
   if (UNEXT == 0) UNEXT = QNEXT;
   INIT = 0;
   setjmp(ENV);
   (* UNEXT)();
}
else
   longjmp(ENV,1);
}
#endif
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_TCGEN_QNEXTE
#undef CERNLIB_TCGEN_QNEXTE
#endif
