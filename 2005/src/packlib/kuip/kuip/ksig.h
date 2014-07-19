/*
 * $Id: ksig.h,v 1.3 1997/11/28 17:27:14 mclareni Exp $
 *
 * $Log: ksig.h,v $
 * Revision 1.3  1997/11/28 17:27:14  mclareni
 * Numerous mods and some new routines to get Control-C working reasonably on NT
 *
 * Revision 1.2  1997/03/14 17:16:38  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1  1996/03/08 15:33:01  mclareni
 * Kuip
 *
 */
/*
 * 1997/11/28 17:16:38  V.Fine
 *  Some new members have been introduced into the kc_break struct
 *  Macro to set the exception handler routine for diff WIN32 platform
 *  was done
 */
/* ksig.h: signal and break handling */

/*
 * Available signal handling package
 *
 *   #define SIGNAL_POSIX  ==>  sigaction()  for Unix
 *   #define SIGNAL_BSD    ==>  sigvec()     for VMS and NeXT
 *   #define SIGNAL_V7     ==>  signal()
 */
#if !defined(SIGNAL_BSD) && !defined(SIGNAL_V7)
#  define SIGNAL_POSIX
#elif !defined(WIN32)
#  define sigjmp_buf          jmp_buf
#  define sigsetjmp(buf,save) setjmp(buf)
#  define siglongjmp(buf,val) longjmp(buf,val)
#  ifdef vms
#    define sv_flags sv_onstack
#  endif
#elif defined(WIN32)
#  include <float.h>
#  define sigjmp_buf          jmp_buf
   int  sigsetjmp(sigjmp_buf *buffer, int save);
   void siglongjmp (sigjmp_buf buffer, int val); 
   BOOL ConsoleSigHandler(DWORD sig);
#endif

EXTERN struct {
  int         trap_enabled;     /* flag if exceptions should be trapped */
  int         intr_enabled;     /* flag if ^C delivery is allowed */
  int         intr_pending;     /* flag if ^C happened while disabled */
  int         intr_count;       /* count number of consecutive ^C interrupts */
  int         traceback;        /* print traceback on signal */
#ifdef WIN32
  int         fperr;            /* type of floating-point exception under Windows  */
  HANDLE      hdThread;         /* The real handle of the thread to jump in        */
  unsigned int uRetAddr;        /* return address to the subroutine calling setjmp */
#endif
  char       *error_msg;        /* messages is handler cannot do print */
  int         soft_intr;        /* flag to stop at a convenient point */
  int         jump_set;         /* flag if stack has been setup */
  sigjmp_buf  stack;
  int         sockfd;           /* socket descriptor and routine to */
  void      (*piaf_sync)();     /* resynchronize Piaf communication */
} kc_break;

#ifdef WIN32
#  if defined(_X86_)
#    define PROGCTR(Context)  (Context.Eip)
#  endif

#  if defined(_MIPS_)
#    define PROGCTR(Context)  (Context.Fir)
#  endif

#  if defined(_ALPHA_)
#    define PROGCTR(Context)  (Context.Fir)
#  endif

#  if defined(_PPC_)
#    define PROGCTR(Context)  (Context.Iar)
#  endif

#  if !defined(PROGCTR)
#    error Module contains CPU-specific code; modify and recompile.
#  endif
#endif /* WIN32 */
