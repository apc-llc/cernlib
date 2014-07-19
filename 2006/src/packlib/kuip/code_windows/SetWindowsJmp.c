/*
 * $Id: SetWindowsJmp.c,v 1.1 1997/11/28 17:27:03 mclareni Exp $
 *
 * $Log: SetWindowsJmp.c,v $
 * Revision 1.1  1997/11/28 17:27:03  mclareni
 * Numerous mods and some new routines to get Control-C working reasonably on NT
 *
 */
#include "kuip/kuip.h"
#include "kuip/ksig.h"

int iFlag = 0;
sigjmp_buf SIGbuffer;

# define iStackLength  14
DWORD retaddress[iStackLength];
DWORD *iSp;

//___________________________________________________
// Terminate the worker thread by getting it to execute this function 
static void WINAPI SwitchMainThread (void) 
{
   sigsetjmp(&SIGbuffer, 0 );
}

//___________________________________________________
static void swap_signal_handler(sigjmp_buf buffer, int val)
{
  CONTEXT context;

  SuspendThread(kc_break.hdThread);

  // Get the worker thread's current CPU registers
  context.ContextFlags = CONTEXT_CONTROL;
  GetThreadContext(kc_break.hdThread, &context);

  // Change the instruction pointer to our function
  PROGCTR(context) = (DWORD) SwitchMainThread;
  SetThreadContext(kc_break.hdThread,&context);

//***  iFlag = val;
  iFlag = -2;
  // Resuming the thread forces our function to be called
  ResumeThread(kc_break.hdThread);
}

//______________________________________________________________________________
BOOL ConsoleSigHandler(DWORD sig)
{
 
 // WinNT signal handler for Console events (by V.Fine 19.12.96 JINR, Dubna)
 
  switch (sig) {
  case CTRL_BREAK_EVENT:
       ++kc_break.intr_count;
  case CTRL_C_EVENT:
  case CTRL_LOGOFF_EVENT:
  case CTRL_SHUTDOWN_EVENT:
  case CTRL_CLOSE_EVENT:
//    signal_handler( SIGINT );
      if (kc_break.intr_enabled) 
          swap_signal_handler(SIGbuffer,sig);
      else 
          signal_handler( SIGINT );
      return TRUE;
  default:
      return FALSE;
  }
}

//___________________________________________________
void siglongjmp (sigjmp_buf buffer, int val) {
//    int i = sigsetjmp(&SIGbuffer, 0 );
    longjmp(kc_break.stack, val );
}

//___________________________________________________
int sigsetjmp(sigjmp_buf *buffer, int saveflag)
{
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*  return value:
//*-*  ------------
//*-*         returns 0 after saving the stack environment. 
//*-*                   If setjmp returns as a result of a longjmp call, it 
//*-*         returns the value argument of longjmp, or if the value argument
//*-*                   of longjmp is 0, setjmp returns 1. There is no error 
//*-*                   return.
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
  static sigjmp_buf *lpBuffer = &SIGbuffer;
  static int iret;
  static int save;
  save = saveflag;

  if (save == 0) {
//      fpreset();
      fflush(NULL);
      longjmp(kc_break.stack, iFlag );
  }
  // Preserve the return address
  __asm {
          mov iSp, ebp
  }
  memcpy (retaddress, (iSp+1)-iStackLength+1,iStackLength*sizeof(DWORD)); 
//  memcpy (retaddress,  iSp-iStackLength+1,iStackLength*sizeof(DWORD)); 
  
  kc_break.uRetAddr = retaddress;

  iFlag = 0;
  // Polling the current thread handle 
  DuplicateHandle(
      GetCurrentProcess(),             // handle to process with handle to duplicate 
      GetCurrentThread(),              // handle to duplicate 
      GetCurrentProcess(),             // handle to process to duplicate to 
      &(kc_break.hdThread),            // pointer to duplicate handle 
      (void *)0,                       // access for duplicate handle 
      FALSE,                           // handle inheritance flag 
      DUPLICATE_SAME_ACCESS            // optional actions 
      ); 
        
  iret = setjmp(kc_break.stack);

  // Restore the return address

  if (iret) {
//     printf(" longjmp sigsetjmp(sigjmp_buf *buffer, int save = %d) \n", save);
     memcpy ((iSp+1)-iStackLength+1,retaddress,iStackLength*sizeof(DWORD)); 
//   memcpy (iSp-iStackLength+1,retaddress,iStackLength*sizeof(DWORD)); 
  }

  return iret;
}
