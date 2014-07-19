#define CERNLIB_QFMSOFT
#define CERNLIB_QXCAPT
#define type_of_call _stdcall

#include <windows.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

#include "ix_win32.h"
#include "iw_text.h"

#ifdef CERNLIB_QX_NC
#define igxmes  igxmes_
#define ixopnwi ixopnwi_
#endif

#ifdef CERNLIB_QXCAPT
#define igxmes  type_of_call IGXMES
#define ixopnwi type_of_call IXOPNWI
#endif

static BOOL open = {FALSE};

int lenocc(char *c,int l)
 {
   int i;
   for (i=l-1;i>=0 && c[i] == " ";i--);
   return i+1;
  }


/*_______________________________________________________________*/
/*  This is a pool of routines to present text in Windows/NT */

#ifndef CERNLIB_QFMSOFT
void igxmes(ix, iy, nc, n, chmess, chtit, chopt, chmess_len,
	chtit_len, chopt_len)
#else
void igxmes(ix, iy, nc, n, chmess, chmess_len, 
                      chtit,  chtit_len,
		      chopt,  chopt_len)
#endif

int *ix, *iy, *nc, *n;
char *chmess, *chtit, *chopt;
int chmess_len;
int chtit_len;
int chopt_len;
{
 char *title;
 static HWND hwnd, hwndEdit;
 struct CREATEHIGZWINDOW WinThr;
 static RECT FmtRect;
 char *temp;
 int i,j,k, sw;

 sw=0; for(i=0;i<chopt_len;i++) if (*(chopt+i) == 'P') sw = i+1;

 if (sw) {
   if (!open) {

     WinThr.x = *ix;
     WinThr.y = *iy;
     WinThr.w = 180;

     WinThr.h = 160;

     FmtRect.left = 10;
     FmtRect.right = WinThr.w - FmtRect.left - 10;
     FmtRect.top   = 0;
     FmtRect.bottom = WinThr.h;

     WinThr.lentit = lenocc(chtit,chtit_len)+1;
     title = malloc( WinThr.lentit );
     memcpy(title,chtit,WinThr.lentit-1);
     title[WinThr.lentit-1] = (char)NULL;
     WinThr.title = title;
     WinThr.flag = 2;


      /* Create HIGZ  message window */

     WinThr.semaphore = CreateSemaphore(NULL, 0, 1, NULL);

     while(!PostThreadMessage(IDThread,
           IX11_HIGZ_MSG,MAKEWPARAM(IX_OPNWI,HIGZ_Control), &WinThr));

     WaitForSingleObject(WinThr.semaphore, INFINITE);
     CloseHandle(WinThr.semaphore);
     free(title);

     hwnd     = WinThr.hwnd;
     hwndEdit = (HWND)GetWindowLong(hwnd,0);

     ShowWindow(WinThr.hwnd,SW_SHOWDEFAULT);
     UpdateWindow(WinThr.hwnd);
   }
/* Allocate intermediate  buffer for whole page */
   j = (chmess_len+2)*(*n);    /* Two bytes should be reserved to intend "hard end of line" */
   title = malloc(j+1);
/* Copy a user message to itermediate buffer    */
   j = 0; k=0;
   for (i=0;i<*n;i++) {
     memcpy(&title[j],&chmess[k],chmess_len); /* Copy one string */
     j += lenocc(&chmess[k],chmess_len);      /* Truncate it     */
     title[j] = (char)13;                     /* insert "hard end-of-line" */
     j++;
     title[j] = (char)10;
     j++;
     title[j] = (char)0;
     k += chmess_len;
    }


    if (!open) {
          SendMessage(hwndEdit, EM_SETRECT, 0, &FmtRect);
          open = TRUE;
    }  
    i = SendMessage(hwndEdit, EM_GETFIRSTVISIBLELINE, 0, 0);
    SendMessage(hwndEdit,WM_SETTEXT,0,(LPARAM) title);
    if (i)  SendMessage(hwndEdit,EM_LINESCROLL,0, (LPARAM) i);
    free(title);
  }
  sw=0; for(i=0;i<chopt_len;i++) if (chopt[i] == 'C') sw = 1;
  if (sw) {
  if (open) {
     SendMessage(hwnd,WM_CLOSE, 0, 0);
     open = FALSE;
  }
 }
 sw=0; for(i=0;i<chopt_len;i++) if (chopt[i] == 'D') sw = 1;
 if (sw)  SendMessage(hwndEdit,WM_SETTEXT,0, 0);
 }

/*_______________________________________________________________*/
HWND CreateTextClass(WinThr)
  struct CREATEHIGZWINDOW *WinThr;
  {
   HWND  hwndHIGZ;
   HMENU hmenu;
   WNDCLASS  text_display;

/* Make sure that this window hasn't been registered yet */

  if (!GetClassInfo(GetModuleHandle(NULL),TEXTCLASS,&text_display)) {

  /* Set the common wndClass information. This is common for all windows
  ** of this application.
  */

   text_display.style      = CS_HREDRAW | CS_VREDRAW;
   text_display.cbClsExtra = 0;

   text_display.cbWndExtra = 4;

   text_display.hCursor    = LoadCursor(NULL, IDC_IBEAM);
   text_display.hInstance  = GetModuleHandle (NULL);

  /*
  ** Reister the main top-level window
  */

   text_display.lpfnWndProc   = (WNDPROC) WndText;
   text_display.hIcon         = LoadIcon(NULL, IDI_APPLICATION);
   text_display.hbrBackground = GetStockObject(WHITE_BRUSH);
   text_display.lpszMenuName   = NULL;
   text_display.lpszClassName  = TEXTCLASS;

   if (!RegisterClass(&text_display)) return -1;
  }
/* Create "frame" window */
   hmenu = CreateMenu();
   AppendMenu(hmenu,MF_STRING,IDM_EDCOPY,"&Copy");
   hwndHIGZ = CreateWindowEx(
              WS_EX_TOPMOST,
              TEXTCLASS,                 /*extended window style                       */
              string(WinThr->title,WinThr->lentit),   /*address of window name                      */
//=              WS_OVERLAPPED | WS_VISIBLE |
//=              WS_THICKFRAME,
              WS_OVERLAPPEDWINDOW,
 //            | WS_VSCROLL | WS_HSCROLL,                /* Window style   */
              WinThr->x, WinThr->y,                   // *x, *y,
              WinThr->w, WinThr->h,                   // *w, *h,
              NULL,                     /*handle of parent of owner window            */
              hmenu,                    /*handle of menu, or child-window identifier  */
              text_display.hInstance,   /*handle of application instance              */
              NULL);                    /*address of window-creation data             */
    return  hwndHIGZ;
}

/*_______________________________________________________________*/
LRESULT APIENTRY WndText(hwnd, uMsg, wParam, lParam)
HWND   hwnd;
UINT   uMsg;
WPARAM wParam;
LPARAM lParam;
{
    static HWND hwndEdit;

    switch (uMsg) {
       case WM_CREATE:
           hwndEdit = CreateWindow(
                            "EDIT",                /* Predefined class */
                             NULL,                 /* no window title  */
                             WS_CHILD | WS_VISIBLE | WS_VSCROLL |
                             ES_LEFT | ES_MULTILINE | ES_AUTOVSCROLL,
                             0,0,0,0,   /* set size in WM_SIZE message */
                             hwnd,              /* parent window */
                             (HMENU) ID_EDITCHILD , /* edit control ID */
                             (HINSTANCE) GetWindowLong(hwnd, GWL_HINSTANCE),
                             NULL);                  /* pointer not need */
           SetWindowLong(hwnd,0,  hwndEdit);
           break;
         case WM_COMMAND:
           switch (wParam) {
            case IDM_EDCOPY:
               SendMessage(hwndEdit, WM_COPY, 0, 0);
               break;
             default:
               return DefWindowProc(hwnd,uMsg,wParam,lParam);
            }
          break;

          case WM_SETFOCUS:
            SetFocus(hwndEdit);
            break;

          case WM_SIZE:

           /*
            * Make the edit control the size of the window's
            * client area
            */

            MoveWindow(hwndEdit,
                       0,0,  /* Starting x- and y-coordinates   */
                       LOWORD(lParam), /* width of client area  */
                       HIWORD(lParam), /* height of client area */
                       TRUE);          /* repaint window        */
            break;

           case WM_DESTROY:
             open = FALSE;
             break;
           default: break;

    }
    return DefWindowProc(hwnd,uMsg,wParam,lParam);
 }

