#define IX11_HIGZ_MSG WM_USER+10 /* ID of HIGZ messages           */
                                 /*   +10 because WM_USER is used */
                                 /*       by WIN32 itself ! ! !   */
 enum HIGZ_Graphics_Msg {
                        HIGZ_Control, HIGZ_Primitive, HIGZ_Text   , HIGZ_Attribute,
                        HIGZ_Marker , HIGZ_Input    , HIGZ_Inquiry, HIGZ_Pixmap,
                        HIGZ_Dummies
                       };
/*     Emulation of X11 control HIGZ routines
**
**               HIGZ_Control set
*/

enum    L_HIGZ_Control
     {
        IX_OPNDS,      // Open X11 display
        IX_OPNWI,      // Open X11 window
        IX_SELWI,      // Select the current X11 window
        IX_CLSWI,      // Close an X11 window
        IX_CLSDS,      // Close an X11 session
        IX_SETHN,      // Set X11 host name
        IX_CLRWI,      // Clear an X11 window
        IX_UPDWI,      // Update an X11 window
        IX_RSCWI,      // Resize an X11 window
        IX_CLIP ,      // Define the X11 clipping rectangle
        IX_NOCLI       // Deactivate the X11 clipping rectangle
     };
#define ExtHIGZWindow 0
struct HIGZWindow {
       struct HIGZWindow  *ptbw;   // Previous Winodws in this list
       struct HIGZWindow  *ptfw;   // Next Windows in this list
       HWND hwnd;           // Handle of this Windows
       HDC  hdcWindow;      // Device context of display
       RECT HIGZWindowSize; /* size of the client area of the          */
                            /* window in logical untis                 */
       int  WinType;        // Type of HIGZ window
                            /* window variables for HIGZ text input fasility*/
       DWORD   dwCharX, dwCharY, dwAscent;

       HDC  hdcPixBuffer;   // Device context of the bitmap buffer
       HBITMAP hbBuffer;    // Bitmap handle for double buffering
       HDC  hdcHistory;     // History metafile DC
       HRGN HIGZClipRegion;  /* != NULL clip is ON and                  */
                             /* clip = (handle) RECT of the clip region */
       BOOL HIGZCursor,
            SystemCursorVisible,
            SetTextInput;
       int  MouseInit;       // Type of HIGZ mouse cursor or 0
       POINT loc, locp;      // Positions of HIGZ graphics locator
       int   button_press, button_up;
       BOOL  isopening;      /* HIGZ window is restoring its own position
                              * from icon state
                              */
       HCURSOR hHIGZCursor;
       } HIGZWindow;


HWND CreateTextClass(struct CREATEHIGZWINDOW *);
struct CREATEHIGZWINDOW { int x,y,w,h,lentit,flag; char *title;HANDLE semaphore;
                          HWND hwnd;};
#define HIGZCLASS "HIGZ"
#define WHITE_HIGZ_COLOR HIGZColorIndex(0)

/*   ID of the current thread manageing main windows loop. */
struct {DWORD ID_Thread;} IDStruct_;
#define IDThread (IDStruct_.ID_Thread)
char*  string(char *, int);
LRESULT APIENTRY HIGZ_Act(HWND, UINT, WPARAM, LPARAM);
LRESULT APIENTRY Wnd_S2BUF(HWND, UINT, WPARAM, LPARAM);
LRESULT APIENTRY Wnd_CPPX(HWND, UINT, WPARAM, LPARAM);
LRESULT APIENTRY Wnd_WIPX(HWND, UINT, WPARAM, LPARAM);
LRESULT APIENTRY Wnd_MARKE(HWND, UINT, WPARAM, LPARAM);
LRESULT APIENTRY Wnd_FLARE(HWND, UINT, WPARAM, LPARAM);
LRESULT APIENTRY Wnd_BOX  (HWND, UINT, WPARAM, LPARAM);
LRESULT APIENTRY Wnd_CA   (HWND, UINT, WPARAM, LPARAM);
LRESULT APIENTRY Wnd_REQLO(HWND, UINT, WPARAM, LPARAM);
LRESULT APIENTRY Wnd_REQST(HWND, UINT, WPARAM, LPARAM);
LPTHREAD_START_ROUTINE HIGZ_MsgLoop(HANDLE);
