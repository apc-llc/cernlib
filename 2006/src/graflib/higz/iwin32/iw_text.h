#define TEXTCLASS "HIGZTEXT"

#define IDM_EDCOPY   1
#define ID_EDITCHILD 1
#define TEXT_BUFFER 2048

LRESULT APIENTRY WndText(HWND, UINT, WPARAM, LPARAM);

typedef struct tagTextWindow {
       struct TextWindow  *ptbw;   // Previous Winodws in this list
       struct TextWindow  *ptfw;   // Next Windows in this list
       HWND hwnd;                  // Handle of this Windows
       HDC  hdcWindow;             // Device context of display
       RECT HIGZWindowSize;        // size of the client area of the 
                                   // window in logical untis
       int  WinType;               // Type of HIGZ window
                                   // window variables for HIGZ text input facility
       DWORD   dwCharX, dwCharY, dwAscent;
       } TextWindow;
